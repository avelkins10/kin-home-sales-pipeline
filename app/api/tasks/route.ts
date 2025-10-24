export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import {
  PROJECT_FIELDS,
  TASK_GROUP_FIELDS,
  TASK_FIELDS,
  TASK_SUBMISSION_FIELDS
} from '@/lib/constants/fieldIds';
import { Task, TaskSubmission } from '@/lib/types/task';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { sql } from '@/lib/db/client';

// Helper functions to avoid import issues
async function getUserEmail(userId: string): Promise<string | null> {
  try {
    const result = await sql`
      SELECT email
      FROM users
      WHERE id = ${userId}
      AND email IS NOT NULL
    `;
    return result.rows.length > 0 ? result.rows[0].email : null;
  } catch (error) {
    logError('Failed to get user email', error as Error);
    return null;
  }
}

async function getManagedUserEmails(managerId: string): Promise<string[]> {
  try {
    const result = await sql`
      SELECT u.email
      FROM user_hierarchies uh
      JOIN users u ON uh.user_id = u.id
      WHERE uh.manager_id = ${managerId}
      AND u.email IS NOT NULL
    `;
    return result.rows.map(row => row.email).filter(Boolean);
  } catch (error) {
    logError('Failed to get managed user emails', error as Error);
    return [];
  }
}

async function getAssignedOffices(userId: string): Promise<number[]> {
  try {
    const result = await sql`
      SELECT o.quickbase_office_id
      FROM office_assignments oa
      JOIN offices o ON oa.office_name = o.name
      WHERE oa.user_id = ${userId}
        AND o.quickbase_office_id IS NOT NULL
    `;
    return result.rows.map(row => row.quickbase_office_id);
  } catch (error) {
    logError('Failed to get assigned offices', error as Error, { userId });
    return [];
  }
}

function isManagerRole(role: string): boolean {
  return ['office_leader', 'area_director', 'divisional', 'regional', 'team_lead', 'super_admin'].includes(role);
}

interface TaskWithProject extends Task {
  projectId: number;
  projectName: string;
  projectStatus: string;
  customerName: string;
}

/**
 * GET /api/tasks
 * Get all tasks for the current user across all their accessible projects
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('GET', '/api/tasks', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userId = (auth.session.user as any).id as string;
    const userRole = auth.session.user.role;

    // Get user email from database (required for email-based filtering)
    let userEmail: string | null = null;
    if (['closer', 'setter', 'coordinator'].includes(userRole) || isManagerRole(userRole)) {
      userEmail = await getUserEmail(userId);
    }

    // Get managed user emails for team leads
    let managedEmails: string[] | undefined;
    if (userRole === 'team_lead') {
      managedEmails = await getManagedUserEmails(userId);
    }

    // Get assigned office IDs for office-based roles
    let effectiveOfficeIds: number[] | undefined;
    if (['office_leader', 'area_director', 'divisional', 'regional'].includes(userRole)) {
      effectiveOfficeIds = await getAssignedOffices(userId);
      logInfo('[TASKS_API] Fetched offices from database', { userId, role: userRole, officeCount: effectiveOfficeIds?.length || 0, reqId });
    }

    // Step 1: Get accessible projects for this user
    const QB_TABLE_PROJECTS = (process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na').trim();
    const QB_TABLE_TASK_GROUPS = (process.env.QUICKBASE_TABLE_TASK_GROUPS || 'bu36gem4p').trim();
    const QB_TABLE_TASKS = (process.env.QUICKBASE_TABLE_TASKS || 'bu36ggiht').trim();
    const QB_TABLE_TASK_SUBMISSIONS = (process.env.QUICKBASE_TABLE_TASK_SUBMISSIONS || 'bu36g8j99').trim();
    const projectAccessClause = buildProjectAccessClause(userEmail, userRole, effectiveOfficeIds, managedEmails, reqId);

    logInfo('[TASKS_API] Generated WHERE clause', {
      clause: projectAccessClause,
      clauseLength: projectAccessClause?.length || 0,
      userEmail: userEmail ? 'present' : 'null',
      role: userRole,
      reqId
    });

    logInfo('[TASKS_API] Using table IDs', {
      QB_TABLE_PROJECTS,
      QB_TABLE_TASK_GROUPS,
      QB_TABLE_TASKS,
      QB_TABLE_TASK_SUBMISSIONS,
      reqId
    });

    // TEMPORARY DEBUG: Just try the first query
    console.log('[TASKS_API] About to query QuickBase with:', {
      table: QB_TABLE_PROJECTS,
      whereClause: projectAccessClause,
      selectFields: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.CUSTOMER_NAME,
        PROJECT_FIELDS.PROJECT_STATUS
      ]
    });

    let projectsResponse;
    try {
      projectsResponse = await qbClient.queryRecords({
        from: QB_TABLE_PROJECTS,
        select: [
          PROJECT_FIELDS.RECORD_ID,
          PROJECT_FIELDS.PROJECT_ID,
          PROJECT_FIELDS.CUSTOMER_NAME,
          PROJECT_FIELDS.PROJECT_STATUS
        ],
        where: projectAccessClause
      });

      console.log('[TASKS_API] QuickBase query succeeded, got', projectsResponse.data?.length, 'projects');
    } catch (queryError: any) {
      console.error('[TASKS_API] QuickBase query FAILED:', {
        errorMessage: queryError?.message,
        errorStack: queryError?.stack,
        whereClause: projectAccessClause,
        tableId: QB_TABLE_PROJECTS
      });

      logError('Projects query failed', queryError as Error, {
        whereClause: projectAccessClause,
        tableId: QB_TABLE_PROJECTS,
        errorMessage: queryError?.message,
        errorDetails: queryError?.response?.data || queryError?.response || 'no response data',
        reqId
      });

      // Return a simple error response for now
      return NextResponse.json({
        error: 'QuickBase Query Failed',
        message: queryError?.message || 'Unknown error',
        whereClause: projectAccessClause,
        table: QB_TABLE_PROJECTS
      }, { status: 500 });
    }

    if (projectsResponse.data.length === 0) {
      logApiResponse('GET', '/api/tasks', Date.now() - startedAt, { tasks: 0 }, reqId);
      return NextResponse.json([], { status: 200 });
    }

    const projectIds = projectsResponse.data.map((p: any) => p[PROJECT_FIELDS.RECORD_ID]?.value).filter(Boolean);

    // Create project lookup map
    const projectMap = new Map();
    projectsResponse.data.forEach((p: any) => {
      const recordId = p[PROJECT_FIELDS.RECORD_ID]?.value;
      if (recordId) {
        projectMap.set(recordId, {
          projectId: p[PROJECT_FIELDS.PROJECT_ID]?.value || recordId,
          customerName: p[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown',
          projectStatus: p[PROJECT_FIELDS.PROJECT_STATUS]?.value || ''
        });
      }
    });

    // Step 2: Get task groups for these projects
    const taskGroupsWhereClause = projectIds
      .map((id: number) => `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${id}}`)
      .join('OR');

    const taskGroupsResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_GROUPS,
      select: [
        TASK_GROUP_FIELDS.RECORD_ID,
        TASK_GROUP_FIELDS.RELATED_PROJECT
      ],
      where: taskGroupsWhereClause
    });

    if (taskGroupsResponse.data.length === 0) {
      logApiResponse('GET', '/api/tasks', Date.now() - startedAt, { tasks: 0 }, reqId);
      return NextResponse.json([], { status: 200 });
    }

    // Map task groups to projects
    const taskGroupToProjectMap = new Map();
    taskGroupsResponse.data.forEach((tg: any) => {
      const groupId = tg[TASK_GROUP_FIELDS.RECORD_ID]?.value;
      const projectId = tg[TASK_GROUP_FIELDS.RELATED_PROJECT]?.value;
      if (groupId && projectId) {
        taskGroupToProjectMap.set(groupId, projectId);
      }
    });

    const taskGroupIds = taskGroupsResponse.data
      .map((tg: any) => tg[TASK_GROUP_FIELDS.RECORD_ID]?.value)
      .filter(Boolean);

    // Step 3: Get tasks for these task groups
    const tasksWhereClause = taskGroupIds
      .map((id: number) => `{${TASK_FIELDS.TASK_GROUP}.EX.${id}}`)
      .join('OR');

    const tasksResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASKS,
      select: [
        TASK_FIELDS.RECORD_ID,
        TASK_FIELDS.DATE_CREATED,
        TASK_FIELDS.DATE_MODIFIED,
        TASK_FIELDS.TASK_GROUP,
        TASK_FIELDS.STATUS,
        TASK_FIELDS.NAME,
        TASK_FIELDS.DESCRIPTION,
        TASK_FIELDS.MAX_SUBMISSION_STATUS,
        TASK_FIELDS.TASK_TEMPLATE,
        TASK_FIELDS.TASK_CATEGORY,
        TASK_FIELDS.TASK_MISSING_ITEM,
        TASK_FIELDS.REVIEWED_BY_OPS,
        TASK_FIELDS.REVIEWED_BY_OPS_USER,
        TASK_FIELDS.OPS_REVIEW_NOTE
      ],
      where: tasksWhereClause
    });

    // Step 4: Get submissions for these tasks (for urgency calculation)
    const taskIds = tasksResponse.data
      .map((t: any) => t[TASK_FIELDS.RECORD_ID]?.value)
      .filter(Boolean);

    let submissionsMap = new Map<number, TaskSubmission[]>();

    if (taskIds.length > 0) {
      const submissionsWhereClause = taskIds
        .map((id: number) => `{${TASK_SUBMISSION_FIELDS.RELATED_TASK}.EX.${id}}`)
        .join('OR');

      const submissionsResponse = await qbClient.queryRecords({
        from: QB_TABLE_TASK_SUBMISSIONS,
        select: [
          TASK_SUBMISSION_FIELDS.RECORD_ID,
          TASK_SUBMISSION_FIELDS.DATE_CREATED,
          TASK_SUBMISSION_FIELDS.RELATED_TASK,
          TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS,
          TASK_SUBMISSION_FIELDS.OPS_DISPOSITION,
          TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1,
          TASK_SUBMISSION_FIELDS.IS_MAX_SUBMISSION
        ],
        where: submissionsWhereClause
      });

      submissionsResponse.data.forEach((s: any) => {
        const taskId = s[TASK_SUBMISSION_FIELDS.RELATED_TASK]?.value;
        if (!taskId) return;

        const submission: TaskSubmission = {
          recordId: s[TASK_SUBMISSION_FIELDS.RECORD_ID]?.value || 0,
          dateCreated: s[TASK_SUBMISSION_FIELDS.DATE_CREATED]?.value || '',
          relatedTask: taskId,
          submissionStatus: s[TASK_SUBMISSION_FIELDS.SUBMISSION_STATUS]?.value || 'Pending Approval',
          opsDisposition: s[TASK_SUBMISSION_FIELDS.OPS_DISPOSITION]?.value || null,
          fileAttachment1: s[TASK_SUBMISSION_FIELDS.FILE_ATTACHMENT_1]?.value || null,
          isMaxSubmission: s[TASK_SUBMISSION_FIELDS.IS_MAX_SUBMISSION]?.value || false
        };

        if (!submissionsMap.has(taskId)) {
          submissionsMap.set(taskId, []);
        }
        submissionsMap.get(taskId)!.push(submission);
      });

      // Sort submissions by date desc
      submissionsMap.forEach((submissions) => {
        submissions.sort((a, b) => new Date(b.dateCreated).getTime() - new Date(a.dateCreated).getTime());
      });
    }

    // Step 5: Build tasks with project context
    const tasksWithProject: TaskWithProject[] = tasksResponse.data.map((record: any) => {
      const taskGroupId = record[TASK_FIELDS.TASK_GROUP]?.value;
      const projectId = taskGroupToProjectMap.get(taskGroupId);
      const projectInfo = projectMap.get(projectId) || {
        projectId: projectId,
        customerName: 'Unknown',
        projectStatus: ''
      };

      const taskId = record[TASK_FIELDS.RECORD_ID]?.value || 0;

      return {
        recordId: taskId,
        dateCreated: record[TASK_FIELDS.DATE_CREATED]?.value || null,
        dateModified: record[TASK_FIELDS.DATE_MODIFIED]?.value || null,
        taskGroup: taskGroupId || 0,
        status: record[TASK_FIELDS.STATUS]?.value || 'Not Started',
        name: record[TASK_FIELDS.NAME]?.value || '',
        description: record[TASK_FIELDS.DESCRIPTION]?.value || null,
        maxSubmissionStatus: record[TASK_FIELDS.MAX_SUBMISSION_STATUS]?.value || null,
        taskTemplate: record[TASK_FIELDS.TASK_TEMPLATE]?.value || null,
        category: record[TASK_FIELDS.TASK_CATEGORY]?.value || null,
        missingItem: record[TASK_FIELDS.TASK_MISSING_ITEM]?.value || null,
        reviewedByOps: record[TASK_FIELDS.REVIEWED_BY_OPS]?.value || null,
        reviewedByOpsUser: record[TASK_FIELDS.REVIEWED_BY_OPS_USER]?.value || null,
        opsReviewNote: record[TASK_FIELDS.OPS_REVIEW_NOTE]?.value || null,
        submissions: submissionsMap.get(taskId) || [],
        projectId,
        projectName: projectInfo.customerName,
        projectStatus: projectInfo.projectStatus,
        customerName: projectInfo.customerName
      };
    });

    // Log response
    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/tasks', duration, {
      tasks: tasksWithProject.length,
      projects: projectIds.length
    }, reqId);

    return NextResponse.json(tasksWithProject, { status: 200 });

  } catch (error) {
    // Log detailed error information for debugging
    const errorDetails: any = {
      message: (error as Error).message,
      stack: (error as Error).stack,
      reqId
    };

    // If error has additional properties (like from QuickBase API), include them
    if (error && typeof error === 'object') {
      Object.keys(error).forEach(key => {
        if (key !== 'stack' && key !== 'message') {
          errorDetails[key] = (error as any)[key];
        }
      });
    }

    logError('Failed to fetch tasks', error as Error, errorDetails);

    // TEMPORARY: Always return error details for debugging
    return NextResponse.json({
      error: 'Internal Server Error',
      details: (error as Error).message,
      errorName: (error as Error).name,
      stack: (error as Error).stack?.split('\n').slice(0, 10),
      // Include any additional error properties
      ...(error && typeof error === 'object' && {
        originalError: (error as any).originalError,
        whereClause: (error as any).whereClause,
        tableId: (error as any).tableId,
        quickbaseError: (error as any).quickbaseError,
        requestParams: (error as any).requestParams,
        statusCode: (error as any).statusCode
      })
    }, { status: 500 });
  }
}
