export const runtime = 'nodejs';
// Increase timeout to 60 seconds for complex task queries with multiple QuickBase API calls
export const maxDuration = 60;

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
    const result = await sql.query(
      'SELECT email FROM users WHERE id = $1 AND email IS NOT NULL',
      [userId]
    );
    return result.rows.length > 0 ? result.rows[0].email : null;
  } catch (error) {
    logError('Failed to get user email', error as Error, { userId });
    return null;
  }
}

async function getManagedUserEmails(managerId: string): Promise<string[]> {
  try {
    const result = await sql.query(`
      SELECT u.email
      FROM user_hierarchies uh
      JOIN users u ON uh.user_id = u.id
      WHERE uh.manager_id = $1
      AND u.email IS NOT NULL
    `, [managerId]);
    return result.rows.map(row => row.email).filter(Boolean);
  } catch (error) {
    logError('Failed to get managed user emails', error as Error, { managerId });
    return [];
  }
}

async function getAssignedOffices(userId: string): Promise<number[]> {
  try {
    const result = await sql.query(`
      SELECT o.quickbase_office_id
      FROM office_assignments oa
      JOIN offices o ON oa.office_name = o.name
      WHERE oa.user_id = $1
        AND o.quickbase_office_id IS NOT NULL
    `, [userId]);
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
  const TIMEOUT_WARNING_MS = 50000; // Warn if we're approaching 60s timeout
  const TIMEOUT_ERROR_MS = 58000; // Error out at 58s to avoid Vercel 504

  logApiRequest('GET', '/api/tasks', undefined, reqId);

  // Helper to check if we're running out of time
  const checkTimeout = () => {
    const elapsed = Date.now() - startedAt;
    if (elapsed > TIMEOUT_ERROR_MS) {
      throw new Error(`Request timeout: operation took ${elapsed}ms, exceeding ${TIMEOUT_ERROR_MS}ms limit`);
    }
    if (elapsed > TIMEOUT_WARNING_MS) {
      logInfo('[TASKS_API] Approaching timeout', { elapsed, reqId });
    }
    return elapsed;
  };

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

      // Critical check: Closers and setters MUST have an email
      if (!userEmail && ['closer', 'setter'].includes(userRole)) {
        logError('User has no email in database', new Error('Missing email for rep role'), {
          userId,
          role: userRole,
          reqId
        });
        return NextResponse.json({
          error: 'User Configuration Error',
          message: 'Your account is missing required information. Please contact an administrator.',
          details: 'Email address is required for your role but is not configured in the database.'
        }, { status: 500 });
      }
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

    // Parse query parameters for filtering
    const { searchParams } = new URL(req.url);
    const range = searchParams.get('range') || 'ytd'; // ytd, all, 90, 30

    // QuickBase table IDs
    const QB_TABLE_PROJECTS = (process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na').trim();
    const QB_TABLE_TASK_GROUPS = (process.env.QUICKBASE_TABLE_TASK_GROUPS || 'bu36gem4p').trim();
    const QB_TABLE_TASKS = (process.env.QUICKBASE_TABLE_TASKS || 'bu36ggiht').trim();
    const QB_TABLE_TASK_SUBMISSIONS = (process.env.QUICKBASE_TABLE_TASK_SUBMISSIONS || 'bu36g8j99').trim();

    // Helper: Format date for QuickBase (MM-DD-YYYY)
    const formatQBDate = (date: Date): string => {
      const month = String(date.getMonth() + 1).padStart(2, '0');
      const day = String(date.getDate()).padStart(2, '0');
      const year = date.getFullYear();
      return `${month}-${day}-${year}`;
    };

    // Build date filter based on range parameter
    let dateFilter = '';
    if (range === 'ytd') {
      dateFilter = `AND{${TASK_FIELDS.DATE_CREATED}.OAF.'01-01-2025'}`;
    } else if (range !== 'all') {
      const days = parseInt(range);
      if (!isNaN(days)) {
        const date = new Date();
        date.setDate(date.getDate() - days);
        dateFilter = `AND{${TASK_FIELDS.DATE_CREATED}.OAF.'${formatQBDate(date)}'}`;
      }
    }

    // STEP 1: Query incomplete tasks first (task-first strategy)
    // This is the key optimization - we query tasks directly instead of projects
    const tasksWhereClause = `{${TASK_FIELDS.STATUS}.XEX.'Completed'}${dateFilter}`;

    logInfo('[TASKS_API] Querying incomplete tasks', {
      range,
      dateFilter,
      tasksWhereClause,
      reqId
    });

    // Query all incomplete tasks (with date filter)
    checkTimeout(); // Check before expensive query
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
    checkTimeout(); // Check after tasks query

    // Show sample of tasks pulled
    const sampleTasks = tasksResponse.data.slice(0, 5).map((t: any) => ({
      id: t[TASK_FIELDS.RECORD_ID]?.value,
      name: t[TASK_FIELDS.NAME]?.value,
      status: t[TASK_FIELDS.STATUS]?.value,
      created: t[TASK_FIELDS.DATE_CREATED]?.value
    }));

    logInfo('[TASKS_API] Tasks query complete', {
      tasksCount: tasksResponse.data.length,
      sampleTasks: sampleTasks,
      fieldsSelected: [
        'RECORD_ID', 'DATE_CREATED', 'DATE_MODIFIED', 'TASK_GROUP',
        'STATUS', 'NAME', 'DESCRIPTION', 'MAX_SUBMISSION_STATUS',
        'TASK_TEMPLATE', 'TASK_CATEGORY', 'TASK_MISSING_ITEM',
        'REVIEWED_BY_OPS', 'REVIEWED_BY_OPS_USER', 'OPS_REVIEW_NOTE'
      ],
      reqId
    });

    if (tasksResponse.data.length === 0) {
      logApiResponse('GET', '/api/tasks', Date.now() - startedAt, { tasks: 0 }, reqId);
      return NextResponse.json([], { status: 200 });
    }

    // STEP 2: Get task groups for these tasks (with batching if needed)
    const taskGroupIds = [...new Set(
      tasksResponse.data
        .map((t: any) => t[TASK_FIELDS.TASK_GROUP]?.value)
        .filter(Boolean)
    )];

    logInfo('[TASKS_API] Querying task groups', {
      taskGroupCount: taskGroupIds.length,
      reqId
    });

    checkTimeout(); // Check before task groups query
    const BATCH_SIZE = 100;
    const allTaskGroups: any[] = [];

    // Batch if we have many task groups
    if (taskGroupIds.length > BATCH_SIZE) {
      logInfo('[TASKS_API] Batching task groups query', {
        batches: Math.ceil(taskGroupIds.length / BATCH_SIZE),
        reqId
      });

      for (let i = 0; i < taskGroupIds.length; i += BATCH_SIZE) {
        const batch = taskGroupIds.slice(i, i + BATCH_SIZE);
        const taskGroupsWhereClause = batch
          .map((id: number) => `{${TASK_GROUP_FIELDS.RECORD_ID}.EX.${id}}`)
          .join('OR');

        const batchResponse = await qbClient.queryRecords({
          from: QB_TABLE_TASK_GROUPS,
          select: [
            TASK_GROUP_FIELDS.RECORD_ID,
            TASK_GROUP_FIELDS.RELATED_PROJECT
          ],
          where: taskGroupsWhereClause
        });

        allTaskGroups.push(...batchResponse.data);
      }
    } else {
      const taskGroupsWhereClause = taskGroupIds
        .map((id: number) => `{${TASK_GROUP_FIELDS.RECORD_ID}.EX.${id}}`)
        .join('OR');

      const taskGroupsResponse = await qbClient.queryRecords({
        from: QB_TABLE_TASK_GROUPS,
        select: [
          TASK_GROUP_FIELDS.RECORD_ID,
          TASK_GROUP_FIELDS.RELATED_PROJECT
        ],
        where: taskGroupsWhereClause
      });

      allTaskGroups.push(...taskGroupsResponse.data);
    }

    // Map task groups to projects
    const taskGroupToProjectMap = new Map();
    allTaskGroups.forEach((tg: any) => {
      const groupId = tg[TASK_GROUP_FIELDS.RECORD_ID]?.value;
      const projectId = tg[TASK_GROUP_FIELDS.RELATED_PROJECT]?.value;
      if (groupId && projectId) {
        taskGroupToProjectMap.set(groupId, projectId);
      }
    });

    // STEP 3: Get projects for these task groups (with batching if needed)
    const projectIds = [...new Set(
      allTaskGroups
        .map((tg: any) => tg[TASK_GROUP_FIELDS.RELATED_PROJECT]?.value)
        .filter(Boolean)
    )];

    logInfo('[TASKS_API] Querying projects', {
      projectCount: projectIds.length,
      reqId
    });

    checkTimeout(); // Check before projects query
    const allProjects: any[] = [];

    // Batch if we have many projects
    if (projectIds.length > BATCH_SIZE) {
      logInfo('[TASKS_API] Batching projects query', {
        batches: Math.ceil(projectIds.length / BATCH_SIZE),
        reqId
      });

      for (let i = 0; i < projectIds.length; i += BATCH_SIZE) {
        const batch = projectIds.slice(i, i + BATCH_SIZE);
        const projectsWhereClause = batch
          .map((id: number) => `{${PROJECT_FIELDS.RECORD_ID}.EX.${id}}`)
          .join('OR');

        const batchResponse = await qbClient.queryRecords({
          from: QB_TABLE_PROJECTS,
          select: [
            PROJECT_FIELDS.RECORD_ID,
            PROJECT_FIELDS.PROJECT_ID,
            PROJECT_FIELDS.CUSTOMER_NAME,
            PROJECT_FIELDS.PROJECT_STATUS,
            PROJECT_FIELDS.CLOSER_EMAIL,
            PROJECT_FIELDS.CLOSER_NAME,
            PROJECT_FIELDS.SETTER_EMAIL,
            PROJECT_FIELDS.SALES_OFFICE,
            PROJECT_FIELDS.OFFICE_RECORD_ID
          ],
          where: projectsWhereClause
        });

        allProjects.push(...batchResponse.data);
      }
    } else {
      const projectsWhereClause = projectIds
        .map((id: number) => `{${PROJECT_FIELDS.RECORD_ID}.EX.${id}}`)
        .join('OR');

      const projectsResponse = await qbClient.queryRecords({
        from: QB_TABLE_PROJECTS,
        select: [
          PROJECT_FIELDS.RECORD_ID,
          PROJECT_FIELDS.PROJECT_ID,
          PROJECT_FIELDS.CUSTOMER_NAME,
          PROJECT_FIELDS.PROJECT_STATUS,
          PROJECT_FIELDS.CLOSER_EMAIL,
          PROJECT_FIELDS.CLOSER_NAME,
          PROJECT_FIELDS.SETTER_EMAIL,
          PROJECT_FIELDS.SALES_OFFICE,
          PROJECT_FIELDS.OFFICE_RECORD_ID
        ],
        where: projectsWhereClause
      });

      allProjects.push(...projectsResponse.data);
    }

    // Show sample of projects pulled
    const sampleProjects = allProjects.slice(0, 5).map((p: any) => ({
      id: p[PROJECT_FIELDS.RECORD_ID]?.value,
      projectId: p[PROJECT_FIELDS.PROJECT_ID]?.value,
      customerName: p[PROJECT_FIELDS.CUSTOMER_NAME]?.value,
      status: p[PROJECT_FIELDS.PROJECT_STATUS]?.value,
      officeId: p[PROJECT_FIELDS.OFFICE_RECORD_ID]?.value,
      closerEmail: p[PROJECT_FIELDS.CLOSER_EMAIL]?.value,
      setterEmail: p[PROJECT_FIELDS.SETTER_EMAIL]?.value
    }));

    logInfo('[TASKS_API] Projects query complete', {
      projectsCount: allProjects.length,
      sampleProjects: sampleProjects,
      fieldsSelected: [
        'RECORD_ID', 'PROJECT_ID', 'CUSTOMER_NAME', 'PROJECT_STATUS',
        'CLOSER_EMAIL', 'SETTER_EMAIL', 'OFFICE_RECORD_ID'
      ],
      reqId
    });

    // STEP 4: Filter projects by user authorization
    const authorizedProjectIds = new Set(
      allProjects
        .filter((project: any) => {
          // For super_admin and regional without office filter, allow all
          if (['super_admin', 'regional'].includes(userRole) && (!effectiveOfficeIds || effectiveOfficeIds.length === 0)) {
            return true;
          }

          // For office-based roles, check office ID
          if (effectiveOfficeIds && effectiveOfficeIds.length > 0) {
            const projectOfficeId = project[PROJECT_FIELDS.OFFICE_RECORD_ID]?.value;
            return effectiveOfficeIds.includes(projectOfficeId);
          }

          // For reps, check email match
          if (['closer', 'setter'].includes(userRole) && userEmail) {
            const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value?.toLowerCase();
            const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value?.toLowerCase();
            return closerEmail === userEmail.toLowerCase() || setterEmail === userEmail.toLowerCase();
          }

          // For team leads, check managed emails
          if (userRole === 'team_lead' && managedEmails && managedEmails.length > 0) {
            const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value?.toLowerCase();
            const setterEmail = project[PROJECT_FIELDS.SETTER_EMAIL]?.value?.toLowerCase();
            const managedLower = managedEmails.map(e => e.toLowerCase());
            return managedLower.includes(closerEmail) || managedLower.includes(setterEmail);
          }

          return false;
        })
        .map((p: any) => p[PROJECT_FIELDS.RECORD_ID]?.value)
    );

    // Show what got filtered out
    const filteredOutCount = allProjects.length - authorizedProjectIds.size;
    const authorizedProjectsSample = allProjects
      .filter((p: any) => authorizedProjectIds.has(p[PROJECT_FIELDS.RECORD_ID]?.value))
      .slice(0, 5)
      .map((p: any) => ({
        id: p[PROJECT_FIELDS.RECORD_ID]?.value,
        projectId: p[PROJECT_FIELDS.PROJECT_ID]?.value,
        customerName: p[PROJECT_FIELDS.CUSTOMER_NAME]?.value,
        closerEmail: p[PROJECT_FIELDS.CLOSER_EMAIL]?.value,
        setterEmail: p[PROJECT_FIELDS.SETTER_EMAIL]?.value
      }));

    logInfo('[TASKS_API] Authorization filter applied', {
      totalProjects: allProjects.length,
      authorizedProjects: authorizedProjectIds.size,
      filteredOutCount: filteredOutCount,
      userRole: userRole,
      userEmail: userEmail,
      effectiveOfficeIds: effectiveOfficeIds,
      authorizedProjectsSample: authorizedProjectsSample,
      reqId
    });

    // Create project lookup map (only for authorized projects)
    // Also identify cancelled projects that need their tasks closed
    const projectMap = new Map();
    const cancelledProjectIds: number[] = [];
    
    allProjects.forEach((p: any) => {
      const recordId = p[PROJECT_FIELDS.RECORD_ID]?.value;
      const projectStatus = p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
      
      if (recordId && authorizedProjectIds.has(recordId)) {
        projectMap.set(recordId, {
          projectId: p[PROJECT_FIELDS.PROJECT_ID]?.value || recordId,
          customerName: p[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown',
          projectStatus: projectStatus,
          closerName: p[PROJECT_FIELDS.CLOSER_NAME]?.value || null,
          salesOffice: p[PROJECT_FIELDS.SALES_OFFICE]?.value || null
        });

        // Check if project is cancelled (and not pending cancel)
        const isCancelled = typeof projectStatus === 'string' && 
          (projectStatus.toLowerCase().includes('cancel') || projectStatus === 'Cancelled') &&
          !projectStatus.toLowerCase().includes('pending');
        
        if (isCancelled) {
          cancelledProjectIds.push(recordId);
        }
      }
    });

    // Auto-close tasks for cancelled projects (in background, don't block response)
    if (cancelledProjectIds.length > 0) {
      const { closeTasksForCancelledProject } = await import('@/lib/quickbase/queries');
      
      // Process in background without blocking the response
      Promise.all(
        cancelledProjectIds.map(projectId => 
          closeTasksForCancelledProject(projectId, reqId).catch(error => {
            logError('[TASKS_API] Failed to close tasks for cancelled project', error as Error, {
              projectId,
              reqId
            });
          })
        )
      ).catch(error => {
        logError('[TASKS_API] Background task closure failed', error as Error, { reqId });
      });

      logInfo('[TASKS_API] Queued task closure for cancelled projects', {
        cancelledProjectCount: cancelledProjectIds.length,
        cancelledProjectIds: cancelledProjectIds.slice(0, 10), // Log first 10
        reqId
      });
    }

    // Filter tasks to only authorized projects
    const allTasks = tasksResponse.data.filter((task: any) => {
      const taskGroupId = task[TASK_FIELDS.TASK_GROUP]?.value;
      const projectId = taskGroupToProjectMap.get(taskGroupId);
      return projectId && authorizedProjectIds.has(projectId);
    });

    logInfo('[TASKS_API] Tasks filtered by authorization', {
      totalTasksQueried: tasksResponse.data.length,
      authorizedTasks: allTasks.length,
      filteredOutTasks: tasksResponse.data.length - allTasks.length,
      reqId
    });

    // STEP 5: Get submissions for these tasks
    const taskIds = allTasks
      .map((t: any) => t[TASK_FIELDS.RECORD_ID]?.value)
      .filter(Boolean);

    let submissionsMap = new Map<number, TaskSubmission[]>();

    if (taskIds.length > 0) {
      logInfo('[TASKS_API] Querying submissions', {
        totalTasks: taskIds.length,
        reqId
      });

      checkTimeout(); // Check before submissions query
      // Batch submissions query if we have many tasks
      if (taskIds.length > BATCH_SIZE) {
        logInfo('[TASKS_API] Batching submissions query', {
          batches: Math.ceil(taskIds.length / BATCH_SIZE),
          reqId
        });

        for (let i = 0; i < taskIds.length; i += BATCH_SIZE) {
          const batch = taskIds.slice(i, i + BATCH_SIZE);
          const submissionsWhereClause = batch
            .map((id: number) => `{${TASK_SUBMISSION_FIELDS.RELATED_TASK}.EX.${id}}`)
            .join('OR');

          const batchResponse = await qbClient.queryRecords({
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

          batchResponse.data.forEach((s: any) => {
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
        }
      } else {
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
      }

      // Sort submissions by date desc
      submissionsMap.forEach((submissions) => {
        submissions.sort((a, b) => new Date(b.dateCreated).getTime() - new Date(a.dateCreated).getTime());
      });
    }

    // Step 5: Build tasks with project context
    const tasksWithProject: TaskWithProject[] = allTasks.map((record: any) => {
      const taskGroupId = record[TASK_FIELDS.TASK_GROUP]?.value;
      const projectId = taskGroupToProjectMap.get(taskGroupId);
      const projectInfo = projectMap.get(projectId) || {
        projectId: projectId,
        customerName: 'Unknown',
        projectStatus: '',
        closerName: null,
        salesOffice: null
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
        customerName: projectInfo.customerName,
        closerName: projectInfo.closerName,
        salesOffice: projectInfo.salesOffice
      };
    });

    // Final summary log
    const totalSubmissions = Array.from(submissionsMap.values()).reduce((sum, subs) => sum + subs.length, 0);
    const sampleFinalTasks = tasksWithProject.slice(0, 3).map(t => ({
      id: t.recordId,
      name: t.name,
      status: t.status,
      projectId: t.projectId,
      customerName: t.customerName,
      submissionsCount: t.submissions.length
    }));

    logInfo('[TASKS_API] Final result summary', {
      totalTasksReturned: tasksWithProject.length,
      totalSubmissions: totalSubmissions,
      uniqueProjects: authorizedProjectIds.size,
      sampleFinalTasks: sampleFinalTasks,
      queryStrategy: 'task-first with YTD filter',
      dateFilter: range,
      reqId
    });

    // Log response
    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/tasks', duration, {
      tasks: tasksWithProject.length,
      projects: authorizedProjectIds.size,
      submissions: totalSubmissions
    }, reqId);

    return NextResponse.json(tasksWithProject, { status: 200 });

  } catch (error) {
    const elapsed = Date.now() - startedAt;
    const errorMessage = (error as Error).message || 'Unknown error';
    const isTimeout = errorMessage.includes('timeout') || elapsed > TIMEOUT_ERROR_MS;

    // CRITICAL: Console.error to ensure it shows in Vercel logs
    console.error('[TASKS_API] CRITICAL ERROR:', {
      message: errorMessage,
      name: (error as Error).name,
      stack: (error as Error).stack,
      elapsed,
      isTimeout,
      reqId
    });

    // Log detailed error information for debugging
    const errorDetails: any = {
      message: errorMessage,
      stack: (error as Error).stack,
      elapsed,
      isTimeout,
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

    // Return 504 Gateway Timeout for timeout errors
    if (isTimeout) {
      return NextResponse.json({
        error: 'Gateway Timeout',
        message: 'The request took too long to complete. This can happen when there are many tasks to process.',
        details: `Operation took ${elapsed}ms, exceeding the ${TIMEOUT_ERROR_MS}ms limit.`,
        suggestion: 'Try filtering by a shorter date range or contact support if this persists.',
        elapsed,
        reqId
      }, { status: 504 });
    }

    // TEMPORARY: Always return error details for debugging
    const errorResponse: Record<string, any> = {
      error: 'Internal Server Error',
      details: errorMessage,
      errorName: (error as Error).name,
      elapsed,
      stack: (error as Error).stack?.split('\n').slice(0, 10)
    };

    // Include any additional error properties
    if (error && typeof error === 'object') {
      errorResponse.originalError = (error as any).originalError;
      errorResponse.whereClause = (error as any).whereClause;
      errorResponse.tableId = (error as any).tableId;
      errorResponse.quickbaseError = (error as any).quickbaseError;
      errorResponse.requestParams = (error as any).requestParams;
      errorResponse.statusCode = (error as any).statusCode;
    }

    return NextResponse.json(errorResponse, { status: 500 });
  }
}
