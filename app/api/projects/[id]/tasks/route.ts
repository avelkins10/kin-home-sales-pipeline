import { NextResponse } from 'next/server';
import { requireAuth, requireProjectAccessById } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { TASK_GROUP_FIELDS, TASK_FIELDS } from '@/lib/constants/fieldIds';
import { TaskGroup, Task } from '@/lib/types/task';

export const runtime = 'nodejs';

export async function GET(req: Request, { params }: { params: { id: string } }) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  // Log request
  logApiRequest('GET', '/api/projects/{id}/tasks', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    // Parse and validate project ID
    const numericId = parseInt(params.id, 10);
    if (isNaN(numericId)) {
      return NextResponse.json({ error: 'Invalid project ID' }, { status: 400 });
    }

    // Authorization - verify user can access this project
    const access = await requireProjectAccessById(numericId);
    if (!access.authorized) {
      return access.response;
    }

    // Table IDs with correct fallbacks
    const QB_TABLE_TASK_GROUPS = (process.env.QUICKBASE_TABLE_TASK_GROUPS || 'bu36gem4p').trim();
    const QB_TABLE_TASKS = (process.env.QUICKBASE_TABLE_TASKS || 'bu36ggiht').trim();

    // Step 1: Get task groups for this project with all fields
    const taskGroupsResponse = await qbClient.queryRecords({
      from: QB_TABLE_TASK_GROUPS,
      select: [
        TASK_GROUP_FIELDS.RECORD_ID,
        TASK_GROUP_FIELDS.RELATED_PROJECT,
        TASK_GROUP_FIELDS.TOTAL_TASKS,
        TASK_GROUP_FIELDS.UNAPPROVED_TASKS
      ],
      where: `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${numericId}}`
    });

    // If no task groups, return empty array
    if (taskGroupsResponse.data.length === 0) {
      logApiResponse('GET', '/api/projects/{id}/tasks', Date.now() - startedAt, { count: 0 }, reqId);
      return NextResponse.json([], { status: 200 });
    }

    // Build task groups with metadata
    const taskGroupsMap = new Map<number, TaskGroup>();
    const taskGroupIds: number[] = [];

    taskGroupsResponse.data.forEach((record: any) => {
      const recordId = record[TASK_GROUP_FIELDS.RECORD_ID]?.value;
      if (recordId) {
        taskGroupIds.push(recordId);
        taskGroupsMap.set(recordId, {
          recordId,
          relatedProject: record[TASK_GROUP_FIELDS.RELATED_PROJECT]?.value || numericId,
          totalTasks: record[TASK_GROUP_FIELDS.TOTAL_TASKS]?.value || 0,
          unapprovedTasks: record[TASK_GROUP_FIELDS.UNAPPROVED_TASKS]?.value || 0,
          tasks: []
        });
      }
    });

    // Step 2: Get tasks for these task groups
    if (taskGroupIds.length > 0) {
      const whereClause = taskGroupIds.map((id: number) => `{${TASK_FIELDS.TASK_GROUP}.EX.${id}}`).join('OR');

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
        where: whereClause
      });

      // Group tasks by their task group
      tasksResponse.data.forEach((record: any) => {
        const taskGroupId = record[TASK_FIELDS.TASK_GROUP]?.value;
        const task: Task = {
          recordId: record[TASK_FIELDS.RECORD_ID]?.value || 0,
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
          submissions: []
        };

        const group = taskGroupsMap.get(taskGroupId);
        if (group) {
          group.tasks.push(task);
        }
      });
    }

    // Convert map to array
    const taskGroups = Array.from(taskGroupsMap.values());

    // Log response
    const duration = Date.now() - startedAt;
    const totalTasks = taskGroups.reduce((sum, group) => sum + group.tasks.length, 0);
    logApiResponse('GET', '/api/projects/{id}/tasks', duration, {
      groups: taskGroups.length,
      tasks: totalTasks
    }, reqId);

    return NextResponse.json(taskGroups, { status: 200 });

  } catch (error) {
    logError('Failed to fetch tasks', error as Error, { projectId: params.id });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
