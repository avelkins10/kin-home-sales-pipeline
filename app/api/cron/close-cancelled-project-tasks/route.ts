import { NextRequest, NextResponse } from 'next/server';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { closeTasksForCancelledProject } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS, TASK_GROUP_FIELDS, TASK_FIELDS, QB_TABLE_PROJECTS, QB_TABLE_TASK_GROUPS, QB_TABLE_TASKS } from '@/lib/constants/fieldIds';

/**
 * GET /api/cron/close-cancelled-project-tasks
 * 
 * Periodic job to find projects with status "Cancelled" that still have open tasks
 * and automatically close those tasks.
 * 
 * This can be called by:
 * - Vercel Cron Jobs (recommended: every hour)
 * - Manual trigger for testing
 * 
 * Query params:
 * - dryRun=true - Don't actually close tasks, just report what would be closed
 * - limit=100 - Maximum number of projects to process (default: 100)
 */
export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('GET', '/api/cron/close-cancelled-project-tasks', undefined, reqId);

  try {
    const { searchParams } = new URL(request.url);
    const dryRun = searchParams.get('dryRun') === 'true';
    const limit = parseInt(searchParams.get('limit') || '100', 10);

    logInfo('[Close Cancelled Tasks Cron] Starting sync', {
      dryRun,
      limit,
      reqId
    });

    // Step 1: Find all cancelled projects
    const cancelledProjectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [PROJECT_FIELDS.RECORD_ID, PROJECT_FIELDS.PROJECT_ID, PROJECT_FIELDS.CUSTOMER_NAME, PROJECT_FIELDS.PROJECT_STATUS],
      where: `{${PROJECT_FIELDS.PROJECT_STATUS}.CT.'Cancel'}AND{${PROJECT_FIELDS.PROJECT_STATUS}.XEX.'Pending Cancel'}`,
      options: {
        top: limit
      }
    });

    const cancelledProjects = cancelledProjectsResponse.data.map((record: any) => ({
      recordId: record[PROJECT_FIELDS.RECORD_ID]?.value,
      projectId: record[PROJECT_FIELDS.PROJECT_ID]?.value,
      customerName: record[PROJECT_FIELDS.CUSTOMER_NAME]?.value || 'Unknown',
      status: record[PROJECT_FIELDS.PROJECT_STATUS]?.value || ''
    }));

    logInfo('[Close Cancelled Tasks Cron] Found cancelled projects', {
      count: cancelledProjects.length,
      reqId
    });

    if (cancelledProjects.length === 0) {
      return NextResponse.json({
        success: true,
        message: 'No cancelled projects found',
        processed: 0,
        closed: 0
      });
    }

    // Step 2: For each cancelled project, check if it has open tasks
    const results: Array<{
      projectId: string;
      recordId: number;
      customerName: string;
      closedCount: number;
      taskIds: number[];
    }> = [];

    for (const project of cancelledProjects) {
      try {
        if (dryRun) {
          // In dry run mode, just check if there are open tasks
          const taskGroupsResponse = await qbClient.queryRecords({
            from: QB_TABLE_TASK_GROUPS,
            select: [TASK_GROUP_FIELDS.RECORD_ID],
            where: `{${TASK_GROUP_FIELDS.RELATED_PROJECT}.EX.${project.recordId}}`
          });

          if (taskGroupsResponse.data.length === 0) {
            continue;
          }

          const taskGroupIds = taskGroupsResponse.data
            .map((record: any) => record[TASK_GROUP_FIELDS.RECORD_ID]?.value)
            .filter((id: any) => id);

          if (taskGroupIds.length === 0) {
            continue;
          }

          const whereClause = taskGroupIds
            .map((id: number) => `{${TASK_FIELDS.TASK_GROUP}.EX.${id}}`)
            .join('OR');

          const tasksResponse = await qbClient.queryRecords({
            from: QB_TABLE_TASKS,
            select: [TASK_FIELDS.RECORD_ID, TASK_FIELDS.STATUS],
            where: `${whereClause}AND{${TASK_FIELDS.STATUS}.XEX.'Completed'}AND{${TASK_FIELDS.STATUS}.XEX.'Closed by Ops'}`
          });

          const openTaskIds = tasksResponse.data
            .map((record: any) => record[TASK_FIELDS.RECORD_ID]?.value)
            .filter((id: any) => id);

          if (openTaskIds.length > 0) {
            results.push({
              projectId: project.projectId || String(project.recordId),
              recordId: project.recordId,
              customerName: project.customerName,
              closedCount: openTaskIds.length,
              taskIds: openTaskIds
            });
          }
        } else {
          // Actually close the tasks
          const result = await closeTasksForCancelledProject(project.recordId, reqId);
          
          if (result.closedCount > 0) {
            results.push({
              projectId: project.projectId || String(project.recordId),
              recordId: project.recordId,
              customerName: project.customerName,
              closedCount: result.closedCount,
              taskIds: result.taskIds
            });
          }
        }
      } catch (error) {
        logError('[Close Cancelled Tasks Cron] Failed to process project', error as Error, {
          projectId: project.projectId,
          recordId: project.recordId,
          reqId
        });
        // Continue with next project
      }
    }

    const totalClosed = results.reduce((sum, r) => sum + r.closedCount, 0);

    logInfo('[Close Cancelled Tasks Cron] Sync complete', {
      projectsProcessed: cancelledProjects.length,
      projectsWithTasksClosed: results.length,
      totalTasksClosed: totalClosed,
      dryRun,
      reqId
    });

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/cron/close-cancelled-project-tasks', duration, {
      projectsProcessed: cancelledProjects.length,
      projectsWithTasksClosed: results.length,
      totalTasksClosed: totalClosed,
      dryRun
    }, reqId);

    return NextResponse.json({
      success: true,
      message: dryRun 
        ? `Dry run: Would close ${totalClosed} task(s) across ${results.length} project(s)`
        : `Closed ${totalClosed} task(s) across ${results.length} project(s)`,
      dryRun,
      projectsProcessed: cancelledProjects.length,
      projectsWithTasksClosed: results.length,
      totalTasksClosed: totalClosed,
      results: results.slice(0, 20) // Return first 20 for API response
    });

  } catch (error) {
    logError('[Close Cancelled Tasks Cron] Failed to run sync', error as Error, { reqId });

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/cron/close-cancelled-project-tasks', duration, {
      error: (error as Error).message
    }, reqId);

    return NextResponse.json(
      {
        error: 'Internal Server Error',
        message: 'Failed to close tasks for cancelled projects',
        details: (error as Error).message
      },
      { status: 500 }
    );
  }
}


