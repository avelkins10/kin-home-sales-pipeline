import { NextResponse } from 'next/server';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { closeTasksForCancelledProject } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS, QB_TABLE_PROJECTS } from '@/lib/constants/fieldIds';

/**
 * POST /api/webhooks/quickbase/project-status
 * Webhook endpoint to handle project status changes from Quickbase
 * When a project status changes to "Cancelled", automatically close all associated tasks
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('POST', '/api/webhooks/quickbase/project-status', undefined, reqId);

  try {
    const body = await req.json();
    const { recordId, projectId, status, previousStatus } = body;

    // Validate required fields
    if (!recordId && !projectId) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'recordId or projectId is required' },
        { status: 400 }
      );
    }

    const projectRecordId = recordId || projectId;
    const projectStatus = status || body.projectStatus || body[PROJECT_FIELDS.PROJECT_STATUS];

    if (!projectStatus) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'status is required' },
        { status: 400 }
      );
    }

    logInfo('[Project Status Webhook] Received project status change', {
      projectRecordId,
      status: projectStatus,
      previousStatus,
      reqId
    });

    // Check if project status is "Cancelled"
    const isCancelled = typeof projectStatus === 'string' && 
      (projectStatus.toLowerCase().includes('cancel') || projectStatus === 'Cancelled');

    if (!isCancelled) {
      logInfo('[Project Status Webhook] Project not cancelled, skipping task closure', {
        projectRecordId,
        status: projectStatus,
        reqId
      });

      return NextResponse.json({
        success: true,
        message: 'Project status is not cancelled, no action taken',
        projectRecordId,
        status: projectStatus
      });
    }

    // Verify project status in Quickbase before closing tasks
    try {
      const projectResponse = await qbClient.queryRecords({
        from: QB_TABLE_PROJECTS,
        select: [PROJECT_FIELDS.RECORD_ID, PROJECT_FIELDS.PROJECT_STATUS],
        where: `{${PROJECT_FIELDS.RECORD_ID}.EX.${projectRecordId}}`
      });

      if (projectResponse.data.length === 0) {
        return NextResponse.json(
          { error: 'Not Found', message: 'Project not found' },
          { status: 404 }
        );
      }

      const currentStatus = projectResponse.data[0][PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
      const isActuallyCancelled = currentStatus.toLowerCase().includes('cancel') || currentStatus === 'Cancelled';

      if (!isActuallyCancelled) {
        logInfo('[Project Status Webhook] Project status changed before task closure, skipping', {
          projectRecordId,
          currentStatus,
          reqId
        });

        return NextResponse.json({
          success: true,
          message: 'Project status is no longer cancelled',
          projectRecordId,
          currentStatus
        });
      }

      // Close all tasks for this cancelled project
      const result = await closeTasksForCancelledProject(projectRecordId, reqId);

      logInfo('[Project Status Webhook] Successfully closed tasks for cancelled project', {
        projectRecordId,
        closedCount: result.closedCount,
        taskIds: result.taskIds.slice(0, 10), // Log first 10
        reqId
      });

      const duration = Date.now() - startedAt;
      logApiResponse('POST', '/api/webhooks/quickbase/project-status', duration, {
        projectRecordId,
        closedCount: result.closedCount,
        success: true
      }, reqId);

      return NextResponse.json({
        success: true,
        message: `Closed ${result.closedCount} task(s) for cancelled project`,
        projectRecordId,
        closedCount: result.closedCount,
        taskIds: result.taskIds
      });

    } catch (error) {
      logError('[Project Status Webhook] Failed to verify or close tasks', error as Error, {
        projectRecordId,
        reqId
      });

      return NextResponse.json(
        { 
          error: 'Internal Server Error', 
          message: 'Failed to close tasks for cancelled project',
          details: (error as Error).message
        },
        { status: 500 }
      );
    }

  } catch (error) {
    logError('[Project Status Webhook] Failed to process webhook', error as Error, { reqId });

    const duration = Date.now() - startedAt;
    logApiResponse('POST', '/api/webhooks/quickbase/project-status', duration, {
      error: (error as Error).message
    }, reqId);

    return NextResponse.json(
      { 
        error: 'Internal Server Error', 
        message: 'Failed to process webhook',
        details: (error as Error).message
      },
      { status: 500 }
    );
  }
}

