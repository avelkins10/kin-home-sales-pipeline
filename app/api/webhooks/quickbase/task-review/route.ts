export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { createTaskNotification, getTaskById, checkAllTasksComplete, markProjectTasksComplete } from '@/lib/quickbase/queries';
import { TASK_SUBMISSION_FIELDS } from '@/lib/constants/fieldIds';

/**
 * POST /api/webhooks/quickbase/task-review
 * Webhook endpoint called by QuickBase when ops reviews a task submission
 *
 * Expected payload from QuickBase:
 * {
 *   submissionId: number,
 *   taskId: number,
 *   projectId: number,
 *   userId: string,  // Rep's email
 *   disposition: "Approved" | "Needs Revision",
 *   feedback?: string
 * }
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', '/api/webhooks/quickbase/task-review', undefined, reqId);

  try {
    // Parse webhook payload
    const payload = await req.json();
    const { submissionId, taskId, projectId, userId, disposition, feedback } = payload;

    // Validate required fields
    if (!submissionId || !taskId || !projectId || !userId || !disposition) {
      return NextResponse.json({
        error: 'Missing required fields: submissionId, taskId, projectId, userId, disposition'
      }, { status: 400 });
    }

    // Validate disposition value
    if (disposition !== 'Approved' && disposition !== 'Needs Revision') {
      return NextResponse.json({
        error: 'Invalid disposition. Must be "Approved" or "Needs Revision"'
      }, { status: 400 });
    }

    console.log('[Webhook] Task review received:', { submissionId, taskId, projectId, disposition });

    // Get task details
    const task = await getTaskById(taskId);
    if (!task) {
      return NextResponse.json({ error: 'Task not found' }, { status: 404 });
    }

    // Create appropriate notification based on disposition
    if (disposition === 'Approved') {
      // Task approved notification
      await createTaskNotification({
        userId,
        projectId,
        taskId,
        taskName: task.name,
        taskCategory: task.category || undefined,
        type: 'task_approved',
        submissionId,
        opsDisposition: 'Approved'
      });

      console.log('[Webhook] Task approved notification created');

      // Check if all tasks are now complete
      const completionStatus = await checkAllTasksComplete(projectId);

      if (completionStatus.allComplete) {
        console.log('[Webhook] All tasks complete for project', projectId);

        // Send all-tasks-complete notification
        await createTaskNotification({
          userId,
          projectId,
          taskId,
          taskName: task.name,
          taskCategory: task.category || undefined,
          type: 'all_tasks_complete',
          totalTasks: completionStatus.totalTasks,
          approvedTasks: completionStatus.approvedTasks
        });

        // Update project status to indicate tasks are complete
        await markProjectTasksComplete(projectId);

        console.log('[Webhook] Project marked as tasks complete');
      }

    } else {
      // Revision needed notification
      await createTaskNotification({
        userId,
        projectId,
        taskId,
        taskName: task.name,
        taskCategory: task.category || undefined,
        type: 'task_revision_needed',
        submissionId,
        opsDisposition: 'Needs Revision',
        opsFeedback: feedback
      });

      console.log('[Webhook] Revision needed notification created');
    }

    const duration = Date.now() - startedAt;
    logApiResponse('POST', '/api/webhooks/quickbase/task-review', duration, {
      taskId,
      disposition
    }, reqId);

    return NextResponse.json({
      success: true,
      message: `Task review processed: ${disposition}`
    }, { status: 200 });

  } catch (error) {
    logError('Failed to process task review webhook', error as Error, {});
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
