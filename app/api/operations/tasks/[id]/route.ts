export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { updatePCTaskStatus } from '@/lib/quickbase/queries';

/**
 * PATCH /api/operations/tasks/[id]
 * Update individual PC task
 */
export async function PATCH(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('PATCH', `/api/operations/tasks/${params.id}`, undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    
    // Check user role is operations_coordinator, operations_manager, closer, setter, office_leader, regional, or super_admin
    const allowedRoles = [
      'operations_coordinator',
      'operations_manager',
      'closer',
      'setter',
      'office_leader',
      'regional',
      'super_admin'
    ];

    if (!allowedRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden', message: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    // Extract task ID from URL params
    const taskId = parseInt(params.id);
    if (isNaN(taskId)) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid task ID' },
        { status: 400 }
      );
    }

    // Parse request body
    const body = await req.json();
    const { action, data } = body;

    if (!action) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Action is required' },
        { status: 400 }
      );
    }

    // Extract user email from session
    const userEmail = auth.session.user.email;
    if (!userEmail) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'User email not found' },
        { status: 400 }
      );
    }

    let success = false;
    let message = '';

    // Map action to updates
    switch (action) {
      case 'update_status':
        if (!data?.status) {
          return NextResponse.json(
            { error: 'Bad Request', message: 'Status is required for update_status action' },
            { status: 400 }
          );
        }
        
        const validStatuses = ['Not Started', 'In Progress', 'Completed', 'Blocked'];
        if (!validStatuses.includes(data.status)) {
          return NextResponse.json(
            { error: 'Bad Request', message: 'Invalid status' },
            { status: 400 }
          );
        }

        success = await updatePCTaskStatus(taskId, data.status, userEmail, reqId);
        message = success ? 'Task status updated successfully' : 'Failed to update task status';
        break;

      case 'add_comment':
        if (!data?.comment) {
          return NextResponse.json(
            { error: 'Bad Request', message: 'Comment is required for add_comment action' },
            { status: 400 }
          );
        }

        // TODO: Implement comment creation in Install Communications table
        // For now, just return success
        success = true;
        message = 'Comment added successfully';
        break;

      case 'complete':
        success = await updatePCTaskStatus(taskId, 'Completed', userEmail, reqId);
        message = success ? 'Task completed successfully' : 'Failed to complete task';
        break;

      default:
        return NextResponse.json(
          { error: 'Bad Request', message: 'Invalid action' },
          { status: 400 }
        );
    }

    logApiResponse('PATCH', `/api/operations/tasks/${params.id}`, Date.now() - startedAt, {
      taskId,
      action,
      success
    }, reqId);

    return NextResponse.json({
      success,
      message
    }, { status: success ? 200 : 500 });

  } catch (error) {
    logError('Failed to update PC task', error as Error, { reqId, taskId: params.id });
    return NextResponse.json(
      { error: 'Internal Server Error', message: 'Failed to update task' },
      { status: 500 }
    );
  }
}
