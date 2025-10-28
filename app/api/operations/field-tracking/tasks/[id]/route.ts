// app/api/operations/field-tracking/tasks/[id]/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { 
  getArrivyTaskByProjectId, 
  getArrivyTaskByArrivyId,
  getTaskStatusHistory,
  getArrivyEventsForTask,
  deleteArrivyTask,
  upsertArrivyTask,
  getArrivyEntityById,
} from '@/lib/db/arrivy';

/**
 * GET - Get specific task by ID (project ID or Arrivy task ID)
 */
export async function GET(
  req: NextRequest,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Operations role required.' 
      }, { status: 403 });
    }

    const taskId = params.id;

    // Try to fetch task (check if it's a project ID or Arrivy task ID)
    let task = await getArrivyTaskByProjectId(taskId);
    
    if (!task) {
      // Try as Arrivy task ID
      const arrivyTaskId = parseInt(taskId, 10);
      if (!isNaN(arrivyTaskId)) {
        task = await getArrivyTaskByArrivyId(arrivyTaskId);
      }
    }

    if (!task) {
      return NextResponse.json({ 
        error: 'Task not found' 
      }, { status: 404 });
    }

    // Fetch status history
    const statusHistory = await getTaskStatusHistory(task.arrivy_task_id, 20);

    // Fetch events
    const events = await getArrivyEventsForTask(task.arrivy_task_id, 20);

    // Fetch entity names for assigned crew members
    const entityNames: string[] = [];
    if (task.assigned_entity_ids && task.assigned_entity_ids.length > 0) {
      for (const entityId of task.assigned_entity_ids) {
        try {
          const entity = await getArrivyEntityById(entityId);
          if (entity) {
            entityNames.push(entity.name);
          }
        } catch (error) {
          // Log but continue if entity fetch fails
          logError('Failed to fetch entity name', error as Error, { entityId });
        }
      }
    }

    logApiResponse('GET', `/api/operations/field-tracking/tasks/${taskId}`, Date.now() - startedAt, { 
      found: true,
    }, reqId);

    return NextResponse.json({ 
      task: {
        ...task,
        entity_names: entityNames,
      },
      statusHistory,
      events,
    }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch field tracking task', error as Error, { reqId, taskId: params.id });
    return NextResponse.json({ 
      error: 'Failed to load task' 
    }, { status: 500 });
  }
}

/**
 * PUT - Update task
 */
export async function PUT(
  req: NextRequest,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Operations role required.' 
      }, { status: 403 });
    }

    if (!arrivyClient) {
      return NextResponse.json({ 
        error: 'Arrivy not configured' 
      }, { status: 503 });
    }

    const taskId = params.id;
    const body = await req.json();

    // Fetch existing task
    let task = await getArrivyTaskByProjectId(taskId);
    
    if (!task) {
      const arrivyTaskId = parseInt(taskId, 10);
      if (!isNaN(arrivyTaskId)) {
        task = await getArrivyTaskByArrivyId(arrivyTaskId);
      }
    }

    if (!task) {
      return NextResponse.json({ 
        error: 'Task not found' 
      }, { status: 404 });
    }

    // Update task in Arrivy
    const updatedTask = await arrivyClient.updateTask(task.arrivy_task_id, body);

    // Update local database
    await upsertArrivyTask({
      arrivy_task_id: updatedTask.id,
      url_safe_id: updatedTask.url_safe_id,
      quickbase_project_id: task.quickbase_project_id,
      quickbase_record_id: task.quickbase_record_id,
      customer_name: updatedTask.customer_name,
      customer_phone: updatedTask.customer_phone,
      customer_email: updatedTask.customer_email,
      current_status: updatedTask.status,
      synced_at: new Date(),
    });

    logApiResponse('PUT', `/api/operations/field-tracking/tasks/${taskId}`, Date.now() - startedAt, {}, reqId);

    return NextResponse.json({ 
      success: true,
      task: updatedTask,
    }, { status: 200 });

  } catch (error) {
    logError('Failed to update field tracking task', error as Error, { reqId, taskId: params.id });
    return NextResponse.json({ 
      error: 'Failed to update task',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, { status: 500 });
  }
}

/**
 * DELETE - Cancel/delete task
 */
export async function DELETE(
  req: NextRequest,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Manager role required.' 
      }, { status: 403 });
    }

    if (!arrivyClient) {
      return NextResponse.json({ 
        error: 'Arrivy not configured' 
      }, { status: 503 });
    }

    const taskId = params.id;

    // Fetch existing task
    let task = await getArrivyTaskByProjectId(taskId);
    
    if (!task) {
      const arrivyTaskId = parseInt(taskId, 10);
      if (!isNaN(arrivyTaskId)) {
        task = await getArrivyTaskByArrivyId(arrivyTaskId);
      }
    }

    if (!task) {
      return NextResponse.json({ 
        error: 'Task not found' 
      }, { status: 404 });
    }

    // Delete task in Arrivy
    await arrivyClient.deleteTask(task.arrivy_task_id);

    // Delete from local database
    await deleteArrivyTask(task.arrivy_task_id);

    logApiResponse('DELETE', `/api/operations/field-tracking/tasks/${taskId}`, Date.now() - startedAt, {}, reqId);

    return NextResponse.json({ 
      success: true,
      message: 'Task deleted successfully',
    }, { status: 200 });

  } catch (error) {
    logError('Failed to delete field tracking task', error as Error, { reqId, taskId: params.id });
    return NextResponse.json({ 
      error: 'Failed to delete task',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, { status: 500 });
  }
}

