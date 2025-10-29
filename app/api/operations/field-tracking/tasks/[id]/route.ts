// app/api/operations/field-tracking/tasks/[id]/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { getBusinessTrackerUrl } from '@/lib/integrations/arrivy';
import { 
  getArrivyTaskByProjectId, 
  getArrivyTaskByArrivyId,
  getTaskStatusHistory,
  getArrivyEventsForTask,
  deleteArrivyTask,
  upsertArrivyTask,
  getArrivyEntityById,
  getTaskRatings,
  getCustomerNotes,
  getCrewContactsForTask,
  calculateTaskDurationMetrics,
} from '@/lib/db/arrivy';
import type { 
  EnhancedTaskDetails, 
  TaskAttachment, 
  TaskRating, 
  CustomerNote,
  CrewContact, 
  TaskDurationMetrics 
} from '@/lib/types/operations';

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

    // Fetch core data in parallel
    // Use higher limit for status history to ensure STARTED/COMPLETE statuses are captured for duration metrics
    const [statusHistory, events] = await Promise.all([
      getTaskStatusHistory(task.arrivy_task_id, 100),
      getArrivyEventsForTask(task.arrivy_task_id, 20),
    ]);

    // Fetch enhanced data in parallel
    const [attachments, ratings, customerNotes, crewContacts] = await Promise.all([
      // Fetch attachments from Arrivy API
      (async (): Promise<TaskAttachment[]> => {
        if (!arrivyClient) return [];
        try {
          const arrivyStatuses = await arrivyClient.getTaskStatuses(task.arrivy_task_id);
          return arrivyStatuses.flatMap(status => 
            (status.files || []).map(file => ({
              file_id: file.file_id,
              file_path: file.file_path,
              filename: file.filename,
              status_id: status.id,
              uploaded_by: status.reporter_name || null,
              uploaded_at: new Date(status.time),
            }))
          );
        } catch (error) {
          logError('Failed to fetch attachments from Arrivy', error as Error, { taskId: task.arrivy_task_id });
          return [];
        }
      })(),
      
      // Fetch customer ratings
      (async (): Promise<TaskRating[]> => {
        try {
          return await getTaskRatings(task.arrivy_task_id);
        } catch (error) {
          logError('Failed to fetch task ratings', error as Error, { taskId: task.arrivy_task_id });
          return [];
        }
      })(),
      
      // Fetch customer notes
      (async (): Promise<CustomerNote[]> => {
        try {
          return await getCustomerNotes(task.arrivy_task_id);
        } catch (error) {
          logError('Failed to fetch customer notes', error as Error, { taskId: task.arrivy_task_id });
          return [];
        }
      })(),
      
      // Fetch crew contacts
      (async (): Promise<CrewContact[]> => {
        if (!task.assigned_entity_ids || task.assigned_entity_ids.length === 0) return [];
        try {
          return await getCrewContactsForTask(task.assigned_entity_ids);
        } catch (error) {
          logError('Failed to fetch crew contacts', error as Error, { entityIds: task.assigned_entity_ids });
          return [];
        }
      })(),
    ]);

    // Calculate duration metrics
    const durationMetrics = calculateTaskDurationMetrics(task, statusHistory);

    // Generate business tracker URL
    const businessTrackerUrl = getBusinessTrackerUrl(task.arrivy_task_id);

    // Compose enhanced response
    const enhancedDetails: EnhancedTaskDetails = {
      task: {
        ...task,
        business_tracker_url: businessTrackerUrl,
        hasQuickBaseLink: task.quickbase_project_id != null,
      },
      statusHistory,
      events,
      attachments,
      ratings,
      customerNotes,
      crewContacts,
      durationMetrics,
    };

    logApiResponse('GET', `/api/operations/field-tracking/tasks/${taskId}`, Date.now() - startedAt, { 
      found: true,
      attachmentsCount: attachments.length,
      ratingsCount: ratings.length,
      customerNotesCount: customerNotes.length,
      crewCount: crewContacts.length,
    }, reqId);

    return NextResponse.json(enhancedDetails, { status: 200 });

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

    // Merge existing task data with updated task data
    // COALESCE logic in upsertArrivyTask will preserve fields when new values are undefined/null
    await upsertArrivyTask({
      arrivy_task_id: updatedTask.id,
      url_safe_id: updatedTask.url_safe_id,
      quickbase_project_id: updatedTask.external_id || task.quickbase_project_id,
      quickbase_record_id: task.quickbase_record_id,
      customer_name: updatedTask.customer_name || task.customer_name,
      customer_phone: updatedTask.customer_phone || task.customer_phone,
      customer_email: updatedTask.customer_email || task.customer_email,
      customer_address: task.customer_address,
      task_type: task.task_type,
      scheduled_start: updatedTask.start_datetime ? new Date(updatedTask.start_datetime) : task.scheduled_start,
      scheduled_end: updatedTask.end_datetime ? new Date(updatedTask.end_datetime) : task.scheduled_end,
      assigned_entity_ids: updatedTask.entity_ids || task.assigned_entity_ids,
      current_status: updatedTask.status || task.current_status,
      tracker_url: task.tracker_url,
      template_id: updatedTask.template_id?.toString() || task.template_id,
      extra_fields: updatedTask.extra_fields || task.extra_fields,
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

