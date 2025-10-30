// app/api/admin/fix-task-status-and-type/route.ts
import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { extractTaskType, calculateArrivalWindow } from '@/lib/integrations/arrivy/utils';
import { formatTaskAddress } from '@/lib/integrations/arrivy/utils';
import { upsertArrivyTask } from '@/lib/db/arrivy';
import type { ArrivyTaskData } from '@/lib/db/arrivy';

export const runtime = 'nodejs';
export const maxDuration = 300;

/**
 * Force sync task status and type from Arrivy API
 * GET /api/admin/fix-task-status-and-type?customer=Knutsen&updateStatus=true&updateType=true
 */
export async function GET(request: Request) {
  try {
    const { searchParams } = new URL(request.url);
    const customer = searchParams.get('customer') || '';
    const updateStatus = searchParams.get('updateStatus') !== 'false';
    const updateType = searchParams.get('updateType') !== 'false';

    if (!arrivyClient) {
      return NextResponse.json({ error: 'Arrivy client not configured' }, { status: 500 });
    }

    // Get tasks from database
    const { rows: dbTasks } = await sql`
      SELECT arrivy_task_id, customer_name, task_type, current_status
      FROM arrivy_tasks
      WHERE customer_name ILIKE ${'%' + customer + '%'}
      ORDER BY scheduled_start DESC
      LIMIT 10
    `;

    if (dbTasks.length === 0) {
      return NextResponse.json({ error: 'No tasks found' }, { status: 404 });
    }

    const results = [];

    for (const dbTask of dbTasks) {
      try {
        // Fetch fresh data from Arrivy API
        const arrivyTask = await arrivyClient.getTask(dbTask.arrivy_task_id);
        
        // Get latest status from Arrivy
        const statusHistory = await arrivyClient.getTaskStatuses(dbTask.arrivy_task_id);
        const latestStatus = statusHistory.length > 0 
          ? statusHistory[statusHistory.length - 1].type 
          : arrivyTask.status || 'NOT_STARTED';

        // Calculate arrival window
        const scheduledStart = arrivyTask.start_datetime ? new Date(arrivyTask.start_datetime) : null;
        const arrivalWindow = calculateArrivalWindow(
          scheduledStart,
          arrivyTask.time_window_start,
          arrivyTask.duration
        );

        // Detect task type
        const newTaskType = extractTaskType(arrivyTask);

        // Determine what changed
        const statusChanged = updateStatus && latestStatus !== dbTask.current_status;
        const typeChanged = updateType && newTaskType !== dbTask.task_type;

        if (statusChanged || typeChanged) {
          // Get existing task to preserve QuickBase links
          const { rows: existing } = await sql`
            SELECT quickbase_project_id, quickbase_record_id, tracker_url
            FROM arrivy_tasks
            WHERE arrivy_task_id = ${dbTask.arrivy_task_id}
            LIMIT 1
          `;
          const existingTask = existing[0];

          // Update task with latest data
          const taskData: ArrivyTaskData = {
            arrivy_task_id: arrivyTask.id,
            url_safe_id: arrivyTask.url_safe_id,
            quickbase_project_id: existingTask?.quickbase_project_id || arrivyTask.external_id || null,
            quickbase_record_id: existingTask?.quickbase_record_id || null,
            customer_name: arrivyTask.customer_name,
            customer_phone: arrivyTask.customer_phone,
            customer_email: arrivyTask.customer_email,
            customer_address: formatTaskAddress(arrivyTask),
            task_type: newTaskType,
            scheduled_start: scheduledStart ?? undefined,
            scheduled_end: arrivyTask.end_datetime ? new Date(arrivyTask.end_datetime) : undefined,
            start_datetime_window_start: arrivalWindow.start,
            start_datetime_window_end: arrivalWindow.end,
            assigned_entity_ids: arrivyTask.entity_ids || [],
            current_status: latestStatus,
            tracker_url: existingTask?.tracker_url || undefined,
            template_id: arrivyTask.template_id?.toString(),
            extra_fields: arrivyTask.extra_fields,
            synced_at: new Date(),
          };

          await upsertArrivyTask(taskData);

          results.push({
            task_id: dbTask.arrivy_task_id,
            customer: dbTask.customer_name,
            old_status: dbTask.current_status,
            new_status: latestStatus,
            old_type: dbTask.task_type,
            new_type: newTaskType,
            status_changed: statusChanged,
            type_changed: typeChanged,
            arrivy_status: arrivyTask.status,
            latest_status_from_history: latestStatus,
            template_id: arrivyTask.template_id,
            extra_fields_keys: Object.keys(arrivyTask.extra_fields || {}),
          });
        } else {
          results.push({
            task_id: dbTask.arrivy_task_id,
            customer: dbTask.customer_name,
            status: 'no_change',
            current_status: dbTask.current_status,
            current_type: dbTask.task_type,
            arrivy_status: arrivyTask.status,
            latest_status_from_history: latestStatus,
            template_id: arrivyTask.template_id,
            extra_fields_keys: Object.keys(arrivyTask.extra_fields || {}),
          });
        }
      } catch (error) {
        results.push({
          task_id: dbTask.arrivy_task_id,
          customer: dbTask.customer_name,
          error: error instanceof Error ? error.message : 'Unknown error',
        });
      }
    }

    return NextResponse.json({
      success: true,
      updated: results.filter(r => r.status_changed || r.type_changed).length,
      results,
    });
  } catch (error) {
    return NextResponse.json(
      { error: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}

