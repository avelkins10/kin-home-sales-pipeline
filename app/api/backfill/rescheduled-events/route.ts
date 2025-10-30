// app/api/backfill/rescheduled-events/route.ts
import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { upsertArrivyTask, getArrivyTaskByArrivyId } from '@/lib/db/arrivy';
import { getCustomerTrackerUrl } from '@/lib/integrations/arrivy/service';
import { extractTaskType, formatTaskAddress } from '@/lib/integrations/arrivy/utils';
import { logInfo, logError } from '@/lib/logging/logger';
import type { ArrivyTask } from '@/lib/integrations/arrivy/types';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';
export const maxDuration = 60;

/**
 * Backfill existing TASK_RESCHEDULED events to update task schedules
 * GET /api/backfill/rescheduled-events
 */
export async function GET() {
  const startTime = Date.now();

  try {
    if (!arrivyClient) {
      return NextResponse.json(
        { error: 'Arrivy client not configured' },
        { status: 500 }
      );
    }

    // Get all TASK_RESCHEDULED events
    const events = await sql`
      SELECT DISTINCT
        arrivy_task_id,
        message,
        MAX(event_time) as latest_reschedule
      FROM arrivy_events
      WHERE event_type = 'TASK_RESCHEDULED'
      GROUP BY arrivy_task_id, message
      ORDER BY latest_reschedule DESC
    `;

    logInfo('[Backfill] Found TASK_RESCHEDULED events', {
      count: events.rows.length,
    });

    const results = [];
    let updated = 0;
    let errors = 0;

    for (const event of events.rows) {
      try {
        const taskId = parseInt(event.arrivy_task_id);

        // Fetch task from Arrivy API
        const task = await arrivyClient.getTask(taskId);
        if (!task) {
          logError('[Backfill] Task not found in Arrivy', new Error('Not found'), {
            arrivy_task_id: taskId,
          });
          errors++;
          results.push({
            task_id: taskId,
            success: false,
            error: 'Task not found in Arrivy API',
          });
          continue;
        }

        const existingTask = await getArrivyTaskByArrivyId(taskId);

        // Update task
        await upsertArrivyTask({
          arrivy_task_id: task.id,
          url_safe_id: task.url_safe_id,
          quickbase_project_id: existingTask?.quickbase_project_id || task.external_id || null,
          quickbase_record_id: existingTask?.quickbase_record_id || null,
          customer_name: task.customer_name,
          customer_phone: task.customer_phone,
          customer_email: task.customer_email,
          customer_address: formatTaskAddress(task),
          task_type: extractTaskType(task),
          scheduled_start: task.start_datetime ? new Date(task.start_datetime) : null,
          scheduled_end: task.end_datetime ? new Date(task.end_datetime) : null,
          assigned_entity_ids: task.entity_ids || [],
          current_status: task.status || 'NOT_STARTED',
          tracker_url: existingTask?.tracker_url || getCustomerTrackerUrl(task.id, task.url_safe_id),
          template_id: task.template_id?.toString(),
          extra_fields: task.extra_fields,
          synced_at: new Date(),
        });

        updated++;
        results.push({
          task_id: taskId,
          customer_name: task.customer_name,
          scheduled_start: task.start_datetime,
          scheduled_end: task.end_datetime,
          success: true,
        });

        logInfo('[Backfill] Updated task schedule', {
          task_id: taskId,
          customer_name: task.customer_name,
          scheduled_start: task.start_datetime,
        });
      } catch (error) {
        errors++;
        logError('[Backfill] Failed to update task', error as Error, {
          arrivy_task_id: event.arrivy_task_id,
        });
        results.push({
          task_id: event.arrivy_task_id,
          success: false,
          error: error instanceof Error ? error.message : 'Unknown error',
        });
      }
    }

    const duration = Date.now() - startTime;

    return NextResponse.json({
      success: true,
      stats: {
        total: events.rows.length,
        updated,
        errors,
      },
      results,
      durationMs: duration,
    });
  } catch (error) {
    logError('[Backfill] Backfill failed', error as Error);
    return NextResponse.json(
      {
        success: false,
        error: error instanceof Error ? error.message : 'Unknown error',
        durationMs: Date.now() - startTime,
      },
      { status: 500 }
    );
  }
}
