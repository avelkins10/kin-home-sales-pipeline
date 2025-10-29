// app/api/sync/arrivy-future/route.ts
import { NextResponse } from 'next/server';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { upsertArrivyTask, type ArrivyTaskData } from '@/lib/db/arrivy';
import { getCustomerTrackerUrl } from '@/lib/integrations/arrivy/service';
import { logInfo, logError } from '@/lib/logging/logger';
import type { ArrivyTask } from '@/lib/integrations/arrivy/types';

export const runtime = 'nodejs';
export const maxDuration = 300; // 5 minutes

/**
 * Manual trigger endpoint to sync future tasks from Arrivy
 * GET /api/sync/arrivy-future?months=3
 */
export async function GET(request: Request) {
  const startTime = Date.now();

  try {
    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const months = parseInt(searchParams.get('months') || '3', 10);

    if (!arrivyClient) {
      return NextResponse.json(
        { error: 'Arrivy client not configured' },
        { status: 500 }
      );
    }

    // Calculate date range: today -> N months ahead
    const today = new Date();
    const futureDate = new Date(today);
    futureDate.setMonth(futureDate.getMonth() + months);

    const startDate = today.toISOString().split('T')[0];
    const endDate = futureDate.toISOString().split('T')[0];

    logInfo(`[Arrivy Sync] Starting future task sync from ${startDate} to ${endDate}`);

    // Fetch ALL tasks from Arrivy (no date filter)
    // The start_date/end_date parameters filter by creation date, not scheduled date
    // So we fetch all tasks and filter by scheduled date locally
    logInfo('[Arrivy Sync] Fetching all tasks from Arrivy (API date filters don\'t work as expected)');
    const tasks = await arrivyClient.listTasks();

    logInfo(`[Arrivy Sync] Fetched ${tasks.length} total tasks from Arrivy`);

    // Filter to only tasks scheduled from today forward
    const futureTasks = tasks.filter(task => {
      if (!task.start_datetime) return false;
      const scheduledDate = new Date(task.start_datetime);
      const todayStart = new Date(today.getFullYear(), today.getMonth(), today.getDate());
      return scheduledDate >= todayStart;
    });

    logInfo(`[Arrivy Sync] Found ${futureTasks.length} tasks scheduled from today forward`);

    // Sync each task
    let created = 0;
    let updated = 0;
    let errors = 0;
    let skipped = 0;

    for (const task of futureTasks) {
      try {
        const exists = await upsertArrivyTask({
          arrivy_task_id: task.id,
          url_safe_id: task.url_safe_id,
          quickbase_project_id: task.external_id || null,
          quickbase_record_id: null,
          customer_name: task.customer_name,
          customer_phone: task.customer_phone,
          customer_email: task.customer_email,
          customer_address: formatAddress(task),
          task_type: extractTaskType(task),
          scheduled_start: task.start_datetime ? new Date(task.start_datetime) : null,
          scheduled_end: task.end_datetime ? new Date(task.end_datetime) : null,
          assigned_entity_ids: task.entity_ids || [],
          current_status: task.status || 'NOT_STARTED',
          tracker_url: getCustomerTrackerUrl(task.id, task.url_safe_id),
          template_id: task.template_id?.toString(),
          extra_fields: task.extra_fields,
          synced_at: new Date(),
        });

        if (exists.created_at.getTime() === exists.updated_at.getTime()) {
          created++;
        } else {
          updated++;
        }
      } catch (error) {
        errors++;
        logError('[Arrivy Sync] Failed to sync task', error as Error, {
          task_id: task.id,
        });
      }
    }

    const duration = Date.now() - startTime;

    const result = {
      success: true,
      stats: {
        totalFetched: tasks.length,
        futureTasksFound: futureTasks.length,
        created,
        updated,
        errors,
      },
      dateRange: {
        start: startDate,
        end: endDate,
      },
      durationMs: duration,
      timestamp: new Date().toISOString(),
    };

    logInfo('[Arrivy Sync] Future task sync completed', result);

    return NextResponse.json(result);
  } catch (error) {
    logError('[Arrivy Sync] Future task sync failed', error as Error);

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

// Helper functions
function formatAddress(task: ArrivyTask): string | undefined {
  const parts = [
    task.customer_address_line_1,
    task.customer_city,
    task.customer_state,
    task.customer_zipcode
  ].filter(Boolean);
  return parts.length > 0 ? parts.join(', ') : undefined;
}

function extractTaskType(task: ArrivyTask): string {
  if (task.extra_fields?.task_type) {
    return task.extra_fields.task_type;
  }
  // Try to infer from title
  const title = task.title?.toLowerCase() || '';
  if (title.includes('survey')) return 'survey';
  if (title.includes('install')) return 'install';
  if (title.includes('inspection')) return 'inspection';
  return 'service'; // default
}
