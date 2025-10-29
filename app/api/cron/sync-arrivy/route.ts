// app/api/cron/sync-arrivy/route.ts
export const runtime = 'nodejs';
export const maxDuration = 300; // 5 minutes for large syncs

import { NextRequest, NextResponse } from 'next/server';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import {
  upsertArrivyTask,
  upsertArrivyEntity,
  getArrivyTaskByArrivyId,
  insertArrivyTaskStatus,
  insertArrivyTaskAttachment,
  type ArrivyTaskData,
  type ArrivyEntityData,
  type ArrivyTaskStatusData,
  type ArrivyTaskAttachmentData
} from '@/lib/db/arrivy';
import { getCustomerTrackerUrl } from '@/lib/integrations/arrivy/service';
import type { ArrivyTask, ArrivyEntity, ArrivyTaskStatus } from '@/lib/integrations/arrivy/types';
import { logInfo, logError } from '@/lib/logging/logger';

interface SyncStats {
  totalTasks: number;
  tasksCreated: number;
  tasksUpdated: number;
  entitiesSynced: number;
  statusHistorySynced: number;
  attachmentsSynced: number;
  errors: number;
  duration: number;
}

/**
 * Vercel Cron endpoint for automated Arrivy task synchronization
 *
 * Runs periodically to:
 * - Sync recent tasks from Arrivy
 * - Update status history
 * - Cache attachments
 * - Keep production database current
 *
 * Works alongside webhooks as a safety net to catch missed events
 */
export async function GET(request: NextRequest) {
  const startTime = Date.now();

  // Verify cron secret for security
  const authHeader = request.headers.get('authorization');
  const cronSecret = process.env.CRON_SECRET;

  if (cronSecret && authHeader !== `Bearer ${cronSecret}`) {
    logError('Unauthorized sync-arrivy cron attempt', new Error('Invalid auth'), {});
    return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
  }

  try {
    // Get query parameters for custom date range (optional)
    const searchParams = request.nextUrl.searchParams;
    const daysBack = parseInt(searchParams.get('days') || '3', 10); // Changed from 7 to 3 days since we run hourly
    const backfill = searchParams.get('backfill') === 'true';
    const syncEntities = searchParams.get('entities') === 'true';

    // Calculate date range
    const endDate = new Date();
    const startDate = new Date();

    if (backfill) {
      // Backfill mode: sync from Oct 22, 2025 to now
      startDate.setFullYear(2025, 9, 22); // Oct 22, 2025
      logInfo('[Arrivy Cron] Running BACKFILL sync (Oct 22 - present)');
    } else {
      // Normal mode: sync last N days
      startDate.setDate(startDate.getDate() - daysBack);
      logInfo(`[Arrivy Cron] Running hourly sync (last ${daysBack} days)`);
    }

    const stats: SyncStats = {
      totalTasks: 0,
      tasksCreated: 0,
      tasksUpdated: 0,
      entitiesSynced: 0,
      statusHistorySynced: 0,
      attachmentsSynced: 0,
      errors: 0,
      duration: 0,
    };

    // Verify Arrivy client is configured
    if (!arrivyClient) {
      throw new Error('Arrivy client not configured');
    }

    // Step 1: Sync entities if requested
    if (syncEntities) {
      logInfo('[Arrivy Cron] Syncing entities...');
      const entities = await arrivyClient.getEntities();

      for (const entity of entities) {
        try {
          const entityData: ArrivyEntityData = {
            arrivy_entity_id: entity.id,
            name: entity.name,
            entity_type: entity.type,
            phone: entity.phone,
            email: entity.email,
            color: entity.color,
            image_url: entity.image_url,
            synced_at: new Date(),
          };

          await upsertArrivyEntity(entityData);
          stats.entitiesSynced++;
        } catch (error) {
          logError(`Failed to sync entity ${entity.id}`, error as Error, { entityId: entity.id });
          stats.errors++;
        }
      }

      logInfo(`[Arrivy Cron] Synced ${stats.entitiesSynced} entities`);
    }

    // Step 2: Fetch tasks from Arrivy
    logInfo('[Arrivy Cron] Fetching tasks from Arrivy...', {
      startDate: startDate.toISOString(),
      endDate: endDate.toISOString(),
    });

    const tasks = await arrivyClient.getTasks({
      start_date: startDate,
      end_date: endDate,
    });

    stats.totalTasks = tasks.length;
    logInfo(`[Arrivy Cron] Found ${tasks.length} tasks`);

    // Step 3: Sync each task
    for (const task of tasks) {
      try {
        // Check if task exists
        const existingTask = await getArrivyTaskByArrivyId(task.id);
        const isNewTask = !existingTask;

        // Prepare task data
        const taskData: ArrivyTaskData = {
          arrivy_task_id: task.id,
          url_safe_id: task.url_safe_id,
          quickbase_project_id: task.external_id || null,
          quickbase_record_id: null,
          customer_name: task.customer_name || null,
          customer_phone: task.customer_phone || null,
          customer_email: task.customer_email || null,
          customer_address: task.customer_address || null,
          task_type: null, // Will be detected from template
          scheduled_start: task.start_datetime ? new Date(task.start_datetime) : null,
          scheduled_end: task.end_datetime ? new Date(task.end_datetime) : null,
          assigned_entity_ids: task.entity_ids || [],
          current_status: task.status || 'NOT_STARTED',
          tracker_url: getCustomerTrackerUrl(task.id, task.url_safe_id),
          template_id: task.template_id?.toString() || null,
          extra_fields: task.extra_fields || null,
          synced_at: new Date(),
        };

        // Upsert task
        await upsertArrivyTask(taskData);

        if (isNewTask) {
          stats.tasksCreated++;
        } else {
          stats.tasksUpdated++;
        }

        // Step 4: Sync status history for each task
        try {
          const statusHistory = await arrivyClient.getTaskStatuses(task.id);

          for (const status of statusHistory) {
            const statusData: ArrivyTaskStatusData = {
              arrivy_task_id: task.id,
              status_type: status.type,
              reporter_id: status.reporter_id,
              reporter_name: status.reporter_name,
              reported_at: new Date(status.time),
              notes: status.notes || null,
              has_attachments: status.files && status.files.length > 0,
              visible_to_customer: true,
              source: 'sync',
            };

            await insertArrivyTaskStatus(statusData);
            stats.statusHistorySynced++;

            // Step 5: Sync attachments for this status (Phase 2 feature)
            if (status.files && status.files.length > 0) {
              for (const file of status.files) {
                try {
                  const attachmentData: ArrivyTaskAttachmentData = {
                    arrivy_task_id: task.id,
                    arrivy_status_id: status.id,
                    file_id: file.file_id,
                    file_path: file.file_path,
                    filename: file.filename,
                    uploaded_by: status.reporter_name || null,
                    uploaded_at: new Date(status.time),
                  };

                  const result = await insertArrivyTaskAttachment(attachmentData);
                  if (result) {
                    stats.attachmentsSynced++;
                  }
                } catch (error) {
                  // Ignore duplicate attachment errors (already synced)
                  if (!(error as Error).message?.includes('duplicate')) {
                    logError(`Failed to sync attachment ${file.file_id}`, error as Error, {
                      taskId: task.id,
                      statusId: status.id,
                    });
                  }
                }
              }
            }
          }
        } catch (error) {
          logError(`Failed to sync status history for task ${task.id}`, error as Error, { taskId: task.id });
          stats.errors++;
        }

      } catch (error) {
        logError(`Failed to sync task ${task.id}`, error as Error, { taskId: task.id });
        stats.errors++;
      }
    }

    // Calculate stats
    stats.duration = Date.now() - startTime;
    const successRate = stats.totalTasks > 0
      ? ((stats.tasksCreated + stats.tasksUpdated) / stats.totalTasks * 100).toFixed(1)
      : '0';

    logInfo('[Arrivy Cron] Sync completed', {
      stats,
      successRate: `${successRate}%`,
    });

    return NextResponse.json({
      success: true,
      stats,
      successRate: `${successRate}%`,
      timestamp: new Date().toISOString(),
      mode: backfill ? 'backfill' : 'daily',
      dateRange: {
        start: startDate.toISOString(),
        end: endDate.toISOString(),
      },
    });

  } catch (error) {
    const duration = Date.now() - startTime;
    logError('[Arrivy Cron] Sync failed', error as Error, { duration });

    return NextResponse.json({
      success: false,
      error: error instanceof Error ? error.message : 'Unknown error',
      duration,
      timestamp: new Date().toISOString(),
    }, { status: 500 });
  }
}
