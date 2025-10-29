/**
 * Production Backfill Sync Script
 * Runs the same logic as /api/cron/sync-arrivy but locally
 */

import { config } from 'dotenv';
import { arrivyClient } from './lib/integrations/arrivy/client';
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
} from './lib/db/arrivy';
import { getCustomerTrackerUrl } from './lib/integrations/arrivy/service';

// Load environment
config({ path: '.env.local' });

interface SyncStats {
  totalTasks: number;
  tasksCreated: number;
  tasksUpdated: number;
  entitiesSynced: number;
  statusHistorySynced: number;
  attachmentsSynced: number;
  errors: number;
}

async function runBackfillSync() {
  const startTime = Date.now();

  console.log('🚀 Starting BACKFILL sync to production database...');
  console.log('📅 Date range: Oct 22, 2025 - Present');
  console.log('⚠️  Database:', process.env.POSTGRES_URL?.substring(0, 50) + '...\n');

  const stats: SyncStats = {
    totalTasks: 0,
    tasksCreated: 0,
    tasksUpdated: 0,
    entitiesSynced: 0,
    statusHistorySynced: 0,
    attachmentsSynced: 0,
    errors: 0,
  };

  try {
    if (!arrivyClient) {
      throw new Error('Arrivy client not configured. Check ARRIVY_AUTH_KEY and ARRIVY_AUTH_TOKEN');
    }

    // Step 1: Sync entities
    console.log('👥 Syncing entities (crew members)...');
    try {
      const entities = await arrivyClient.getEntities();
      console.log(`   Found ${entities.length} entities`);

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

          if (stats.entitiesSynced % 10 === 0) {
            console.log(`   Synced ${stats.entitiesSynced} entities...`);
          }
        } catch (error) {
          console.error(`   ❌ Failed to sync entity ${entity.id}:`, (error as Error).message);
          stats.errors++;
        }
      }

      console.log(`✅ Synced ${stats.entitiesSynced} entities\n`);
    } catch (error) {
      console.error('❌ Failed to sync entities:', (error as Error).message);
    }

    // Step 2: Calculate date range
    const endDate = new Date();
    const startDate = new Date(2025, 9, 22); // Oct 22, 2025

    console.log(`📦 Fetching tasks from Arrivy...`);
    console.log(`   Start: ${startDate.toISOString()}`);
    console.log(`   End: ${endDate.toISOString()}`);

    const tasks = await arrivyClient.getTasks({
      start_date: startDate,
      end_date: endDate,
    });

    stats.totalTasks = tasks.length;
    console.log(`✅ Found ${tasks.length} tasks\n`);

    // Step 3: Sync each task
    console.log('📝 Syncing tasks to database...');
    for (let i = 0; i < tasks.length; i++) {
      const task = tasks[i];

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
          task_type: null,
          scheduled_start: task.start_datetime ? new Date(task.start_datetime) : null,
          scheduled_end: task.end_datetime ? new Date(task.end_datetime) : null,
          assigned_entity_ids: task.entity_ids || [],
          current_status: task.status || 'NOT_STARTED',
          tracker_url: getCustomerTrackerUrl(task.id, task.url_safe_id),
          template_id: task.template_id?.toString() || null,
          extra_fields: task.extra_fields || null,
          synced_at: new Date(),
        };

        await upsertArrivyTask(taskData);

        if (isNewTask) {
          stats.tasksCreated++;
        } else {
          stats.tasksUpdated++;
        }

        // Step 4: Sync status history
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

            // Step 5: Sync attachments
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
                  // Ignore duplicate errors
                  if (!(error as Error).message?.includes('duplicate')) {
                    console.error(`   ⚠️  Failed to sync attachment ${file.file_id}`);
                  }
                }
              }
            }
          }
        } catch (error) {
          console.error(`   ❌ Failed to sync status history for task ${task.id}`);
          stats.errors++;
        }

        // Progress update every 50 tasks
        if ((i + 1) % 50 === 0) {
          console.log(`   Progress: ${i + 1}/${tasks.length} tasks (${Math.round((i + 1) / tasks.length * 100)}%)`);
        }

      } catch (error) {
        console.error(`   ❌ Failed to sync task ${task.id}:`, (error as Error).message);
        stats.errors++;
      }
    }

    // Final stats
    const duration = Date.now() - startTime;
    const successRate = stats.totalTasks > 0
      ? ((stats.tasksCreated + stats.tasksUpdated) / stats.totalTasks * 100).toFixed(1)
      : '0';

    console.log('\n' + '='.repeat(60));
    console.log('✅ BACKFILL SYNC COMPLETE');
    console.log('='.repeat(60));
    console.log(`Total tasks processed:  ${stats.totalTasks}`);
    console.log(`Tasks created:          ${stats.tasksCreated}`);
    console.log(`Tasks updated:          ${stats.tasksUpdated}`);
    console.log(`Entities synced:        ${stats.entitiesSynced}`);
    console.log(`Status history synced:  ${stats.statusHistorySynced}`);
    console.log(`Attachments cached:     ${stats.attachmentsSynced}`);
    console.log(`Errors:                 ${stats.errors}`);
    console.log(`Success rate:           ${successRate}%`);
    console.log(`Duration:               ${Math.round(duration / 1000)}s`);
    console.log('='.repeat(60));

    console.log('\n🎉 Production database is now synced!');
    console.log('📌 Visit /operations/field-tracking to see your tasks');

  } catch (error) {
    console.error('\n❌ Backfill sync failed:', error);
    console.error((error as Error).stack);
    process.exit(1);
  }
}

runBackfillSync();
