/**
 * Initial sync script to fetch all existing tasks from Arrivy and populate local database
 * 
 * This script:
 * - Fetches all tasks from Arrivy API in date range batches
 * - Optionally syncs entities (crew members) first
 * - Maps Arrivy task fields to local database schema
 * - Uses upsertArrivyTask() for idempotent inserts/updates
 * - Provides detailed progress tracking and summary statistics
 * 
 * Usage:
 *   npm run sync:arrivy                              # Full sync of all tasks
 *   npm run sync:arrivy:dry-run                      # Preview without making changes
 *   npm run sync:arrivy:verbose                      # Detailed logging
 *   npm run sync:arrivy:entities                     # Sync entities before tasks
 *   npm run sync:arrivy -- --start-date=2024-01-01   # Custom date range
 *   npm run sync:arrivy -- --limit=10                # Test with first 10 tasks
 * 
 * @author Kin Home Sales Pipeline Team
 * @version 1.0.0
 */

import path from 'path';
import { sql } from '@vercel/postgres';
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
import type { ArrivyTask, ArrivyEntity } from '@/lib/integrations/arrivy/types';

interface SyncOptions {
  dryRun: boolean;
  verbose: boolean;
  startDate?: string; // YYYY-MM-DD
  endDate?: string; // YYYY-MM-DD
  limit?: number;
  syncEntities: boolean;
}

interface SyncStats {
  totalTasks: number;
  tasksCreated: number;
  tasksUpdated: number;
  entitiesSynced: number;
  errors: number;
  errorDetails: Array<{ taskId: number; error: string }>;
  skipped: number;
  duration: number;
}

interface SyncResult {
  success: boolean;
  stats: SyncStats;
  executionTimeMs: number;
  timestamp: string;
}

interface DateRange {
  start: Date;
  end: Date;
}

class ArrivySyncService {
  private stats: SyncStats;

  constructor() {
    this.stats = {
      totalTasks: 0,
      tasksCreated: 0,
      tasksUpdated: 0,
      entitiesSynced: 0,
      errors: 0,
      errorDetails: [],
      skipped: 0,
      duration: 0,
    };
  }

  async sync(options: SyncOptions): Promise<SyncResult> {
    const startTime = Date.now();
    const timestamp = new Date().toISOString();

    console.log('🚀 Starting Arrivy task sync...');
    console.log(`Options: ${JSON.stringify(options, null, 2)}`);

    // Verify environment
    await this.verifyEnvironment();

    try {
      // Step 1: Sync entities if requested
      if (options.syncEntities) {
        console.log('\n📋 Step 1: Syncing entities...');
        await this.syncEntitiesFromArrivy(options);
      }

      // Step 2: Determine date ranges
      const dateRanges = this.generateDateRanges(options);
      console.log(`\n📅 Fetching tasks in ${dateRanges.length} batches...`);

      // Step 3: Fetch and process tasks in batches
      let processedCount = 0;
      for (let i = 0; i < dateRanges.length; i++) {
        const range = dateRanges[i];
        console.log(`\n📦 Processing batch ${i + 1}/${dateRanges.length}`);
        console.log(`   Date range: ${range.start.toISOString().split('T')[0]} to ${range.end.toISOString().split('T')[0]}`);

        try {
          // Fetch tasks with adaptive sub-chunking for pagination handling
          const tasks = await this.fetchTasksWithAdaptiveChunking(range, options);

          console.log(`   Found ${tasks.length} tasks in this batch`);

          // Process each task
          for (const task of tasks) {
            // Check limit
            if (options.limit && processedCount >= options.limit) {
              console.log(`\n⏸️  Reached limit of ${options.limit} tasks`);
              break;
            }

            await this.syncTask(task, options);
            processedCount++;

            // Show progress every 10 tasks
            if (!options.verbose && processedCount % 10 === 0) {
              console.log(`   📈 Processed ${processedCount} tasks...`);
            }
          }

          // Check if we hit the limit
          if (options.limit && processedCount >= options.limit) {
            break;
          }
        } catch (error) {
          console.error(`   ❌ Error fetching batch ${i + 1}:`, error instanceof Error ? error.message : error);
          this.stats.errors++;
        }
      }

      this.stats.totalTasks = processedCount;

      // Print summary
      this.printSummary(options);

      const executionTimeMs = Date.now() - startTime;
      this.stats.duration = executionTimeMs;
      const success = this.stats.errors === 0 || this.stats.errors < this.stats.totalTasks * 0.05; // Allow up to 5% error rate

      return {
        success,
        stats: { ...this.stats },
        executionTimeMs,
        timestamp,
      };
    } catch (error) {
      console.error('❌ Sync failed:', error);
      const executionTimeMs = Date.now() - startTime;

      return {
        success: false,
        stats: { ...this.stats },
        executionTimeMs,
        timestamp,
      };
    }
  }

  /**
   * Fetch tasks with adaptive sub-chunking to handle implicit pagination limits
   * If a date range returns a suspected threshold count (1000 tasks), split the range
   * and recursively fetch to ensure complete coverage
   */
  private async fetchTasksWithAdaptiveChunking(
    range: DateRange,
    options: SyncOptions,
    depth: number = 0
  ): Promise<ArrivyTask[]> {
    const SUSPECTED_LIMIT = 1000; // Arrivy's implicit pagination threshold
    const MAX_DEPTH = 10; // Prevent infinite recursion

    if (!arrivyClient) {
      throw new Error('Arrivy client not configured');
    }

    if (depth > MAX_DEPTH) {
      console.warn(`   ⚠️  Max recursion depth reached for range ${range.start.toISOString()} to ${range.end.toISOString()}`);
      return [];
    }

    // Fetch tasks for this date range
    const tasks = await arrivyClient.listTasks({
      start_date: range.start.toISOString(),
      end_date: range.end.toISOString(),
    });

    // If we hit the suspected limit, split the range and recurse
    if (tasks.length >= SUSPECTED_LIMIT) {
      const rangeMs = range.end.getTime() - range.start.getTime();
      
      // Only split if the range is more than 1 hour (prevent infinite splitting)
      if (rangeMs > 60 * 60 * 1000) {
        if (options.verbose) {
          console.log(`   🔄 Suspected pagination limit hit (${tasks.length} tasks), splitting range...`);
        }

        const midpoint = new Date(range.start.getTime() + rangeMs / 2);
        
        // Recursively fetch first half
        const firstHalf = await this.fetchTasksWithAdaptiveChunking(
          { start: range.start, end: midpoint },
          options,
          depth + 1
        );

        // Recursively fetch second half (start 1ms after midpoint to avoid overlap)
        const secondHalf = await this.fetchTasksWithAdaptiveChunking(
          { start: new Date(midpoint.getTime() + 1), end: range.end },
          options,
          depth + 1
        );

        // Combine and deduplicate by arrivy_task_id
        const allTasks = [...firstHalf, ...secondHalf];
        const uniqueTasks = Array.from(
          new Map(allTasks.map(task => [task.id, task])).values()
        );

        if (options.verbose) {
          console.log(`   ✅ Split yielded ${uniqueTasks.length} unique tasks (from ${allTasks.length} total)`);
        }

        return uniqueTasks;
      } else {
        // Range too small to split further, return what we have
        if (options.verbose) {
          console.log(`   ⚠️  Range too small to split (${Math.round(rangeMs / 1000)}s), returning ${tasks.length} tasks`);
        }
        return tasks;
      }
    }

    // Normal case: returned fewer than threshold
    return tasks;
  }

  private async syncEntitiesFromArrivy(options: SyncOptions): Promise<void> {
    try {
      if (!arrivyClient) {
        throw new Error('Arrivy client not configured');
      }

      const entities = await arrivyClient.listEntities();
      console.log(`📊 Found ${entities.length} entities to sync`);

      for (const entity of entities) {
        try {
          const entityData: ArrivyEntityData = {
            arrivy_entity_id: entity.id,
            name: entity.name || `Entity ${entity.id}`,
            email: entity.email || null,
            phone: entity.phone || entity.mobile_number || null,
            entity_type: entity.type || 'CREW',
            quickbase_user_id: entity.extra_fields?.quickbase_user_id || null,
            extra_fields: entity.extra_fields || null,
          };

          if (!options.dryRun) {
            await upsertArrivyEntity(entityData);
            this.stats.entitiesSynced++;

            if (options.verbose) {
              console.log(`   ✅ Synced entity: ${entity.name} (${entity.id})`);
            }
          } else {
            this.stats.entitiesSynced++;
            if (options.verbose) {
              console.log(`   🔍 [DRY RUN] Would sync entity: ${entity.name} (${entity.id})`);
            }
          }
        } catch (error) {
          console.error(`   ❌ Error syncing entity ${entity.id}:`, error instanceof Error ? error.message : error);
          this.stats.errors++;
        }
      }

      console.log(`✅ Entities synced: ${this.stats.entitiesSynced}/${entities.length}`);
    } catch (error) {
      console.error('❌ Failed to sync entities:', error);
      throw error;
    }
  }

  private async syncTask(task: ArrivyTask, options: SyncOptions): Promise<void> {
    try {
      if (options.verbose) {
        console.log(`   🔄 Syncing task ${task.id}: ${task.title || task.customer_name}`);
      }

      if (options.dryRun) {
        this.stats.skipped++;
        if (options.verbose) {
          console.log(`   🔍 [DRY RUN] Would sync task ${task.id}`);
        }
        return;
      }

      // Check if task already exists to track created vs updated
      const existingTask = await getArrivyTaskByArrivyId(task.id);
      const isUpdate = !!existingTask;

      // Determine QuickBase IDs - preserve existing valid IDs if updating
      let qbProjectId = task.external_id || null;
      let qbRecordId = null;

      // Preserve existing QuickBase IDs if they exist and are valid
      if (existingTask) {
        if (existingTask.quickbase_project_id && 
            !existingTask.quickbase_project_id.startsWith('ARRIVY-')) {
          qbProjectId = existingTask.quickbase_project_id;
        }
        if (existingTask.quickbase_record_id && 
            existingTask.quickbase_record_id !== 0) {
          qbRecordId = existingTask.quickbase_record_id;
        }
      }

      // Map Arrivy task to database schema
      const taskData: ArrivyTaskData = {
        arrivy_task_id: task.id,
        url_safe_id: task.url_safe_id,
        quickbase_project_id: qbProjectId,
        quickbase_record_id: qbRecordId,
        customer_name: task.customer_name || null,
        customer_phone: task.customer_phone || task.customer_mobile_number || null,
        customer_email: task.customer_email || null,
        customer_address: this.formatAddress(task),
        task_type: this.extractTaskType(task),
        scheduled_start: task.start_datetime ? new Date(task.start_datetime) : null,
        scheduled_end: task.end_datetime ? new Date(task.end_datetime) : null,
        assigned_entity_ids: task.entity_ids || [],
        current_status: task.status || 'NOT_STARTED',
        tracker_url: getCustomerTrackerUrl(task.id, task.url_safe_id),
        template_id: task.template_id?.toString() || null,
        extra_fields: task.extra_fields || null,
        synced_at: new Date(),
      };

      // Upsert task with retry logic
      await this.syncTaskWithRetry(taskData, options);

      // Update stats
      if (isUpdate) {
        this.stats.tasksUpdated++;
        if (options.verbose) {
          console.log(`   ✅ Updated task ${task.id} (${qbProjectId ? 'linked to QB' : 'Arrivy-only'})`);
        }
      } else {
        this.stats.tasksCreated++;
        if (options.verbose) {
          console.log(`   ✅ Created task ${task.id} (${qbProjectId ? 'linked to QB' : 'Arrivy-only'})`);
        }
      }
    } catch (error) {
      this.stats.errors++;
      const errorMessage = error instanceof Error ? error.message : String(error);
      this.stats.errorDetails.push({
        taskId: task.id,
        error: errorMessage,
      });

      console.error(`   ❌ Error syncing task ${task.id}:`, errorMessage);
      // Continue processing other tasks
    }
  }

  private async syncTaskWithRetry(
    taskData: ArrivyTaskData,
    options: SyncOptions,
    maxRetries: number = 3
  ): Promise<void> {
    let lastError: Error | null = null;

    for (let attempt = 1; attempt <= maxRetries; attempt++) {
      try {
        await upsertArrivyTask(taskData);

        // After successfully syncing the task, also sync its status history
        if (!options.dryRun) {
          await this.syncTaskStatusHistory(taskData.arrivy_task_id, options);
        }

        return; // Success
      } catch (error) {
        lastError = error instanceof Error ? error : new Error(String(error));

        // Check if error is retryable
        const isRetryable = this.isRetryableError(lastError.message);

        if (!isRetryable || attempt === maxRetries) {
          throw lastError; // Give up
        }

        // Wait with exponential backoff
        const delayMs = Math.min(1000 * Math.pow(2, attempt - 1), 8000); // Max 8 seconds
        if (options.verbose) {
          console.log(`   🔄 Retrying task ${taskData.arrivy_task_id} (attempt ${attempt}/${maxRetries}) after ${delayMs}ms`);
        }

        await new Promise(resolve => setTimeout(resolve, delayMs));
      }
    }

    // All retries exhausted
    if (lastError) {
      throw lastError;
    }
  }

  /**
   * Sync status history for a task
   */
  private async syncTaskStatusHistory(
    arrivyTaskId: number,
    options: SyncOptions
  ): Promise<number> {
    try {
      if (!arrivyClient) {
        throw new Error('Arrivy client not configured');
      }

      // Fetch status history from Arrivy
      const statuses = await arrivyClient.getTaskStatuses(arrivyTaskId);

      if (options.dryRun) {
        if (options.verbose) {
          console.log(`   📋 Would sync ${statuses.length} status entries for task ${arrivyTaskId}`);
        }
        return statuses.length;
      }

      // Insert each status into database
      let syncedCount = 0;
      for (const status of statuses) {
        try {
          const statusData: ArrivyTaskStatusData = {
            arrivy_task_id: arrivyTaskId,
            status_type: status.type,
            reporter_id: status.reporter_id || null,
            reporter_name: status.reporter_name || null,
            reported_at: new Date(status.time),
            notes: status.notes || null,
            has_attachments: status.files && status.files.length > 0,
            visible_to_customer: status.visible_to_customer !== false,
            source: status.source || null,
          };

          await insertArrivyTaskStatus(statusData);
          syncedCount++;

          // Also sync attachments for this status if present
          if (status.files && status.files.length > 0) {
            let attachmentsSynced = 0;
            for (const file of status.files) {
              try {
                const attachmentData: ArrivyTaskAttachmentData = {
                  arrivy_task_id: arrivyTaskId,
                  arrivy_status_id: status.id,
                  file_id: file.file_id,
                  file_path: file.file_path,
                  filename: file.filename,
                  uploaded_by: status.reporter_name || null,
                  uploaded_at: new Date(status.time),
                };

                const result = await insertArrivyTaskAttachment(attachmentData);
                if (result) {
                  attachmentsSynced++;
                }
              } catch (error) {
                // Log but don't fail - attachments are non-critical
                if (options.verbose) {
                  console.error(`   ⚠️  Failed to sync attachment ${file.file_id}:`, error);
                }
              }
            }

            if (options.verbose && attachmentsSynced > 0) {
              console.log(`   📎 Synced ${attachmentsSynced} attachment(s) for status ${status.id}`);
            }
          }
        } catch (error) {
          // Log error but continue with other statuses
          if (options.verbose) {
            console.error(`   ⚠️  Failed to sync status ${status.id}:`, error);
          }
        }
      }

      if (options.verbose && syncedCount > 0) {
        console.log(`   📋 Synced ${syncedCount} status entries for task ${arrivyTaskId}`);
      }

      return syncedCount;
    } catch (error) {
      // Non-fatal error - log and continue
      if (options.verbose) {
        console.error(`   ⚠️  Failed to fetch status history for task ${arrivyTaskId}:`, error);
      }
      return 0;
    }
  }

  private isRetryableError(errorMessage: string): boolean {
    const retryablePatterns = [
      /timeout/i,
      /network/i,
      /connection/i,
      /5\d\d/i, // 5xx HTTP status codes
      /rate.?limit/i,
      /429/i,
      /temporary/i,
      /unavailable/i,
      /ECONNRESET/i,
      /ETIMEDOUT/i,
    ];

    return retryablePatterns.some(pattern => pattern.test(errorMessage));
  }

  private formatAddress(task: ArrivyTask): string | null {
    const parts = [
      task.customer_address_line_1,
      task.customer_city,
      task.customer_state,
      task.customer_zipcode,
    ].filter(Boolean);

    return parts.length > 0 ? parts.join(', ') : null;
  }

  private extractTaskType(task: ArrivyTask): string | null {
    // Try to extract task type from extra_fields first
    if (task.extra_fields?.task_type) {
      return task.extra_fields.task_type.toLowerCase();
    }

    // Check extra_fields for task-type indicators (e.g., "Notes for Surveyor" → survey)
    if (task.extra_fields) {
      const fieldKeys = Object.keys(task.extra_fields).join(' ').toLowerCase();
      if (fieldKeys.includes('surveyor') || fieldKeys.includes('survey')) return 'survey';
      if (fieldKeys.includes('install')) return 'install';
      if (fieldKeys.includes('inspection') || fieldKeys.includes('inspector')) return 'inspection';
    }

    // Combine title and customer_name for pattern matching
    const searchText = [
      task.title || '',
      task.customer_name || '',
      task.description || ''
    ].join(' ').toLowerCase();

    // Check for specific task types with priority order
    // 1. Survey (most common in this dataset)
    if (searchText.includes('survey') || searchText.includes('site survey')) {
      return 'survey';
    }

    // 2. Installation types
    if (searchText.match(/\b(install|installation|pv|solar|roof\s*pv|panel|electrical)\b/)) {
      if (searchText.includes('roof') || searchText.includes('pv') || searchText.includes('solar')) {
        return 'install'; // Solar/PV installation
      }
      if (searchText.includes('electrical')) {
        return 'install'; // Electrical installation
      }
      if (searchText.includes('install')) {
        return 'install';
      }
    }

    // 3. Inspection
    if (searchText.match(/\b(inspection|inspect|audit)\b/)) {
      return 'inspection';
    }

    // 4. Service/Maintenance
    if (searchText.match(/\b(service|maintenance|repair|fix)\b/)) {
      return 'service';
    }

    // 5. Booking/Appointment (appears in test data)
    if (searchText.includes('booking')) {
      return 'other'; // Generic appointment
    }

    // Try to infer from template
    if (task.template) {
      const template = String(task.template).toLowerCase();
      if (template.includes('survey')) return 'survey';
      if (template.includes('install')) return 'install';
      if (template.includes('inspection')) return 'inspection';
      if (template.includes('service')) return 'service';
    }

    // Default to null if no pattern matches
    return null;
  }

  private generateDateRanges(options: SyncOptions): DateRange[] {
    const ranges: DateRange[] = [];
    
    // Default: last 2 years
    let endDate = options.endDate ? new Date(options.endDate) : new Date();
    // Normalize end date to end of day (23:59:59.999)
    endDate = new Date(endDate.getFullYear(), endDate.getMonth(), endDate.getDate(), 23, 59, 59, 999);
    
    const startDate = options.startDate 
      ? new Date(options.startDate)
      : new Date(endDate.getFullYear() - 2, endDate.getMonth(), endDate.getDate());

    // Split into 3-month chunks to avoid API timeouts and memory issues
    const chunkMonths = 3;
    let currentStart = new Date(startDate);

    while (currentStart < endDate) {
      const currentEnd = new Date(currentStart);
      currentEnd.setMonth(currentEnd.getMonth() + chunkMonths);

      // Don't go past the end date
      if (currentEnd > endDate) {
        currentEnd.setTime(endDate.getTime());
      } else {
        // Set to end of day (23:59:59.999) to include all tasks on that day
        currentEnd.setHours(23, 59, 59, 999);
      }

      ranges.push({
        start: new Date(currentStart),
        end: new Date(currentEnd),
      });

      // Next start is 1ms after previous end to avoid overlap gaps
      currentStart = new Date(currentEnd.getTime() + 1);
    }

    return ranges;
  }

  private async verifyEnvironment(): Promise<void> {
    const requiredEnvVars = ['ARRIVY_AUTH_KEY', 'ARRIVY_AUTH_TOKEN', 'DATABASE_URL'];

    for (const envVar of requiredEnvVars) {
      if (!process.env[envVar]) {
        throw new Error(`Missing required environment variable: ${envVar}`);
      }
    }

    // Check if Arrivy client is configured
    if (!arrivyClient) {
      throw new Error('Arrivy client not configured. Check ARRIVY_AUTH_KEY and ARRIVY_AUTH_TOKEN.');
    }

    // Test database connection
    try {
      await sql`SELECT 1 as test`;
      console.log('✅ Database connection verified');
    } catch (error) {
      throw new Error(`Database connection failed: ${error}`);
    }

    // Test Arrivy API connection with minimal 1-minute window query
    try {
      const now = new Date();
      const oneMinuteAgo = new Date(now.getTime() - 60000);
      await arrivyClient.listTasks({ 
        start_date: oneMinuteAgo.toISOString(),
        end_date: now.toISOString()
      });
      console.log('✅ Arrivy API connection verified');
    } catch (error) {
      throw new Error(`Arrivy API connection failed: ${error}`);
    }

    console.log('✅ Environment verification complete');
  }

  private printSummary(options: SyncOptions): void {
    const timestamp = new Date().toISOString();
    const successRate = this.stats.totalTasks > 0
      ? Math.round(((this.stats.tasksCreated + this.stats.tasksUpdated) / this.stats.totalTasks) * 100)
      : 0;

    console.log('\n📊 SYNC SUMMARY');
    console.log('='.repeat(60));
    console.log(`Timestamp: ${timestamp}`);
    console.log(`Total tasks processed: ${this.stats.totalTasks}`);

    if (options.dryRun) {
      console.log(`Would sync: ${this.stats.skipped} tasks (DRY RUN)`);
      console.log(`Entities synced: ${this.stats.entitiesSynced} (DRY RUN)`);
    } else {
      console.log(`Tasks created: ${this.stats.tasksCreated}`);
      console.log(`Tasks updated: ${this.stats.tasksUpdated}`);
      console.log(`Success rate: ${successRate}%`);
      if (options.syncEntities) {
        console.log(`Entities synced: ${this.stats.entitiesSynced}`);
      }
    }

    console.log(`Errors: ${this.stats.errors}`);

    if (this.stats.skipped > 0 && !options.dryRun) {
      console.log(`Skipped: ${this.stats.skipped}`);
    }

    // Warnings
    if (this.stats.errors > 0) {
      const errorRate = Math.round((this.stats.errors / this.stats.totalTasks) * 100);
      if (errorRate > 5) {
        console.log(`\n⚠️  WARNING: High error rate (${errorRate}%)`);
        console.log('   Review errors and check Arrivy API credentials');
      }

      // Show error details (first 10)
      console.log(`\n❌ ERROR DETAILS (${Math.min(this.stats.errorDetails.length, 10)} of ${this.stats.errorDetails.length}):`);
      this.stats.errorDetails.slice(0, 10).forEach(({ taskId, error }) => {
        console.log(`   Task ${taskId}: ${error}`);
      });

      if (this.stats.errorDetails.length > 10) {
        console.log(`   ... and ${this.stats.errorDetails.length - 10} more errors`);
      }
    }

    // Execution time
    const durationSec = Math.round(this.stats.duration / 1000);
    const minutes = Math.floor(durationSec / 60);
    const seconds = durationSec % 60;
    console.log(`\n⏱️  Execution time: ${minutes}m ${seconds}s`);

    // Next steps
    console.log('\n💡 NEXT STEPS:');
    if (options.dryRun) {
      console.log('   • Run without --dry-run to sync tasks to database');
    } else {
      if (this.stats.errors > 0) {
        console.log('   • Review error details above');
        console.log('   • Run with --verbose for detailed logging');
      }
      if (this.stats.tasksCreated + this.stats.tasksUpdated > 0) {
        console.log('   • Verify task count matches Arrivy dashboard');
        console.log('   • Check field tracking dashboard displays tasks correctly');
        console.log('   • Test webhook integration');
      }
      if (!options.syncEntities) {
        console.log('   • Consider running with --sync-entities to sync crew members');
      }
    }

    console.log('\n✅ Sync completed');
  }
}

// =============================================================================
// HELPER FUNCTIONS
// =============================================================================

function parseArgs(): SyncOptions {
  const args = process.argv.slice(2);

  return {
    dryRun: args.includes('--dry-run'),
    verbose: args.includes('--verbose') || args.includes('-v'),
    syncEntities: args.includes('--sync-entities'),
    startDate: args.find(arg => arg.startsWith('--start-date='))?.split('=')[1],
    endDate: args.find(arg => arg.startsWith('--end-date='))?.split('=')[1],
    limit: args.find(arg => arg.startsWith('--limit='))?.split('=')[1]
      ? parseInt(args.find(arg => arg.startsWith('--limit='))!.split('=')[1])
      : undefined,
  };
}

// =============================================================================
// EXPORT FOR API USAGE
// =============================================================================

async function runSync(options: SyncOptions): Promise<SyncResult> {
  const syncService = new ArrivySyncService();
  const result = await syncService.sync(options);

  return result;
}

// =============================================================================
// MAIN EXECUTION
// =============================================================================

if (require.main === module) {
  // Load environment variables for CLI usage only (not in production)
  // Only load .env.local if required environment variables are not already set
  if (process.env.NODE_ENV !== 'production' && !process.env.ARRIVY_AUTH_KEY) {
    const dotenv = require('dotenv');
    const envPath = path.join(__dirname, '..', '.env.local');
    console.log(`🔧 Loading environment from: ${envPath}`);
    dotenv.config({ path: envPath });
  } else if (process.env.NODE_ENV === 'production') {
    console.log('🚀 Running in production mode - using environment variables from system');
  } else {
    console.log('✅ Using existing environment variables');
  }

  (async () => {
    try {
      const options = parseArgs();
      const result = await runSync(options);

      // Close database connection for CLI usage
      await sql.end();

      if (result.success) {
        process.exit(0);
      } else {
        process.exit(1);
      }
    } catch (error) {
      console.error('❌ Script failed:', error);
      await sql.end();
      process.exit(1);
    }
  })();
}

export { ArrivySyncService, runSync, SyncResult, SyncOptions, SyncStats };

