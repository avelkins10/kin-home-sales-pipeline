// scripts/force-fix-tasks.ts
// Force sync Tania and Shawn tasks from Arrivy API
import { arrivyClient } from '../lib/integrations/arrivy/client';
import { sql } from '@vercel/postgres';
import { extractTaskType, calculateArrivalWindow, formatTaskAddress } from '../lib/integrations/arrivy/utils';
import { upsertArrivyTask } from '../lib/db/arrivy';
import type { ArrivyTaskData } from '../lib/db/arrivy';

async function forceFixTasks() {
  if (!arrivyClient) {
    console.error('❌ Arrivy client not configured');
    process.exit(1);
  }

  const customers = ['Tania', 'Shawn', 'Knutsen'];
  
  for (const customer of customers) {
    console.log(`\n🔍 Checking tasks for ${customer}...`);
    
    // Get tasks from database
    const { rows: dbTasks } = await sql`
      SELECT arrivy_task_id, customer_name, task_type, current_status, template_id
      FROM arrivy_tasks
      WHERE customer_name ILIKE ${'%' + customer + '%'}
      ORDER BY scheduled_start DESC
      LIMIT 5
    `;

    if (dbTasks.length === 0) {
      console.log(`   ⚠️  No tasks found for ${customer}`);
      continue;
    }

    for (const dbTask of dbTasks) {
      try {
        console.log(`\n   📋 Task ${dbTask.arrivy_task_id} (${dbTask.customer_name})`);
        console.log(`      Current DB: type="${dbTask.task_type}", status="${dbTask.current_status}"`);

        // Fetch fresh data from Arrivy API
        const arrivyTask = await arrivyClient.getTask(dbTask.arrivy_task_id);
        
        // Get latest status from Arrivy status history
        const statusHistory = await arrivyClient.getTaskStatuses(dbTask.arrivy_task_id);
        const latestStatus = statusHistory.length > 0 
          ? statusHistory[statusHistory.length - 1].type 
          : arrivyTask.status || 'NOT_STARTED';

        console.log(`      Arrivy API: status="${arrivyTask.status}"`);
        console.log(`      Status History (${statusHistory.length} entries):`, 
          statusHistory.map(s => `${s.type}@${s.time}`).join(', '));
        console.log(`      Latest Status: "${latestStatus}"`);
        console.log(`      Template: ${arrivyTask.template} (type: ${typeof arrivyTask.template})`);
        console.log(`      Template ID: ${arrivyTask.template_id}`);
        console.log(`      Extra Fields Keys:`, Object.keys(arrivyTask.extra_fields || {}));

        // Detect task type
        const newTaskType = extractTaskType(arrivyTask);
        console.log(`      Detected Type: "${newTaskType}"`);

        // Calculate arrival window
        const scheduledStart = arrivyTask.start_datetime ? new Date(arrivyTask.start_datetime) : null;
        const arrivalWindow = calculateArrivalWindow(
          scheduledStart,
          arrivyTask.time_window_start,
          arrivyTask.duration
        );

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
          current_status: latestStatus, // Use latest from history
          tracker_url: existingTask?.tracker_url || undefined,
          template_id: arrivyTask.template_id?.toString(),
          extra_fields: arrivyTask.extra_fields,
          synced_at: new Date(),
        };

        await upsertArrivyTask(taskData);

        console.log(`      ✅ Updated: type="${newTaskType}", status="${latestStatus}"`);

      } catch (error) {
        console.error(`      ❌ Error:`, error instanceof Error ? error.message : error);
      }
    }
  }

  console.log('\n✅ Fix complete!');
  process.exit(0);
}

forceFixTasks();





