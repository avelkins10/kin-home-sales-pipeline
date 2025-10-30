#!/usr/bin/env tsx
/**
 * Emergency script to fix task types for existing Arrivy tasks
 * Run with: npx tsx scripts/fix-task-types-now.ts
 */

import { sql } from '@vercel/postgres';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { extractTaskType } from '@/lib/integrations/arrivy/utils';

async function main() {
  console.log('🔧 Starting task type fix...\n');

  try {
    // Get all tasks that are currently 'service' or generic
    const { rows: tasks } = await sql`
      SELECT arrivy_task_id, customer_name, task_type, extra_fields
      FROM arrivy_tasks
      WHERE task_type IN ('service', 'Service - General')
         OR task_type LIKE '%Service%'
      ORDER BY scheduled_start DESC
      LIMIT 100
    `;

    console.log(`📋 Found ${tasks.length} tasks to check\n`);

    if (!arrivyClient) {
      console.error('❌ Arrivy client not configured');
      process.exit(1);
    }

    let updated = 0;
    let unchanged = 0;
    let errors = 0;

    for (const dbTask of tasks) {
      try {
        // Fetch full task from Arrivy
        const arrivyTask = await arrivyClient.getTask(dbTask.arrivy_task_id);

        if (!arrivyTask) {
          console.log(`⚠️  Task ${dbTask.arrivy_task_id} not found in Arrivy`);
          errors++;
          continue;
        }

        // Extract correct task type
        const newTaskType = extractTaskType(arrivyTask);

        if (newTaskType !== dbTask.task_type) {
          await sql`
            UPDATE arrivy_tasks
            SET task_type = ${newTaskType},
                synced_at = NOW()
            WHERE arrivy_task_id = ${dbTask.arrivy_task_id}
          `;

          console.log(`✅ Updated: ${dbTask.customer_name || 'Unknown'}`);
          console.log(`   ${dbTask.task_type} → ${newTaskType}\n`);
          updated++;
        } else {
          unchanged++;
        }
      } catch (error) {
        console.error(`❌ Error processing task ${dbTask.arrivy_task_id}:`, error);
        errors++;
      }
    }

    console.log('\n📊 Summary:');
    console.log(`   ✅ Updated: ${updated}`);
    console.log(`   ⏭️  Unchanged: ${unchanged}`);
    console.log(`   ❌ Errors: ${errors}`);

    // Show final distribution
    const { rows: distribution } = await sql`
      SELECT task_type, COUNT(*) as count
      FROM arrivy_tasks
      GROUP BY task_type
      ORDER BY count DESC
    `;

    console.log('\n📈 Current task type distribution:');
    for (const row of distribution) {
      console.log(`   ${row.task_type}: ${row.count}`);
    }

  } catch (error) {
    console.error('\n❌ Fatal error:', error);
    process.exit(1);
  }
}

main();
