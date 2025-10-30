// app/api/admin/fix-task-types/route.ts
import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { extractTaskType } from '@/lib/integrations/arrivy/utils';
import type { ArrivyTask } from '@/lib/integrations/arrivy/types';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';
export const maxDuration = 300;

/**
 * Admin endpoint to fix task types for existing tasks
 * Fetches tasks from Arrivy API and updates their task_type using the comprehensive logic
 * GET /api/admin/fix-task-types
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

    // Get all tasks from database that have 'service' or generic types
    const { rows: tasksToFix } = await sql`
      SELECT arrivy_task_id, task_type
      FROM arrivy_tasks
      WHERE task_type IN ('service', 'Service - General')
         OR task_type LIKE '%Service%'
      ORDER BY scheduled_start DESC
      LIMIT 100
    `;

    console.log(`[Fix Task Types] Found ${tasksToFix.length} tasks to check`);

    let updated = 0;
    let unchanged = 0;
    let errors = 0;
    const results: any[] = [];

    for (const dbTask of tasksToFix) {
      try {
        // Fetch full task details from Arrivy API
        const arrivyTask: ArrivyTask = await arrivyClient.getTask(dbTask.arrivy_task_id);

        if (!arrivyTask) {
          errors++;
          continue;
        }

        // Extract task type using comprehensive logic
        const newTaskType = extractTaskType(arrivyTask);

        // Only update if different
        if (newTaskType !== dbTask.task_type) {
          await sql`
            UPDATE arrivy_tasks
            SET task_type = ${newTaskType},
                synced_at = NOW()
            WHERE arrivy_task_id = ${dbTask.arrivy_task_id}
          `;

          updated++;
          results.push({
            task_id: dbTask.arrivy_task_id,
            customer: arrivyTask.customer_name,
            old_type: dbTask.task_type,
            new_type: newTaskType,
          });
        } else {
          unchanged++;
        }
      } catch (error) {
        console.error(`[Fix Task Types] Error processing task ${dbTask.arrivy_task_id}:`, error);
        errors++;
      }
    }

    const result = {
      success: true,
      stats: {
        tasksChecked: tasksToFix.length,
        updated,
        unchanged,
        errors,
      },
      sampleUpdates: results.slice(0, 10),
      durationMs: Date.now() - startTime,
    };

    console.log('[Fix Task Types] Completed:', result);

    return NextResponse.json(result);
  } catch (error) {
    console.error('[Fix Task Types] Failed:', error);
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
