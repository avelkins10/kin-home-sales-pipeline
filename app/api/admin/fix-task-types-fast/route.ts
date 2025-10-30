// app/api/admin/fix-task-types-fast/route.ts
import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';

export const runtime = 'nodejs';
export const maxDuration = 60;

/**
 * Fast admin endpoint to fix task types using data already in the database
 * Does NOT call Arrivy API - uses extra_fields and template_id stored in DB
 * GET /api/admin/fix-task-types-fast
 */
export async function GET() {
  const startTime = Date.now();

  try {
    console.log('[Fix Task Types Fast] Starting database-only fix...');

    // Update tasks based on extra_fields->task_type or template patterns
    const result = await sql`
      WITH updated_tasks AS (
        UPDATE arrivy_tasks
        SET task_type = CASE
          -- Check extra_fields.task_type first
          WHEN extra_fields->>'task_type' ILIKE '%survey%' THEN 'Surveys - Site Survey'
          WHEN extra_fields->>'task_type' ILIKE '%install%' THEN 'Installations - General'
          WHEN extra_fields->>'task_type' ILIKE '%inspection%' THEN 'Inspections - General'

          -- Check extra_fields for form indicators
          WHEN extra_fields::text ILIKE '%site survey%' THEN 'Surveys - Site Survey'
          WHEN extra_fields::text ILIKE '%install%' THEN 'Installations - General'
          WHEN extra_fields::text ILIKE '%inspection%' THEN 'Inspections - General'

          -- Keep existing non-service types
          WHEN task_type NOT LIKE '%Service%' AND task_type != 'service' THEN task_type

          -- Default to Service - General
          ELSE 'Service - General'
        END,
        synced_at = NOW()
        WHERE task_type IN ('service', 'Service - General')
           OR task_type LIKE '%Service%'
        RETURNING arrivy_task_id, customer_name, task_type
      )
      SELECT * FROM updated_tasks
    `;

    console.log(`[Fix Task Types Fast] Updated ${result.rowCount} tasks`);

    // Get current distribution
    const { rows: distribution } = await sql`
      SELECT task_type, COUNT(*) as count
      FROM arrivy_tasks
      GROUP BY task_type
      ORDER BY count DESC
    `;

    const response = {
      success: true,
      stats: {
        updated: result.rowCount,
      },
      distribution: distribution.map(r => ({ type: r.task_type, count: parseInt(r.count) })),
      sampleUpdates: result.rows.slice(0, 10).map(r => ({
        task_id: r.arrivy_task_id,
        customer: r.customer_name,
        new_type: r.task_type,
      })),
      durationMs: Date.now() - startTime,
    };

    console.log('[Fix Task Types Fast] Completed:', response);

    return NextResponse.json(response);
  } catch (error) {
    console.error('[Fix Task Types Fast] Failed:', error);
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
