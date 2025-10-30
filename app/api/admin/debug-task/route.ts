// app/api/admin/debug-task/route.ts
import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';
import { arrivyClient } from '@/lib/integrations/arrivy/client';

export const runtime = 'nodejs';
export const maxDuration = 30;

/**
 * Debug endpoint to see what data we have for a specific task
 * GET /api/admin/debug-task?customer=Justin
 */
export async function GET(request: Request) {
  try {
    const { searchParams } = new URL(request.url);
    const customer = searchParams.get('customer') || 'Justin';

    // Get task from database
    const { rows } = await sql`
      SELECT *
      FROM arrivy_tasks
      WHERE customer_name ILIKE ${'%' + customer + '%'}
      ORDER BY scheduled_start DESC
      LIMIT 1
    `;

    if (rows.length === 0) {
      return NextResponse.json({ error: 'Task not found' }, { status: 404 });
    }

    const dbTask = rows[0];

    // Try to fetch from Arrivy API
    let arrivyTask = null;
    if (arrivyClient) {
      try {
        arrivyTask = await arrivyClient.getTask(dbTask.arrivy_task_id);
      } catch (error) {
        console.error('Failed to fetch from Arrivy:', error);
      }
    }

    return NextResponse.json({
      database: {
        arrivy_task_id: dbTask.arrivy_task_id,
        customer_name: dbTask.customer_name,
        task_type: dbTask.task_type,
        template_id: dbTask.template_id,
        extra_fields: dbTask.extra_fields,
        scheduled_start: dbTask.scheduled_start,
      },
      arrivy: arrivyTask ? {
        id: arrivyTask.id,
        title: arrivyTask.title,
        template: arrivyTask.template,
        template_id: arrivyTask.template_id,
        extra_fields: arrivyTask.extra_fields,
        group: arrivyTask.group,
        customer_name: arrivyTask.customer_name,
      } : null,
    });
  } catch (error) {
    return NextResponse.json(
      { error: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
