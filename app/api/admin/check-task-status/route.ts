import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';

export const dynamic = 'force-dynamic';

export async function GET(request: Request) {
  try {
    const { searchParams } = new URL(request.url);
    const customer = searchParams.get('customer') || 'tania';

    // Check task status in database
    const { rows: tasks } = await sql`
      SELECT
        arrivy_task_id,
        customer_name,
        task_type,
        current_status,
        scheduled_start,
        scheduled_end,
        start_datetime_window_start,
        start_datetime_window_end,
        synced_at
      FROM arrivy_tasks
      WHERE customer_name ILIKE ${'%' + customer + '%'}
      ORDER BY scheduled_start DESC
      LIMIT 5
    `;

    // Check recent webhook events
    const { rows: events } = await sql`
      SELECT
        event_id,
        event_type,
        event_sub_type,
        object_id,
        event_time,
        processed_at
      FROM arrivy_webhook_events
      WHERE object_id = ANY(${tasks.map(t => t.arrivy_task_id)})
      ORDER BY event_time DESC
      LIMIT 20
    `;

    // Check task status history
    const { rows: statusHistory } = await sql`
      SELECT
        arrivy_task_id,
        status_type,
        reporter_name,
        reported_at,
        notes,
        source
      FROM arrivy_task_statuses
      WHERE arrivy_task_id = ANY(${tasks.map(t => t.arrivy_task_id)})
      ORDER BY reported_at DESC
      LIMIT 20
    `;

    return NextResponse.json({
      tasks,
      recentEvents: events,
      statusHistory,
      summary: {
        totalTasks: tasks.length,
        totalEvents: events.length,
        totalStatusUpdates: statusHistory.length,
      }
    });
  } catch (error) {
    console.error('Error checking task status:', error);
    return NextResponse.json(
      { error: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
