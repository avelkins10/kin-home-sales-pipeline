import { NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { sql } from '@vercel/postgres';
import { arrivyClient } from '@/lib/integrations/arrivy/client';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

export async function GET() {
  try {
    // Check authentication
    const session = await getServerSession(authOptions);
    if (!session?.user) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    // Check for admin role
    if (session.user.role !== 'super_admin') {
      return NextResponse.json({ error: 'Forbidden' }, { status: 403 });
    }

    // Query database stats
    const [taskStats, eventStats, recentTasks, recentEvents] = await Promise.all([
      // Task statistics
      sql`
        SELECT
          COUNT(*) as total_tasks,
          COUNT(CASE WHEN scheduled_start >= CURRENT_DATE - INTERVAL '7 days' THEN 1 END) as tasks_last_7_days,
          COUNT(CASE WHEN scheduled_start >= CURRENT_DATE - INTERVAL '1 day' THEN 1 END) as tasks_last_24h,
          COUNT(CASE WHEN scheduled_start >= CURRENT_DATE THEN 1 END) as tasks_today,
          MIN(scheduled_start) as earliest_task,
          MAX(scheduled_start) as latest_task,
          MAX(created_at) as last_task_sync
        FROM arrivy_tasks
      `,

      // Event statistics
      sql`
        SELECT
          COUNT(*) as total_events,
          COUNT(CASE WHEN event_time >= NOW() - INTERVAL '7 days' THEN 1 END) as events_last_7_days,
          COUNT(CASE WHEN event_time >= NOW() - INTERVAL '1 day' THEN 1 END) as events_last_24h,
          COUNT(CASE WHEN event_time >= CURRENT_DATE THEN 1 END) as events_today,
          MIN(event_time) as earliest_event,
          MAX(event_time) as latest_event,
          COUNT(DISTINCT event_type) as event_types_count,
          MAX(created_at) as last_event_received
        FROM arrivy_events
      `,

      // Recent tasks
      sql`
        SELECT
          arrivy_task_id,
          title,
          template_name,
          status,
          scheduled_start,
          created_at
        FROM arrivy_tasks
        ORDER BY scheduled_start DESC NULLS LAST
        LIMIT 10
      `,

      // Recent events
      sql`
        SELECT
          e.arrivy_task_id,
          e.event_type,
          e.event_time,
          e.message,
          e.created_at,
          t.title as task_title
        FROM arrivy_events e
        LEFT JOIN arrivy_tasks t ON e.arrivy_task_id = t.arrivy_task_id
        ORDER BY e.event_time DESC
        LIMIT 20
      `
    ]);

    // Get event type breakdown
    const eventTypeBreakdown = await sql`
      SELECT
        event_type,
        COUNT(*) as count,
        MAX(event_time) as latest_event
      FROM arrivy_events
      GROUP BY event_type
      ORDER BY count DESC
    `;

    // Try to fetch tasks from Arrivy API
    let apiTasksCount = 0;
    let apiError = null;
    try {
      const today = new Date();
      today.setHours(0, 0, 0, 0);
      const apiTasks = await arrivyClient.getTasks({
        start: today.toISOString(),
        template_type: 'TASK',
      });
      apiTasksCount = apiTasks.length;
    } catch (error: any) {
      apiError = error.message;
    }

    return NextResponse.json({
      database: {
        tasks: taskStats.rows[0],
        events: eventStats.rows[0],
        eventTypeBreakdown: eventTypeBreakdown.rows,
      },
      recent: {
        tasks: recentTasks.rows,
        events: recentEvents.rows,
      },
      arrivy_api: {
        tasks_from_api_today: apiTasksCount,
        error: apiError,
      },
      timestamp: new Date().toISOString(),
    });

  } catch (error) {
    console.error('Error fetching Arrivy status:', error);
    return NextResponse.json(
      { error: 'Failed to fetch Arrivy status', details: String(error) },
      { status: 500 }
    );
  }
}
