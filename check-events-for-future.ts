import { config } from 'dotenv';
import { sql } from '@vercel/postgres';

config({ path: '.env.local' });

async function checkEvents() {
  try {
    console.log('\n=== Recent Events (Last 20) ===');
    const recentEvents = await sql`
      SELECT 
        event_type,
        event_sub_type,
        arrivy_task_id,
        reporter_name,
        TO_CHAR(event_time, 'YYYY-MM-DD HH24:MI:SS') as event_time,
        title,
        LEFT(message, 50) as message_preview
      FROM arrivy_events
      ORDER BY event_time DESC
      LIMIT 20
    `;
    console.table(recentEvents.rows);

    console.log('\n=== Events with Future Task References ===');
    const futureEvents = await sql`
      SELECT 
        e.event_type,
        e.event_sub_type,
        e.arrivy_task_id,
        t.customer_name,
        TO_CHAR(t.scheduled_start, 'YYYY-MM-DD HH24:MI') as task_scheduled,
        TO_CHAR(e.event_time, 'YYYY-MM-DD HH24:MI:SS') as event_time,
        CASE 
          WHEN t.scheduled_start::date > CURRENT_DATE THEN 'FUTURE'
          WHEN t.scheduled_start::date = CURRENT_DATE THEN 'TODAY'
          ELSE 'PAST'
        END as timeframe
      FROM arrivy_events e
      LEFT JOIN arrivy_tasks t ON e.arrivy_task_id = t.arrivy_task_id
      WHERE t.scheduled_start IS NOT NULL
      ORDER BY t.scheduled_start DESC
      LIMIT 30
    `;
    console.table(futureEvents.rows);

    console.log('\n=== Event Types Count ===');
    const eventTypes = await sql`
      SELECT 
        event_type,
        COUNT(*) as count
      FROM arrivy_events
      GROUP BY event_type
      ORDER BY count DESC
    `;
    console.table(eventTypes.rows);

    console.log('\n=== Check for TASK_RESCHEDULED Events (might indicate future tasks) ===');
    const rescheduled = await sql`
      SELECT 
        e.arrivy_task_id,
        t.customer_name,
        TO_CHAR(t.scheduled_start, 'YYYY-MM-DD HH24:MI') as new_schedule,
        TO_CHAR(e.event_time, 'YYYY-MM-DD HH24:MI:SS') as rescheduled_at,
        e.message,
        CASE 
          WHEN t.scheduled_start > CURRENT_DATE THEN 'FUTURE'
          ELSE 'PAST/TODAY'
        END as timeframe
      FROM arrivy_events e
      LEFT JOIN arrivy_tasks t ON e.arrivy_task_id = t.arrivy_task_id
      WHERE e.event_type = 'TASK_RESCHEDULED'
      ORDER BY t.scheduled_start DESC
      LIMIT 15
    `;
    console.table(rescheduled.rows);

  } catch (error) {
    console.error('Error:', error);
  } finally {
    process.exit(0);
  }
}

checkEvents();
