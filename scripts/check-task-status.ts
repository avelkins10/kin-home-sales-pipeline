import { sql } from '@vercel/postgres';

async function checkTaskStatus() {
  try {
    console.log('Checking task status for Tania Morris...\n');

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
      WHERE customer_name ILIKE '%tania%' OR customer_name ILIKE '%morris%'
      ORDER BY scheduled_start DESC
      LIMIT 5
    `;

    console.log('Tasks found:', tasks.length);
    console.log(JSON.stringify(tasks, null, 2));

    if (tasks.length > 0) {
      const taskIds = tasks.map(t => t.arrivy_task_id);

      // Check recent webhook events
      console.log('\n\nChecking recent webhook events...\n');
      const { rows: events } = await sql`
        SELECT
          event_id,
          event_type,
          event_sub_type,
          object_id,
          event_time,
          processed_at
        FROM arrivy_webhook_events
        WHERE object_id = ANY(${taskIds})
        ORDER BY event_time DESC
        LIMIT 20
      `;

      console.log('Webhook events found:', events.length);
      console.log(JSON.stringify(events, null, 2));

      // Check task status history
      console.log('\n\nChecking task status history...\n');
      const { rows: statusHistory } = await sql`
        SELECT
          arrivy_task_id,
          status_type,
          reporter_name,
          reported_at,
          notes,
          source
        FROM arrivy_task_statuses
        WHERE arrivy_task_id = ANY(${taskIds})
        ORDER BY reported_at DESC
        LIMIT 20
      `;

      console.log('Status updates found:', statusHistory.length);
      console.log(JSON.stringify(statusHistory, null, 2));
    }

    console.log('\n✅ Check complete');
    process.exit(0);
  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

checkTaskStatus();
