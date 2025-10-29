import { sql } from '@vercel/postgres';

async function checkWebhooks() {
  try {
    console.log('\n=== Recent Arrivy Events (last 10) ===');
    const events = await sql`
      SELECT 
        event_type,
        event_sub_type,
        arrivy_task_id,
        reporter_name,
        TO_CHAR(created_at, 'YYYY-MM-DD HH24:MI:SS') as created_at
      FROM arrivy_events 
      ORDER BY created_at DESC 
      LIMIT 10
    `;
    console.table(events.rows);

    console.log('\n=== Recent Task Updates (last 10) ===');
    const tasks = await sql`
      SELECT 
        arrivy_task_id,
        customer_name,
        task_type,
        current_status,
        TO_CHAR(updated_at, 'YYYY-MM-DD HH24:MI:SS') as updated_at
      FROM arrivy_tasks 
      ORDER BY updated_at DESC 
      LIMIT 10
    `;
    console.table(tasks.rows);

    console.log('\n=== Event Count by Type (today) ===');
    const counts = await sql`
      SELECT 
        event_type,
        COUNT(*) as count
      FROM arrivy_events 
      WHERE created_at >= CURRENT_DATE
      GROUP BY event_type
      ORDER BY count DESC
    `;
    console.table(counts.rows);

  } catch (error) {
    console.error('Error:', error);
  } finally {
    process.exit(0);
  }
}

checkWebhooks();
