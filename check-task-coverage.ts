import { config } from 'dotenv';
import { sql } from '@vercel/postgres';

config({ path: '.env.local' });

async function checkTaskCoverage() {
  try {
    console.log('\n=== Total Tasks in Database ===');
    const total = await sql`
      SELECT COUNT(*) as total_tasks
      FROM arrivy_tasks
    `;
    console.log(`Total tasks: ${total.rows[0].total_tasks}`);

    console.log('\n=== Task Date Range ===');
    const dateRange = await sql`
      SELECT 
        TO_CHAR(MIN(scheduled_start), 'YYYY-MM-DD') as earliest_task,
        TO_CHAR(MAX(scheduled_start), 'YYYY-MM-DD') as latest_task,
        COUNT(*) FILTER (WHERE scheduled_start < CURRENT_DATE) as past_tasks,
        COUNT(*) FILTER (WHERE scheduled_start::date = CURRENT_DATE) as today_tasks,
        COUNT(*) FILTER (WHERE scheduled_start > CURRENT_DATE) as future_tasks,
        COUNT(*) FILTER (WHERE scheduled_start IS NULL) as unscheduled_tasks
      FROM arrivy_tasks
    `;
    console.table(dateRange.rows);

    console.log('\n=== Tasks by Status ===');
    const byStatus = await sql`
      SELECT 
        current_status,
        COUNT(*) as count
      FROM arrivy_tasks
      GROUP BY current_status
      ORDER BY count DESC
    `;
    console.table(byStatus.rows);

    console.log('\n=== Recent Tasks (created/updated in last 24 hours) ===');
    const recent = await sql`
      SELECT 
        customer_name,
        task_type,
        current_status,
        TO_CHAR(scheduled_start, 'YYYY-MM-DD HH24:MI') as scheduled,
        TO_CHAR(synced_at, 'YYYY-MM-DD HH24:MI:SS') as synced_at
      FROM arrivy_tasks
      WHERE synced_at >= NOW() - INTERVAL '24 hours'
      ORDER BY synced_at DESC
      LIMIT 15
    `;
    console.table(recent.rows);

    console.log('\n=== Upcoming Tasks (next 7 days) ===');
    const upcoming = await sql`
      SELECT 
        TO_CHAR(scheduled_start::date, 'YYYY-MM-DD') as date,
        COUNT(*) as task_count
      FROM arrivy_tasks
      WHERE scheduled_start >= CURRENT_DATE 
        AND scheduled_start < CURRENT_DATE + INTERVAL '7 days'
      GROUP BY scheduled_start::date
      ORDER BY date
    `;
    console.table(upcoming.rows);

    console.log('\n=== Tasks with QuickBase Association ===');
    const withQB = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE quickbase_project_id IS NOT NULL AND quickbase_project_id != '' AND NOT quickbase_project_id LIKE 'ARRIVY-%') as with_quickbase,
        COUNT(*) FILTER (WHERE quickbase_project_id IS NULL OR quickbase_project_id = '' OR quickbase_project_id LIKE 'ARRIVY-%') as arrivy_only
      FROM arrivy_tasks
    `;
    console.table(withQB.rows);

  } catch (error) {
    console.error('Error:', error);
  } finally {
    process.exit(0);
  }
}

checkTaskCoverage();
