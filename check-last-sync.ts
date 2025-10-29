import { config } from 'dotenv';
import { sql } from '@vercel/postgres';

config({ path: '.env.local' });

async function checkLastSync() {
  try {
    console.log('\n=== Last Sync Times ===');
    const syncTimes = await sql`
      SELECT 
        TO_CHAR(synced_at, 'YYYY-MM-DD HH24:MI:SS') as sync_time,
        COUNT(*) as tasks_synced
      FROM arrivy_tasks
      WHERE synced_at >= CURRENT_DATE - INTERVAL '7 days'
      GROUP BY synced_at
      ORDER BY synced_at DESC
      LIMIT 10
    `;
    console.table(syncTimes.rows);

    console.log('\n=== Sync Coverage Analysis ===');
    const coverage = await sql`
      SELECT 
        'Most recent sync' as metric,
        TO_CHAR(MAX(synced_at), 'YYYY-MM-DD HH24:MI:SS') as value
      FROM arrivy_tasks
      UNION ALL
      SELECT 
        'Tasks synced today' as metric,
        COUNT(*)::text as value
      FROM arrivy_tasks
      WHERE synced_at::date = CURRENT_DATE
      UNION ALL
      SELECT 
        'Tasks with future schedules' as metric,
        COUNT(*)::text as value
      FROM arrivy_tasks
      WHERE scheduled_start > CURRENT_DATE
    `;
    console.table(coverage.rows);

    console.log('\n=== Future Scheduled Tasks Details ===');
    const futureDetails = await sql`
      SELECT 
        arrivy_task_id,
        customer_name,
        TO_CHAR(scheduled_start, 'YYYY-MM-DD HH24:MI') as scheduled_start,
        current_status,
        TO_CHAR(synced_at, 'YYYY-MM-DD HH24:MI:SS') as synced_at
      FROM arrivy_tasks
      WHERE scheduled_start > CURRENT_DATE
      ORDER BY scheduled_start
    `;
    
    if (futureDetails.rows.length > 0) {
      console.table(futureDetails.rows);
    } else {
      console.log('❌ No tasks scheduled beyond today');
    }

  } catch (error) {
    console.error('Error:', error);
  } finally {
    process.exit(0);
  }
}

checkLastSync();
