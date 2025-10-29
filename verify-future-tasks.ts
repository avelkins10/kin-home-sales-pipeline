import { config } from 'dotenv';
import { sql } from '@vercel/postgres';

config({ path: '.env.local' });

async function verify() {
  try {
    console.log('\n=== Future Tasks (scheduled beyond today) ===');
    const future = await sql`
      SELECT 
        customer_name,
        TO_CHAR(scheduled_start, 'YYYY-MM-DD HH24:MI') as scheduled_start,
        TO_CHAR(scheduled_end, 'YYYY-MM-DD HH24:MI') as scheduled_end,
        current_status,
        task_type
      FROM arrivy_tasks
      WHERE scheduled_start > CURRENT_DATE
      ORDER BY scheduled_start
    `;
    console.table(future.rows);
    console.log(`\nTotal future tasks: ${future.rows.length}`);

  } catch (error) {
    console.error('Error:', error);
  } finally {
    process.exit(0);
  }
}

verify();
