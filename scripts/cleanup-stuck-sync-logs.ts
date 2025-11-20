/**
 * Cleanup Stuck Sync Logs
 * 
 * Marks old "running" sync logs as "failed" to clean up the database
 */

import { sql } from '@/lib/db/client';

async function main() {
  console.log('🧹 Cleaning up stuck sync logs...\n');

  try {
    // Find stuck logs (running for more than 10 minutes)
    const stuckLogsResult = await sql`
      SELECT 
        id,
        entity_type,
        started_at,
        EXTRACT(EPOCH FROM (NOW() - started_at)) / 60 as minutes_running
      FROM repcard_sync_log
      WHERE status = 'running'
        AND started_at < NOW() - INTERVAL '10 minutes'
      ORDER BY started_at DESC
    `;

    const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);
    const stuckLogs = getRows(stuckLogsResult);

    console.log(`Found ${stuckLogs.length} stuck sync logs\n`);

    if (stuckLogs.length === 0) {
      console.log('✅ No stuck logs to clean up');
      return;
    }

    // Mark them as failed
    const updateResult = await sql`
      UPDATE repcard_sync_log
      SET
        status = 'failed',
        completed_at = NOW(),
        error_message = 'Marked as failed due to timeout (stuck in running status)'
      WHERE status = 'running'
        AND started_at < NOW() - INTERVAL '10 minutes'
    `;

    console.log(`✅ Marked ${stuckLogs.length} stuck sync logs as failed`);
    console.log('\n📊 Summary by entity type:');
    
    const byType: Record<string, number> = {};
    stuckLogs.forEach((log: any) => {
      byType[log.entity_type] = (byType[log.entity_type] || 0) + 1;
    });
    
    Object.entries(byType).forEach(([type, count]) => {
      console.log(`  - ${type}: ${count}`);
    });

  } catch (error) {
    console.error('❌ Error cleaning up stuck logs:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
      console.error('Error stack:', error.stack);
    }
    process.exit(1);
  }
}

main()
  .then(() => {
    console.log('\n✅ Cleanup complete');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });

