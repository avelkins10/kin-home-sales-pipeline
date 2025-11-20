/**
 * Monitor RepCard Sync Status
 * 
 * Checks the latest sync status and shows progress
 */

import { sql } from '@/lib/db/client';

async function main() {
  console.log('📊 Monitoring RepCard Sync Status...\n');

  try {
    const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

    // Get latest syncs by entity type
    const latestSyncsResult = await sql`
      SELECT DISTINCT ON (entity_type)
        id,
        entity_type,
        sync_type,
        status,
        started_at,
        completed_at,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        error_message
      FROM repcard_sync_log
      ORDER BY entity_type, started_at DESC
    `;
    const latestSyncs = getRows(latestSyncsResult);

    console.log('Latest Sync Status by Entity Type:\n');
    latestSyncs.forEach((sync: any) => {
      const statusIcon = sync.status === 'completed' ? '✅' : sync.status === 'failed' ? '❌' : '🔄';
      const duration = sync.completed_at 
        ? `${Math.round((new Date(sync.completed_at).getTime() - new Date(sync.started_at).getTime()) / 1000)}s`
        : sync.status === 'running' 
          ? `${Math.round((Date.now() - new Date(sync.started_at).getTime()) / 1000)}s (running)`
          : 'N/A';
      
      console.log(`${statusIcon} ${sync.entity_type.toUpperCase()} (${sync.sync_type})`);
      console.log(`   Status: ${sync.status}`);
      console.log(`   Started: ${new Date(sync.started_at).toLocaleString()}`);
      if (sync.completed_at) {
        console.log(`   Completed: ${new Date(sync.completed_at).toLocaleString()}`);
      }
      console.log(`   Duration: ${duration}`);
      console.log(`   Records: Fetched=${sync.records_fetched}, Inserted=${sync.records_inserted}, Updated=${sync.records_updated}, Failed=${sync.records_failed}`);
      if (sync.error_message) {
        console.log(`   Error: ${sync.error_message.substring(0, 200)}${sync.error_message.length > 200 ? '...' : ''}`);
      }
      console.log('');
    });

    // Get current record counts
    console.log('Current Database Record Counts:\n');
    const userCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_users`)[0]?.count || 0;
    const customerCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_customers`)[0]?.count || 0;
    const appointmentCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_appointments`)[0]?.count || 0;
    const officeCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_offices`)[0]?.count || 0;

    console.log(`  Users: ${userCount}`);
    console.log(`  Customers: ${customerCount}`);
    console.log(`  Appointments: ${appointmentCount}`);
    console.log(`  Offices: ${officeCount}`);
    console.log('');

    // Check for running syncs
    const runningSyncs = latestSyncs.filter((s: any) => s.status === 'running');
    if (runningSyncs.length > 0) {
      console.log(`🔄 ${runningSyncs.length} sync(s) currently running:\n`);
      runningSyncs.forEach((sync: any) => {
        const elapsed = Math.round((Date.now() - new Date(sync.started_at).getTime()) / 1000);
        console.log(`  - ${sync.entity_type}: Running for ${elapsed}s`);
      });
    }

  } catch (error) {
    console.error('❌ Error monitoring sync:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
    }
    process.exit(1);
  }
}

main()
  .then(() => {
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });

