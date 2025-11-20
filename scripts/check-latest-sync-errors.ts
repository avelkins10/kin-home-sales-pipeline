/**
 * Check Latest Sync Errors
 * 
 * Shows the most recent sync errors to diagnose issues
 */

import { sql } from '@/lib/db/client';

async function main() {
  console.log('🔍 Checking latest sync errors...\n');

  try {
    const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

    // Get latest failed syncs
    const failedSyncsResult = await sql`
      SELECT 
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
      WHERE status = 'failed'
        AND error_message IS NOT NULL
      ORDER BY started_at DESC
      LIMIT 10
    `;

    const failedSyncs = getRows(failedSyncsResult);
    
    if (failedSyncs.length === 0) {
      console.log('✅ No failed syncs with error messages found');
    } else {
      console.log(`Found ${failedSyncs.length} recent failed syncs:\n`);
      failedSyncs.forEach((sync: any, index: number) => {
        console.log(`${index + 1}. ${sync.entity_type} (${sync.sync_type})`);
        console.log(`   Started: ${sync.started_at}`);
        console.log(`   Completed: ${sync.completed_at || 'N/A'}`);
        console.log(`   Records: Fetched=${sync.records_fetched}, Inserted=${sync.records_inserted}, Updated=${sync.records_updated}, Failed=${sync.records_failed}`);
        console.log(`   Error: ${sync.error_message}`);
        console.log('');
      });
    }

    // Get latest users sync specifically
    console.log('\n📊 Latest Users Sync:');
    const usersSyncResult = await sql`
      SELECT 
        id,
        status,
        started_at,
        completed_at,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        error_message
      FROM repcard_sync_log
      WHERE entity_type = 'users'
      ORDER BY started_at DESC
      LIMIT 1
    `;

    const usersSync = getRows(usersSyncResult)[0];
    if (usersSync) {
      console.log(`  Status: ${usersSync.status}`);
      console.log(`  Started: ${usersSync.started_at}`);
      console.log(`  Completed: ${usersSync.completed_at || 'N/A'}`);
      console.log(`  Records: Fetched=${usersSync.records_fetched}, Inserted=${usersSync.records_inserted}, Updated=${usersSync.records_updated}, Failed=${usersSync.records_failed}`);
      if (usersSync.error_message) {
        console.log(`  Error: ${usersSync.error_message}`);
      }
    } else {
      console.log('  No users sync found');
    }

    // Get latest offices sync
    console.log('\n📊 Latest Offices Sync:');
    const officesSyncResult = await sql`
      SELECT 
        id,
        status,
        started_at,
        completed_at,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        error_message
      FROM repcard_sync_log
      WHERE entity_type = 'offices'
      ORDER BY started_at DESC
      LIMIT 1
    `;

    const officesSync = getRows(officesSyncResult)[0];
    if (officesSync) {
      console.log(`  Status: ${officesSync.status}`);
      console.log(`  Started: ${officesSync.started_at}`);
      console.log(`  Completed: ${officesSync.completed_at || 'N/A'}`);
      console.log(`  Records: Fetched=${officesSync.records_fetched}, Inserted=${officesSync.records_inserted}, Updated=${officesSync.records_updated}, Failed=${officesSync.records_failed}`);
      if (officesSync.error_message) {
        console.log(`  Error: ${officesSync.error_message}`);
      }
    } else {
      console.log('  No offices sync found');
    }

  } catch (error) {
    console.error('❌ Error checking sync errors:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
      console.error('Error stack:', error.stack);
    }
    process.exit(1);
  }
}

main()
  .then(() => {
    console.log('\n✅ Check complete');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });

