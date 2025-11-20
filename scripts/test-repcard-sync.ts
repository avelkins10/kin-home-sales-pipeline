/**
 * Test RepCard Sync - Comprehensive Diagnostic
 * 
 * This script tests the RepCard sync functionality and verifies:
 * 1. Database connection
 * 2. Sync log creation
 * 3. Record insertion
 * 4. Sync log completion
 */

import { sql } from '@/lib/db/client';
import { syncUsers } from '@/lib/repcard/comprehensive-sync';
import { syncCustomers, syncAppointments } from '@/lib/repcard/sync-service';

async function main() {
  console.log('🔍 Starting RepCard Sync Diagnostic Test...\n');

  try {
    // Helper to extract rows from @vercel/postgres result
    const getRows = (result: any) => {
      if (result.rows) return result.rows;
      if (Array.isArray(result)) return result;
      return [];
    };

    // Test 1: Database connection
    console.log('Test 1: Checking database connection...');
    const dbTest = await sql`SELECT NOW() as current_time, version() as pg_version`;
    const dbRows = getRows(dbTest);
    console.log('✅ Database connected:', dbRows[0]);
    console.log('');

    // Test 2: Check existing sync logs
    console.log('Test 2: Checking existing sync logs...');
    const syncLogsResult = await sql`
      SELECT 
        entity_type,
        status,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        started_at,
        completed_at
      FROM repcard_sync_log
      ORDER BY started_at DESC
      LIMIT 10
    `;
    const syncLogs = getRows(syncLogsResult);
    console.log(`Found ${syncLogs.length} recent sync logs:`);
    syncLogs.forEach((log: any) => {
      console.log(`  - ${log.entity_type}: ${log.status} | Fetched: ${log.records_fetched}, Inserted: ${log.records_inserted}, Updated: ${log.records_updated}, Failed: ${log.records_failed}`);
    });
    console.log('');

    // Test 3: Check record counts
    console.log('Test 3: Checking record counts in database...');
    const userCountResult = await sql`SELECT COUNT(*) as count FROM repcard_users`;
    const customerCountResult = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const appointmentCountResult = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    const officeCountResult = await sql`SELECT COUNT(*) as count FROM repcard_offices`;
    
    const userCount = getRows(userCountResult)[0]?.count || 0;
    const customerCount = getRows(customerCountResult)[0]?.count || 0;
    const appointmentCount = getRows(appointmentCountResult)[0]?.count || 0;
    const officeCount = getRows(officeCountResult)[0]?.count || 0;
    
    console.log('Current record counts:');
    console.log(`  - Users: ${userCount}`);
    console.log(`  - Customers: ${customerCount}`);
    console.log(`  - Appointments: ${appointmentCount}`);
    console.log(`  - Offices: ${officeCount}`);
    console.log('');

    // Test 4: Test sync log creation
    console.log('Test 4: Testing sync log creation...');
    const testSyncLogIdResult = await sql`
      INSERT INTO repcard_sync_log (entity_type, sync_type, status, started_at)
      VALUES ('test', 'full', 'running', NOW())
      RETURNING id
    `;
    const testId = getRows(testSyncLogIdResult)[0]?.id;
    console.log(`✅ Created test sync log with ID: ${testId}`);

    // Test 5: Test sync log completion
    console.log('Test 5: Testing sync log completion...');
    await sql`
      UPDATE repcard_sync_log
      SET
        status = 'completed',
        completed_at = NOW(),
        records_fetched = 10,
        records_inserted = 5,
        records_updated = 5,
        records_failed = 0
      WHERE id = ${testId}
    `;
    console.log('✅ Updated sync log successfully');

    // Verify the update
    const verifyLogResult = await sql`
      SELECT * FROM repcard_sync_log WHERE id = ${testId}
    `;
    const log = getRows(verifyLogResult)[0];
    console.log(`✅ Verified sync log:`, {
      id: log?.id,
      status: log?.status,
      records_fetched: log?.records_fetched,
      records_inserted: log?.records_inserted,
      completed_at: log?.completed_at
    });
    console.log('');

    // Test 6: Check if sync log query works (same as admin endpoint)
    console.log('Test 6: Testing sync log query (same as admin endpoint)...');
    const latestSyncsRawResult = await sql`
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
        last_record_date,
        error_message
      FROM repcard_sync_log
      WHERE entity_type != 'test'
      ORDER BY entity_type, started_at DESC
    `;
    const latestSyncs = getRows(latestSyncsRawResult);
    console.log(`Found ${latestSyncs.length} latest syncs by entity type:`);
    latestSyncs.forEach((sync: any) => {
      console.log(`  - ${sync.entity_type}: ${sync.status} | Started: ${sync.started_at}, Completed: ${sync.completed_at || 'N/A'}`);
    });
    console.log('');

    // Test 7: Check for any sync logs that are stuck in 'running' status
    console.log('Test 7: Checking for stuck sync logs...');
    const stuckLogsResult = await sql`
      SELECT 
        id,
        entity_type,
        started_at,
        EXTRACT(EPOCH FROM (NOW() - started_at)) / 60 as minutes_running
      FROM repcard_sync_log
      WHERE status = 'running'
        AND started_at < NOW() - INTERVAL '10 minutes'
    `;
    const stuck = getRows(stuckLogsResult);
    if (stuck.length > 0) {
      console.log(`⚠️  Found ${stuck.length} stuck sync logs:`);
      stuck.forEach((log: any) => {
        console.log(`  - ${log.entity_type}: Running for ${Math.round(log.minutes_running)} minutes`);
      });
    } else {
      console.log('✅ No stuck sync logs found');
    }
    console.log('');

    // Test 8: Check sync log table structure
    console.log('Test 8: Verifying sync log table structure...');
    const tableInfoResult = await sql`
      SELECT 
        column_name,
        data_type,
        is_nullable
      FROM information_schema.columns
      WHERE table_name = 'repcard_sync_log'
      ORDER BY ordinal_position
    `;
    const tableInfo = getRows(tableInfoResult);
    console.log('Sync log table columns:');
    tableInfo.forEach((col: any) => {
      console.log(`  - ${col.column_name}: ${col.data_type} (nullable: ${col.is_nullable})`);
    });
    console.log('');

    console.log('✅ All diagnostic tests completed successfully!');
    console.log('\n📊 Summary:');
    console.log(`  - Database: Connected`);
    console.log(`  - Sync logs: ${syncLogs.length} found`);
    console.log(`  - Users: ${userCount}`);
    console.log(`  - Customers: ${customerCount}`);
    console.log(`  - Appointments: ${appointmentCount}`);
    console.log(`  - Offices: ${officeCount}`);

  } catch (error) {
    console.error('❌ Diagnostic test failed:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
      console.error('Error stack:', error.stack);
    }
    process.exit(1);
  }
}

main()
  .then(() => {
    console.log('\n✅ Diagnostic complete');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });

