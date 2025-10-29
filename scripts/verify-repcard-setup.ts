/**
 * Verify RepCard Dashboard & Leaderboard Configuration
 * Checks that all tables, APIs, and components are properly configured
 */

import { sql } from '@/lib/db/client';

async function verifySetup() {
  console.log('🔍 Verifying RepCard Dashboard & Leaderboard Configuration\n');
  console.log('='.repeat(60));

  let allGood = true;

  // 1. Check database tables exist
  console.log('\n1️⃣ Checking Database Tables...');
  const tables = [
    'repcard_customers',
    'repcard_appointments',
    'repcard_status_logs',
    'repcard_users',
    'repcard_offices',
    'repcard_customer_attachments',
    'repcard_appointment_attachments',
    'repcard_leaderboard_snapshots',
    'repcard_sync_log'
  ];

  for (const table of tables) {
    try {
      const result = await sql`
        SELECT EXISTS (
          SELECT FROM information_schema.tables 
          WHERE table_name = ${table}
        )
      `;
      const exists = Array.from(result)[0]?.exists || false;
      if (exists) {
        console.log(`   ✅ ${table}`);
      } else {
        console.log(`   ❌ ${table} - MISSING`);
        allGood = false;
      }
    } catch (error) {
      console.log(`   ❌ ${table} - ERROR: ${error}`);
      allGood = false;
    }
  }

  // 2. Check data counts
  console.log('\n2️⃣ Checking Data Counts...');
  try {
    const customerCount = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const appointmentCount = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    const statusLogCount = await sql`SELECT COUNT(*) as count FROM repcard_status_logs`;
    const userCount = await sql`SELECT COUNT(*) as count FROM repcard_users`;
    const officeCount = await sql`SELECT COUNT(*) as count FROM repcard_offices`;

    const customers = Array.from(customerCount)[0]?.count || 0;
    const appointments = Array.from(appointmentCount)[0]?.count || 0;
    const statusLogs = Array.from(statusLogCount)[0]?.count || 0;
    const users = Array.from(userCount)[0]?.count || 0;
    const offices = Array.from(officeCount)[0]?.count || 0;

    console.log(`   Customers: ${customers}`);
    console.log(`   Appointments: ${appointments}`);
    console.log(`   Status Logs: ${statusLogs}`);
    console.log(`   Users: ${users}`);
    console.log(`   Offices: ${offices}`);

    if (customers === 0 && appointments === 0) {
      console.log('   ⚠️  No RepCard data synced yet - run sync first');
    }
  } catch (error) {
    console.log(`   ❌ Error checking counts: ${error}`);
    allGood = false;
  }

  // 3. Check sync logs
  console.log('\n3️⃣ Checking Recent Sync Logs...');
  try {
    const syncLogs = await sql`
      SELECT 
        entity_type,
        sync_type,
        status,
        records_fetched,
        records_inserted,
        started_at,
        completed_at
      FROM repcard_sync_log
      ORDER BY started_at DESC
      LIMIT 5
    `;
    const logs = Array.from(syncLogs);
    if (logs.length === 0) {
      console.log('   ⚠️  No sync logs found - sync may not have run yet');
    } else {
      logs.forEach((log: any) => {
        const status = log.status === 'completed' ? '✅' : log.status === 'failed' ? '❌' : '🟡';
        console.log(`   ${status} ${log.entity_type} - ${log.sync_type} (${log.records_inserted} inserted)`);
      });
    }
  } catch (error) {
    console.log(`   ⚠️  Could not check sync logs: ${error}`);
  }

  // 4. Check API files exist
  console.log('\n4️⃣ Checking API Routes...');
  const fs = require('fs');
  const path = require('path');
  const apiFiles = [
    'app/api/repcard/leaderboard/route.ts',
    'app/api/repcard/data/route.ts',
    'app/api/repcard/users/[userId]/stats/route.ts',
    'app/api/admin/repcard/comprehensive-sync/route.ts'
  ];

  for (const file of apiFiles) {
    const filePath = path.join(process.cwd(), file);
    if (fs.existsSync(filePath)) {
      console.log(`   ✅ ${file}`);
    } else {
      console.log(`   ❌ ${file} - MISSING`);
      allGood = false;
    }
  }

  // 5. Check component files exist
  console.log('\n5️⃣ Checking Components...');
  const componentFiles = [
    'components/analytics/ConfigurableLeaderboard.tsx',
    'components/dashboard/RepCardMetricsCard.tsx',
    'components/analytics/CanvassingOverviewCard.tsx',
    'components/analytics/AppointmentRatesCard.tsx'
  ];

  for (const file of componentFiles) {
    const filePath = path.join(process.cwd(), file);
    if (fs.existsSync(filePath)) {
      console.log(`   ✅ ${file}`);
    } else {
      console.log(`   ❌ ${file} - MISSING`);
      allGood = false;
    }
  }

  // Summary
  console.log('\n' + '='.repeat(60));
  if (allGood) {
    console.log('✅ All configuration checks passed!');
    console.log('\n📋 Next Steps:');
    console.log('   1. Run initial sync: POST /api/admin/repcard/comprehensive-sync');
    console.log('   2. Check dashboard: http://localhost:3000');
    console.log('   3. Check analytics: http://localhost:3000/analytics');
    console.log('   4. Check leaderboards: http://localhost:3000/analytics (Leaderboards tab)');
  } else {
    console.log('❌ Some configuration issues found - check above');
  }
  console.log('='.repeat(60));
}

verifySetup()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('❌ Verification failed:', error);
    process.exit(1);
  });

