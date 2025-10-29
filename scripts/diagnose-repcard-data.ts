import dotenv from 'dotenv';
import path from 'path';

// Load environment variables
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function diagnose() {
  console.log('🔍 Diagnosing RepCard Data Issues...\n');

  // Check database tables
  console.log('1. Checking database tables...');
  const tables = ['repcard_users', 'repcard_offices', 'repcard_customers', 'repcard_appointments', 'repcard_status_logs'];
  
  for (const table of tables) {
    try {
      const result = await sql`SELECT COUNT(*) as count FROM ${sql.unsafe(table)}`;
      const count = Array.isArray(result) ? result[0]?.count : result.rows?.[0]?.count || 0;
      console.log(`   ${table}: ${count} records`);
    } catch (error: any) {
      console.log(`   ${table}: ERROR - ${error.message}`);
    }
  }

  // Check user linking
  console.log('\n2. Checking user linking...');
  try {
    const linkedUsers = await sql`SELECT COUNT(*) as count FROM users WHERE repcard_user_id IS NOT NULL`;
    const linked = Array.isArray(linkedUsers) ? linkedUsers[0]?.count : linkedUsers.rows?.[0]?.count || 0;
    const totalUsers = await sql`SELECT COUNT(*) as count FROM users`;
    const total = Array.isArray(totalUsers) ? totalUsers[0]?.count : totalUsers.rows?.[0]?.count || 0;
    console.log(`   Total users: ${total}`);
    console.log(`   Users linked to RepCard: ${linked}`);
    console.log(`   Users NOT linked: ${total - linked}`);
  } catch (error: any) {
    console.log(`   Error checking user linking: ${error.message}`);
  }

  // Check sync logs
  console.log('\n3. Checking recent sync logs...');
  try {
    const logs = await sql`
      SELECT entity_type, status, records_fetched, records_inserted, records_failed, started_at
      FROM repcard_sync_log
      ORDER BY started_at DESC
      LIMIT 10
    `;
    const logArray = Array.isArray(logs) ? logs : logs.rows || [];
    console.log(`   Found ${logArray.length} recent sync logs:`);
    logArray.forEach((log: any) => {
      console.log(`   - ${log.entity_type}: ${log.status} (fetched: ${log.records_fetched}, inserted: ${log.records_inserted}, failed: ${log.records_failed}) at ${log.started_at}`);
    });
  } catch (error: any) {
    console.log(`   Error checking sync logs: ${error.message}`);
  }

  // Check sample data
  console.log('\n4. Checking sample data...');
  try {
    const sampleCustomers = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const customerCount = Array.isArray(sampleCustomers) ? sampleCustomers[0]?.count : sampleCustomers.rows?.[0]?.count || 0;
    
    if (customerCount > 0) {
      const sample = await sql`SELECT repcard_customer_id, name, created_at, setter_user_id FROM repcard_customers LIMIT 1`;
      const sampleData = Array.isArray(sample) ? sample[0] : sample.rows?.[0];
      console.log(`   Sample customer: ${JSON.stringify(sampleData, null, 2)}`);
    } else {
      console.log('   ❌ No customers found in database');
    }
  } catch (error: any) {
    console.log(`   Error checking sample data: ${error.message}`);
  }

  // Check API readiness
  console.log('\n5. Checking API readiness...');
  try {
    const usersWithRepcardId = await sql`
      SELECT COUNT(*) as count FROM users WHERE repcard_user_id IS NOT NULL
    `;
    const linked = Array.isArray(usersWithRepcardId) ? usersWithRepcardId[0]?.count : usersWithRepcardId.rows?.[0]?.count || 0;
    
    if (linked === 0) {
      console.log('   ⚠️  CRITICAL: No users linked to RepCard!');
      console.log('   → The leaderboard API will return empty results');
      console.log('   → Solution: Run user linking sync');
    } else {
      console.log(`   ✅ ${linked} users linked - API should work`);
    }
  } catch (error: any) {
    console.log(`   Error: ${error.message}`);
  }

  process.exit(0);
}

diagnose().catch(console.error);
