/**
 * RepCard Integration Diagnostic Script
 * 
 * This script tests the RepCard integration to identify issues with:
 * - API key configuration
 * - User mapping (repcard_user_id)
 * - API connectivity
 * - Data fetching
 * - Response parsing
 */

import { sql } from '../lib/db/client';
import { RepCardClient } from '../lib/repcard/client';

async function diagnoseRepCard() {
  console.log('🔍 RepCard Integration Diagnostic\n');
  console.log('='.repeat(60));

  // 1. Check API Key
  console.log('\n1️⃣ Checking API Key Configuration...');
  const apiKey = process.env.REPCARD_API_KEY;
  const apiUrl = process.env.REPCARD_API_URL || 'https://api.repcard.com';
  
  if (!apiKey) {
    console.log('❌ REPCARD_API_KEY is not set!');
    console.log('   Set it with: export REPCARD_API_KEY="your-key-here"');
    return;
  } else {
    console.log('✅ REPCARD_API_KEY is set');
    console.log(`   Length: ${apiKey.length} characters`);
    console.log(`   Starts with: ${apiKey.substring(0, 4)}...`);
  }
  console.log(`✅ API URL: ${apiUrl}`);

  // 2. Test API Connectivity
  console.log('\n2️⃣ Testing API Connectivity...');
  const client = new RepCardClient();
  
  try {
    // Try to fetch users (minimal endpoint, lightweight)
    const usersResponse = await client.getUsersMinimal({ perPage: 1 });
    console.log('✅ API connection successful!');
    console.log(`   Status: ${usersResponse.status}`);
    console.log(`   Message: ${usersResponse.message}`);
    console.log(`   Total users in system: ${usersResponse.result.total || 'N/A'}`);
  } catch (error) {
    console.log('❌ API connection failed!');
    console.log(`   Error: ${error instanceof Error ? error.message : 'Unknown error'}`);
    if (error instanceof Error && error.message.includes('401')) {
      console.log('   🔴 This indicates an invalid API key!');
    }
    if (error instanceof Error && error.message.includes('429')) {
      console.log('   🟡 Rate limit exceeded - wait a few minutes');
    }
    return;
  }

  // 3. Check User Mappings
  console.log('\n3️⃣ Checking User Mappings...');
  const usersWithRepcard = await sql`
    SELECT 
      id, 
      name, 
      email, 
      repcard_user_id,
      role,
      sales_office[1] as office
    FROM users
    WHERE repcard_user_id IS NOT NULL
    ORDER BY name
    LIMIT 10
  `;
  
  const usersArray = Array.from(usersWithRepcard);
  console.log(`   Found ${usersArray.length} users with repcard_user_id`);
  
  if (usersArray.length === 0) {
    console.log('   ⚠️  No users have repcard_user_id set!');
    console.log('   Users need repcard_user_id to fetch RepCard data.');
  } else {
    console.log('   Sample users:');
    usersArray.slice(0, 5).forEach((user: any) => {
      console.log(`     - ${user.name} (${user.email}): RepCard ID ${user.repcard_user_id}`);
    });
  }

  // 4. Test Data Fetching for First User
  if (usersArray.length > 0) {
    console.log('\n4️⃣ Testing Data Fetching...');
    const testUser = usersArray[0] as any;
    console.log(`   Testing with user: ${testUser.name} (RepCard ID: ${testUser.repcard_user_id})`);
    
    try {
      // Test customers fetch
      console.log('   Fetching customers...');
      const customersResponse = await client.getCustomers({
        userId: testUser.repcard_user_id,
        page: 1,
        perPage: 10
      });
      console.log(`   ✅ Customers fetch successful`);
      console.log(`      Total: ${customersResponse.result.total || 0}`);
      console.log(`      On this page: ${customersResponse.result.data.length}`);
      
      // Test appointments fetch
      console.log('   Fetching appointments...');
      const startDate = new Date();
      startDate.setMonth(startDate.getMonth() - 1);
      const appointmentsResponse = await client.getAppointments({
        setterIds: testUser.repcard_user_id,
        fromDate: startDate.toISOString().split('T')[0],
        toDate: new Date().toISOString().split('T')[0],
        page: 1,
        perPage: 10
      });
      console.log(`   ✅ Appointments fetch successful`);
      console.log(`      Total: ${appointmentsResponse.result.total || 0}`);
      console.log(`      On this page: ${appointmentsResponse.result.data.length}`);
      
    } catch (error) {
      console.log(`   ❌ Data fetch failed`);
      console.log(`      Error: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
  }

  // 5. Check Database Sync Status
  console.log('\n5️⃣ Checking Database Sync Status...');
  try {
    const syncLogs = await sql`
      SELECT 
        entity_type,
        sync_type,
        started_at,
        completed_at,
        status,
        records_fetched,
        records_inserted,
        records_updated,
        error_message
      FROM repcard_sync_log
      ORDER BY started_at DESC
      LIMIT 5
    `;
    
    const logsArray = Array.from(syncLogs);
    if (logsArray.length === 0) {
      console.log('   ⚠️  No sync logs found - sync may not have run yet');
    } else {
      console.log(`   Found ${logsArray.length} recent sync logs:`);
      logsArray.forEach((log: any) => {
        const status = log.status === 'completed' ? '✅' : log.status === 'failed' ? '❌' : '🟡';
        console.log(`     ${status} ${log.entity_type} - ${log.sync_type} (${log.status})`);
        if (log.error_message) {
          console.log(`        Error: ${log.error_message}`);
        }
      });
    }
  } catch (error) {
    console.log('   ⚠️  Could not check sync logs (table may not exist)');
  }

  // 6. Check Database Data Counts
  console.log('\n6️⃣ Checking Database Data Counts...');
  try {
    const customersCount = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const appointmentsCount = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    
    const customersTotal = Array.from(customersCount)[0]?.count || 0;
    const appointmentsTotal = Array.from(appointmentsCount)[0]?.count || 0;
    
    console.log(`   Customers in database: ${customersTotal}`);
    console.log(`   Appointments in database: ${appointmentsTotal}`);
    
    if (customersTotal === 0 && appointmentsTotal === 0) {
      console.log('   ⚠️  No RepCard data in database - sync may be needed');
    }
  } catch (error) {
    console.log('   ⚠️  Could not check data counts (tables may not exist)');
  }

  // 7. Test API Endpoint
  console.log('\n7️⃣ Testing /api/repcard/users/[userId]/stats endpoint...');
  if (usersArray.length > 0) {
    const testUser = usersArray[0] as any;
    const baseUrl = process.env.NEXT_PUBLIC_BASE_URL || 'http://localhost:3000';
    const testUrl = `${baseUrl}/api/repcard/users/${testUser.id}/stats?startDate=2025-01-01&endDate=2025-12-31`;
    
    console.log(`   URL: ${testUrl}`);
    console.log(`   ⚠️  Note: This requires authentication and running server`);
    console.log(`   You can test manually with: curl -H "Cookie: ..." "${testUrl}"`);
  }

  console.log('\n' + '='.repeat(60));
  console.log('✅ Diagnostic complete!\n');
}

// Run diagnostic
diagnoseRepCard().catch(console.error);

