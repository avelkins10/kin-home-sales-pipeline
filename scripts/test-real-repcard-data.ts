import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function test() {
  console.log('🔍 Testing REAL RepCard data flow...\n');

  // 1. Check if we have ANY data
  const customers = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
  const appts = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
  const users = await sql`SELECT COUNT(*) as count FROM users WHERE repcard_user_id IS NOT NULL`;
  
  const custCount = Array.isArray(customers) ? customers[0]?.count : customers.rows?.[0]?.count || 0;
  const apptCount = Array.isArray(appts) ? appts[0]?.count : appts.rows?.[0]?.count || 0;
  const userCount = Array.isArray(users) ? users[0]?.count : users.rows?.[0]?.count || 0;
  
  console.log(`✅ Database has:`);
  console.log(`   Customers: ${custCount}`);
  console.log(`   Appointments: ${apptCount}`);
  console.log(`   Linked Users: ${userCount}\n`);

  // 2. Test leaderboard query directly
  const testUserId = '139864';
  const dateRange = {
    start: '2025-10-01',
    end: '2025-10-29'
  };
  
  console.log(`Testing leaderboard query for user ${testUserId}:`);
  
  const result = await sql`
    SELECT
      setter_user_id::text,
      COUNT(*) as count
    FROM repcard_customers
    WHERE setter_user_id::text = ${testUserId}
      AND created_at >= ${dateRange.start}::timestamp
      AND created_at <= (${dateRange.end}::timestamp + INTERVAL '1 day')
    GROUP BY setter_user_id
  `;
  
  const results = Array.isArray(result) ? result : result.rows || [];
  console.log(`   Results: ${results.length} rows`);
  if (results.length > 0) {
    console.log(`   ✅ Found ${results[0].count} customers`);
  } else {
    console.log(`   ❌ No customers found - THIS IS THE PROBLEM!`);
  }

  // 3. Check what users exist with repcard_user_id
  const userList = await sql`
    SELECT id, name, email, repcard_user_id 
    FROM users 
    WHERE repcard_user_id IS NOT NULL 
    LIMIT 5
  `;
  const usersList = Array.isArray(userList) ? userList : userList.rows || [];
  console.log(`\nSample users with repcard_user_id:`);
  usersList.forEach((u: any) => {
    console.log(`   - ${u.name}: ${u.repcard_user_id} (type: ${typeof u.repcard_user_id})`);
  });

  process.exit(0);
}

test().catch(console.error);
