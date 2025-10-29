import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function debug() {
  console.log('🔍 Debugging Top Setters - Doors Knocked API...\n');

  // Simulate the exact API call
  const role = 'setter';
  const metric = 'doors_knocked';
  const timeRange = 'month'; // "Month to Date"
  
  // Calculate date range (same as API)
  const now = new Date();
  const calculatedStartDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
  const calculatedEndDate = now.toISOString().split('T')[0];
  
  console.log(`Date range: ${calculatedStartDate} to ${calculatedEndDate}\n`);

  // Step 1: Get setters with repcard_user_id
  console.log('1. Fetching setters with repcard_user_id...');
  const setters = await sql`
    SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
    FROM users
    WHERE repcard_user_id IS NOT NULL AND role = ${role}
    LIMIT 20
  `;
  const setterArray = Array.isArray(setters) ? setters : setters.rows || [];
  console.log(`   Found ${setterArray.length} setters with repcard_user_id`);
  
  if (setterArray.length === 0) {
    console.log('   ❌ NO SETTERS FOUND! This is the problem.');
    process.exit(1);
  }
  
  console.log(`   Sample setters:`);
  setterArray.slice(0, 3).forEach((s: any) => {
    console.log(`     - ${s.name}: repcard_user_id = ${s.repcard_user_id}`);
  });

  // Step 2: Get repcard_user_ids
  const repcardUserIds = setterArray.map((u: any) => u.repcard_user_id);
  console.log(`\n2. Testing doors_knocked query with ${repcardUserIds.length} users...`);

  // Step 3: Query customers (doors knocked)
  const customerCountsRaw = await sql`
    SELECT
      setter_user_id::text as setter_user_id,
      COUNT(*) as count
    FROM repcard_customers
    WHERE setter_user_id::text = ANY(${repcardUserIds.map(String)}::text[])
      AND created_at >= ${calculatedStartDate}::timestamp
      AND created_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day')
    GROUP BY setter_user_id
    ORDER BY count DESC
  `;
  const customerCounts = Array.from(customerCountsRaw);
  
  console.log(`   Found ${customerCounts.length} users with doors knocked`);
  
  if (customerCounts.length === 0) {
    console.log('   ⚠️  No customers found in date range!');
    
    // Check if ANY customers exist for these users
    const allCustomers = await sql`
      SELECT
        setter_user_id::text as setter_user_id,
        COUNT(*) as count,
        MIN(created_at) as earliest,
        MAX(created_at) as latest
      FROM repcard_customers
      WHERE setter_user_id::text = ANY(${repcardUserIds.map(String)}::text[])
      GROUP BY setter_user_id
      ORDER BY count DESC
      LIMIT 5
    `;
    const allCust = Array.from(allCustomers);
    console.log(`\n   Checking ALL customers (no date filter):`);
    allCust.forEach((c: any) => {
      console.log(`     - User ${c.setter_user_id}: ${c.count} customers (earliest: ${c.earliest}, latest: ${c.latest})`);
    });
  } else {
    console.log(`   Top 5:`);
    customerCounts.slice(0, 5).forEach((c: any) => {
      console.log(`     - User ${c.setter_user_id}: ${c.count} doors`);
    });
  }

  // Step 4: Check total customers in database
  const totalCustomers = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
  const total = Array.isArray(totalCustomers) ? totalCustomers[0]?.count : totalCustomers.rows?.[0]?.count || 0;
  console.log(`\n3. Total customers in database: ${total}`);

  // Step 5: Check customers in date range (any user)
  const customersInRange = await sql`
    SELECT COUNT(*) as count
    FROM repcard_customers
    WHERE created_at >= ${calculatedStartDate}::timestamp
      AND created_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day')
  `;
  const inRange = Array.isArray(customersInRange) ? customersInRange[0]?.count : customersInRange.rows?.[0]?.count || 0;
  console.log(`   Customers in date range (any user): ${inRange}`);

  process.exit(0);
}

debug().catch(console.error);
