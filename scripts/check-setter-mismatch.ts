import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function check() {
  console.log('🔍 Checking setter_user_id mismatch...\n');

  // Get sample customers
  const customers = await sql`
    SELECT 
      repcard_customer_id,
      setter_user_id,
      name,
      created_at
    FROM repcard_customers
    WHERE created_at >= '2025-10-01'::timestamp
      AND created_at <= ('2025-10-29'::timestamp + INTERVAL '1 day')
    LIMIT 10
  `;
  const custArray = Array.isArray(customers) ? customers : customers.rows || [];
  
  console.log(`Sample customers from this month:`);
  custArray.forEach((c: any) => {
    console.log(`  - Customer ${c.repcard_customer_id}: setter_user_id = ${c.setter_user_id} (type: ${typeof c.setter_user_id})`);
  });

  // Get sample setters
  const setters = await sql`
    SELECT id, name, repcard_user_id
    FROM users
    WHERE repcard_user_id IS NOT NULL AND role = 'setter'
    LIMIT 5
  `;
  const setterArray = Array.isArray(setters) ? setters : setters.rows || [];
  
  console.log(`\nSample setters:`);
  setterArray.forEach((s: any) => {
    console.log(`  - ${s.name}: repcard_user_id = ${s.repcard_user_id} (type: ${typeof s.repcard_user_id})`);
  });

  // Check if any customers match setters
  const setterIds = setterArray.map((s: any) => s.repcard_user_id);
  console.log(`\nChecking if customer setter_user_ids match any setter repcard_user_ids...`);
  
  const matching = await sql`
    SELECT COUNT(*) as count
    FROM repcard_customers
    WHERE setter_user_id::text = ANY(${setterIds.map(String)}::text[])
      AND created_at >= '2025-10-01'::timestamp
      AND created_at <= ('2025-10-29'::timestamp + INTERVAL '1 day')
  `;
  const matchCount = Array.isArray(matching) ? matching[0]?.count : matching.rows?.[0]?.count || 0;
  console.log(`   Matches: ${matchCount}`);

  // Check unique setter_user_ids in customers table
  const uniqueSetters = await sql`
    SELECT DISTINCT setter_user_id, COUNT(*) as count
    FROM repcard_customers
    WHERE created_at >= '2025-10-01'::timestamp
      AND created_at <= ('2025-10-29'::timestamp + INTERVAL '1 day')
    GROUP BY setter_user_id
    ORDER BY count DESC
    LIMIT 10
  `;
  const uniqueArray = Array.isArray(uniqueSetters) ? uniqueSetters : uniqueSetters.rows || [];
  console.log(`\nUnique setter_user_ids in customers (this month):`);
  uniqueArray.forEach((u: any) => {
    console.log(`  - setter_user_id: ${u.setter_user_id} (${u.count} customers)`);
  });

  // Check if these setter_user_ids exist in users table
  const setterUserIdsFromCustomers = uniqueArray.map((u: any) => u.setter_user_id);
  if (setterUserIdsFromCustomers.length > 0) {
    const usersWithTheseIds = await sql`
      SELECT id, name, repcard_user_id, role
      FROM users
      WHERE repcard_user_id::text = ANY(${setterUserIdsFromCustomers.map(String)}::text[])
    `;
    const usersArray = Array.isArray(usersWithTheseIds) ? usersWithTheseIds : usersWithTheseIds.rows || [];
    console.log(`\nUsers with matching repcard_user_ids:`);
    if (usersArray.length === 0) {
      console.log(`  ❌ NONE! This is the problem - customers have setter_user_ids that don't match any user's repcard_user_id`);
    } else {
      usersArray.forEach((u: any) => {
        console.log(`  - ${u.name}: repcard_user_id = ${u.repcard_user_id}, role = ${u.role}`);
      });
    }
  }

  process.exit(0);
}

check().catch(console.error);
