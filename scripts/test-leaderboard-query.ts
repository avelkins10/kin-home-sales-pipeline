import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function test() {
  console.log('Testing leaderboard queries...\n');

  // Get a sample user with repcard_user_id
  const users = await sql`
    SELECT id, name, email, repcard_user_id
    FROM users
    WHERE repcard_user_id IS NOT NULL
    LIMIT 5
  `;
  const userArray = Array.isArray(users) ? users : users.rows || [];
  console.log(`Found ${userArray.length} users with repcard_user_id:\n`);
  userArray.forEach((u: any) => {
    console.log(`  - ${u.name} (${u.email}): repcard_user_id = ${u.repcard_user_id} (type: ${typeof u.repcard_user_id})`);
  });

  if (userArray.length === 0) {
    console.log('\n❌ No users with repcard_user_id found!');
    process.exit(1);
  }

  const testUser = userArray[0];
  console.log(`\nTesting with user: ${testUser.name} (repcard_user_id: ${testUser.repcard_user_id})\n`);

  // Test customers query
  console.log('1. Testing customers query...');
  const customerCounts = await sql`
    SELECT
      setter_user_id::text as setter_user_id,
      COUNT(*) as count
    FROM repcard_customers
    WHERE setter_user_id::text = ${String(testUser.repcard_user_id)}
      AND created_at >= (NOW() - INTERVAL '30 days')::timestamp
      AND created_at <= (NOW() + INTERVAL '1 day')::timestamp
    GROUP BY setter_user_id
  `;
  const counts = Array.isArray(customerCounts) ? customerCounts : customerCounts.rows || [];
  console.log(`   Results: ${counts.length} rows`);
  if (counts.length > 0) {
    console.log(`   Customer count for ${testUser.repcard_user_id}: ${counts[0].count}`);
  } else {
    console.log(`   ⚠️  No customers found for this user`);
    
    // Check if ANY customers exist
    const allCustomers = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const total = Array.isArray(allCustomers) ? allCustomers[0]?.count : allCustomers.rows?.[0]?.count || 0;
    console.log(`   Total customers in database: ${total}`);
    
    if (total > 0) {
      const sample = await sql`SELECT setter_user_id, name, created_at FROM repcard_customers LIMIT 3`;
      const samples = Array.isArray(sample) ? sample : sample.rows || [];
      console.log(`   Sample customers:`);
      samples.forEach((c: any) => {
        console.log(`     - setter_user_id: ${c.setter_user_id} (type: ${typeof c.setter_user_id}), name: ${c.name}, created: ${c.created_at}`);
      });
    }
  }

  // Test appointments query
  console.log('\n2. Testing appointments query...');
  const appointmentCounts = await sql`
    SELECT
      setter_user_id::text as setter_user_id,
      COUNT(*) as count
    FROM repcard_appointments
    WHERE setter_user_id::text = ${String(testUser.repcard_user_id)}
      AND scheduled_at >= (NOW() - INTERVAL '30 days')::timestamp
      AND scheduled_at <= (NOW() + INTERVAL '1 day')::timestamp
    GROUP BY setter_user_id
  `;
  const apptCounts = Array.isArray(appointmentCounts) ? appointmentCounts : appointmentCounts.rows || [];
  console.log(`   Results: ${apptCounts.length} rows`);
  if (apptCounts.length > 0) {
    console.log(`   Appointment count for ${testUser.repcard_user_id}: ${apptCounts[0].count}`);
  } else {
    console.log(`   ⚠️  No appointments found for this user`);
    
    // Check if ANY appointments exist
    const allAppts = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    const total = Array.isArray(allAppts) ? allAppts[0]?.count : allAppts.rows?.[0]?.count || 0;
    console.log(`   Total appointments in database: ${total}`);
  }

  process.exit(0);
}

test().catch(console.error);
