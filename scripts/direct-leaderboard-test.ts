import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function test() {
  console.log('Testing leaderboard query directly...\n');

  // Get users with repcard_user_id
  const users = await sql`
    SELECT id, name, email, repcard_user_id
    FROM users
    WHERE repcard_user_id IS NOT NULL
    LIMIT 10
  `;
  const userArray = Array.isArray(users) ? users : users.rows || [];
  
  if (userArray.length === 0) {
    console.log('❌ No users found');
    process.exit(1);
  }

  const repcardUserIds = userArray.map((u: any) => u.repcard_user_id);
  console.log(`Testing with ${repcardUserIds.length} users\n`);

  // Test with different date ranges
  const now = new Date();
  const ranges = {
    month: {
      start: new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0],
      end: now.toISOString().split('T')[0]
    },
    last_30: {
      start: new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
      end: now.toISOString().split('T')[0]
    },
    last_90: {
      start: new Date(now.getTime() - 90 * 24 * 60 * 60 * 1000).toISOString().split('T')[0],
      end: now.toISOString().split('T')[0]
    },
    last_12_months: {
      start: new Date(now.getFullYear(), now.getMonth() - 12, 1).toISOString().split('T')[0],
      end: now.toISOString().split('T')[0]
    }
  };

  for (const [name, range] of Object.entries(ranges)) {
    console.log(`\n${name.toUpperCase()} (${range.start} to ${range.end}):`);
    
    // Test doors_knocked
    const customers = await sql`
      SELECT
        setter_user_id::text as setter_user_id,
        COUNT(*) as count
      FROM repcard_customers
      WHERE setter_user_id::text = ANY(${repcardUserIds.map(String)}::text[])
        AND created_at >= ${range.start}::timestamp
        AND created_at <= (${range.end}::timestamp + INTERVAL '1 day')
      GROUP BY setter_user_id
      ORDER BY count DESC
      LIMIT 5
    `;
    const custCounts = Array.isArray(customers) ? customers : customers.rows || [];
    console.log(`  Doors knocked: ${custCounts.length} users with data`);
    if (custCounts.length > 0) {
      console.log(`  Top 3:`);
      custCounts.slice(0, 3).forEach((c: any) => {
        console.log(`    - User ${c.setter_user_id}: ${c.count} doors`);
      });
    }

    // Test appointments_set
    const appointments = await sql`
      SELECT
        setter_user_id::text as setter_user_id,
        COUNT(*) as count
      FROM repcard_appointments
      WHERE setter_user_id::text = ANY(${repcardUserIds.map(String)}::text[])
        AND (
          (scheduled_at IS NOT NULL AND scheduled_at >= ${range.start}::timestamp AND scheduled_at <= (${range.end}::timestamp + INTERVAL '1 day'))
          OR
          (scheduled_at IS NULL AND created_at >= ${range.start}::timestamp AND created_at <= (${range.end}::timestamp + INTERVAL '1 day'))
        )
      GROUP BY setter_user_id
      ORDER BY count DESC
      LIMIT 5
    `;
    const apptCounts = Array.isArray(appointments) ? appointments : appointments.rows || [];
    console.log(`  Appointments set: ${apptCounts.length} users with data`);
    if (apptCounts.length > 0) {
      console.log(`  Top 3:`);
      apptCounts.slice(0, 3).forEach((a: any) => {
        console.log(`    - User ${a.setter_user_id}: ${a.count} appointments`);
      });
    }
  }

  process.exit(0);
}

test().catch(console.error);
