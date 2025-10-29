import dotenv from 'dotenv';
import path from 'path';
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function test() {
  console.log('Testing appointments data...\n');

  // Check appointments table structure
  const sampleAppts = await sql`
    SELECT 
      repcard_appointment_id,
      setter_user_id,
      scheduled_at,
      created_at,
      status_category,
      disposition
    FROM repcard_appointments
    LIMIT 5
  `;
  const appts = Array.isArray(sampleAppts) ? sampleAppts : sampleAppts.rows || [];
  console.log(`Sample appointments (first 5):\n`);
  appts.forEach((a: any) => {
    console.log(`  - setter_user_id: ${a.setter_user_id} (type: ${typeof a.setter_user_id})`);
    console.log(`    scheduled_at: ${a.scheduled_at} (${a.scheduled_at ? 'has date' : 'NULL'})`);
    console.log(`    created_at: ${a.created_at}`);
    console.log(`    status: ${a.status_category || a.disposition || 'N/A'}\n`);
  });

  // Check date range issue
  const dateRange = {
    start: new Date(new Date().getFullYear(), new Date().getMonth(), 1).toISOString().split('T')[0],
    end: new Date().toISOString().split('T')[0]
  };
  console.log(`\nTesting date range: ${dateRange.start} to ${dateRange.end}\n`);

  // Count appointments with scheduled_at in range
  const withScheduled = await sql`
    SELECT COUNT(*) as count
    FROM repcard_appointments
    WHERE scheduled_at IS NOT NULL
      AND scheduled_at >= ${dateRange.start}::timestamp
      AND scheduled_at <= (${dateRange.end}::timestamp + INTERVAL '1 day')
  `;
  const scheduledCount = Array.isArray(withScheduled) ? withScheduled[0]?.count : withScheduled.rows?.[0]?.count || 0;
  console.log(`Appointments with scheduled_at in date range: ${scheduledCount}`);

  // Count appointments with NULL scheduled_at
  const nullScheduled = await sql`
    SELECT COUNT(*) as count
    FROM repcard_appointments
    WHERE scheduled_at IS NULL
  `;
  const nullCount = Array.isArray(nullScheduled) ? nullScheduled[0]?.count : nullScheduled.rows?.[0]?.count || 0;
  console.log(`Appointments with NULL scheduled_at: ${nullCount}`);

  // Count appointments using created_at instead
  const withCreated = await sql`
    SELECT COUNT(*) as count
    FROM repcard_appointments
    WHERE created_at >= ${dateRange.start}::timestamp
      AND created_at <= (${dateRange.end}::timestamp + INTERVAL '1 day')
  `;
  const createdCount = Array.isArray(withCreated) ? withCreated[0]?.count : withCreated.rows?.[0]?.count || 0;
  console.log(`Appointments with created_at in date range: ${createdCount}`);

  // Test with a specific user
  const testUserId = '139864';
  console.log(`\nTesting with user ${testUserId}:`);
  
  const userAppts = await sql`
    SELECT 
      COUNT(*) as count,
      COUNT(scheduled_at) as with_scheduled,
      COUNT(CASE WHEN scheduled_at IS NULL THEN 1 END) as null_scheduled
    FROM repcard_appointments
    WHERE setter_user_id::text = ${testUserId}
  `;
  const userStats = Array.isArray(userAppts) ? userAppts[0] : userAppts.rows?.[0];
  console.log(`  Total appointments: ${userStats.count}`);
  console.log(`  With scheduled_at: ${userStats.with_scheduled}`);
  console.log(`  NULL scheduled_at: ${userStats.null_scheduled}`);

  process.exit(0);
}

test().catch(console.error);
