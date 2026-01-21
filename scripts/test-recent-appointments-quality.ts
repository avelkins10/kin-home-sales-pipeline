#!/usr/bin/env tsx
import { sql } from '../lib/db/client';

async function testRecentAppointments() {
  console.log('🔍 Testing Recent Appointments Quality Metrics...\n');

  try {
    // Get appointments from last 2 days
    const recentAppts = await sql`
      SELECT 
        a.repcard_appointment_id,
        a.scheduled_at,
        a.is_within_48_hours,
        a.has_power_bill,
        c.repcard_customer_id,
        c.created_at as customer_created_at,
        (a.scheduled_at - c.created_at) as time_diff,
        CASE 
          WHEN (a.scheduled_at - c.created_at) <= INTERVAL '48 hours' 
            AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours'
          THEN TRUE 
          ELSE FALSE 
        END as should_be_48h
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
      WHERE a.scheduled_at >= NOW() - INTERVAL '2 days'
      ORDER BY a.scheduled_at DESC
      LIMIT 20
    `;

    const rows = Array.from(recentAppts);
    console.log(`Found ${rows.length} appointments in last 2 days:\n`);

    for (const row of rows) {
      const hasPB = await sql`
        SELECT COUNT(*)::int as count
        FROM (
          SELECT 1 FROM repcard_customer_attachments ca
          WHERE ca.repcard_customer_id::text = ${row.repcard_customer_id}::text
            AND (
              ca.attachment_type ILIKE '%power%' 
              OR ca.attachment_type ILIKE '%bill%' 
              OR ca.file_name ILIKE '%power%' 
              OR ca.file_name ILIKE '%bill%'
            )
          UNION ALL
          SELECT 1 FROM repcard_appointment_attachments aa
          WHERE aa.repcard_appointment_id::text = ${row.repcard_appointment_id}::text
            AND (
              aa.attachment_type ILIKE '%power%' 
              OR aa.attachment_type ILIKE '%bill%' 
              OR aa.file_name ILIKE '%power%' 
              OR aa.file_name ILIKE '%bill%'
            )
        ) x
      `;
      const pbCount = Array.from(hasPB)[0]?.count || 0;
      const shouldHavePB = pbCount > 0;

      console.log(`Appointment ${row.repcard_appointment_id}:`);
      console.log(`  Scheduled: ${row.scheduled_at}`);
      console.log(`  Customer created: ${row.customer_created_at}`);
      console.log(`  Time diff: ${row.time_diff}`);
      console.log(`  Should be 48h: ${row.should_be_48h}`);
      console.log(`  Is 48h (stored): ${row.is_within_48_hours}`);
      console.log(`  Should have PB: ${shouldHavePB}`);
      console.log(`  Has PB (stored): ${row.has_power_bill}`);
      console.log(`  Match 48h: ${row.should_be_48h === row.is_within_48_hours ? '✅' : '❌'}`);
      console.log(`  Match PB: ${shouldHavePB === row.has_power_bill ? '✅' : '❌'}`);
      console.log('');
    }

  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

testRecentAppointments();
