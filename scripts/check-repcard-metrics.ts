#!/usr/bin/env tsx
import { sql } from '../lib/db/client';

async function checkMetrics() {
  console.log('🔍 Checking RepCard Metrics...\n');

  try {
    // Check total appointments
    const totalResult = await sql`
      SELECT COUNT(*)::int as total
      FROM repcard_appointments
      WHERE scheduled_at >= '2025-12-31'::timestamp
        AND scheduled_at <= '2026-01-20'::timestamp
    `;
    const total = Array.from(totalResult)[0]?.total || 0;
    console.log(`📊 Total appointments (Dec 31 - Jan 20): ${total}`);

    // Check within 48 hours
    const within48Result = await sql`
      SELECT COUNT(*)::int as count
      FROM repcard_appointments
      WHERE scheduled_at >= '2025-12-31'::timestamp
        AND scheduled_at <= '2026-01-20'::timestamp
        AND is_within_48_hours = TRUE
    `;
    const within48h = Array.from(within48Result)[0]?.count || 0;
    console.log(`⏱️  Appointments within 48h: ${within48h} (${total > 0 ? ((within48h / total) * 100).toFixed(1) : 0}%)`);

    // Check power bill
    const powerBillResult = await sql`
      SELECT COUNT(*)::int as count
      FROM repcard_appointments
      WHERE scheduled_at >= '2025-12-31'::timestamp
        AND scheduled_at <= '2026-01-20'::timestamp
        AND has_power_bill = TRUE
    `;
    const withPowerBill = Array.from(powerBillResult)[0]?.count || 0;
    console.log(`📄 Appointments with power bill: ${withPowerBill} (${total > 0 ? ((withPowerBill / total) * 100).toFixed(1) : 0}%)`);

    // Check if columns exist and have data
    const columnCheck = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE is_within_48_hours IS NOT NULL) as has_48h_data,
        COUNT(*) FILTER (WHERE has_power_bill IS NOT NULL) as has_pb_data,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE) as true_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE) as true_pb
      FROM repcard_appointments
      WHERE scheduled_at >= '2025-12-31'::timestamp
        AND scheduled_at <= '2026-01-20'::timestamp
    `;
    const check = Array.from(columnCheck)[0];
    console.log(`\n📋 Column Status:`);
    console.log(`   is_within_48_hours: ${check?.has_48h_data || 0} rows with data, ${check?.true_48h || 0} TRUE values`);
    console.log(`   has_power_bill: ${check?.has_pb_data || 0} rows with data, ${check?.true_pb || 0} TRUE values`);

    // Sample a few appointments to see their values
    const sampleResult = await sql`
      SELECT 
        repcard_appointment_id,
        scheduled_at,
        is_within_48_hours,
        has_power_bill,
        setter_user_id,
        closer_user_id
      FROM repcard_appointments
      WHERE scheduled_at >= '2025-12-31'::timestamp
        AND scheduled_at <= '2026-01-20'::timestamp
      LIMIT 5
    `;
    console.log(`\n📝 Sample appointments:`);
    Array.from(sampleResult).forEach((apt: any) => {
      console.log(`   ID ${apt.repcard_appointment_id}: within48h=${apt.is_within_48_hours}, hasPB=${apt.has_power_bill}`);
    });

  } catch (error) {
    console.error('❌ Error:', error);
  }
}

checkMetrics()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
