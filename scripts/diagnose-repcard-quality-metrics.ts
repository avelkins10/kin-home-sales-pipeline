#!/usr/bin/env tsx
import { sql } from '../lib/db/client';

async function diagnoseQualityMetrics() {
  console.log('🔍 Diagnosing RepCard Quality Metrics for Month to Date (12/31/2025 - 1/20/2026)...\n');

  const startDate = '2025-12-31';
  const endDate = '2026-01-20';

  try {
    // 1. Check total appointments in date range
    const totalResult = await sql`
      SELECT COUNT(*)::int as total
      FROM repcard_appointments
      WHERE scheduled_at >= ${startDate}::timestamptz
        AND scheduled_at <= ${endDate}::timestamptz
    `;
    const total = Array.from(totalResult)[0]?.total || 0;
    console.log(`📊 Total appointments: ${total}`);

    // 2. Check NULL vs TRUE vs FALSE for is_within_48_hours
    const within48Status = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_count,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as true_count,
        COUNT(*) FILTER (WHERE is_within_48_hours = FALSE)::int as false_count
      FROM repcard_appointments
      WHERE scheduled_at >= ${startDate}::timestamptz
        AND scheduled_at <= ${endDate}::timestamptz
    `;
    const status48 = Array.from(within48Status)[0];
    console.log(`\n⏱️  is_within_48_hours status:`);
    console.log(`   NULL: ${status48?.null_count || 0}`);
    console.log(`   TRUE: ${status48?.true_count || 0}`);
    console.log(`   FALSE: ${status48?.false_count || 0}`);

    // 3. Check NULL vs TRUE vs FALSE for has_power_bill
    const powerBillStatus = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_count,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as true_count,
        COUNT(*) FILTER (WHERE has_power_bill = FALSE)::int as false_count
      FROM repcard_appointments
      WHERE scheduled_at >= ${startDate}::timestamptz
        AND scheduled_at <= ${endDate}::timestamptz
    `;
    const statusPB = Array.from(powerBillStatus)[0];
    console.log(`\n📄 has_power_bill status:`);
    console.log(`   NULL: ${statusPB?.null_count || 0}`);
    console.log(`   TRUE: ${statusPB?.true_count || 0}`);
    console.log(`   FALSE: ${statusPB?.false_count || 0}`);

    // 4. Check if appointments have matching customers
    const customerMatch = await sql`
      SELECT 
        COUNT(DISTINCT a.id)::int as total,
        COUNT(DISTINCT a.id) FILTER (WHERE c.repcard_customer_id IS NOT NULL)::int as with_customer,
        COUNT(DISTINCT a.id) FILTER (WHERE c.repcard_customer_id IS NULL)::int as no_customer
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
      WHERE a.scheduled_at >= ${startDate}::timestamptz
        AND a.scheduled_at <= ${endDate}::timestamptz
    `;
    const match = Array.from(customerMatch)[0];
    console.log(`\n👥 Customer matching:`);
    console.log(`   Total: ${match?.total || 0}`);
    console.log(`   With customer: ${match?.with_customer || 0}`);
    console.log(`   No customer: ${match?.no_customer || 0}`);

    // 5. Sample calculation: How many SHOULD be within 48h?
    const shouldBe48h = await sql`
      SELECT COUNT(DISTINCT a.id)::int as count
      FROM repcard_appointments a
      INNER JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
      WHERE a.scheduled_at >= ${startDate}::timestamptz
        AND a.scheduled_at <= ${endDate}::timestamptz
        AND a.scheduled_at IS NOT NULL
        AND c.created_at IS NOT NULL
        AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours'
        AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours'
    `;
    const should48h = Array.from(shouldBe48h)[0]?.count || 0;
    console.log(`\n✅ Should be within 48h (calculated): ${should48h} (${total > 0 ? ((should48h / total) * 100).toFixed(1) : 0}%)`);

    // 6. Sample calculation: How many SHOULD have power bill?
    const shouldHavePB = await sql`
      SELECT COUNT(DISTINCT a.id)::int as count
      FROM repcard_appointments a
      WHERE a.scheduled_at >= ${startDate}::timestamptz
        AND a.scheduled_at <= ${endDate}::timestamptz
        AND (
          EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca
            WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
              AND (
                ca.attachment_type ILIKE '%power%' 
                OR ca.attachment_type ILIKE '%bill%' 
                OR ca.file_name ILIKE '%power%' 
                OR ca.file_name ILIKE '%bill%'
              )
          ) OR EXISTS (
            SELECT 1 FROM repcard_appointment_attachments aa
            WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
              AND (
                aa.attachment_type ILIKE '%power%' 
                OR aa.attachment_type ILIKE '%bill%' 
                OR aa.file_name ILIKE '%power%' 
                OR aa.file_name ILIKE '%bill%'
              )
          )
        )
    `;
    const shouldPB = Array.from(shouldHavePB)[0]?.count || 0;
    console.log(`\n✅ Should have power bill (calculated): ${shouldPB} (${total > 0 ? ((shouldPB / total) * 100).toFixed(1) : 0}%)`);

    // 7. Recommendation
    console.log(`\n💡 Recommendation:`);
    if ((status48?.null_count || 0) > 0 || (statusPB?.null_count || 0) > 0) {
      console.log(`   ⚠️  Columns have NULL values. Run backfill to populate them.`);
      console.log(`   Run: POST /api/admin/repcard/backfill-metrics`);
    } else if (should48h > (status48?.true_count || 0) || shouldPB > (statusPB?.true_count || 0)) {
      console.log(`   ⚠️  Calculated values don't match stored values. Re-run backfill.`);
    } else {
      console.log(`   ✅ Data looks correct. Issue may be in query logic.`);
    }

  } catch (error) {
    console.error('❌ Error:', error);
    process.exit(1);
  }
}

diagnoseQualityMetrics();
