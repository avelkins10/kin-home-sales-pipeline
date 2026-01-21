#!/usr/bin/env tsx
/**
 * Backfill RepCard Metrics
 * 
 * This script recalculates is_within_48_hours and has_power_bill for all appointments
 * that don't have these values set correctly.
 * 
 * Usage: DATABASE_URL="your-db-url" npx tsx scripts/backfill-repcard-metrics.ts
 * Or: npx tsx scripts/backfill-repcard-metrics.ts (if .env.local exists)
 */

import { config } from 'dotenv';
import { sql } from '../lib/db/client';

// Load environment variables
config({ path: '.env.local' });
config({ path: '.env' });

// Ensure DATABASE_URL is mapped to POSTGRES_URL for @vercel/postgres
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function backfillMetrics() {
  console.log('🔄 Backfilling RepCard Metrics...\n');

  try {
    // Step 1: Backfill is_within_48_hours
    console.log('📊 Step 1: Backfilling is_within_48_hours...');
    const within48Result = await sql`
      UPDATE repcard_appointments a
      SET is_within_48_hours = (
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL
            AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours' 
            AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours' 
          THEN TRUE
          ELSE FALSE
        END
      )
      FROM repcard_customers c
      WHERE a.repcard_customer_id = c.repcard_customer_id
        AND (
          a.is_within_48_hours IS NULL 
          OR a.is_within_48_hours != (
            CASE
              WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL
                AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours' 
                AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours' 
              THEN TRUE
              ELSE FALSE
            END
          )
        )
    `;
    const within48Updated = Array.isArray(within48Result) ? 0 : within48Result.rowCount || 0;
    console.log(`   ✅ Updated ${within48Updated} appointments`);

    // Step 2: Backfill has_power_bill
    console.log('\n📄 Step 2: Backfilling has_power_bill...');
    const powerBillResult = await sql`
      UPDATE repcard_appointments a
      SET has_power_bill = (
        CASE
          WHEN EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca
            WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
              AND (
                ca.attachment_type ILIKE '%power%' 
                OR ca.attachment_type ILIKE '%bill%' 
                OR ca.file_name ILIKE '%power%' 
                OR ca.file_name ILIKE '%bill%'
                OR ca.attachment_type IS NULL  -- If type is null, check filename
              )
          ) OR EXISTS (
            SELECT 1 FROM repcard_appointment_attachments aa
            WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
              AND (
                aa.attachment_type ILIKE '%power%' 
                OR aa.attachment_type ILIKE '%bill%' 
                OR aa.file_name ILIKE '%power%' 
                OR aa.file_name ILIKE '%bill%'
                OR aa.attachment_type IS NULL
              )
          ) THEN TRUE
          ELSE FALSE
        END
      )
      WHERE has_power_bill IS NULL 
        OR has_power_bill != (
          CASE
            WHEN EXISTS (
              SELECT 1 FROM repcard_customer_attachments ca
              WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
                AND (
                  ca.attachment_type ILIKE '%power%' 
                  OR ca.attachment_type ILIKE '%bill%' 
                  OR ca.file_name ILIKE '%power%' 
                  OR ca.file_name ILIKE '%bill%'
                  OR ca.attachment_type IS NULL
                )
            ) OR EXISTS (
              SELECT 1 FROM repcard_appointment_attachments aa
              WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
                AND (
                  aa.attachment_type ILIKE '%power%' 
                  OR aa.attachment_type ILIKE '%bill%' 
                  OR aa.file_name ILIKE '%power%' 
                  OR aa.file_name ILIKE '%bill%'
                  OR aa.attachment_type IS NULL
                )
            ) THEN TRUE
            ELSE FALSE
          END
        )
    `;
    const powerBillUpdated = Array.isArray(powerBillResult) ? 0 : powerBillResult.rowCount || 0;
    console.log(`   ✅ Updated ${powerBillUpdated} appointments`);

    // Step 3: Verify results
    console.log('\n📋 Step 3: Verifying results...');
    const verifyResult = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_pb,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb
      FROM repcard_appointments
      WHERE scheduled_at >= '2025-12-31'::timestamp
        AND scheduled_at <= '2026-01-20'::timestamp
    `;
    const verify = Array.from(verifyResult)[0];
    console.log(`   Total appointments (Dec 31 - Jan 20): ${verify?.total || 0}`);
    console.log(`   Within 48h: ${verify?.within_48h || 0} (${verify?.total > 0 ? ((verify?.within_48h / verify?.total) * 100).toFixed(1) : 0}%)`);
    console.log(`   With power bill: ${verify?.with_pb || 0} (${verify?.total > 0 ? ((verify?.with_pb / verify?.total) * 100).toFixed(1) : 0}%)`);
    console.log(`   NULL within_48h: ${verify?.null_48h || 0}`);
    console.log(`   NULL has_power_bill: ${verify?.null_pb || 0}`);

    console.log('\n✅ Backfill complete!');
  } catch (error) {
    console.error('❌ Error:', error);
    throw error;
  }
}

backfillMetrics()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
