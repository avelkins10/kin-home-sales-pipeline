#!/usr/bin/env tsx
/**
 * Verify Reschedule Schema
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function verify() {
  console.log('🔍 Verifying Reschedule Tracking Schema\n');
  console.log('='.repeat(70));

  try {
    // Check columns
    console.log('\n📋 Checking columns in repcard_appointments:');
    const columns = await sql`
      SELECT column_name, data_type, is_nullable, column_default
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND table_name = 'repcard_appointments'
        AND column_name IN ('is_reschedule', 'reschedule_count', 'original_appointment_id', 'reschedule_reason')
      ORDER BY column_name;
    `;

    if (columns.length > 0) {
      console.log('  ✅ Reschedule tracking columns found:');
      columns.forEach((col: any) => {
        console.log(`     - ${col.column_name}`);
        console.log(`       Type: ${col.data_type}`);
        console.log(`       Nullable: ${col.is_nullable}`);
        console.log(`       Default: ${col.column_default || 'none'}`);
      });
    } else {
      console.log('  ❌ No reschedule tracking columns found');
      return;
    }

    // Check view
    console.log('\n📋 Checking repcard_appointment_chains view:');
    const viewCheck = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.views
        WHERE table_schema = 'public'
        AND table_name = 'repcard_appointment_chains'
      ) as exists;
    `;

    if ((viewCheck[0] as any)?.exists) {
      console.log('  ✅ View exists');
    } else {
      console.log('  ❌ View NOT found');
    }

    // Check data
    console.log('\n📊 Current reschedule data:');
    const result = await sql`
      SELECT
        COUNT(*)::int as total,
        COUNT(CASE WHEN is_reschedule = TRUE THEN 1 END)::int as reschedules,
        COUNT(DISTINCT CASE WHEN is_reschedule = TRUE THEN customer_id END)::int as customers
      FROM repcard_appointments
      WHERE customer_id IS NOT NULL;
    `;

    const rows = Array.isArray(result) ? result : [result];
    const data = rows[0] as any;

    console.log(`  Total appointments: ${data?.total || 0}`);
    console.log(`  Marked as reschedules: ${data?.reschedules || 0}`);
    console.log(`  Customers with reschedules: ${data?.customers || 0}`);

    if (data && data.total > 0) {
      const rate = ((data.reschedules / data.total) * 100).toFixed(1);
      console.log(`  Reschedule rate: ${rate}%`);

      if (data.reschedules === 0) {
        console.log('\n  ⚠️  No reschedules detected - sync needed to populate data');
      }
    }

    console.log('\n' + '='.repeat(70));
    console.log('✅ Schema verification complete!');

  } catch (error) {
    console.error('\n❌ Error:', error);
    throw error;
  }
}

verify()
  .then(() => {
    console.log('\n');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Fatal error:', error);
    process.exit(1);
  });
