#!/usr/bin/env tsx
/**
 * Check Migration Status and Run Sync
 *
 * This script:
 * 1. Checks which migrations have been applied
 * 2. Verifies reschedule tracking schema exists
 * 3. Runs a full RepCard sync to populate data
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function main() {
  console.log('🔍 Checking Migration Status\n');
  console.log('='.repeat(70));

  try {
    // Check if schema_migrations table exists
    const tableCheck = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_name = 'schema_migrations'
      );
    `;

    const tableExists = (tableCheck[0] as any)?.exists;

    if (tableExists) {
      // Get applied migrations
      const migrations = await sql`
        SELECT version, name, applied_at
        FROM schema_migrations
        WHERE version::int >= 28
        ORDER BY version::int DESC;
      `;

      console.log('\n📋 Recent Migrations (028+):');
      console.log('-'.repeat(70));
      if (migrations.length > 0) {
        migrations.forEach((m: any) => {
          const date = new Date(m.applied_at).toLocaleString();
          console.log(`  ✅ ${m.version} - ${m.name || 'Unnamed'}`);
          console.log(`     Applied: ${date}`);
        });
      } else {
        console.log('  ⚠️  No migrations 028+ found');
      }
    } else {
      console.log('  ⚠️  schema_migrations table does not exist');
    }

    // Check if reschedule columns exist
    console.log('\n🔍 Checking Reschedule Schema:');
    console.log('-'.repeat(70));

    const columnCheck = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND table_name = 'repcard_appointments'
        AND column_name IN ('is_reschedule', 'reschedule_count', 'original_appointment_id', 'reschedule_reason')
      ORDER BY column_name;
    `;

    if (columnCheck.length > 0) {
      console.log('  ✅ Reschedule tracking columns exist:');
      columnCheck.forEach((col: any) => {
        console.log(`     - ${col.column_name} (${col.data_type})`);
      });
    } else {
      console.log('  ❌ Reschedule tracking columns NOT found');
      console.log('  → Migration 028 needs to be applied');
    }

    // Check appointment chains view
    const viewCheck = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.views
        WHERE table_schema = 'public'
        AND table_name = 'repcard_appointment_chains'
      );
    `;

    const viewExists = (viewCheck[0] as any)?.exists;
    if (viewExists) {
      console.log('  ✅ repcard_appointment_chains view exists');
    } else {
      console.log('  ❌ repcard_appointment_chains view NOT found');
    }

    // Check current reschedule data
    console.log('\n📊 Current Reschedule Data:');
    console.log('-'.repeat(70));

    const dataCheck = await sql`
      SELECT
        COUNT(*) as total_appointments,
        COUNT(*) FILTER (WHERE is_reschedule = TRUE) as reschedules,
        COUNT(DISTINCT customer_id) FILTER (WHERE is_reschedule = TRUE) as customers_with_reschedules
      FROM repcard_appointments;
    `;

    const data = dataCheck[0] as any;
    console.log(`  Total appointments: ${data.total_appointments}`);
    console.log(`  Reschedules: ${data.reschedules}`);
    console.log(`  Customers with reschedules: ${data.customers_with_reschedules}`);

    if (parseInt(data.reschedules) === 0 && parseInt(data.total_appointments) > 0) {
      console.log('\n  ⚠️  No reschedule data detected');
      console.log('  → Run full sync to populate reschedule tracking');
    } else if (parseInt(data.reschedules) > 0) {
      const rate = (parseInt(data.reschedules) / parseInt(data.total_appointments)) * 100;
      console.log(`\n  📈 Reschedule rate: ${rate.toFixed(1)}%`);
    }

    console.log('\n' + '='.repeat(70));
    console.log('\n✅ Migration check complete!');

  } catch (error) {
    console.error('\n❌ Error:', error);
    throw error;
  }
}

main()
  .then(() => {
    console.log('\n✅ Check complete\n');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Fatal error:', error);
    process.exit(1);
  });
