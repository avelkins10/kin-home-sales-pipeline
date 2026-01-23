#!/usr/bin/env tsx
/**
 * Verify Migrations 034 and 035
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
  console.log('🔍 Verifying Migrations 034 and 035\n');
  console.log('='.repeat(70));

  try {
    // Check migration 034
    console.log('\n📋 Migration 034: repcard_appointment_id column');
    const check034 = await sql`
      SELECT column_name, data_type, is_nullable
      FROM information_schema.columns
      WHERE table_name = 'repcard_metric_audit'
        AND column_name = 'repcard_appointment_id'
    `;
    const cols034 = Array.isArray(check034) ? check034 : (check034?.rows || []);
    if (cols034.length > 0) {
      console.log('  ✅ Column exists:', cols034[0]);
    } else {
      console.log('  ❌ Column not found');
    }

    // Check FK constraint
    const fkCheck = await sql`
      SELECT conname, pg_get_constraintdef(oid) as definition
      FROM pg_constraint
      WHERE conrelid = 'repcard_metric_audit'::regclass
        AND conname LIKE '%repcard_appointment_id%'
    `;
    const fks = Array.isArray(fkCheck) ? fkCheck : (fkCheck?.rows || []);
    if (fks.length > 0) {
      console.log('  ✅ FK Constraint exists:', fks[0].conname);
      console.log('     Definition:', fks[0].definition);
    } else {
      console.log('  ❌ FK Constraint not found');
    }

    // Check migration 035 - appointments
    console.log('\n📋 Migration 035: repcard_appointments new fields');
    const check035Appt = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_appointments'
        AND column_name IN ('appointment_link', 'remind_at', 'remind_text', 'appointment_location', 'latitude', 'longitude', 'contact_source')
      ORDER BY column_name
    `;
    const cols035Appt = Array.isArray(check035Appt) ? check035Appt : (check035Appt?.rows || []);
    console.log(`  Found ${cols035Appt.length}/7 fields:`);
    cols035Appt.forEach((col: any) => {
      console.log(`    ✅ ${col.column_name}: ${col.data_type}`);
    });

    // Check migration 035 - customers
    console.log('\n📋 Migration 035: repcard_customers new fields');
    const check035Cust = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_customers'
        AND column_name IN ('contact_source', 'latitude', 'longitude')
      ORDER BY column_name
    `;
    const cols035Cust = Array.isArray(check035Cust) ? check035Cust : (check035Cust?.rows || []);
    console.log(`  Found ${cols035Cust.length}/3 fields:`);
    cols035Cust.forEach((col: any) => {
      console.log(`    ✅ ${col.column_name}: ${col.data_type}`);
    });

    // Check indexes
    console.log('\n📋 Migration 035: Indexes');
    const indexCheck = await sql`
      SELECT indexname, indexdef
      FROM pg_indexes
      WHERE tablename IN ('repcard_appointments', 'repcard_customers')
        AND indexname LIKE '%appointment_link%'
         OR indexname LIKE '%remind_at%'
         OR indexname LIKE '%contact_source%'
         OR indexname LIKE '%location%'
      ORDER BY tablename, indexname
    `;
    const indexes = Array.isArray(indexCheck) ? indexCheck : (indexCheck?.rows || []);
    if (indexes.length > 0) {
      console.log(`  Found ${indexes.length} indexes:`);
      indexes.forEach((idx: any) => {
        console.log(`    ✅ ${idx.indexname}`);
      });
    } else {
      console.log('  ⚠️  No new indexes found (may be partial indexes)');
    }

    console.log('\n' + '='.repeat(70));
    console.log('✅ Verification complete!');
    process.exit(0);
  } catch (error: any) {
    console.error('\n❌ Verification failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

verify();
