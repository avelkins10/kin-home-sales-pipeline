#!/usr/bin/env tsx
/**
 * Run Migrations 034 and 035 directly
 * 
 * This script runs the migrations directly using the database client
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { readFileSync } from 'fs';
import { sql } from '@/lib/db/client';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function runMigration(fileName: string) {
  try {
    console.log(`\n🚀 Running Migration: ${fileName}\n`);
    console.log('='.repeat(70));

    // Read migration file
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', fileName);
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log('📄 Migration file loaded');
    console.log('Executing migration...\n');

    // Execute migration using sql.unsafe for raw SQL
    await (sql as any).query(migrationSQL);

    console.log(`✅ Migration ${fileName} completed successfully!\n`);
    return true;
  } catch (error: any) {
    // Check if error is because object already exists (safe to skip)
    if (
      error?.message?.includes('already exists') ||
      error?.code === '42P07' || // table exists
      error?.code === '42710' || // object exists
      error?.code === '42P16' || // invalid table definition
      error?.code === '42704' || // object does not exist (for DROP IF EXISTS)
      error?.message?.includes('does not exist') // DROP IF EXISTS
    ) {
      console.log(`⚠️  ${fileName} - Already applied or safe to skip`);
      return true;
    } else {
      console.error(`❌ Migration ${fileName} failed:`, error.message);
      console.error(error.stack);
      return false;
    }
  }
}

async function verifyMigrations() {
  console.log('\n🔍 Verifying Migrations\n');
  console.log('='.repeat(70));

  try {
    // Check migration 034: repcard_appointment_id column
    const check034 = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_metric_audit'
        AND column_name = 'repcard_appointment_id'
    `;
    const cols034 = Array.isArray(check034) ? check034 : (check034?.rows || []);
    console.log('Migration 034 (repcard_appointment_id):', cols034.length > 0 ? '✅ Applied' : '❌ Not applied');

    // Check migration 035: new columns in appointments
    const check035Appt = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_appointments'
        AND column_name IN ('appointment_link', 'remind_at', 'contact_source')
      ORDER BY column_name
    `;
    const cols035Appt = Array.isArray(check035Appt) ? check035Appt : (check035Appt?.rows || []);
    console.log('Migration 035 (appointments fields):', cols035Appt.length >= 3 ? '✅ Applied' : '❌ Not applied');

    // Check migration 035: new columns in customers
    const check035Cust = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_customers'
        AND column_name IN ('contact_source', 'latitude', 'longitude')
      ORDER BY column_name
    `;
    const cols035Cust = Array.isArray(check035Cust) ? check035Cust : (check035Cust?.rows || []);
    console.log('Migration 035 (customers fields):', cols035Cust.length >= 3 ? '✅ Applied' : '❌ Not applied');

    console.log('\n✅ Verification complete!');
  } catch (error: any) {
    console.error('❌ Verification failed:', error.message);
  }
}

async function main() {
  try {
    console.log('🚀 Running RepCard Migrations 034 and 035\n');
    console.log('='.repeat(70));

    const migrations = [
      '034_fix_metric_audit_fk_use_repcard_id.sql',
      '035_add_useful_webhook_fields.sql'
    ];

    let allSuccess = true;
    for (const migration of migrations) {
      const success = await runMigration(migration);
      if (!success) {
        allSuccess = false;
      }
    }

    // Verify migrations
    await verifyMigrations();

    if (allSuccess) {
      console.log('\n✅ All migrations completed successfully!');
      process.exit(0);
    } else {
      console.log('\n⚠️  Some migrations had issues. Check logs above.');
      process.exit(1);
    }
  } catch (error: any) {
    console.error('\n❌ Fatal error:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

main();
