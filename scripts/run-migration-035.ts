#!/usr/bin/env tsx
/**
 * Run Migration 035: Add useful webhook fields
 * 
 * This migration extracts useful fields from webhook payloads (raw_data) into dedicated columns:
 * - appointment_link, remind_at, remind_text, appointment_location, latitude, longitude, contact_source
 * 
 * Usage:
 *   npx tsx scripts/run-migration-035.ts
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

async function runMigration() {
  try {
    console.log('🚀 Running Migration 035: Add useful webhook fields\n');

    // Read migration file
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations/035_add_useful_webhook_fields.sql');
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log('📄 Migration file loaded\n');
    console.log('Executing migration...\n');

    // Execute migration
    await sql.unsafe(migrationSQL);

    console.log('✅ Migration 035 completed successfully!\n');

    // Verify migration
    console.log('🔍 Verifying migration...\n');
    
    const appointmentColumns = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_appointments'
        AND column_name IN ('appointment_link', 'remind_at', 'remind_text', 'appointment_location', 'latitude', 'longitude', 'contact_source')
      ORDER BY column_name
    `;
    const apptCols = Array.isArray(appointmentColumns) ? appointmentColumns : (appointmentColumns?.rows || []);
    
    console.log('New columns in repcard_appointments:');
    apptCols.forEach((col: any) => {
      console.log(`  ✅ ${col.column_name}: ${col.data_type}`);
    });

    const customerColumns = await sql`
      SELECT column_name, data_type
      FROM information_schema.columns
      WHERE table_name = 'repcard_customers'
        AND column_name IN ('contact_source', 'latitude', 'longitude')
      ORDER BY column_name
    `;
    const custCols = Array.isArray(customerColumns) ? customerColumns : (customerColumns?.rows || []);
    
    console.log('\nNew columns in repcard_customers:');
    custCols.forEach((col: any) => {
      console.log(`  ✅ ${col.column_name}: ${col.data_type}`);
    });

    console.log('\n✅ Verification complete!');
    process.exit(0);
  } catch (error: any) {
    console.error('\n❌ Migration failed:', error.message);
    console.error(error.stack);
    process.exit(1);
  }
}

runMigration();
