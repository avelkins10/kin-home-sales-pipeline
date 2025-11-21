#!/usr/bin/env tsx
/**
 * Run Migration 028: Add RepCard Reschedule Tracking
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { readFileSync } from 'fs';
import { sql } from '@/lib/db/client';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function runMigration() {
  console.log('🚀 Running Migration 028: Add RepCard Reschedule Tracking\n');
  console.log('='.repeat(70));

  const migrationFile = '028_add_repcard_reschedule_tracking.sql';
  const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

  try {
    console.log(`\n📄 Reading: ${migrationFile}`);
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log('📤 Executing migration...\n');

    // Execute migration
    await (sql as any).query(migrationSQL);

    console.log('✅ Migration 028 completed successfully!\n');

    // Verify columns were added
    console.log('🔍 Verifying schema changes...');
    const columnCheck = await sql`
      SELECT column_name, data_type, is_nullable
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND table_name = 'repcard_appointments'
        AND column_name IN ('is_reschedule', 'reschedule_count', 'original_appointment_id', 'reschedule_reason')
      ORDER BY column_name;
    `;

    if (columnCheck.length > 0) {
      console.log('  ✅ Columns added:');
      columnCheck.forEach((col: any) => {
        console.log(`     - ${col.column_name} (${col.data_type}, nullable: ${col.is_nullable})`);
      });
    } else {
      console.log('  ⚠️  Could not verify columns');
    }

    // Check view
    const viewCheck = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.views
        WHERE table_schema = 'public'
        AND table_name = 'repcard_appointment_chains'
      ) as exists;
    `;

    if ((viewCheck[0] as any)?.exists) {
      console.log('  ✅ repcard_appointment_chains view created');
    } else {
      console.log('  ⚠️  Could not verify view');
    }

    // Check current data
    console.log('\n📊 Current appointment data:');
    const stats = await sql`
      SELECT
        COUNT(*) as total,
        COUNT(*) FILTER (WHERE is_reschedule = TRUE) as reschedules
      FROM repcard_appointments;
    `;

    const data = stats[0] as any;
    console.log(`  Total appointments: ${data.total}`);
    console.log(`  Currently marked as reschedules: ${data.reschedules}`);

    console.log('\n' + '='.repeat(70));
    console.log('✅ Migration 028 complete!');
    console.log('\nNext step: Run full sync to populate reschedule data');
    console.log('  → curl -X POST "https://your-domain.com/api/admin/repcard/sync?type=full"');

  } catch (error: any) {
    if (error?.message?.includes('already exists') || error?.code === '42P07') {
      console.log('⚠️  Columns already exist - migration may have been applied');
    } else {
      console.error('❌ Migration failed:');
      console.error(error instanceof Error ? error.message : String(error));
      throw error;
    }
  }
}

runMigration()
  .then(() => {
    console.log('\n');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Fatal error:', error);
    process.exit(1);
  });
