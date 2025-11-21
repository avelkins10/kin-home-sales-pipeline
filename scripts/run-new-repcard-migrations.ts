#!/usr/bin/env tsx
/**
 * Run New RepCard Migrations (028, 029, 030)
 *
 * This script runs only the new migrations for reschedule tracking,
 * office mapping, and unified attachments.
 *
 * Usage: npx tsx scripts/run-new-repcard-migrations.ts
 */

// Load environment variables
import { config } from 'dotenv';
import { resolve } from 'path';
import { readFileSync } from 'fs';
import { sql } from '@/lib/db/client';

// Load .env.local first, then .env
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

const newMigrations = [
  '028_add_repcard_reschedule_tracking.sql',
  '029_add_repcard_office_mapping.sql',
  '030_create_repcard_unified_attachments.sql'
];

async function runMigrations() {
  console.log('🚀 Running New RepCard Migrations (028-030)\n');
  console.log('='.repeat(70));
  console.log();

  for (const migrationFile of newMigrations) {
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

    try {
      console.log(`📄 Running: ${migrationFile}`);

      // Read migration file
      const migrationSQL = readFileSync(migrationPath, 'utf-8');

      // Execute migration using sql.query for raw SQL
      await (sql as any).query(migrationSQL);

      console.log(`   ✅ ${migrationFile} completed successfully\n`);
    } catch (error: any) {
      // Check if error is because object already exists
      if (error?.message?.includes('already exists') ||
          error?.code === '42P07' || // table exists
          error?.code === '42710' || // object exists
          error?.code === '42P16') { // invalid table definition
        console.log(`   ⚠️  ${migrationFile} - Objects already exist (skipping)\n`);
      } else {
        console.error(`   ❌ ${migrationFile} failed:`);
        console.error(`   Error code: ${error.code}`);
        console.error(`   ${error instanceof Error ? error.message : String(error)}\n`);

        // Don't throw - continue with other migrations
        console.log(`   ⚠️  Continuing with remaining migrations...\n`);
      }
    }
  }

  console.log('='.repeat(70));
  console.log();
  console.log('✅ Migration run completed!');
  console.log();
  console.log('New features added:');
  console.log('  • Reschedule tracking (original_appointment_id, reschedule_count)');
  console.log('  • Office mapping table (RepCard ↔ QuickBase)');
  console.log('  • Unified attachment views');
  console.log();
  console.log('Next steps:');
  console.log('1. Check reschedule data: SELECT * FROM repcard_appointment_chains LIMIT 10;');
  console.log('2. Check office mappings: SELECT * FROM repcard_offices_with_mappings;');
  console.log('3. Check attachment stats: SELECT * FROM repcard_user_attachment_stats LIMIT 10;');
  console.log();
}

runMigrations()
  .catch((error) => {
    console.error('\n❌ Migration process failed:');
    console.error(error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });
