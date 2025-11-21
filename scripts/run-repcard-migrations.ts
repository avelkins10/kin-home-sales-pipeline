#!/usr/bin/env tsx
/**
 * Run RepCard Migrations
 * 
 * This script runs all RepCard-related migrations in order.
 * 
 * Usage: npx tsx scripts/run-repcard-migrations.ts
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

const migrations = [
  '012_repcard_sync_tables.sql',
  '013_fix_repcard_id_types.sql',
  '014_repcard_comprehensive_tables.sql',
  '015_repcard_comprehensive_fields.sql',
  '016_repcard_complete_data.sql',
  '017_repcard_settings.sql',
  '017_make_repcard_users_company_id_nullable.sql',
  '018_normalize_repcard_user_ids_to_integer.sql',
  '020_add_repcard_composite_indexes.sql',
  '028_add_repcard_reschedule_tracking.sql',
  '029_add_repcard_office_mapping.sql',
  '030_create_repcard_unified_attachments.sql'
];

async function runMigrations() {
  console.log('🚀 Running RepCard Migrations\n');
  console.log('='.repeat(70));
  console.log();

  for (const migrationFile of migrations) {
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);
    
    try {
      console.log(`📄 Running: ${migrationFile}`);
      
      // Check if file exists
      const migrationSQL = readFileSync(migrationPath, 'utf-8');
      
      // Execute migration using sql.query for raw SQL
      await (sql as any).query(migrationSQL);
      
      console.log(`   ✅ ${migrationFile} completed successfully\n`);
    } catch (error: any) {
      // Check if error is because table already exists
      if (error?.message?.includes('already exists') || error?.code === '42P07') {
        console.log(`   ⚠️  ${migrationFile} - Tables already exist (skipping)\n`);
      } else {
        console.error(`   ❌ ${migrationFile} failed:`);
        console.error(`   ${error instanceof Error ? error.message : String(error)}\n`);
        throw error;
      }
    }
  }

  console.log('='.repeat(70));
  console.log();
  console.log('✅ All migrations completed!');
  console.log();
  console.log('Next steps:');
  console.log('1. Verify tables exist: npx tsx scripts/check-repcard-tables.ts');
  console.log('2. Run initial sync via admin dashboard: /admin/repcard-sync');
  console.log('3. Link users: npx tsx scripts/link-users-to-repcard.ts (or run SQL)');
  console.log();
}

runMigrations()
  .catch((error) => {
    console.error('\n❌ Migration failed:');
    console.error(error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });

