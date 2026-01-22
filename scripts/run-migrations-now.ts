#!/usr/bin/env tsx
/**
 * Run Migrations Now - Direct Execution
 * Runs all pending migrations (031, 032, 033) directly
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { readFileSync } from 'fs';
import { sql } from '@/lib/db/client';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

const MIGRATIONS = [
  '031_fix_48_hour_speed_calculation.sql',
  '032_repcard_event_driven_metrics.sql',
  '033_fix_repcard_metric_audit_fk.sql',
];

async function runMigrations() {
  console.log('🚀 Running Migrations Directly\n');
  console.log('='.repeat(70));
  console.log();

  const results = {
    success: [] as string[],
    skipped: [] as string[],
    failed: [] as Array<{ file: string; error: string }>,
  };

  for (const migrationFile of MIGRATIONS) {
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

    try {
      console.log(`📄 Running: ${migrationFile}`);

      const migrationSQL = readFileSync(migrationPath, 'utf-8');
      await (sql as any).query(migrationSQL);

      console.log(`   ✅ ${migrationFile} completed successfully\n`);
      results.success.push(migrationFile);
    } catch (error: any) {
      if (
        error?.message?.includes('already exists') ||
        error?.code === '42P07' ||
        error?.code === '42710' ||
        error?.code === '42P16' ||
        error?.code === '42704'
      ) {
        console.log(`   ⚠️  ${migrationFile} - Already applied (skipping)\n`);
        results.skipped.push(migrationFile);
      } else {
        console.error(`   ❌ ${migrationFile} failed:`);
        console.error(`   ${error.message || String(error)}\n`);
        results.failed.push({
          file: migrationFile,
          error: error.message || String(error),
        });
      }
    }
  }

  console.log('='.repeat(70));
  console.log('\n📊 Results:');
  console.log(`   ✅ Success: ${results.success.length}`);
  console.log(`   ⚠️  Skipped: ${results.skipped.length}`);
  console.log(`   ❌ Failed: ${results.failed.length}`);

  if (results.failed.length > 0) {
    console.log('\n❌ Failed migrations:');
    results.failed.forEach(f => {
      console.log(`   - ${f.file}: ${f.error}`);
    });
    process.exit(1);
  }

  console.log('\n✅ All migrations completed!');
}

runMigrations().catch(error => {
  console.error('\n❌ Fatal error:', error);
  process.exit(1);
});
