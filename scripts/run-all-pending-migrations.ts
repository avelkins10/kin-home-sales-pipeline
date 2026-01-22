#!/usr/bin/env tsx
/**
 * Run All Pending Migrations
 * 
 * Checks which migrations have been run and executes any pending ones.
 * Focuses on the latest migrations (031, 032, 033) that fix critical issues.
 */

import { config } from 'dotenv';
import { sql } from '@/lib/db/client';
import { readFileSync } from 'fs';
import { resolve } from 'path';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

// All migrations that should be run (in order)
const allMigrations = [
  '031_fix_48_hour_speed_calculation.sql',
  '032_repcard_event_driven_metrics.sql',
  '033_fix_repcard_metric_audit_fk.sql',
];

async function checkMigrationStatus() {
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
      // Get all applied migrations
      const migrations = await sql`
        SELECT version, name, applied_at
        FROM schema_migrations
        ORDER BY version::int DESC;
      `;

      console.log('\n📋 Applied Migrations:');
      console.log('-'.repeat(70));
      if (migrations.length > 0) {
        migrations.forEach((m: any) => {
          const date = new Date(m.applied_at).toLocaleString();
          console.log(`  ✅ ${m.version} - ${m.name || 'Unnamed'}`);
          console.log(`     Applied: ${date}`);
        });
      } else {
        console.log('  ⚠️  No migrations found in schema_migrations table');
      }
    } else {
      console.log('  ⚠️  schema_migrations table does not exist');
      console.log('  ℹ️  Migrations will still run, but tracking won\'t be recorded');
    }

    // Check for specific tables/columns that indicate migrations have run
    console.log('\n🔍 Checking Migration Indicators:');
    console.log('-'.repeat(70));

    // Check for migration 032 (event-driven metrics)
    const metricAuditCheck = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_name = 'repcard_metric_audit'
      );
    `;
    const hasMetricAudit = (metricAuditCheck[0] as any)?.exists;
    console.log(`  ${hasMetricAudit ? '✅' : '❌'} repcard_metric_audit table: ${hasMetricAudit ? 'EXISTS' : 'MISSING'}`);

    // Check for migration 033 (DEFERRABLE FK)
    if (hasMetricAudit) {
      const fkCheck = await sql`
        SELECT conname, condeferred, condeferrable
        FROM pg_constraint
        WHERE conrelid = 'repcard_metric_audit'::regclass
        AND conname = 'repcard_metric_audit_appointment_id_fkey';
      `;
      const fk = fkCheck[0] as any;
      if (fk) {
        const isDeferrable = fk.condeferrable === 't' || fk.condeferred === 't';
        console.log(`  ${isDeferrable ? '✅' : '❌'} FK constraint is DEFERRABLE: ${isDeferrable ? 'YES' : 'NO'}`);
      } else {
        console.log(`  ⚠️  FK constraint not found`);
      }
    }

    // Check for migration 031 (48-hour calculation fix)
    const triggerCheck = await sql`
      SELECT tgname
      FROM pg_trigger
      WHERE tgname = 'trigger_update_appointment_metrics';
    `;
    const hasTrigger = triggerCheck.length > 0;
    console.log(`  ${hasTrigger ? '✅' : '❌'} update_appointment_metrics trigger: ${hasTrigger ? 'EXISTS' : 'MISSING'}`);

    return { tableExists, hasMetricAudit, hasTrigger };
  } catch (error) {
    console.error('❌ Error checking migration status:', error);
    throw error;
  }
}

async function runMigrations() {
  console.log('\n\n🚀 Running Pending Migrations\n');
  console.log('='.repeat(70));
  console.log();

  const results = {
    success: [] as string[],
    skipped: [] as string[],
    failed: [] as string[],
  };

  for (const migrationFile of allMigrations) {
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

    try {
      console.log(`📄 Running: ${migrationFile}`);

      // Check if file exists
      const migrationSQL = readFileSync(migrationPath, 'utf-8');

      // Execute migration using sql.query for raw SQL
      await (sql as any).query(migrationSQL);

      console.log(`   ✅ ${migrationFile} completed successfully\n`);
      results.success.push(migrationFile);
    } catch (error: any) {
      // Check if error is because object already exists
      if (
        error?.message?.includes('already exists') ||
        error?.code === '42P07' || // table exists
        error?.code === '42710' || // object exists
        error?.code === '42P16' || // invalid table definition
        error?.code === '42704'    // object does not exist (for DROP IF EXISTS)
      ) {
        console.log(`   ⚠️  ${migrationFile} - Objects already exist (skipping)\n`);
        results.skipped.push(migrationFile);
      } else {
        console.error(`   ❌ ${migrationFile} failed:`);
        console.error(`   Error code: ${error.code || 'N/A'}`);
        console.error(`   ${error instanceof Error ? error.message : String(error)}\n`);
        results.failed.push(migrationFile);
        // Continue with other migrations even if one fails
      }
    }
  }

  console.log('='.repeat(70));
  console.log('\n📊 Migration Summary:');
  console.log(`   ✅ Success: ${results.success.length}`);
  console.log(`   ⚠️  Skipped: ${results.skipped.length}`);
  console.log(`   ❌ Failed: ${results.failed.length}`);

  if (results.failed.length > 0) {
    console.log('\n❌ Some migrations failed. Please review the errors above.');
    process.exit(1);
  } else {
    console.log('\n✅ All migrations completed successfully!');
  }
}

async function main() {
  try {
    // Check current status
    await checkMigrationStatus();

    // Run pending migrations
    await runMigrations();

    // Verify final status
    console.log('\n\n🔍 Final Verification\n');
    console.log('='.repeat(70));
    await checkMigrationStatus();

    console.log('\n✅ Migration process complete!');
  } catch (error) {
    console.error('\n❌ Migration process failed:', error);
    process.exit(1);
  }
}

main();
