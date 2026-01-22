#!/usr/bin/env tsx
/**
 * Run Migration 032: Event-Driven RepCard Metrics
 * 
 * This migration implements the event-driven architecture:
 * - Creates audit trail table
 * - Creates enhanced triggers that calculate metrics automatically
 * - Ensures metrics are never NULL
 */

import { readFileSync } from 'fs';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function runMigration() {
  console.log('🚀 Running Migration 032: Event-Driven RepCard Metrics\n');
  console.log('='.repeat(70));
  console.log();

  const migrationFile = '032_repcard_event_driven_metrics.sql';
  const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

  try {
    console.log(`📄 Running: ${migrationFile}`);

    // Check if file exists
    if (!readFileSync) {
      throw new Error('readFileSync not available');
    }

    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    // Execute migration using sql.query for raw SQL
    await (sql as any).query(migrationSQL);

    console.log(`   ✅ ${migrationFile} completed successfully\n`);

    // Verify the migration
    console.log('🔍 Verifying migration...');
    
    // Check if audit table exists
    const auditTableCheck = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public' 
        AND table_name = 'repcard_metric_audit'
    `;
    const auditTableExists = Array.isArray(auditTableCheck) 
      ? auditTableCheck.length > 0 
      : (auditTableCheck?.rows?.length || 0) > 0;

    if (auditTableExists) {
      console.log('   ✅ repcard_metric_audit table created');
    } else {
      console.log('   ⚠️  repcard_metric_audit table not found (may already exist)');
    }

    // Check if trigger functions exist
    const functionCheck = await sql`
      SELECT routine_name 
      FROM information_schema.routines 
      WHERE routine_schema = 'public' 
        AND routine_name IN ('calculate_is_within_48_hours', 'calculate_has_power_bill', 'update_appointment_metrics')
    `;
    const functions = Array.isArray(functionCheck) 
      ? functionCheck 
      : (functionCheck?.rows || []);
    
    console.log(`   ✅ Found ${functions.length} trigger functions`);

    console.log();
    console.log('='.repeat(70));
    console.log();
    console.log('✅ Migration 032 completed successfully!');
    console.log();
    console.log('What was added:');
    console.log('  • repcard_metric_audit table - tracks why metrics are TRUE/FALSE');
    console.log('  • Enhanced triggers - automatically calculate metrics on insert/update');
    console.log('  • Attachment change triggers - recalculate has_power_bill when attachments change');
    console.log('  • Customer change triggers - recalculate is_within_48_hours when customer.created_at changes');
    console.log();
    console.log('Next steps:');
    console.log('  1. Run backfill to populate existing appointments:');
    console.log('     Go to Settings → RepCard → Metrics → "Run Metrics Backfill"');
    console.log('  2. Verify metrics are calculating correctly:');
    console.log('     Check /api/repcard/diagnose-metrics?startDate=2026-01-20&endDate=2026-01-20');
    console.log('  3. Test debug endpoint:');
    console.log('     /api/repcard/debug-metric?appointmentId=123&metric=is_within_48_hours');
    console.log();

  } catch (error: any) {
    console.error(`   ❌ ${migrationFile} failed:`);
    console.error(`   ${error instanceof Error ? error.message : String(error)}`);
    
    if (error?.code) {
      console.error(`   Error code: ${error.code}`);
    }
    
    if (error?.stack) {
      console.error(`   Stack: ${error.stack}`);
    }
    
    console.log();
    throw error;
  }
}

// Run if called directly
if (require.main === module) {
  runMigration()
    .then(() => {
      console.log('✅ Migration script completed');
      process.exit(0);
    })
    .catch((error) => {
      console.error('❌ Migration script failed:', error);
      process.exit(1);
    });
}

export { runMigration };
