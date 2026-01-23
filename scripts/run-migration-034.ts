#!/usr/bin/env tsx
/**
 * Run Migration 034: Fix metric audit foreign key constraint
 * 
 * This migration changes repcard_metric_audit to use repcard_appointment_id (TEXT)
 * instead of UUID id to avoid foreign key constraint violations in BEFORE INSERT triggers.
 * 
 * Usage:
 *   npx tsx scripts/run-migration-034.ts
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
    console.log('🚀 Running Migration 034: Fix metric audit foreign key constraint\n');

    // Read migration file
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations/034_fix_metric_audit_fk_use_repcard_id.sql');
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log('📄 Migration file loaded\n');
    console.log('Executing migration...\n');

    // Execute migration
    await sql.unsafe(migrationSQL);

    console.log('✅ Migration 034 completed successfully!\n');

    // Verify migration
    console.log('🔍 Verifying migration...\n');
    
    const checkResult = await sql`
      SELECT 
        column_name,
        data_type,
        is_nullable
      FROM information_schema.columns
      WHERE table_name = 'repcard_metric_audit'
        AND column_name IN ('appointment_id', 'repcard_appointment_id')
      ORDER BY column_name
    `;
    const columns = Array.isArray(checkResult) ? checkResult : (checkResult?.rows || []);
    
    console.log('Columns in repcard_metric_audit:');
    columns.forEach((col: any) => {
      console.log(`  - ${col.column_name}: ${col.data_type} (nullable: ${col.is_nullable})`);
    });

    const constraintResult = await sql`
      SELECT 
        conname as constraint_name,
        pg_get_constraintdef(oid) as constraint_definition
      FROM pg_constraint
      WHERE conrelid = 'repcard_metric_audit'::regclass
        AND contype = 'f'
    `;
    const constraints = Array.isArray(constraintResult) ? constraintResult : (constraintResult?.rows || []);
    
    console.log('\nForeign key constraints:');
    constraints.forEach((constraint: any) => {
      console.log(`  - ${constraint.constraint_name}: ${constraint.constraint_definition}`);
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
