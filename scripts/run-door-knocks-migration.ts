#!/usr/bin/env tsx
/**
 * Run Door Knocks Migration (036)
 * Creates repcard_door_knocks table for tracking door knock events
 */

import { config } from 'dotenv';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function runMigration() {
  try {
    console.log('\n🚀 Running Door Knocks Migration (036)\n');
    console.log('='.repeat(70));
    console.log();

    const migrationFile = '036_create_repcard_door_knocks_table.sql';
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

    console.log(`📄 Running: ${migrationFile}`);

    // Check if file exists
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    // Execute migration using sql.query for raw SQL
    await (sql as any).query(migrationSQL);

    console.log(`   ✅ ${migrationFile} completed successfully\n`);
    console.log('='.repeat(70));
    console.log();
    console.log('✅ Migration completed!');
    console.log();
    console.log('Next steps:');
    console.log('1. Door knocks will be populated from webhooks automatically');
    console.log('2. Door knocks will also be extracted from customer data during sync');
    console.log('3. Dashboard queries now use door_knocks table for accurate metrics');
    console.log();
  } catch (error: any) {
    // Check if error is because table already exists
    if (
      error?.message?.includes('already exists') ||
      error?.code === '42P07' ||
      error?.code === '42710'
    ) {
      console.log(`   ⚠️  Migration - Table already exists (skipping)\n`);
      console.log('   This is safe to ignore if the table was already created.\n');
    } else {
      console.error(`   ❌ Migration failed:`);
      console.error(`   ${error instanceof Error ? error.message : String(error)}\n`);
      throw error;
    }
  } finally {
    process.exit(0);
  }
}

runMigration().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});
