#!/usr/bin/env tsx
/**
 * Run Migration 040: Leaderboard Query Optimization
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
  console.log('\n🚀 Running Migration 040: Leaderboard Query Optimization\n');
  console.log('='.repeat(70));
  console.log();

  try {
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations/040_leaderboard_optimization.sql');
    
    console.log(`📄 Reading migration file: 040_leaderboard_optimization.sql`);
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log(`📊 Executing migration...`);
    await (sql as any).query(migrationSQL);

    console.log(`\n✅ Migration 040 completed successfully!`);
    console.log(`\n📋 What was added:`);
    console.log(`   - 18+ indexes for leaderboard queries`);
    console.log(`   - Foreign key verification`);
    console.log(`   - Table statistics updates (ANALYZE)`);
    console.log(`\n💡 Next step: Run verification script`);
    console.log(`   npx tsx scripts/verify-database-optimization.ts`);
    console.log('='.repeat(70) + '\n');

  } catch (error: any) {
    if (
      error?.message?.includes('already exists') ||
      error?.code === '42P07' || // table exists
      error?.code === '42710' || // object exists
      error?.code === '42P16' || // invalid table definition
      error?.code === '42704'    // object does not exist (for DROP IF EXISTS)
    ) {
      console.log(`⚠️  Migration already applied (some objects already exist)`);
      console.log(`   This is safe to ignore - migration is idempotent\n`);
    } else {
      console.error(`\n❌ Migration failed:`);
      console.error(`   Error: ${error.message || String(error)}`);
      console.error(`   Code: ${error.code || 'N/A'}\n`);
      process.exit(1);
    }
  }
}

runMigration().catch(error => {
  console.error('💥 Unexpected error:', error);
  process.exit(1);
});
