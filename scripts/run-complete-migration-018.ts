#!/usr/bin/env tsx
/**
 * Run Migration 018 Complete - Normalize Remaining User IDs to INTEGER
 * 
 * This completes the partial migration 018 by converting:
 * - repcard_customers.setter_user_id: TEXT → INTEGER
 * - repcard_appointments.setter_user_id: TEXT → INTEGER
 * - repcard_appointments.closer_user_id: TEXT → INTEGER
 * 
 * Usage: DATABASE_URL="your-db-url" npx tsx scripts/run-complete-migration-018.ts
 */

// Load environment variables
import { config } from 'dotenv';
import { resolve } from 'path';
import { readFileSync } from 'fs';

// Load .env.local first, then .env
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

// Import after env vars are set
import { sql } from '@/lib/db/client';

async function runMigration() {
  console.log('🚀 Running Migration 018 Complete...\n');

  try {
    // Read the migration file
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations/018_complete_normalize_user_ids.sql');
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    console.log('📄 Migration file loaded');
    console.log('🔧 Executing migration...\n');

    // Execute the migration using sql.query for raw SQL
    await (sql as any).query(migrationSQL);

    console.log('✅ Migration completed successfully!\n');

    // Verify the changes
    console.log('🔍 Verifying schema changes...\n');

    const verification = await sql`
      SELECT 
        table_name,
        column_name,
        data_type
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND table_name IN ('repcard_customers', 'repcard_appointments')
        AND column_name IN ('setter_user_id', 'closer_user_id')
      ORDER BY table_name, column_name
    `;

    console.log('📊 Current schema:');
    for (const row of verification) {
      const status = row.data_type === 'integer' ? '✅' : '❌';
      console.log(`  ${status} ${row.table_name}.${row.column_name}: ${row.data_type}`);
    }

    console.log('\n✅ Migration verification complete!');
    console.log('\n📋 Next steps:');
    console.log('  1. Remove ::text casts from queries for better performance');
    console.log('  2. Test RepCard leaderboard queries');
    console.log('  3. Monitor query performance improvements\n');

  } catch (error) {
    console.error('❌ Migration failed:', error);
    process.exit(1);
  }
}

runMigration()
  .catch((error) => {
    console.error('\n❌ Migration failed:');
    console.error(error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });

