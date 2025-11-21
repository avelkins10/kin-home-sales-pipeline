/**
 * Check RepCard Sync History
 */

import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL for @vercel/postgres
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

async function main() {
  console.log('🔍 Checking RepCard Sync History\n');
  console.log('='.repeat(70));

  try {
    // Check user sync history
    const userSyncs = getRows(await sql`
      SELECT
        id,
        sync_type,
        status,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        started_at,
        completed_at
      FROM repcard_sync_log
      WHERE entity_type = 'users'
      ORDER BY started_at DESC
      LIMIT 10
    `);

    console.log('\n📋 User Sync History (last 10):');
    userSyncs.forEach((sync: any) => {
      console.log(`\n  Sync ID: ${sync.id}`);
      console.log(`    Type: ${sync.sync_type}, Status: ${sync.status}`);
      console.log(`    Fetched: ${sync.records_fetched}, Inserted: ${sync.records_inserted}, Updated: ${sync.records_updated}, Failed: ${sync.records_failed}`);
      console.log(`    Started: ${sync.started_at}`);
      console.log(`    Completed: ${sync.completed_at || 'N/A'}`);
    });

    // Check total unique users in database
    const userStats = getRows(await sql`
      SELECT
        COUNT(*) as total,
        COUNT(CASE WHEN status = 1 THEN 1 END) as active,
        COUNT(CASE WHEN status = 0 THEN 1 END) as inactive,
        COUNT(CASE WHEN role IS NOT NULL THEN 1 END) as with_role,
        COUNT(CASE WHEN role IS NULL THEN 1 END) as without_role
      FROM repcard_users
    `)[0];

    console.log('\n\n📊 Current RepCard Users in Database:');
    console.log(`  Total users: ${userStats?.total || 0}`);
    console.log(`  Active (status=1): ${userStats?.active || 0}`);
    console.log(`  Inactive (status=0): ${userStats?.inactive || 0}`);
    console.log(`  With role: ${userStats?.with_role || 0}`);
    console.log(`  Without role: ${userStats?.without_role || 0}`);

    // Check when data was last synced
    const lastSync = getRows(await sql`
      SELECT MAX(synced_at) as last_synced
      FROM repcard_users
    `)[0];

    console.log(`\n  Last user synced: ${lastSync?.last_synced || 'N/A'}`);

    // Check app users table
    const appUserStats = getRows(await sql`
      SELECT
        COUNT(*) as total,
        COUNT(CASE WHEN repcard_user_id IS NOT NULL THEN 1 END) as linked_to_repcard,
        COUNT(CASE WHEN role = 'setter' THEN 1 END) as setters,
        COUNT(CASE WHEN role = 'closer' THEN 1 END) as closers
      FROM users
    `)[0];

    console.log('\n\n📊 App Users Table:');
    console.log(`  Total users: ${appUserStats?.total || 0}`);
    console.log(`  Linked to RepCard: ${appUserStats?.linked_to_repcard || 0}`);
    console.log(`  Setters: ${appUserStats?.setters || 0}`);
    console.log(`  Closers: ${appUserStats?.closers || 0}`);

  } catch (error) {
    console.error('❌ Error querying database:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
      console.error('Stack:', error.stack);
    }
    process.exit(1);
  }
}

main()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
