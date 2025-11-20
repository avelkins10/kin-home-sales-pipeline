/**
 * Check Production Database Record Counts
 * Always uses production DATABASE_URL
 */

import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

// Load production environment variables
config({ path: resolve(process.cwd(), '.env.production') });
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL for @vercel/postgres
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

async function main() {
  console.log('🔍 Checking Production Database Record Counts\n');
  console.log('='.repeat(70));
  console.log(`DATABASE_URL: ${process.env.DATABASE_URL ? 'SET' : 'NOT SET'}`);
  console.log(`POSTGRES_URL: ${process.env.POSTGRES_URL ? 'SET' : 'NOT SET'}`);
  console.log('='.repeat(70));
  console.log('');

  try {
    // Check each table
    const customerCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_customers`)[0]?.count || 0;
    const appointmentCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_appointments`)[0]?.count || 0;
    const userCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_users`)[0]?.count || 0;
    const officeCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_offices`)[0]?.count || 0;
    const statusLogCount = getRows(await sql`SELECT COUNT(*) as count FROM repcard_status_logs`)[0]?.count || 0;

    console.log('📊 Production Database Record Counts:');
    console.log(`  Customers: ${customerCount}`);
    console.log(`  Appointments: ${appointmentCount}`);
    console.log(`  Users: ${userCount}`);
    console.log(`  Offices: ${officeCount}`);
    console.log(`  Status Logs: ${statusLogCount}`);
    console.log('');

    // Check latest sync logs
    const latestSyncs = getRows(await sql`
      SELECT DISTINCT ON (entity_type)
        entity_type,
        status,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        started_at,
        completed_at
      FROM repcard_sync_log
      ORDER BY entity_type, started_at DESC
    `);

    if (latestSyncs.length > 0) {
      console.log('📋 Latest Sync Status by Entity:');
      latestSyncs.forEach((sync: any) => {
        console.log(`  ${sync.entity_type}: ${sync.status}`);
        console.log(`    Fetched: ${sync.records_fetched}, Inserted: ${sync.records_inserted}, Updated: ${sync.records_updated}, Failed: ${sync.records_failed}`);
        console.log(`    Started: ${sync.started_at}, Completed: ${sync.completed_at || 'N/A'}`);
        console.log('');
      });
    } else {
      console.log('⚠️  No sync logs found');
    }

  } catch (error) {
    console.error('❌ Error querying production database:', error);
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

