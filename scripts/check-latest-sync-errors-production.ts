/**
 * Check Latest Sync Errors in Production
 */

import { sql } from '@/lib/db/client';
import { config } from 'dotenv';
import { resolve } from 'path';

config({ path: resolve(process.cwd(), '.env.production') });
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);

async function main() {
  console.log('🔍 Checking Latest Sync Errors in Production\n');
  console.log('='.repeat(70));

  try {
    // Get latest failed syncs
    const failedSyncs = getRows(await sql`
      SELECT 
        entity_type,
        status,
        records_fetched,
        records_failed,
        error_message,
        error_details,
        started_at,
        completed_at
      FROM repcard_sync_log
      WHERE entity_type IN ('users', 'offices')
        AND status IN ('failed', 'completed')
        AND records_failed > 0
      ORDER BY started_at DESC
      LIMIT 10
    `);

    if (failedSyncs.length === 0) {
      console.log('No failed syncs found');
      return;
    }

    console.log(`Found ${failedSyncs.length} failed syncs:\n`);

    failedSyncs.forEach((sync: any, idx: number) => {
      console.log(`${idx + 1}. ${sync.entity_type} (${sync.status})`);
      console.log(`   Started: ${sync.started_at}`);
      console.log(`   Fetched: ${sync.records_fetched}, Failed: ${sync.records_failed}`);
      console.log(`   Error Message: ${sync.error_message || 'N/A'}`);
      if (sync.error_details) {
        console.log(`   Error Details: ${JSON.stringify(sync.error_details, null, 2)}`);
      }
      console.log('');
    });

  } catch (error) {
    console.error('❌ Error:', error);
    if (error instanceof Error) {
      console.error('Error message:', error.message);
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

