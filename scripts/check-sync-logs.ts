#!/usr/bin/env tsx
/**
 * Check sync logs to see what actually happened
 */

import dotenv from 'dotenv';
import * as path from 'path';

dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function checkSyncLogs() {
  console.log('🔍 Checking Sync Logs...\n');

  const logs = await sql`
    SELECT 
      entity_type,
      sync_type,
      status,
      records_fetched,
      records_inserted,
      records_updated,
      records_failed,
      started_at,
      completed_at,
      last_record_date,
      error_message
    FROM repcard_sync_log
    ORDER BY started_at DESC
    LIMIT 10
  `;
  
  const logArray = Array.from(logs);
  console.log(`Found ${logArray.length} recent sync logs:\n`);
  
  logArray.forEach((log: any) => {
    console.log(`${log.entity_type} (${log.sync_type}):`);
    console.log(`  Status: ${log.status}`);
    console.log(`  Fetched: ${log.records_fetched}, Inserted: ${log.records_inserted}, Updated: ${log.records_updated}, Failed: ${log.records_failed}`);
    console.log(`  Started: ${log.started_at}, Completed: ${log.completed_at}`);
    if (log.error_message) {
      console.log(`  Error: ${log.error_message.substring(0, 200)}`);
    }
    console.log('');
  });
}

checkSyncLogs().catch(console.error);

