#!/usr/bin/env tsx
/**
 * Check Schema and Run Sync
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function checkAndSync() {
  console.log('🔍 Checking Reschedule Schema & Triggering Sync\n');
  console.log('='.repeat(70));

  try {
    // Check if reschedule columns exist
    console.log('\n📋 Checking reschedule tracking schema...');

    const result = await sql`
      SELECT column_name
      FROM information_schema.columns
      WHERE table_schema = 'public'
        AND table_name = 'repcard_appointments'
        AND column_name IN ('is_reschedule', 'reschedule_count', 'original_appointment_id')
      ORDER BY column_name;
    `;

    const columns = Array.isArray(result) ? result : Array.from(result);

    if (columns.length >= 3) {
      console.log('  ✅ Reschedule tracking schema exists');
      console.log(`  Found ${columns.length} columns:`, columns.map((c: any) => c.column_name).join(', '));
    } else {
      console.log('  ⚠️  Reschedule tracking schema incomplete');
      console.log('  Migration 028 needs to be applied');
      return;
    }

    // Check current data
    console.log('\n📊 Checking current reschedule data...');
    const stats = await sql`
      SELECT
        COUNT(*)::int as total,
        COUNT(CASE WHEN is_reschedule = TRUE THEN 1 END)::int as reschedules
      FROM repcard_appointments;
    `;

    const statsArray = Array.isArray(stats) ? stats : Array.from(stats);
    const data = statsArray[0] as any;

    if (data) {
      console.log(`  Total appointments: ${data.total}`);
      console.log(`  Reschedules tracked: ${data.reschedules}`);

      if (data.reschedules === 0 && data.total > 0) {
        console.log('\n  ⚠️  No reschedule data found - sync needed');
      } else if (data.reschedules > 0) {
        const rate = ((data.reschedules / data.total) * 100).toFixed(1);
        console.log(`  📈 Current reschedule rate: ${rate}%`);
      }
    }

    console.log('\n' + '='.repeat(70));
    console.log('✅ Schema check complete!');
    console.log('\n🚀 Next: Trigger manual sync or wait for automatic cron sync (every 5 min)');
    console.log('  Manual: curl -X POST "https://your-domain.com/api/admin/repcard/sync?type=full"');
    console.log('  Cron: Automatic sync runs every 5 minutes');

  } catch (error) {
    console.error('\n❌ Error:', error);
    throw error;
  }
}

checkAndSync()
  .then(() => {
    console.log('\n');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Fatal error:', error);
    process.exit(1);
  });
