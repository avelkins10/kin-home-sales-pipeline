#!/usr/bin/env tsx
import { config } from 'dotenv';
import { sql } from '../lib/db/client';

config({ path: '.env.local' });
config({ path: '.env' });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function checkStats() {
  console.log('📊 Checking Overall RepCard Stats...\n');

  try {
    // Overall stats
    const overallResult = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_pb,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb
      FROM repcard_appointments
    `;
    const overall = Array.from(overallResult)[0];
    console.log(`📋 Overall Stats (All Time):`);
    console.log(`   Total appointments: ${overall?.total || 0}`);
    console.log(`   Within 48h: ${overall?.within_48h || 0} (${overall?.total > 0 ? ((overall?.within_48h / overall?.total) * 100).toFixed(1) : 0}%)`);
    console.log(`   With power bill: ${overall?.with_pb || 0} (${overall?.total > 0 ? ((overall?.with_pb / overall?.total) * 100).toFixed(1) : 0}%)`);
    console.log(`   NULL within_48h: ${overall?.null_48h || 0}`);
    console.log(`   NULL has_power_bill: ${overall?.null_pb || 0}`);

    // Last 30 days
    const last30Result = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_pb
      FROM repcard_appointments
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const last30 = Array.from(last30Result)[0];
    console.log(`\n📅 Last 30 Days:`);
    console.log(`   Total appointments: ${last30?.total || 0}`);
    console.log(`   Within 48h: ${last30?.within_48h || 0} (${last30?.total > 0 ? ((last30?.within_48h / last30?.total) * 100).toFixed(1) : 0}%)`);
    console.log(`   With power bill: ${last30?.with_pb || 0} (${last30?.total > 0 ? ((last30?.with_pb / last30?.total) * 100).toFixed(1) : 0}%)`);

  } catch (error) {
    console.error('❌ Error:', error);
  }
}

checkStats()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
