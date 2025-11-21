#!/usr/bin/env tsx
/**
 * Test Database Connection
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function testConnection() {
  console.log('🔍 Testing Database Connection\n');
  console.log('='.repeat(70));

  try {
    console.log('Database URL:', process.env.POSTGRES_URL ? 'Set' : 'NOT SET');
    console.log('DATABASE_URL:', process.env.DATABASE_URL ? 'Set' : 'NOT SET');

    // Test query
    console.log('\n📊 Running test query...');
    const result = await sql`SELECT current_database() as db, version() as pg_version;`;
    const data = result[0] as any;

    console.log(`  ✅ Connected to: ${data.db}`);
    console.log(`  PostgreSQL version: ${data.pg_version.split(',')[0]}`);

    // Check if repcard_appointments exists
    console.log('\n📋 Checking repcard_appointments table...');
    const tableCheck = await sql`
      SELECT EXISTS (
        SELECT FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_name = 'repcard_appointments'
      ) as exists;
    `;

    if ((tableCheck[0] as any)?.exists) {
      console.log('  ✅ Table exists');

      // Get column count
      const colCount = await sql`
        SELECT COUNT(*)::int as count
        FROM information_schema.columns
        WHERE table_schema = 'public'
        AND table_name = 'repcard_appointments';
      `;

      console.log(`  Total columns: ${(colCount[0] as any)?.count || 0}`);

      // List all columns
      const cols = await sql`
        SELECT column_name
        FROM information_schema.columns
        WHERE table_schema = 'public'
        AND table_name = 'repcard_appointments'
        ORDER BY ordinal_position;
      `;

      console.log('\n  Columns:');
      cols.forEach((col: any) => {
        const highlight = ['is_reschedule', 'reschedule_count', 'original_appointment_id', 'reschedule_reason'].includes(col.column_name) ? '  🎯' : '    ';
        console.log(`${highlight} ${col.column_name}`);
      });

    } else {
      console.log('  ❌ Table does NOT exist');
    }

    console.log('\n' + '='.repeat(70));
    console.log('✅ Connection test complete!');

  } catch (error) {
    console.error('\n❌ Connection error:', error);
    throw error;
  }
}

testConnection()
  .then(() => {
    console.log('\n');
    process.exit(0);
  })
  .catch((error) => {
    console.error('\n❌ Fatal error:', error);
    process.exit(1);
  });
