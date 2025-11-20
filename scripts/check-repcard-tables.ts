#!/usr/bin/env tsx
/**
 * Check RepCard Tables in Database
 * 
 * This script checks if all RepCard tables exist in the database
 * and shows their record counts.
 * 
 * Usage: npx tsx scripts/check-repcard-tables.ts
 */

// Load environment variables from .env.local
import { config } from 'dotenv';
import { resolve } from 'path';

// Try to load .env.local first, then .env
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL for @vercel/postgres
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

import { sql } from '@/lib/db/client';

async function checkRepCardTables() {
  console.log('🔍 Checking RepCard Tables in Database\n');
  console.log('='.repeat(70));
  console.log();

  // Expected RepCard tables
  const expectedTables = [
    'repcard_users',
    'repcard_offices',
    'repcard_customers',
    'repcard_appointments',
    'repcard_status_logs',
    'repcard_customer_attachments',
    'repcard_appointment_attachments',
    'repcard_customer_notes',
    'repcard_customer_statuses',
    'repcard_calendars',
    'repcard_custom_fields',
    'repcard_leaderboard_snapshots',
    'repcard_teams',
    'repcard_metric_definitions',
    'repcard_leaderboard_config',
    'repcard_analytics_config'
  ];

  console.log('📊 Table Status:');
  console.log('-'.repeat(70));

  const tableStatus: Array<{ name: string; exists: boolean; count?: number }> = [];

  for (const tableName of expectedTables) {
    try {
      // Check if table exists using direct query
      const tableCheck = await (sql as any).query(`
        SELECT EXISTS (
          SELECT 1 
          FROM information_schema.tables 
          WHERE table_schema = 'public' 
          AND table_name = $1
        ) as exists;
      `, [tableName]);
      
      const exists = tableCheck.rows?.[0]?.exists || false;
      
      if (exists) {
        // Get record count
        try {
          const countResult = await (sql as any).query(`SELECT COUNT(*) as count FROM ${tableName}`);
          const count = countResult.rows?.[0]?.count || 0;
          tableStatus.push({ name: tableName, exists: true, count: Number(count) });
          console.log(`   ✅ ${tableName.padEnd(40)} ${String(count).padStart(10)} records`);
        } catch (error) {
          tableStatus.push({ name: tableName, exists: true });
          console.log(`   ✅ ${tableName.padEnd(40)} (exists, count failed)`);
        }
      } else {
        tableStatus.push({ name: tableName, exists: false });
        console.log(`   ❌ ${tableName.padEnd(40)} MISSING`);
      }
    } catch (error) {
      tableStatus.push({ name: tableName, exists: false });
      console.log(`   ❌ ${tableName.padEnd(40)} ERROR: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  console.log();
  console.log('='.repeat(70));
  console.log();

  // Summary
  const existingTables = tableStatus.filter(t => t.exists);
  const missingTables = tableStatus.filter(t => !t.exists);
  const totalRecords = tableStatus
    .filter(t => t.exists && t.count !== undefined)
    .reduce((sum, t) => sum + (t.count || 0), 0);

  console.log('📊 Summary:');
  console.log('-'.repeat(70));
  console.log(`   Total Expected Tables: ${expectedTables.length}`);
  console.log(`   ✅ Existing Tables: ${existingTables.length}`);
  console.log(`   ❌ Missing Tables: ${missingTables.length}`);
  console.log(`   📈 Total Records: ${totalRecords.toLocaleString()}`);
  console.log();

  if (missingTables.length > 0) {
    console.log('⚠️  Missing Tables:');
    missingTables.forEach(table => {
      console.log(`   - ${table.name}`);
    });
    console.log();
    console.log('💡 Action Required: Run migrations');
    console.log('   → npm run migrate');
    console.log('   → Or: ./scripts/run-migrations-production.sh');
    console.log();
  }

  // Check core tables (required for basic functionality)
  const coreTables = ['repcard_users', 'repcard_customers', 'repcard_appointments', 'repcard_status_logs'];
  const missingCoreTables = coreTables.filter(name => 
    !tableStatus.find(t => t.name === name && t.exists)
  );

  if (missingCoreTables.length > 0) {
    console.log('🚨 CRITICAL: Core tables missing!');
    console.log('   These tables are required for RepCard to work:');
    missingCoreTables.forEach(table => {
      console.log(`   - ${table}`);
    });
    console.log();
  } else {
    console.log('✅ All core tables exist!');
    console.log();
  }

  // Check data counts
  if (existingTables.length > 0) {
    console.log('📊 Data Counts:');
    console.log('-'.repeat(70));
    
    const dataTables = ['repcard_users', 'repcard_customers', 'repcard_appointments', 'repcard_status_logs'];
    for (const tableName of dataTables) {
      const status = tableStatus.find(t => t.name === tableName);
      if (status?.exists) {
        const count = status.count || 0;
        if (count === 0) {
          console.log(`   ⚠️  ${tableName.padEnd(40)} ${String(count).padStart(10)} (no data - sync needed)`);
        } else {
          console.log(`   ✅ ${tableName.padEnd(40)} ${String(count).padStart(10)} records`);
        }
      }
    }
    console.log();
  }

  // Check user linking
  try {
    const userLinking = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE repcard_user_id IS NOT NULL) as linked,
        COUNT(*) as total
      FROM users;
    `;
    const result = Array.from(userLinking)[0];
    const linked = Number(result?.linked || 0);
    const total = Number(result?.total || 0);
    const percentage = total > 0 ? Math.round((linked / total) * 100) : 0;

    console.log('👥 User Linking Status:');
    console.log('-'.repeat(70));
    console.log(`   Linked Users: ${linked} / ${total} (${percentage}%)`);
    
    if (linked === 0) {
      console.log('   ⚠️  No users linked to RepCard!');
      console.log('   → Run: psql "$DATABASE_URL" -f scripts/link-users-to-repcard.sql');
    } else {
      console.log('   ✅ Users are linked');
    }
    console.log();
  } catch (error) {
    console.log('   ⚠️  Could not check user linking status');
    console.log();
  }

  console.log('='.repeat(70));
  console.log();
  
  if (missingTables.length === 0 && totalRecords > 0) {
    console.log('✅ RepCard tables are set up correctly!');
    console.log('   If metrics still not showing, check:');
    console.log('   1. REPCARD_API_KEY is set in Vercel production');
    console.log('   2. Data has been synced (run sync via admin dashboard)');
    console.log('   3. Users are linked to RepCard');
  } else if (missingTables.length === 0 && totalRecords === 0) {
    console.log('⚠️  Tables exist but no data synced yet');
    console.log('   → Run sync via admin dashboard: /admin/repcard-sync');
  } else {
    console.log('❌ Issues found - see details above');
  }
  console.log();
}

// Run check
checkRepCardTables()
  .catch((error) => {
    console.error('\n❌ Check failed with error:');
    console.error(error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });

