#!/usr/bin/env tsx

/**
 * Database Optimization Verification Script
 * 
 * Verifies that all indexes, foreign keys, and constraints are properly set up
 * for optimal leaderboard and integration performance.
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { sql } from '@/lib/db/client';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

interface VerificationResult {
  category: string;
  check: string;
  status: 'pass' | 'fail' | 'warning';
  message: string;
  details?: any;
}

const results: VerificationResult[] = [];

function log(message: string, type: 'info' | 'success' | 'error' | 'warning' = 'info') {
  const colors = {
    info: '\x1b[36m',
    success: '\x1b[32m',
    error: '\x1b[31m',
    warning: '\x1b[33m',
    reset: '\x1b[0m',
  };
  console.log(`${colors[type]}${message}${colors.reset}`);
}

async function verifyTables() {
  log('\n📊 Verifying Required Tables...', 'info');
  
  const requiredTables = [
    'users',
    'sessions',
    'offices',
    'repcard_users',
    'repcard_offices',
    'repcard_customers',
    'repcard_appointments',
    'repcard_door_knocks',
    'repcard_leaderboard_snapshots',
    'repcard_leaderboard_config',
    'repcard_analytics_config',
    'repcard_metric_definitions',
  ];

  try {
    // Build query with array parameter
    const tableNamesArray = `{${requiredTables.join(',')}}`;
    const tableResult = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public' 
      AND table_name = ANY(${tableNamesArray}::text[])
      ORDER BY table_name
    `;

    const existingTables = tableResult.rows.map((r: any) => r.table_name);
    const missingTables = requiredTables.filter(t => !existingTables.includes(t));

    if (missingTables.length === 0) {
      results.push({
        category: 'Tables',
        check: 'All required tables exist',
        status: 'pass',
        message: `✅ All ${requiredTables.length} required tables exist`,
      });
      log(`✅ All ${requiredTables.length} required tables exist`, 'success');
    } else {
      results.push({
        category: 'Tables',
        check: 'All required tables exist',
        status: 'fail',
        message: `❌ Missing tables: ${missingTables.join(', ')}`,
        details: { missing: missingTables, existing: existingTables },
      });
      log(`❌ Missing tables: ${missingTables.join(', ')}`, 'error');
    }
  } catch (error: any) {
    results.push({
      category: 'Tables',
      check: 'Table verification',
      status: 'fail',
      message: `Error: ${error.message}`,
    });
    log(`❌ Error verifying tables: ${error.message}`, 'error');
  }
}

async function verifyIndexes() {
  log('\n🔍 Verifying Critical Indexes...', 'info');

  const criticalIndexes = [
    // Appointment indexes
    'idx_repcard_appointments_setter_created_date',
    'idx_repcard_appointments_closer_scheduled_date',
    'idx_repcard_appointments_office_setter_created',
    'idx_repcard_appointments_office_closer_scheduled',
    'idx_repcard_appointments_status_category',
    'idx_repcard_appointments_setter_quality',
    'idx_repcard_appointments_is_reschedule',
    
    // Door knock indexes
    'idx_repcard_door_knocks_setter_date_range',
    'idx_repcard_door_knocks_office_setter_date',
    'idx_repcard_door_knocks_setter_date',
    
    // User indexes
    'idx_repcard_users_team',
    'idx_repcard_users_status_role_team',
    'idx_repcard_users_office_status_role_team',
    'idx_users_repcard_user_id_int',
    
    // Office indexes
    'idx_offices_name_quickbase_id',
    'idx_repcard_offices_repcard_id',
    
    // Customer indexes
    'idx_repcard_customers_repcard_id_created',
    'idx_repcard_customers_office_setter_created',
  ];

  try {
    // Build query with array parameter
    const indexNamesArray = `{${criticalIndexes.join(',')}}`;
    const indexResult = await sql`
      SELECT indexname 
      FROM pg_indexes 
      WHERE schemaname = 'public' 
      AND indexname = ANY(${indexNamesArray}::text[])
      ORDER BY indexname
    `;

    const existingIndexes = indexResult.rows.map((r: any) => r.indexname);
    const missingIndexes = criticalIndexes.filter(idx => !existingIndexes.includes(idx));

    if (missingIndexes.length === 0) {
      results.push({
        category: 'Indexes',
        check: 'All critical indexes exist',
        status: 'pass',
        message: `✅ All ${criticalIndexes.length} critical indexes exist`,
      });
      log(`✅ All ${criticalIndexes.length} critical indexes exist`, 'success');
    } else {
      results.push({
        category: 'Indexes',
        check: 'All critical indexes exist',
        status: 'warning',
        message: `⚠️  Missing ${missingIndexes.length} indexes: ${missingIndexes.slice(0, 5).join(', ')}${missingIndexes.length > 5 ? '...' : ''}`,
        details: { missing: missingIndexes, existing: existingIndexes },
      });
      log(`⚠️  Missing ${missingIndexes.length} indexes (run migration 040)`, 'warning');
    }

    // Count total indexes
    const totalIndexesResult = await sql`
      SELECT COUNT(*) as count
      FROM pg_indexes 
      WHERE schemaname = 'public'
    `;
    const totalIndexes = totalIndexesResult.rows[0]?.count || 0;
    log(`📊 Total indexes in database: ${totalIndexes}`, 'info');
  } catch (error: any) {
    results.push({
      category: 'Indexes',
      check: 'Index verification',
      status: 'fail',
      message: `Error: ${error.message}`,
    });
    log(`❌ Error verifying indexes: ${error.message}`, 'error');
  }
}

async function verifyForeignKeys() {
  log('\n🔗 Verifying Foreign Key Constraints...', 'info');

  const expectedFKs = [
    {
      table: 'repcard_appointments',
      constraint: 'repcard_appointments_customer_id_fkey',
      references: 'repcard_customers(id)',
    },
    {
      table: 'repcard_door_knocks',
      constraint: 'repcard_door_knocks_customer_id_fkey',
      references: 'repcard_customers(id)',
    },
  ];

  try {
    const fkResult = await sql`
      SELECT 
        tc.table_name,
        tc.constraint_name,
        kcu.column_name,
        ccu.table_name AS foreign_table_name,
        ccu.column_name AS foreign_column_name
      FROM information_schema.table_constraints AS tc
      JOIN information_schema.key_column_usage AS kcu
        ON tc.constraint_name = kcu.constraint_name
        AND tc.table_schema = kcu.table_schema
      JOIN information_schema.constraint_column_usage AS ccu
        ON ccu.constraint_name = tc.constraint_name
        AND ccu.table_schema = tc.table_schema
      WHERE tc.constraint_type = 'FOREIGN KEY'
        AND tc.table_schema = 'public'
        AND tc.table_name IN ('repcard_appointments', 'repcard_door_knocks')
      ORDER BY tc.table_name, tc.constraint_name
    `;

    const existingFKs = fkResult.rows.map((r: any) => ({
      table: r.table_name,
      constraint: r.constraint_name,
      references: `${r.foreign_table_name}(${r.foreign_column_name})`,
    }));

    const missingFKs = expectedFKs.filter(expected => 
      !existingFKs.some(existing => 
        existing.table === expected.table && 
        existing.constraint === expected.constraint
      )
    );

    if (missingFKs.length === 0) {
      results.push({
        category: 'Foreign Keys',
        check: 'All foreign keys exist',
        status: 'pass',
        message: `✅ All ${expectedFKs.length} foreign keys exist`,
      });
      log(`✅ All ${expectedFKs.length} foreign keys exist`, 'success');
    } else {
      results.push({
        category: 'Foreign Keys',
        check: 'All foreign keys exist',
        status: 'warning',
        message: `⚠️  Missing ${missingFKs.length} foreign keys`,
        details: { missing: missingFKs, existing: existingFKs },
      });
      log(`⚠️  Missing ${missingFKs.length} foreign keys`, 'warning');
      missingFKs.forEach(fk => {
        log(`   - ${fk.table}.${fk.constraint} → ${fk.references}`, 'warning');
      });
    }
  } catch (error: any) {
    results.push({
      category: 'Foreign Keys',
      check: 'Foreign key verification',
      status: 'fail',
      message: `Error: ${error.message}`,
    });
    log(`❌ Error verifying foreign keys: ${error.message}`, 'error');
  }
}

async function verifyDataIntegrity() {
  log('\n🔍 Verifying Data Integrity...', 'info');

  try {
    // Check for orphaned appointments (customer deleted)
    const orphanedAppointments = await sql`
      SELECT COUNT(*) as count
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON a.customer_id = c.id
      WHERE a.customer_id IS NOT NULL
        AND c.id IS NULL
    `;
    const orphanedCount = orphanedAppointments.rows[0]?.count || 0;

    if (orphanedCount === 0) {
      results.push({
        category: 'Data Integrity',
        check: 'No orphaned appointments',
        status: 'pass',
        message: '✅ No orphaned appointments found',
      });
      log('✅ No orphaned appointments found', 'success');
    } else {
      results.push({
        category: 'Data Integrity',
        check: 'No orphaned appointments',
        status: 'warning',
        message: `⚠️  Found ${orphanedCount} orphaned appointments`,
      });
      log(`⚠️  Found ${orphanedCount} orphaned appointments`, 'warning');
    }

    // Check for appointments with null setter_user_id (should be rare)
    const nullSetterAppointments = await sql`
      SELECT COUNT(*) as count
      FROM repcard_appointments
      WHERE setter_user_id IS NULL
    `;
    const nullSetterCount = nullSetterAppointments.rows[0]?.count || 0;

    if (nullSetterCount === 0) {
      log('✅ All appointments have setter_user_id', 'success');
    } else {
      log(`⚠️  Found ${nullSetterCount} appointments without setter_user_id`, 'warning');
    }

    // Check table row counts
    const tableCounts = await sql`
      SELECT 
        'repcard_appointments' as table_name, COUNT(*) as count FROM repcard_appointments
      UNION ALL
      SELECT 'repcard_customers', COUNT(*) FROM repcard_customers
      UNION ALL
      SELECT 'repcard_door_knocks', COUNT(*) FROM repcard_door_knocks
      UNION ALL
      SELECT 'repcard_users', COUNT(*) FROM repcard_users
    `;

    log('\n📊 Table Row Counts:', 'info');
    tableCounts.rows.forEach((row: any) => {
      log(`   ${row.table_name}: ${parseInt(row.count).toLocaleString()} rows`, 'info');
    });

  } catch (error: any) {
    results.push({
      category: 'Data Integrity',
      check: 'Data integrity verification',
      status: 'fail',
      message: `Error: ${error.message}`,
    });
    log(`❌ Error verifying data integrity: ${error.message}`, 'error');
  }
}

async function verifyPerformance() {
  log('\n⚡ Verifying Performance Optimizations...', 'info');

  try {
    // Check if ANALYZE has been run recently
    const tableNamesArray = `{repcard_appointments,repcard_door_knocks,repcard_users,repcard_customers}`;
    const statsResult = await sql`
      SELECT 
        schemaname,
        relname as tablename,
        last_analyze,
        last_autoanalyze
      FROM pg_stat_user_tables
      WHERE schemaname = 'public'
        AND relname = ANY(${tableNamesArray}::text[])
      ORDER BY relname
    `;

    const rows = Array.isArray(statsResult) ? statsResult : (statsResult?.rows || []);
    const tablesNeedingAnalyze = rows.filter((row: any) => 
      !row.last_analyze && !row.last_autoanalyze
    );

    if (tablesNeedingAnalyze.length === 0) {
      results.push({
        category: 'Performance',
        check: 'Table statistics updated',
        status: 'pass',
        message: '✅ All tables have statistics',
      });
      log('✅ All tables have statistics', 'success');
    } else {
      results.push({
        category: 'Performance',
        check: 'Table statistics updated',
        status: 'warning',
        message: `⚠️  ${tablesNeedingAnalyze.length} tables need ANALYZE`,
        details: { tables: tablesNeedingAnalyze.map((r: any) => r.relname || r.tablename) },
      });
      log(`⚠️  ${tablesNeedingAnalyze.length} tables need ANALYZE`, 'warning');
      log('   Run: ANALYZE repcard_appointments, repcard_door_knocks, repcard_users, repcard_customers;', 'info');
    }

  } catch (error: any) {
    results.push({
      category: 'Performance',
      check: 'Performance verification',
      status: 'fail',
      message: `Error: ${error.message}`,
    });
    log(`❌ Error verifying performance: ${error.message}`, 'error');
  }
}

async function main() {
  console.log('\n' + '='.repeat(70));
  log('🗄️  DATABASE OPTIMIZATION VERIFICATION', 'info');
  console.log('='.repeat(70));

  try {
    await verifyTables();
    await verifyIndexes();
    await verifyForeignKeys();
    await verifyDataIntegrity();
    await verifyPerformance();

    // Summary
    console.log('\n' + '='.repeat(70));
    log('📊 VERIFICATION SUMMARY', 'info');
    console.log('='.repeat(70));

    const passed = results.filter(r => r.status === 'pass').length;
    const warnings = results.filter(r => r.status === 'warning').length;
    const failed = results.filter(r => r.status === 'fail').length;

    log(`\n✅ Passed: ${passed}`, 'success');
    if (warnings > 0) log(`⚠️  Warnings: ${warnings}`, 'warning');
    if (failed > 0) log(`❌ Failed: ${failed}`, 'error');

    if (warnings > 0 || failed > 0) {
      log('\n📋 Detailed Results:', 'info');
      results.forEach(result => {
        const icon = result.status === 'pass' ? '✅' : result.status === 'warning' ? '⚠️' : '❌';
        const logType = result.status === 'pass' ? 'success' : result.status === 'warning' ? 'warning' : 'error';
      log(`   ${icon} [${result.category}] ${result.check}: ${result.message}`, logType);
      });
    }

    if (failed === 0 && warnings === 0) {
      log('\n🎉 All checks passed! Database is optimized for leaderboard queries.', 'success');
      process.exit(0);
    } else if (failed === 0) {
      log('\n⚠️  Some warnings found. Review above and run migration 040 if needed.', 'warning');
      process.exit(0);
    } else {
      log('\n❌ Some checks failed. Please review and fix issues above.', 'error');
      process.exit(1);
    }
  } catch (error: any) {
    log(`\n💥 Unexpected error: ${error.message}`, 'error');
    console.error(error);
    process.exit(1);
  }
}

main();
