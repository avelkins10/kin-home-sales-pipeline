#!/usr/bin/env tsx
/**
 * Production RepCard Status Checker
 * 
 * Checks production RepCard integration status:
 * - Environment variables (via Vercel API or manual check)
 * - Database tables and data
 * - API endpoint health
 * - User linking status
 * 
 * Usage: 
 *   npx tsx scripts/check-production-repcard.ts
 * 
 * Or with production DATABASE_URL:
 *   DATABASE_URL="your-prod-db-url" npx tsx scripts/check-production-repcard.ts
 */

import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';

interface CheckResult {
  name: string;
  status: 'pass' | 'fail' | 'warning';
  message: string;
  action?: string;
}

async function checkProductionRepCard(): Promise<void> {
  console.log('🔍 Production RepCard Status Check\n');
  console.log('='.repeat(70));
  console.log();
  
  const results: CheckResult[] = [];
  
  // 1. Check Environment Variables
  console.log('📋 Step 1: Environment Variables');
  console.log('-'.repeat(70));
  
  const hasApiKey = !!process.env.REPCARD_API_KEY;
  const hasDbUrl = !!process.env.DATABASE_URL;
  const hasCronSecret = !!process.env.CRON_SECRET;
  
  if (hasApiKey) {
    console.log('   ✅ REPCARD_API_KEY: Configured');
    results.push({ name: 'API Key', status: 'pass', message: 'REPCARD_API_KEY is set' });
  } else {
    console.log('   ❌ REPCARD_API_KEY: MISSING');
    console.log('   ⚠️  ACTION REQUIRED: Add to Vercel production environment');
    console.log('      → https://vercel.com/[project]/settings/environment-variables');
    console.log('      → Key: REPCARD_API_KEY');
    console.log('      → Value: Your RepCard API key');
    console.log('      → Environment: Production');
    console.log('      → Then: Redeploy');
    results.push({ 
      name: 'API Key', 
      status: 'fail', 
      message: 'REPCARD_API_KEY is not set',
      action: 'Add REPCARD_API_KEY to Vercel production environment and redeploy'
    });
  }
  
  if (hasDbUrl) {
    console.log('   ✅ DATABASE_URL: Configured');
    results.push({ name: 'Database URL', status: 'pass', message: 'DATABASE_URL is set' });
  } else {
    console.log('   ❌ DATABASE_URL: MISSING');
    results.push({ 
      name: 'Database URL', 
      status: 'fail', 
      message: 'DATABASE_URL is not set',
      action: 'Set DATABASE_URL environment variable'
    });
  }
  
  if (hasCronSecret) {
    console.log('   ✅ CRON_SECRET: Configured');
    results.push({ name: 'Cron Secret', status: 'pass', message: 'CRON_SECRET is set' });
  } else {
    console.log('   ⚠️  CRON_SECRET: Not set (cron jobs may not work)');
    results.push({ 
      name: 'Cron Secret', 
      status: 'warning', 
      message: 'CRON_SECRET not set - cron jobs may fail',
      action: 'Add CRON_SECRET to Vercel production environment'
    });
  }
  
  console.log();
  
  // 2. Test API Connection
  console.log('🌐 Step 2: RepCard API Connection');
  console.log('-'.repeat(70));
  
  if (hasApiKey) {
    try {
      const response = await repcardClient.getUsersMinimal({ page: 1, perPage: 5 });
      const total = response.result.pagination.total;
      console.log(`   ✅ API Connection: SUCCESS`);
      console.log(`   Total Users in RepCard: ${total}`);
      results.push({ 
        name: 'API Connection', 
        status: 'pass', 
        message: `Connected successfully. Found ${total} users.` 
      });
    } catch (error) {
      console.log(`   ❌ API Connection: FAILED`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      results.push({ 
        name: 'API Connection', 
        status: 'fail', 
        message: `Connection failed: ${error instanceof Error ? error.message : String(error)}`,
        action: 'Verify REPCARD_API_KEY is correct and valid'
      });
    }
  } else {
    console.log('   ⏭️  Skipped (API key not configured)');
    results.push({ 
      name: 'API Connection', 
      status: 'warning', 
      message: 'Skipped - API key not configured' 
    });
  }
  
  console.log();
  
  // 3. Check Database Tables
  console.log('🗄️  Step 3: Database Tables');
  console.log('-'.repeat(70));
  
  if (hasDbUrl) {
    try {
      const tables = await sql`
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_schema = 'public'
          AND table_name LIKE 'repcard_%'
        ORDER BY table_name;
      `;
      
      const tableList = Array.from(tables);
      const expectedTables = [
        'repcard_users',
        'repcard_offices',
        'repcard_customers',
        'repcard_appointments',
        'repcard_status_logs'
      ];
      
      if (tableList.length > 0) {
        console.log(`   ✅ Found ${tableList.length} RepCard tables`);
        const foundTableNames = tableList.map((t: any) => t.table_name);
        
        expectedTables.forEach(expected => {
          if (foundTableNames.includes(expected)) {
            console.log(`      ✅ ${expected}`);
          } else {
            console.log(`      ❌ ${expected} (missing)`);
          }
        });
        
        results.push({ 
          name: 'Database Tables', 
          status: tableList.length >= expectedTables.length ? 'pass' : 'warning',
          message: `Found ${tableList.length} tables (expected ${expectedTables.length}+)` 
        });
      } else {
        console.log('   ❌ No RepCard tables found!');
        console.log('   ⚠️  ACTION REQUIRED: Run migrations');
        console.log('      → ./scripts/run-migrations-production.sh');
        console.log('      → Or: psql "$DATABASE_URL" -f lib/db/migrations/014_repcard_comprehensive_tables.sql');
        results.push({ 
          name: 'Database Tables', 
          status: 'fail', 
          message: 'No RepCard tables found',
          action: 'Run database migrations on production database'
        });
      }
    } catch (error) {
      console.log(`   ❌ Database Query Failed`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      results.push({ 
        name: 'Database Tables', 
        status: 'fail', 
        message: `Query failed: ${error instanceof Error ? error.message : String(error)}` 
      });
    }
  } else {
    console.log('   ⏭️  Skipped (DATABASE_URL not configured)');
    results.push({ 
      name: 'Database Tables', 
      status: 'warning', 
      message: 'Skipped - DATABASE_URL not configured' 
    });
  }
  
  console.log();
  
  // 4. Check Data Sync Status
  console.log('📊 Step 4: Data Sync Status');
  console.log('-'.repeat(70));
  
  if (hasDbUrl) {
    try {
      const counts = await sql`
        SELECT 
          (SELECT COUNT(*) FROM repcard_users) as users,
          (SELECT COUNT(*) FROM repcard_customers) as customers,
          (SELECT COUNT(*) FROM repcard_appointments) as appointments,
          (SELECT COUNT(*) FROM repcard_status_logs) as status_logs;
      `;
      
      const c = Array.from(counts)[0];
      const totalRecords = Number(c.users) + Number(c.customers) + Number(c.appointments) + Number(c.status_logs);
      
      console.log(`   Users: ${c.users}`);
      console.log(`   Customers: ${c.customers}`);
      console.log(`   Appointments: ${c.appointments}`);
      console.log(`   Status Logs: ${c.status_logs}`);
      console.log(`   Total Records: ${totalRecords}`);
      
      if (totalRecords === 0) {
        console.log();
        console.log('   ⚠️  WARNING: No data synced yet!');
        console.log('   ⚠️  ACTION REQUIRED: Run initial sync');
        console.log('      → Navigate to: https://your-domain.com/admin/repcard-sync');
        console.log('      → Click: "Run Full Sync"');
        results.push({ 
          name: 'Data Sync', 
          status: 'fail', 
          message: 'No data synced',
          action: 'Run initial RepCard data sync via admin dashboard or API'
        });
      } else {
        console.log();
        console.log('   ✅ Data has been synced');
        results.push({ 
          name: 'Data Sync', 
          status: 'pass', 
          message: `${totalRecords} total records synced` 
        });
        
        // Check last sync time
        try {
          const lastSync = await sql`
            SELECT started_at, completed_at, total_duration
            FROM sync_runs
            WHERE sync_type = 'repcard_comprehensive'
            ORDER BY started_at DESC
            LIMIT 1;
          `;
          
          if (lastSync.length > 0) {
            const sync = Array.from(lastSync)[0];
            const lastSyncTime = new Date(sync.started_at);
            const hoursAgo = (Date.now() - lastSyncTime.getTime()) / (1000 * 60 * 60);
            console.log(`   Last Sync: ${lastSyncTime.toLocaleString()} (${Math.round(hoursAgo)} hours ago)`);
          }
        } catch (error) {
          // sync_runs table might not exist
        }
      }
    } catch (error) {
      console.log(`   ❌ Failed to check data counts`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      results.push({ 
        name: 'Data Sync', 
        status: 'fail', 
        message: `Query failed: ${error instanceof Error ? error.message : String(error)}` 
      });
    }
  } else {
    console.log('   ⏭️  Skipped (DATABASE_URL not configured)');
    results.push({ 
      name: 'Data Sync', 
      status: 'warning', 
      message: 'Skipped - DATABASE_URL not configured' 
    });
  }
  
  console.log();
  
  // 5. Check User Linking
  console.log('👥 Step 5: User Linking');
  console.log('-'.repeat(70));
  
  if (hasDbUrl) {
    try {
      const linked = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE repcard_user_id IS NOT NULL;
      `;
      const total = await sql`SELECT COUNT(*) as count FROM users;`;
      
      const linkedCount = Number(Array.from(linked)[0].count);
      const totalCount = Number(Array.from(total)[0].count);
      const percentage = totalCount > 0 ? Math.round((linkedCount / totalCount) * 100) : 0;
      
      console.log(`   Linked Users: ${linkedCount} / ${totalCount} (${percentage}%)`);
      
      if (linkedCount === 0) {
        console.log();
        console.log('   ⚠️  WARNING: No users linked to RepCard!');
        console.log('   ⚠️  ACTION REQUIRED: Link users by email');
        console.log('      → Run SQL:');
        console.log('        UPDATE users u');
        console.log('        SET repcard_user_id = ru.repcard_user_id::text');
        console.log('        FROM repcard_users ru');
        console.log('        WHERE LOWER(u.email) = LOWER(ru.email)');
        console.log('          AND u.repcard_user_id IS NULL;');
        results.push({ 
          name: 'User Linking', 
          status: 'fail', 
          message: 'No users linked to RepCard',
          action: 'Link users by email using SQL query'
        });
      } else {
        console.log('   ✅ Users are linked');
        results.push({ 
          name: 'User Linking', 
          status: 'pass', 
          message: `${linkedCount} users linked (${percentage}%)` 
        });
      }
    } catch (error) {
      console.log(`   ❌ Failed to check user linking`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      results.push({ 
        name: 'User Linking', 
        status: 'fail', 
        message: `Query failed: ${error instanceof Error ? error.message : String(error)}` 
      });
    }
  } else {
    console.log('   ⏭️  Skipped (DATABASE_URL not configured)');
    results.push({ 
      name: 'User Linking', 
      status: 'warning', 
      message: 'Skipped - DATABASE_URL not configured' 
    });
  }
  
  console.log();
  
  // Summary
  console.log('='.repeat(70));
  console.log();
  console.log('📊 Summary');
  console.log('-'.repeat(70));
  
  const passed = results.filter(r => r.status === 'pass').length;
  const failed = results.filter(r => r.status === 'fail').length;
  const warnings = results.filter(r => r.status === 'warning').length;
  
  console.log(`   ✅ Passed: ${passed}`);
  console.log(`   ❌ Failed: ${failed}`);
  console.log(`   ⚠️  Warnings: ${warnings}`);
  console.log();
  
  if (failed > 0) {
    console.log('❌ Issues Found - Action Required:');
    console.log();
    results.filter(r => r.status === 'fail').forEach(result => {
      console.log(`   ❌ ${result.name}: ${result.message}`);
      if (result.action) {
        console.log(`      → ${result.action}`);
      }
      console.log();
    });
  }
  
  if (warnings > 0) {
    console.log('⚠️  Warnings:');
    console.log();
    results.filter(r => r.status === 'warning').forEach(result => {
      console.log(`   ⚠️  ${result.name}: ${result.message}`);
      if (result.action) {
        console.log(`      → ${result.action}`);
      }
      console.log();
    });
  }
  
  if (failed === 0 && warnings === 0) {
    console.log('✅ All checks passed! RepCard integration is healthy.');
    console.log();
  }
  
  console.log('For detailed fix instructions, see: REPCARD_PRODUCTION_FIX.md');
  console.log();
}

// Run check
checkProductionRepCard()
  .catch((error) => {
    console.error('\n❌ Check failed with error:');
    console.error(error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });


