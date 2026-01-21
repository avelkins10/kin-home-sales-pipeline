#!/usr/bin/env tsx
/**
 * RepCard Integration Diagnostic Tool
 * 
 * Tests all aspects of the RepCard integration to identify issues:
 * - Environment configuration
 * - API connectivity
 * - Database tables
 * - Data sync status
 * - User linking
 * 
 * Usage: npx tsx scripts/diagnose-repcard-issue.ts
 */

import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';

async function diagnose() {
  console.log('🔍 RepCard Integration Diagnostic Tool\n');
  console.log('='.repeat(60));
  console.log();
  
  let hasIssues = false;
  
  // 1. Check environment variables
  console.log('📋 Step 1: Environment Variables');
  console.log('-'.repeat(60));
  
  const hasApiKey = !!process.env.REPCARD_API_KEY;
  const hasDbUrl = !!process.env.DATABASE_URL;
  const apiUrl = process.env.REPCARD_API_URL || 'https://api.repcard.com (default)';
  
  console.log(`   REPCARD_API_KEY: ${hasApiKey ? '✅ Configured' : '❌ MISSING'}`);
  console.log(`   REPCARD_API_URL: ${apiUrl}`);
  console.log(`   DATABASE_URL: ${hasDbUrl ? '✅ Configured' : '❌ MISSING'}`);
  console.log();
  
  if (!hasApiKey) {
    hasIssues = true;
    console.log('   ⚠️  CRITICAL: REPCARD_API_KEY is not set!');
    console.log('   → Add to .env.local: REPCARD_API_KEY=<your-key>');
    console.log('   → Get key from: https://www.repcard.com/settings/api');
    console.log();
  }
  
  if (!hasDbUrl) {
    hasIssues = true;
    console.log('   ⚠️  CRITICAL: DATABASE_URL is not set!');
    console.log();
  }
  
  // 2. Test API connection
  console.log('🌐 Step 2: RepCard API Connection');
  console.log('-'.repeat(60));
  
  if (hasApiKey) {
    try {
      const response = await repcardClient.getUsersMinimal({ page: 1, perPage: 5 });
      const total = response.result.pagination.total;
      const users = response.result.data;
      
      console.log(`   ✅ API Connection Successful!`);
      console.log(`   Total Users in RepCard: ${total}`);
      console.log(`   Sample Users (first 5):`);
      users.forEach((user: any, i: number) => {
        console.log(`      ${i + 1}. ${user.firstName} ${user.lastName} (${user.email})`);
      });
      console.log();
    } catch (error) {
      hasIssues = true;
      console.log(`   ❌ API Connection Failed!`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      console.log();
      console.log('   Possible Causes:');
      console.log('   - Invalid API key');
      console.log('   - Network connectivity issues');
      console.log('   - RepCard API is down');
      console.log();
    }
  } else {
    hasIssues = true;
    console.log('   ⏭️  Skipped (API key not configured)');
    console.log();
  }
  
  // 3. Check database tables
  console.log('🗄️  Step 3: Database Tables');
  console.log('-'.repeat(60));
  
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
      
      if (tableList.length > 0) {
        console.log(`   ✅ Found ${tableList.length} RepCard tables:`);
        tableList.forEach((t: any) => {
          console.log(`      - ${t.table_name}`);
        });
        console.log();
      } else {
        hasIssues = true;
        console.log('   ❌ No RepCard tables found!');
        console.log();
        console.log('   Action Required: Run migrations');
        console.log('   → npm run migrate');
        console.log('   → Or: psql "$DATABASE_URL" -f lib/db/migrations/014_repcard_comprehensive_tables.sql');
        console.log();
      }
    } catch (error) {
      hasIssues = true;
      console.log(`   ❌ Database Query Failed!`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      console.log();
    }
  } else {
    hasIssues = true;
    console.log('   ⏭️  Skipped (DATABASE_URL not configured)');
    console.log();
  }
  
  // 4. Check data counts
  console.log('📊 Step 4: Data Sync Status');
  console.log('-'.repeat(60));
  
  if (hasDbUrl) {
    try {
      const counts = await sql`
        SELECT 
          (SELECT COUNT(*) FROM repcard_users) as users,
          (SELECT COUNT(*) FROM repcard_customers) as customers,
          (SELECT COUNT(*) FROM repcard_appointments) as appointments,
          (SELECT COUNT(*) FROM repcard_status_logs) as status_logs,
          (SELECT COUNT(*) FROM repcard_customer_attachments) as customer_attachments,
          (SELECT COUNT(*) FROM repcard_appointment_attachments) as appointment_attachments;
      `;
      
      const c = Array.from(counts)[0];
      const totalRecords = Number(c.users) + Number(c.customers) + Number(c.appointments) + 
                           Number(c.status_logs) + Number(c.customer_attachments) + Number(c.appointment_attachments);
      
      console.log(`   Users: ${c.users}`);
      console.log(`   Customers (door knocks): ${c.customers}`);
      console.log(`   Appointments: ${c.appointments}`);
      console.log(`   Status Logs: ${c.status_logs}`);
      console.log(`   Customer Attachments: ${c.customer_attachments}`);
      console.log(`   Appointment Attachments: ${c.appointment_attachments}`);
      console.log();
      console.log(`   Total Records: ${totalRecords}`);
      console.log();
      
      if (totalRecords === 0) {
        hasIssues = true;
        console.log('   ⚠️  WARNING: No data synced yet!');
        console.log();
        console.log('   Action Required: Run initial sync');
        console.log('   → Navigate to: http://localhost:3000/admin/repcard-sync');
        console.log('   → Click: "Run Full Sync"');
        console.log('   → Or: npx tsx scripts/run-comprehensive-repcard-sync.ts');
        console.log();
      } else {
        console.log('   ✅ Data sync has run successfully!');
        console.log();
        
        // Check when last sync ran
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
            console.log(`   Last Sync: ${new Date(sync.started_at).toLocaleString()}`);
            console.log(`   Duration: ${Math.round(sync.total_duration / 1000)}s`);
            console.log();
          }
        } catch (error) {
          // sync_runs table might not exist yet
        }
      }
    } catch (error) {
      hasIssues = true;
      console.log(`   ❌ Failed to check data counts!`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      console.log();
    }
  } else {
    hasIssues = true;
    console.log('   ⏭️  Skipped (DATABASE_URL not configured)');
    console.log();
  }
  
  // 5. Check user linking
  console.log('👥 Step 5: User Linking');
  console.log('-'.repeat(60));
  
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
      console.log();
      
      if (linkedCount === 0) {
        hasIssues = true;
        console.log('   ⚠️  WARNING: No users linked to RepCard!');
        console.log();
        console.log('   Action Required: Link users by email');
        console.log('   → Run SQL:');
        console.log('     UPDATE users u');
        console.log('     SET repcard_user_id = ru.repcard_user_id');
        console.log('     FROM repcard_users ru');
        console.log('     WHERE LOWER(u.email) = LOWER(ru.email)');
        console.log('       AND u.repcard_user_id IS NULL;');
        console.log();
      } else if (linkedCount < totalCount) {
        console.log('   ℹ️  Some users not linked to RepCard');
        console.log('   This is normal if not all users are in RepCard');
        console.log();
      } else {
        console.log('   ✅ All users linked to RepCard!');
        console.log();
      }
      
      // Show sample of linked users
      const sampleUsers = await sql`
        SELECT u.name, u.email, u.role, u.repcard_user_id
        FROM users u
        WHERE u.repcard_user_id IS NOT NULL
        LIMIT 5;
      `;
      
      if (sampleUsers.length > 0) {
        console.log('   Sample Linked Users:');
        Array.from(sampleUsers).forEach((user: any, i: number) => {
          console.log(`      ${i + 1}. ${user.name} (${user.email}) - RepCard ID: ${user.repcard_user_id}`);
        });
        console.log();
      }
    } catch (error) {
      hasIssues = true;
      console.log(`   ❌ Failed to check user linking!`);
      console.log(`   Error: ${error instanceof Error ? error.message : String(error)}`);
      console.log();
    }
  } else {
    hasIssues = true;
    console.log('   ⏭️  Skipped (DATABASE_URL not configured)');
    console.log();
  }
  
  // 6. Final summary
  console.log('='.repeat(60));
  console.log();
  
  if (hasIssues) {
    console.log('❌ Issues Found - Action Required!');
    console.log();
    console.log('Next Steps:');
    console.log('1. Review the errors above');
    console.log('2. Follow the recommended actions');
    console.log('3. See REPCARD_FIX_GUIDE.md for detailed instructions');
    console.log();
  } else {
    console.log('✅ RepCard Integration is Healthy!');
    console.log();
    console.log('Everything looks good. RepCard metrics should display in the dashboard.');
    console.log();
  }
  
  console.log('For more help, see: REPCARD_FIX_GUIDE.md');
  console.log();
}

// Run diagnostic
diagnose()
  .catch((error) => {
    console.error('\n❌ Diagnostic failed with error:');
    console.error(error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });






