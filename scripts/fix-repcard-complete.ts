#!/usr/bin/env tsx
/**
 * Complete RepCard Fix Script
 * 
 * This script fixes all RepCard issues:
 * 1. Checks if repcard_users table has data
 * 2. Links users to RepCard by email
 * 3. Verifies everything is working
 * 
 * Usage: npx tsx scripts/fix-repcard-complete.ts
 */

import { config } from 'dotenv';
import { resolve } from 'path';
config({ path: resolve(process.cwd(), '.env.local') });
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}
import { sql } from '@/lib/db/client';

async function fixRepCard() {
  console.log('🔧 RepCard Complete Fix Script\n');
  console.log('='.repeat(70));
  console.log();

  // Step 1: Check repcard_users table
  console.log('📊 Step 1: Checking repcard_users table...');
  console.log('-'.repeat(70));
  
  try {
    const userCount = await (sql as any).query('SELECT COUNT(*) as count FROM repcard_users');
    const count = Number(userCount.rows?.[0]?.count || 0);
    
    console.log(`   RepCard users in database: ${count}`);
    
    if (count === 0) {
      console.log();
      console.log('   ⚠️  repcard_users table is empty!');
      console.log('   This means users need to be synced from RepCard API first.');
      console.log();
      console.log('   ACTION REQUIRED:');
      console.log('   1. Ensure REPCARD_API_KEY is set in Vercel production');
      console.log('   2. Run sync via admin dashboard: /admin/repcard-sync');
      console.log('   3. Or wait for automatic sync (runs every 5 minutes)');
      console.log();
      console.log('   ⏭️  Skipping user linking (no RepCard users to link to)');
      console.log();
      
      // Check if we can link from existing data
      console.log('   🔍 Checking if we can link from existing customer data...');
      const customerCheck = await (sql as any).query(`
        SELECT COUNT(DISTINCT setter_user_id) as unique_setters
        FROM repcard_customers
        WHERE setter_user_id IS NOT NULL
      `);
      const uniqueSetters = Number(customerCheck.rows?.[0]?.unique_setters || 0);
      console.log(`   Found ${uniqueSetters} unique setter IDs in customer data`);
      
      if (uniqueSetters > 0) {
        console.log('   ℹ️  Once repcard_users is synced, these users can be linked');
      }
      console.log();
      
      return;
    }
    
    console.log('   ✅ repcard_users table has data');
    console.log();
  } catch (error: any) {
    console.error('   ❌ Error checking repcard_users:', error.message);
    return;
  }

  // Step 2: Check current user linking status
  console.log('👥 Step 2: Checking current user linking status...');
  console.log('-'.repeat(70));
  
  try {
    const linkingStatus = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE repcard_user_id IS NOT NULL) as linked,
        COUNT(*) as total
      FROM users
    `;
    const result = Array.from(linkingStatus)[0];
    const linked = Number(result?.linked || 0);
    const total = Number(result?.total || 0);
    const percentage = total > 0 ? Math.round((linked / total) * 100) : 0;
    
    console.log(`   Currently linked: ${linked} / ${total} users (${percentage}%)`);
    console.log();
    
    if (linked === total && total > 0) {
      console.log('   ✅ All users are already linked!');
      console.log();
      return;
    }
  } catch (error: any) {
    console.error('   ❌ Error checking linking status:', error.message);
    return;
  }

  // Step 3: Link users by email
  console.log('🔗 Step 3: Linking users to RepCard by email...');
  console.log('-'.repeat(70));
  
  try {
    // First, show what will be linked
    const preview = await sql`
      SELECT 
        u.id,
        u.name,
        u.email,
        u.role,
        ru.repcard_user_id,
        ru.first_name || ' ' || ru.last_name as repcard_name
      FROM users u
      INNER JOIN repcard_users ru ON LOWER(TRIM(u.email)) = LOWER(TRIM(ru.email))
      WHERE u.repcard_user_id IS NULL
        AND u.email IS NOT NULL
        AND u.email != ''
      LIMIT 10
    `;
    
    const previewResults = Array.from(preview);
    
    if (previewResults.length === 0) {
      console.log('   ℹ️  No users found to link (all users already linked or no email matches)');
      console.log();
      return;
    }
    
    console.log(`   Found ${previewResults.length} users to link (showing first 10):`);
    previewResults.forEach((user: any, i: number) => {
      console.log(`   ${i + 1}. ${user.name} (${user.email}) → RepCard ID: ${user.repcard_user_id}`);
    });
    console.log();
    
    // Perform the linking
    console.log('   Linking users...');
    const linkResult = await sql`
      UPDATE users u
      SET repcard_user_id = ru.repcard_user_id::text
      FROM repcard_users ru
      WHERE LOWER(TRIM(u.email)) = LOWER(TRIM(ru.email))
        AND u.repcard_user_id IS NULL
        AND u.email IS NOT NULL
        AND u.email != ''
    `;
    
    // Get the actual count of updated rows
    const verifyLink = await sql`
      SELECT COUNT(*) as count
      FROM users
      WHERE repcard_user_id IS NOT NULL
    `;
    const newLinkedCount = Number(Array.from(verifyLink)[0]?.count || 0);
    
    console.log(`   ✅ Linked users successfully!`);
    console.log(`   Total linked users: ${newLinkedCount}`);
    console.log();
  } catch (error: any) {
    console.error('   ❌ Error linking users:', error.message);
    console.error(error);
    return;
  }

  // Step 4: Final verification
  console.log('✅ Step 4: Final Verification');
  console.log('-'.repeat(70));
  
  try {
    const finalStatus = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE repcard_user_id IS NOT NULL) as linked,
        COUNT(*) as total
      FROM users
    `;
    const final = Array.from(finalStatus)[0];
    const finalLinked = Number(final?.linked || 0);
    const finalTotal = Number(final?.total || 0);
    const finalPercentage = finalTotal > 0 ? Math.round((finalLinked / finalTotal) * 100) : 0;
    
    console.log(`   Linked Users: ${finalLinked} / ${finalTotal} (${finalPercentage}%)`);
    
    // Check data counts
    const dataCounts = await (sql as any).query(`
      SELECT 
        (SELECT COUNT(*) FROM repcard_customers) as customers,
        (SELECT COUNT(*) FROM repcard_appointments) as appointments,
        (SELECT COUNT(*) FROM repcard_status_logs) as status_logs
    `);
    const counts = dataCounts.rows?.[0];
    
    console.log(`   RepCard Data:`);
    console.log(`     - Customers: ${Number(counts?.customers || 0).toLocaleString()}`);
    console.log(`     - Appointments: ${Number(counts?.appointments || 0).toLocaleString()}`);
    console.log(`     - Status Logs: ${Number(counts?.status_logs || 0).toLocaleString()}`);
    console.log();
    
    if (finalLinked > 0) {
      console.log('   ✅ RepCard is ready! Users are linked and data exists.');
      console.log();
      console.log('   Next steps:');
      console.log('   1. Verify REPCARD_API_KEY is set in Vercel production');
      console.log('   2. Check dashboard - RepCard metrics should now display');
      console.log('   3. If metrics still not showing, check browser console for errors');
    } else {
      console.log('   ⚠️  No users linked yet. Sync repcard_users table first.');
    }
    console.log();
  } catch (error: any) {
    console.error('   ❌ Error in final verification:', error.message);
  }

  console.log('='.repeat(70));
  console.log();
  console.log('✅ Fix script completed!');
  console.log();
}

// Run fix
fixRepCard()
  .catch((error) => {
    console.error('\n❌ Fix failed with error:');
    console.error(error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });





