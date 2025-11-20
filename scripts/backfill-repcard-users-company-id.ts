#!/usr/bin/env tsx
/**
 * Backfill company_id for repcard_users from repcard_offices
 * 
 * This script updates repcard_users.company_id by matching office_id to repcard_offices
 * Run this after offices have been synced.
 * 
 * Usage: npx tsx scripts/backfill-repcard-users-company-id.ts
 */

import { sql } from '@/lib/db/client';

async function backfillCompanyIds() {
  console.log('🔄 Backfilling company_id for repcard_users from repcard_offices...');
  console.log('='.repeat(60));
  console.log();

  try {
    // Check how many users need company_id
    const statsBefore = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE company_id IS NULL) as missing_company_id,
        COUNT(*) FILTER (WHERE company_id IS NOT NULL) as has_company_id,
        COUNT(*) as total
      FROM repcard_users
    `;
    const stats = Array.from(statsBefore)[0] as any;
    console.log(`📊 Current Status:`);
    console.log(`   Total users: ${stats.total}`);
    console.log(`   With company_id: ${stats.has_company_id}`);
    console.log(`   Missing company_id: ${stats.missing_company_id}`);
    console.log();

    if (stats.missing_company_id === 0) {
      console.log('✅ All users already have company_id!');
      return;
    }

    // Backfill company_id from offices
    const result = await sql`
      UPDATE repcard_users ru
      SET company_id = ro.company_id
      FROM repcard_offices ro
      WHERE ru.office_id = ro.repcard_office_id
        AND ru.company_id IS NULL
        AND ro.company_id IS NOT NULL
    `;

    const updatedCount = result.count || 0;
    console.log(`✅ Updated ${updatedCount} users with company_id from offices`);
    console.log();

    // Check final stats
    const statsAfter = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE company_id IS NULL) as missing_company_id,
        COUNT(*) FILTER (WHERE company_id IS NOT NULL) as has_company_id,
        COUNT(*) as total
      FROM repcard_users
    `;
    const finalStats = Array.from(statsAfter)[0] as any;
    console.log(`📊 Final Status:`);
    console.log(`   Total users: ${finalStats.total}`);
    console.log(`   With company_id: ${finalStats.has_company_id}`);
    console.log(`   Still missing company_id: ${finalStats.missing_company_id}`);
    console.log();

    if (finalStats.missing_company_id > 0) {
      console.log('⚠️  Some users still missing company_id. Possible reasons:');
      console.log('   - User has no office_id');
      console.log('   - Office not synced yet');
      console.log('   - Office has no company_id');
      console.log();
      console.log('   Users still missing company_id:');
      const missingUsers = await sql`
        SELECT repcard_user_id, email, office_id, office_name
        FROM repcard_users
        WHERE company_id IS NULL
        LIMIT 10
      `;
      Array.from(missingUsers).forEach((user: any, i: number) => {
        console.log(`   ${i + 1}. User ${user.repcard_user_id} (${user.email || 'no email'}) - Office: ${user.office_id || 'none'}`);
      });
      if (finalStats.missing_company_id > 10) {
        console.log(`   ... and ${finalStats.missing_company_id - 10} more`);
      }
    } else {
      console.log('✅ All users now have company_id!');
    }

  } catch (error) {
    console.error('❌ Error backfilling company_id:', error);
    throw error;
  }
}

// Run backfill
backfillCompanyIds()
  .catch((error) => {
    console.error('\n❌ Backfill failed:', error);
    process.exit(1);
  })
  .finally(() => {
    process.exit(0);
  });

