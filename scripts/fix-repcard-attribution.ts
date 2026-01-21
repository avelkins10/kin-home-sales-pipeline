#!/usr/bin/env tsx
/**
 * Comprehensive RepCard Attribution Fix Script
 * 
 * This script will:
 * 1. Verify database schema state
 * 2. Generate fixes for attribution issues
 * 3. Create a report of what needs to be fixed
 */

import { sql } from '../lib/db/client';

async function checkAttribution() {
  console.log('🔍 Checking RepCard Attribution...\n');

  try {
    // Check schema types
    console.log('📊 Schema Check:');
    const schemaCheck = await sql`
      SELECT 
        table_name,
        column_name,
        data_type
      FROM information_schema.columns
      WHERE table_name IN ('users', 'repcard_customers', 'repcard_appointments')
        AND column_name IN ('repcard_user_id', 'setter_user_id', 'closer_user_id')
      ORDER BY table_name, column_name
    `;
    
    const schema = Array.isArray(schemaCheck) ? schemaCheck : schemaCheck.rows || [];
    schema.forEach((col: any) => {
      console.log(`   ${col.table_name}.${col.column_name}: ${col.data_type}`);
    });
    console.log();

    // Check sample data attribution
    console.log('📊 Sample Attribution Check:');
    
    // Check if appointments have both setter and closer
    const appointmentAttribution = await sql`
      SELECT 
        COUNT(*) as total,
        COUNT(setter_user_id) as with_setter,
        COUNT(closer_user_id) as with_closer,
        COUNT(CASE WHEN setter_user_id IS NOT NULL AND closer_user_id IS NOT NULL THEN 1 END) as with_both
      FROM repcard_appointments
      LIMIT 1000
    `;
    const apptStats = Array.isArray(appointmentAttribution) ? appointmentAttribution[0] : appointmentAttribution.rows?.[0];
    console.log(`   Total appointments: ${apptStats?.total || 0}`);
    console.log(`   With setter: ${apptStats?.with_setter || 0}`);
    console.log(`   With closer: ${apptStats?.with_closer || 0}`);
    console.log(`   With both: ${apptStats?.with_both || 0}`);
    console.log();

    // Check office attribution
    console.log('📊 Office Attribution Check:');
    const officeCheck = await sql`
      SELECT 
        COUNT(DISTINCT u.id) as users_with_office,
        COUNT(DISTINCT c.setter_user_id) as setters_with_customers,
        COUNT(DISTINCT a.closer_user_id) as closers_with_appointments
      FROM users u
      LEFT JOIN repcard_customers c ON u.repcard_user_id::text = c.setter_user_id::text
      LEFT JOIN repcard_appointments a ON u.repcard_user_id::text = a.closer_user_id::text
      WHERE u.repcard_user_id IS NOT NULL
    `;
    const officeStats = Array.isArray(officeCheck) ? officeCheck[0] : officeCheck.rows?.[0];
    console.log(`   Users with office: ${officeStats?.users_with_office || 0}`);
    console.log(`   Setters with customers: ${officeStats?.setters_with_customers || 0}`);
    console.log(`   Closers with appointments: ${officeStats?.closers_with_appointments || 0}`);
    console.log();

    // Summary
    console.log('📋 Summary:');
    const needsTypeCast = schema.some((col: any) => 
      (col.table_name === 'repcard_customers' || col.table_name === 'repcard_appointments') &&
      col.column_name.includes('user_id') &&
      col.data_type === 'text'
    );
    
    if (needsTypeCast) {
      console.log('⚠️  Type casts needed (migration 018 partially applied)');
      console.log('💡 Recommendation: Complete migration 018 or keep type casts');
    } else {
      console.log('✅ All types normalized - can remove type casts');
    }

    console.log('\n✅ Attribution check complete');
  } catch (error) {
    console.error('❌ Error checking attribution:', error);
    process.exit(1);
  }
}

checkAttribution()
  .then(() => process.exit(0))
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
