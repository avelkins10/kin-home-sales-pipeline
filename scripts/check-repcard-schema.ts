#!/usr/bin/env tsx
/**
 * Check RepCard database schema to determine if type normalization migration (018) has been applied
 * This helps us know whether to remove type casts from queries
 */

import { sql } from '../lib/db/client';

async function checkSchema() {
  console.log('🔍 Checking RepCard database schema...\n');

  try {
    // Check users.repcard_user_id
    const usersCheck = await sql`
      SELECT column_name, data_type, is_nullable
      FROM information_schema.columns
      WHERE table_name = 'users' AND column_name = 'repcard_user_id'
    `;
    const usersCol = Array.isArray(usersCheck) ? usersCheck[0] : usersCheck.rows?.[0];
    console.log('📊 users.repcard_user_id:');
    console.log(`   Type: ${usersCol?.data_type || 'NOT FOUND'}`);
    console.log(`   Nullable: ${usersCol?.is_nullable || 'N/A'}\n`);

    // Check repcard_customers.setter_user_id
    const customersCheck = await sql`
      SELECT column_name, data_type, is_nullable
      FROM information_schema.columns
      WHERE table_name = 'repcard_customers' AND column_name = 'setter_user_id'
    `;
    const customersCol = Array.isArray(customersCheck) ? customersCheck[0] : customersCheck.rows?.[0];
    console.log('📊 repcard_customers.setter_user_id:');
    console.log(`   Type: ${customersCol?.data_type || 'NOT FOUND'}`);
    console.log(`   Nullable: ${customersCol?.is_nullable || 'N/A'}\n`);

    // Check repcard_appointments.setter_user_id and closer_user_id
    const appointmentsCheck = await sql`
      SELECT column_name, data_type, is_nullable
      FROM information_schema.columns
      WHERE table_name = 'repcard_appointments' 
        AND column_name IN ('setter_user_id', 'closer_user_id')
      ORDER BY column_name
    `;
    const appointmentsCols = Array.isArray(appointmentsCheck) ? appointmentsCheck : appointmentsCheck.rows || [];
    console.log('📊 repcard_appointments:');
    appointmentsCols.forEach((col: any) => {
      console.log(`   ${col.column_name}: ${col.data_type} (nullable: ${col.is_nullable})`);
    });
    console.log();

    // Summary
    const allInteger = 
      usersCol?.data_type === 'integer' &&
      customersCol?.data_type === 'integer' &&
      appointmentsCols.every((col: any) => col.data_type === 'integer');

    console.log('📋 Summary:');
    if (allInteger) {
      console.log('✅ Migration 018 appears to be applied - all IDs are INTEGER');
      console.log('✅ Type casts can be removed from queries for better performance');
    } else {
      console.log('⚠️  Migration 018 may not be applied - types are mixed');
      console.log('⚠️  Type casts are needed but cause performance issues');
      console.log('💡 Recommendation: Run migration 018 to normalize types');
    }

    // Check data counts
    console.log('\n📊 Data Counts:');
    const userCount = await sql`SELECT COUNT(*) as count FROM users WHERE repcard_user_id IS NOT NULL`;
    const repcardUserCount = await sql`SELECT COUNT(*) as count FROM repcard_users WHERE status = 1`;
    const customerCount = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const appointmentCount = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;

    console.log(`   Users with RepCard ID: ${Array.isArray(userCount) ? userCount[0]?.count : userCount.rows?.[0]?.count || 0}`);
    console.log(`   Active RepCard users: ${Array.isArray(repcardUserCount) ? repcardUserCount[0]?.count : repcardUserCount.rows?.[0]?.count || 0}`);
    console.log(`   Customers: ${Array.isArray(customerCount) ? customerCount[0]?.count : customerCount.rows?.[0]?.count || 0}`);
    console.log(`   Appointments: ${Array.isArray(appointmentCount) ? appointmentCount[0]?.count : appointmentCount.rows?.[0]?.count || 0}`);

  } catch (error) {
    console.error('❌ Error checking schema:', error);
    process.exit(1);
  }
}

checkSchema()
  .then(() => {
    console.log('\n✅ Schema check complete');
    process.exit(0);
  })
  .catch((error) => {
    console.error('❌ Fatal error:', error);
    process.exit(1);
  });
