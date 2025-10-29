#!/usr/bin/env tsx
/**
 * Diagnostic script to check why leaderboard queries return empty results
 */

import dotenv from 'dotenv';
import * as path from 'path';

// Load environment variables
dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function checkLeaderboardData() {
  console.log('🔍 Checking Leaderboard Data...\n');

  // 1. Check if data exists in tables
  console.log('1. Checking table counts...');
  try {
    const customers = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const appointments = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    const users = await sql`SELECT COUNT(*) as count FROM users WHERE repcard_user_id IS NOT NULL`;
    
    const custCount = Array.from(customers)[0]?.count || 0;
    const apptCount = Array.from(appointments)[0]?.count || 0;
    const userCount = Array.from(users)[0]?.count || 0;
    
    console.log(`   repcard_customers: ${custCount}`);
    console.log(`   repcard_appointments: ${apptCount}`);
    console.log(`   users with repcard_user_id: ${userCount}`);
  } catch (error: any) {
    console.error(`   Error: ${error.message}`);
  }

  // 2. Check data date ranges
  console.log('\n2. Checking date ranges...');
  try {
    const customerDates = await sql`
      SELECT 
        MIN(created_at) as earliest,
        MAX(created_at) as latest,
        COUNT(*) as count
      FROM repcard_customers
    `;
    const apptDates = await sql`
      SELECT 
        MIN(COALESCE(scheduled_at, created_at)) as earliest,
        MAX(COALESCE(scheduled_at, created_at)) as latest,
        COUNT(*) as count
      FROM repcard_appointments
    `;
    
    const custDates = Array.from(customerDates)[0];
    const apptDatesResult = Array.from(apptDates)[0];
    
    console.log(`   Customers: ${custDates?.earliest} to ${custDates?.latest} (${custDates?.count} total)`);
    console.log(`   Appointments: ${apptDatesResult?.earliest} to ${apptDatesResult?.latest} (${apptDatesResult?.count} total)`);
  } catch (error: any) {
    console.error(`   Error: ${error.message}`);
  }

  // 3. Test query with last 12 months
  console.log('\n3. Testing leaderboard query (last 12 months)...');
  try {
    const now = new Date();
    const last12 = new Date(now);
    last12.setMonth(now.getMonth() - 12);
    const startDate = last12.toISOString().split('T')[0];
    const endDate = now.toISOString().split('T')[0];
    
    console.log(`   Date range: ${startDate} to ${endDate}`);
    
    // Test doors_knocked query
    const doorsQuery = await sql`
      SELECT
        u.id as user_id,
        u.name as user_name,
        u.repcard_user_id::text as repcard_user_id,
        COUNT(c.repcard_customer_id) as count
      FROM repcard_customers c
      INNER JOIN users u ON u.repcard_user_id::text = c.setter_user_id::text
      WHERE u.repcard_user_id IS NOT NULL
        AND c.created_at >= ${startDate}::timestamp
        AND c.created_at <= (${endDate}::timestamp + INTERVAL '1 day')
      GROUP BY u.id, u.name, u.repcard_user_id
      LIMIT 10
    `;
    
    const doorsResults = Array.from(doorsQuery);
    console.log(`   Doors knocked query returned: ${doorsResults.length} users`);
    if (doorsResults.length > 0) {
      console.log(`   Sample: ${doorsResults[0]?.user_name} - ${doorsResults[0]?.count} doors`);
    }
    
    // Test appointments_set query
    const apptQuery = await sql`
      SELECT
        u.id as user_id,
        u.name as user_name,
        u.repcard_user_id::text as repcard_user_id,
        COUNT(a.repcard_appointment_id) as count
      FROM repcard_appointments a
      INNER JOIN users u ON u.repcard_user_id::text = a.setter_user_id::text
      WHERE u.repcard_user_id IS NOT NULL
        AND (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at >= ${startDate}::timestamp AND a.scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day'))
          OR
          (a.scheduled_at IS NULL AND a.created_at >= ${startDate}::timestamp AND a.created_at <= (${endDate}::timestamp + INTERVAL '1 day'))
        )
      GROUP BY u.id, u.name, u.repcard_user_id
      LIMIT 10
    `;
    
    const apptResults = Array.from(apptQuery);
    console.log(`   Appointments query returned: ${apptResults.length} users`);
    if (apptResults.length > 0) {
      console.log(`   Sample: ${apptResults[0]?.user_name} - ${apptResults[0]?.count} appointments`);
    }
  } catch (error: any) {
    console.error(`   Error: ${error.message}`);
    console.error(`   Stack: ${error.stack}`);
  }

  // 4. Check user linking
  console.log('\n4. Checking user linking...');
  try {
    const linkCheck = await sql`
      SELECT 
        COUNT(*) FILTER (WHERE u.repcard_user_id IS NOT NULL) as linked,
        COUNT(*) FILTER (WHERE u.repcard_user_id IS NULL) as not_linked,
        COUNT(DISTINCT c.setter_user_id) as unique_setters_in_customers,
        COUNT(DISTINCT u.repcard_user_id::text) FILTER (WHERE u.repcard_user_id IS NOT NULL) as unique_linked_users
      FROM repcard_customers c
      LEFT JOIN users u ON u.repcard_user_id::text = c.setter_user_id::text
    `;
    
    const check = Array.from(linkCheck)[0];
    console.log(`   Linked users: ${check?.linked}`);
    console.log(`   Not linked: ${check?.not_linked}`);
    console.log(`   Unique setters in customers: ${check?.unique_setters_in_customers}`);
    console.log(`   Unique linked users: ${check?.unique_linked_users}`);
  } catch (error: any) {
    console.error(`   Error: ${error.message}`);
  }

  // 5. Check type mismatches
  console.log('\n5. Checking for type mismatches...');
  try {
    const typeCheck = await sql`
      SELECT 
        c.setter_user_id,
        c.setter_user_id::text as setter_user_id_text,
        u.repcard_user_id,
        u.repcard_user_id::text as repcard_user_id_text,
        COUNT(*) as count
      FROM repcard_customers c
      LEFT JOIN users u ON u.repcard_user_id::text = c.setter_user_id::text
      WHERE c.setter_user_id IS NOT NULL
      GROUP BY c.setter_user_id, u.repcard_user_id
      LIMIT 5
    `;
    
    const samples = Array.from(typeCheck);
    console.log(`   Sample matches: ${samples.length}`);
    if (samples.length > 0) {
      console.log(`   Example: setter_user_id=${samples[0]?.setter_user_id}, repcard_user_id=${samples[0]?.repcard_user_id}`);
    }
  } catch (error: any) {
    console.error(`   Error: ${error.message}`);
  }

  console.log('\n✅ Diagnostic complete!');
}

checkLeaderboardData().catch(console.error);

