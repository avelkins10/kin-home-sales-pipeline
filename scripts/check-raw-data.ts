#!/usr/bin/env tsx
/**
 * Check if customers exist without date filtering
 */

import dotenv from 'dotenv';
import * as path from 'path';

dotenv.config({ path: path.join(process.cwd(), '.env.local') });

import { sql } from '@/lib/db/client';

async function checkRawData() {
  console.log('🔍 Checking Raw Data (No Date Filters)...\n');

  // Check total counts
  const customers = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
  const appointments = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
  const users = await sql`SELECT COUNT(*) as count FROM users WHERE repcard_user_id IS NOT NULL`;
  
  console.log(`Total customers: ${Array.from(customers)[0]?.count || 0}`);
  console.log(`Total appointments: ${Array.from(appointments)[0]?.count || 0}`);
  console.log(`Users with repcard_user_id: ${Array.from(users)[0]?.count || 0}`);

  // Check sample customers
  const sampleCustomers = await sql`
    SELECT 
      repcard_customer_id,
      setter_user_id,
      created_at,
      updated_at
    FROM repcard_customers
    LIMIT 5
  `;
  
  console.log(`\nSample customers: ${Array.from(sampleCustomers).length}`);
  Array.from(sampleCustomers).forEach((c: any) => {
    console.log(`  Customer ${c.repcard_customer_id}: setter=${c.setter_user_id}, created=${c.created_at}`);
  });

  // Check if users can match
  const matchCheck = await sql`
    SELECT 
      COUNT(*) as total_customers,
      COUNT(u.id) as matched_customers
    FROM repcard_customers c
    LEFT JOIN users u ON u.repcard_user_id::text = c.setter_user_id::text
  `;
  
  const match = Array.from(matchCheck)[0];
  console.log(`\nCustomer matching:`);
  console.log(`  Total customers: ${match?.total_customers || 0}`);
  console.log(`  Matched to users: ${match?.matched_customers || 0}`);
}

checkRawData().catch(console.error);

