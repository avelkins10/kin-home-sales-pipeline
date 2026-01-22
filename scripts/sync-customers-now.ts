#!/usr/bin/env tsx
/**
 * Sync Customers Now - Direct Execution
 * Syncs customers from RepCard to populate missing customer data
 */

import { config } from 'dotenv';
import { resolve } from 'path';
import { runComprehensiveSync } from '@/lib/repcard/comprehensive-sync';

// Load environment variables
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Ensure DATABASE_URL is mapped to POSTGRES_URL
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

async function syncCustomers() {
  console.log('🔄 Starting Customer Sync\n');
  console.log('='.repeat(70));
  console.log();

  try {
    // Run comprehensive sync but only sync customers
    // This will sync customers from RepCard API
    console.log('📥 Syncing customers from RepCard...');
    
    const result = await runComprehensiveSync({
      skipUsers: false, // Need users to link customers
      skipOffices: false, // Need offices to link customers
      skipCustomers: false, // THIS IS WHAT WE WANT
      skipAppointments: true, // Skip appointments (already have them)
      skipStatusLogs: true,
      skipCustomerAttachments: false, // Include attachments
      skipAppointmentAttachments: true,
      incremental: false, // Full sync to get all customers
    });

    console.log('\n' + '='.repeat(70));
    console.log('\n📊 Sync Results:');
    console.log('  Users:', result.users.recordsFetched, 'fetched,', result.users.recordsInserted, 'inserted');
    console.log('  Offices:', result.offices.recordsFetched, 'fetched,', result.offices.recordsInserted, 'inserted');
    console.log('  Customers:', result.customers.recordsFetched, 'fetched,', result.customers.recordsInserted, 'inserted,', result.customers.recordsUpdated, 'updated');
    console.log('  Attachments:', result.customerAttachments?.recordsFetched || 0, 'fetched');

    // Verify customers were synced
    const { sql } = await import('@/lib/db/client');
    const customerCheck = await sql`
      SELECT 
        COUNT(*) as total,
        COUNT(*) FILTER (WHERE created_at IS NOT NULL) as with_created_at
      FROM repcard_customers
    `;
    const check = Array.isArray(customerCheck) ? customerCheck[0] : (customerCheck?.rows?.[0] || {});
    console.log('\n✅ Customer Database Status:');
    console.log('  Total customers:', check.total);
    console.log('  With created_at:', check.with_created_at);

    // Check if recent appointments now have customers
    const linkCheck = await sql`
      SELECT 
        COUNT(*) as total_appts,
        COUNT(*) FILTER (WHERE customer_id IS NOT NULL) as linked,
        COUNT(*) FILTER (WHERE repcard_customer_id IS NOT NULL) as has_repcard_customer_id
      FROM repcard_appointments
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const link = Array.isArray(linkCheck) ? linkCheck[0] : (linkCheck?.rows?.[0] || {});
    console.log('\n📎 Appointment-Customer Linking:');
    console.log('  Recent appointments:', link.total_appts);
    console.log('  Linked (customer_id):', link.linked);
    console.log('  Has repcard_customer_id:', link.has_repcard_customer_id);

    // Auto-link appointments to customers
    if (link.has_repcard_customer_id > 0 && link.linked < link.total_appts) {
      console.log('\n🔗 Auto-linking appointments to customers...');
      const linkResult = await sql`
        UPDATE repcard_appointments a
        SET 
          customer_id = c.id,
          updated_at = NOW()
        FROM repcard_customers c
        WHERE a.repcard_customer_id::text = c.repcard_customer_id::text
          AND a.customer_id IS NULL
          AND c.id IS NOT NULL
      `;
      console.log('  ✅ Linked appointments to customers');
    }

    console.log('\n' + '='.repeat(70));
    console.log('✅ Customer sync completed!');
    console.log('\nNext: Metrics will be recalculated automatically by database triggers.');
    console.log('      Dashboard should now show correct percentages.');

  } catch (error: any) {
    console.error('\n❌ Sync failed:', error.message);
    console.error('Stack:', error.stack);
    process.exit(1);
  }
}

syncCustomers();
