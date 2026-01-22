#!/usr/bin/env tsx
/**
 * Sync Customers Direct - Uses sync-service directly
 */

import { config } from 'dotenv';
import { resolve } from 'path';

// Load environment variables FIRST
config({ path: resolve(process.cwd(), '.env.local') });
config({ path: resolve(process.cwd(), '.env') });

// Set up environment before importing
if (process.env.DATABASE_URL && !process.env.POSTGRES_URL) {
  process.env.POSTGRES_URL = process.env.DATABASE_URL;
}

// Now import after env is set up
const { syncCustomers } = require('@/lib/repcard/sync-service');
const { sql } = require('@/lib/db/client');

async function sync() {
  console.log('🔄 Starting Customer Sync\n');
  console.log('='.repeat(70));
  console.log();

  try {
    console.log('📥 Syncing customers from RepCard API...');
    
    const result = await syncCustomers({
      incremental: false, // Full sync to get all customers
    });

    console.log('\n' + '='.repeat(70));
    console.log('\n📊 Sync Results:');
    console.log('  Fetched:', result.fetched);
    console.log('  Inserted:', result.inserted);
    console.log('  Updated:', result.updated);
    console.log('  Errors:', result.errors);

    // Verify customers were synced
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
      await sql`
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
      
      // Re-check
      const linkCheck2 = await sql`
        SELECT COUNT(*) FILTER (WHERE customer_id IS NOT NULL) as linked
        FROM repcard_appointments
        WHERE scheduled_at >= NOW() - INTERVAL '30 days'
      `;
      const link2 = Array.isArray(linkCheck2) ? linkCheck2[0] : (linkCheck2?.rows?.[0] || {});
      console.log('  Now linked:', link2.linked);
    }

    // Trigger metric recalculation by updating appointments
    console.log('\n🔄 Triggering metric recalculation...');
    await sql`
      UPDATE repcard_appointments
      SET updated_at = NOW()
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
        AND (is_within_48_hours IS NULL OR has_power_bill IS NULL)
    `;
    console.log('  ✅ Metrics will be recalculated by triggers');

    console.log('\n' + '='.repeat(70));
    console.log('✅ Customer sync completed!');
    console.log('\nDashboard should now show correct metrics.');

  } catch (error: any) {
    console.error('\n❌ Sync failed:', error.message);
    if (error.stack) console.error('Stack:', error.stack);
    process.exit(1);
  }
}

sync();
