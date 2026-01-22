#!/usr/bin/env tsx
/**
 * Sync Customers Direct - Calls RepCard API directly and inserts into DB
 * Bypasses sync-service to avoid server-only imports
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

// Import after env is set up
const { sql } = require('@/lib/db/client');
const { repcardClient } = require('@/lib/repcard/client');

async function syncCustomers() {
  console.log('🔄 Starting Customer Sync (Direct API)\n');
  console.log('='.repeat(70));
  console.log();

  try {
    let page = 1;
    let hasMore = true;
    let totalFetched = 0;
    let totalInserted = 0;
    let totalUpdated = 0;

    // Sync customers from last 60 days to get recent ones
    const endDate = new Date();
    const startDate = new Date();
    startDate.setDate(startDate.getDate() - 60);
    
    console.log(`📅 Syncing customers from ${startDate.toISOString().split('T')[0]} to ${endDate.toISOString().split('T')[0]}`);

    while (hasMore && page <= 50) { // Limit to 50 pages (5000 customers max)
      console.log(`📥 Fetching page ${page}...`);
      
      const response = await repcardClient.getCustomers({
        page,
        perPage: 100,
        startDate: startDate.toISOString().split('T')[0],
        endDate: endDate.toISOString().split('T')[0],
      });

      const customers = response.data || response.customers || [];
      hasMore = customers.length === 100 && response.pagination?.hasMore !== false;
      totalFetched += customers.length;

      console.log(`   Got ${customers.length} customers`);

      for (const customer of customers) {
        try {
          const setterUserId = customer.userId || customer.setterId || customer.createdBy || null;
          
          const result = await sql`
            INSERT INTO repcard_customers (
              repcard_customer_id,
              setter_user_id,
              office_id,
              name,
              email,
              phone,
              address,
              city,
              state,
              zip,
              status,
              created_at,
              updated_at,
              raw_data
            )
            VALUES (
              ${customer.id.toString()}::text,
              ${setterUserId},
              ${null}, -- office_id
              ${`${customer.firstName || ''} ${customer.lastName || ''}`.trim() || null},
              ${customer.email || null},
              ${customer.phone || customer.phoneNumber || null},
              ${customer.address || null},
              ${customer.city || null},
              ${customer.state || null},
              ${customer.zipCode || customer.zip || null},
              ${customer.statusId?.toString() || null},
              ${customer.createdAt ? new Date(customer.createdAt).toISOString() : null},
              ${customer.updatedAt ? new Date(customer.updatedAt).toISOString() : null},
              ${JSON.stringify(customer)}
            )
            ON CONFLICT (repcard_customer_id)
            DO UPDATE SET
              setter_user_id = EXCLUDED.setter_user_id,
              name = EXCLUDED.name,
              email = EXCLUDED.email,
              phone = EXCLUDED.phone,
              address = EXCLUDED.address,
              city = EXCLUDED.city,
              state = EXCLUDED.state,
              zip = EXCLUDED.zip,
              status = EXCLUDED.status,
              created_at = COALESCE(repcard_customers.created_at, EXCLUDED.created_at),
              updated_at = EXCLUDED.updated_at,
              raw_data = EXCLUDED.raw_data,
              synced_at = NOW()
            RETURNING (xmax = 0) AS inserted
          `;

          const row = Array.isArray(result) ? result[0] : (result?.rows?.[0] || {});
          if (row?.inserted) {
            totalInserted++;
          } else {
            totalUpdated++;
          }
        } catch (error: any) {
          console.error(`   ❌ Failed to sync customer ${customer.id}:`, error.message);
        }
      }

      page++;
      if (page % 10 === 0) {
        console.log(`   Progress: ${totalFetched} fetched, ${totalInserted} inserted, ${totalUpdated} updated`);
      }
    }

    console.log('\n' + '='.repeat(70));
    console.log('\n📊 Sync Results:');
    console.log('  Fetched:', totalFetched);
    console.log('  Inserted:', totalInserted);
    console.log('  Updated:', totalUpdated);

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

    // Auto-link appointments to customers
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
    const linked = Array.isArray(linkResult) ? 0 : (linkResult as any).rowCount || 0;
    console.log(`  ✅ Linked ${linked} appointments to customers`);

    // Trigger metric recalculation
    console.log('\n🔄 Triggering metric recalculation...');
    await sql`
      UPDATE repcard_appointments
      SET updated_at = NOW()
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
        AND (is_within_48_hours IS NULL OR has_power_bill IS NULL)
    `;
    console.log('  ✅ Metrics will be recalculated by triggers');

    // Check final status
    const finalCheck = await sql`
      SELECT 
        COUNT(*) as total_appts,
        COUNT(*) FILTER (WHERE customer_id IS NOT NULL) as linked,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE) as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE) as with_pb
      FROM repcard_appointments
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const final = Array.isArray(finalCheck) ? finalCheck[0] : (finalCheck?.rows?.[0] || {});
    console.log('\n📊 Final Status (Last 30 Days):');
    console.log('  Total appointments:', final.total_appts);
    console.log('  Linked to customers:', final.linked);
    console.log('  Within 48h:', final.within_48h);
    console.log('  With Power Bill:', final.with_pb);

    console.log('\n' + '='.repeat(70));
    console.log('✅ Customer sync completed!');
    console.log('\nDashboard should now show correct metrics.');

  } catch (error: any) {
    console.error('\n❌ Sync failed:', error.message);
    if (error.stack) console.error('Stack:', error.stack);
    process.exit(1);
  }
}

syncCustomers();
