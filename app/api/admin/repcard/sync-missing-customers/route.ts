import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';

export const runtime = 'nodejs';

/**
 * POST /api/admin/repcard/sync-missing-customers
 * 
 * Syncs specific customer IDs that are referenced by appointments but don't exist in database.
 * This is more efficient than syncing all customers.
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    console.log('[Sync Missing Customers] Starting...');

    // Step 1: Find all customer IDs from appointments that don't exist
    const missingResult = await sql`
      SELECT DISTINCT a.repcard_customer_id
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON a.repcard_customer_id::text = c.repcard_customer_id::text
      WHERE a.repcard_customer_id IS NOT NULL
        AND c.id IS NULL
      ORDER BY a.repcard_customer_id
    `;
    const missingIds = Array.isArray(missingResult) 
      ? missingResult 
      : (missingResult?.rows || []);
    
    const customerIds = missingIds
      .map((m: any) => m.repcard_customer_id)
      .filter((id: any) => id != null)
      .map((id: any) => parseInt(id.toString(), 10))
      .filter((id: number) => !isNaN(id));

    console.log(`[Sync Missing Customers] Found ${customerIds.length} missing customer IDs`);

    if (customerIds.length === 0) {
      return NextResponse.json({
        success: true,
        message: 'No missing customers found',
        synced: 0,
      });
    }

    // Step 2: Fetch each customer from RepCard API (batch in chunks to avoid rate limits)
    const CHUNK_SIZE = 10; // Process 10 at a time
    let synced = 0;
    let failed = 0;
    const errors: string[] = [];

    for (let i = 0; i < customerIds.length; i += CHUNK_SIZE) {
      const chunk = customerIds.slice(i, i + CHUNK_SIZE);
      console.log(`[Sync Missing Customers] Processing chunk ${Math.floor(i / CHUNK_SIZE) + 1}/${Math.ceil(customerIds.length / CHUNK_SIZE)} (${chunk.length} customers)`);

      // Process chunk in parallel (with delay to respect rate limits)
      const promises = chunk.map(async (customerId: number) => {
        try {
          // Fetch customer from RepCard API
          const response = await repcardClient.getCustomerById(customerId);
          const customer = response.result;

          if (!customer) {
            throw new Error(`Customer ${customerId} not found in RepCard API`);
          }

          // Insert/update customer in database
          const setterUserId = (customer as any).userId || customer.assignedUserId || (customer as any).setterUserId || null;

          await sql`
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
              ${customer.phone || null},
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
          `;

          synced++;
          return { success: true, customerId };
        } catch (error: any) {
          failed++;
          const errorMsg = `Customer ${customerId}: ${error.message}`;
          errors.push(errorMsg);
          console.error(`[Sync Missing Customers] Failed to sync customer ${customerId}:`, error.message);
          return { success: false, customerId, error: error.message };
        }
      });

      await Promise.all(promises);

      // Small delay between chunks to respect rate limits
      if (i + CHUNK_SIZE < customerIds.length) {
        await new Promise(resolve => setTimeout(resolve, 500));
      }
    }

    // Step 3: Link appointments to customers
    console.log('[Sync Missing Customers] Linking appointments to customers...');
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

    // Step 4: Trigger metric recalculation
    console.log('[Sync Missing Customers] Triggering metric recalculation...');
    await sql`
      UPDATE repcard_appointments
      SET updated_at = NOW()
      WHERE customer_id IS NOT NULL
        AND (is_within_48_hours IS NULL OR has_power_bill IS NULL)
    `;

    // Final stats
    const finalStats = await sql`
      SELECT 
        COUNT(*)::int as total_recent_appointments,
        COUNT(*) FILTER (WHERE c.id IS NULL)::int as appointments_without_customer,
        COUNT(*) FILTER (WHERE c.id IS NOT NULL AND c.created_at IS NOT NULL)::int as appointments_with_valid_customer
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const stats = Array.isArray(finalStats) ? finalStats[0] : (finalStats?.rows?.[0] || {});

    return NextResponse.json({
      success: true,
      message: `Synced ${synced} missing customers, failed ${failed}`,
      results: {
        totalMissing: customerIds.length,
        synced,
        failed,
        appointmentsLinked: linked,
        errors: errors.slice(0, 10), // First 10 errors
      },
      finalStats: {
        totalRecentAppointments: stats.total_recent_appointments || 0,
        appointmentsWithoutCustomer: stats.appointments_without_customer || 0,
        appointmentsWithValidCustomer: stats.appointments_with_valid_customer || 0,
      },
    });

  } catch (error: any) {
    console.error('[Sync Missing Customers] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to sync missing customers',
        message: error.message || 'Unknown error',
      },
      { status: 500 }
    );
  }
}
