import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';

export const runtime = 'nodejs';
export const maxDuration = 60; // Increase to 60 seconds for Vercel Pro

/**
 * POST /api/admin/repcard/sync-missing-customers
 * 
 * Syncs specific customer IDs that are referenced by appointments but don't exist in database.
 * This is more efficient than syncing all customers.
 * 
 * Rate limit aware: Processes sequentially with delays to respect RepCard API limits (100 req/period)
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    const startTime = Date.now();
    const MAX_DURATION_MS = 50000; // 50 seconds (leave buffer before 60s timeout)

    console.log('[Sync Missing Customers] Starting...');

    // Step 1: Find all customer IDs from appointments that don't exist
    const missingResult = await sql`
      SELECT DISTINCT a.repcard_customer_id
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON a.repcard_customer_id::text = c.repcard_customer_id::text
      WHERE a.repcard_customer_id IS NOT NULL
        AND c.id IS NULL
      ORDER BY a.repcard_customer_id
      LIMIT 500
    `;
    const missingIds = Array.isArray(missingResult) 
      ? missingResult 
      : (missingResult?.rows || []);
    
    const customerIds = missingIds
      .map((m: any) => m.repcard_customer_id)
      .filter((id: any) => id != null)
      .map((id: any) => parseInt(id.toString(), 10))
      .filter((id: number) => !isNaN(id));

    console.log(`[Sync Missing Customers] Found ${customerIds.length} missing customer IDs (limited to 500 per run)`);

    if (customerIds.length === 0) {
      return NextResponse.json({
        success: true,
        message: 'No missing customers found',
        synced: 0,
      });
    }

    // Step 2: Fetch each customer from RepCard API SEQUENTIALLY to respect rate limits
    let synced = 0;
    let notFound = 0; // Customers that don't exist (expected)
    let failed = 0; // Actual errors
    const errors: string[] = [];
    let rateLimitHits = 0;

    // Process sequentially with delays to respect rate limits
    for (let i = 0; i < customerIds.length; i++) {
      // Check timeout
      if (Date.now() - startTime > MAX_DURATION_MS) {
        console.log(`[Sync Missing Customers] Time limit reached, processed ${i}/${customerIds.length}`);
        break;
      }

      const customerId = customerIds[i];
      
      // Progress logging every 50 customers
      if (i % 50 === 0 && i > 0) {
        console.log(`[Sync Missing Customers] Progress: ${i}/${customerIds.length} (synced: ${synced}, not found: ${notFound}, failed: ${failed})`);
      }

      try {
        // Fetch customer from RepCard API
        const response = await repcardClient.getCustomerById(customerId);
        const customer = response.result;

        if (!customer) {
          notFound++;
          // Don't log as error - customer may have been deleted
          continue;
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
            contact_source,
            latitude,
            longitude,
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
            ${(customer as any).contactSource || null},
            ${(customer as any).latitude ? parseFloat((customer as any).latitude.toString()) : null},
            ${(customer as any).longitude ? parseFloat((customer as any).longitude.toString()) : null},
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
            contact_source = EXCLUDED.contact_source,
            latitude = EXCLUDED.latitude,
            longitude = EXCLUDED.longitude,
            created_at = COALESCE(repcard_customers.created_at, EXCLUDED.created_at),
            updated_at = EXCLUDED.updated_at,
            raw_data = EXCLUDED.raw_data,
            synced_at = NOW()
        `;

        synced++;
      } catch (error: any) {
        // Handle rate limit errors specifically
        if (error.message?.includes('429') || error.message?.includes('Too Many Requests')) {
          rateLimitHits++;
          // Wait longer when rate limited
          await new Promise(resolve => setTimeout(resolve, 5000)); // 5 second wait
          // Retry this customer
          i--; // Decrement to retry
          continue;
        }
        
        // Handle "not found" errors gracefully
        if (error.message?.includes('not found') || error.message?.includes('404')) {
          notFound++;
          continue;
        }

        // Other errors
        failed++;
        const errorMsg = `Customer ${customerId}: ${error.message}`;
        if (errors.length < 20) {
          errors.push(errorMsg);
        }
        console.error(`[Sync Missing Customers] Failed to sync customer ${customerId}:`, error.message);
      }

      // Delay between requests to respect rate limits (100 req/period)
      // ~1 request per second = safe rate
      await new Promise(resolve => setTimeout(resolve, 1000));
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
      message: `Synced ${synced} customers, ${notFound} not found (expected), ${failed} errors`,
      results: {
        totalProcessed: synced + notFound + failed,
        totalMissing: customerIds.length,
        synced,
        notFound, // Customers that don't exist in RepCard (expected)
        failed,
        rateLimitHits,
        appointmentsLinked: linked,
        errors: errors.slice(0, 10), // First 10 errors
      },
      finalStats: {
        totalRecentAppointments: stats.total_recent_appointments || 0,
        appointmentsWithoutCustomer: stats.appointments_without_customer || 0,
        appointmentsWithValidCustomer: stats.appointments_with_valid_customer || 0,
      },
      note: notFound > 0 ? 'Many customers may not exist in RepCard (deleted or invalid IDs). This is expected.' : undefined,
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
