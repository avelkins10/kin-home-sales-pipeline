import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { syncCustomers } from '@/lib/repcard/sync-service';

export const runtime = 'nodejs';

/**
 * POST /api/admin/repcard/fix-missing-customers
 * 
 * Comprehensive fix for missing customer links:
 * 1. Syncs customers from RepCard (to get missing ones)
 * 2. Links appointments to customers (updates customer_id FK)
 * 3. Optionally runs metrics backfill
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    const { searchParams } = new URL(request.url);
    const skipCustomerSync = searchParams.get('skipCustomerSync') === 'true';
    const skipLinkAppointments = searchParams.get('skipLinkAppointments') === 'true';
    const skipMetricsBackfill = searchParams.get('skipMetricsBackfill') === 'true';
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;

    const results: any = {
      step1_customerSync: null,
      step2_linkAppointments: null,
      step3_metricsBackfill: null,
    };

    // Step 1: Sync customers from RepCard
    if (!skipCustomerSync) {
      console.log('[Fix Missing Customers] Step 1: Syncing customers from RepCard...');
      try {
        const customerSyncResult = await syncCustomers({
          startDate,
          endDate,
          incremental: false, // Full sync to get all customers
        });
        results.step1_customerSync = {
          success: true,
          recordsFetched: customerSyncResult.recordsFetched,
          recordsInserted: customerSyncResult.recordsInserted,
          recordsUpdated: customerSyncResult.recordsUpdated,
          recordsFailed: customerSyncResult.recordsFailed,
          duration: customerSyncResult.duration,
        };
        console.log('[Fix Missing Customers] ✅ Customer sync completed:', results.step1_customerSync);
      } catch (error: any) {
        console.error('[Fix Missing Customers] ❌ Customer sync failed:', error);
        results.step1_customerSync = {
          success: false,
          error: error.message || 'Unknown error',
        };
        // Continue to next step even if sync fails (might have partial data)
      }
    } else {
      results.step1_customerSync = { skipped: true };
    }

    // Step 2: Link appointments to customers
    if (!skipLinkAppointments) {
      console.log('[Fix Missing Customers] Step 2: Linking appointments to customers...');
      try {
        // Update appointments where repcard_customer_id matches but customer_id is NULL
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
        const appointmentsLinked = Array.isArray(linkResult) ? 0 : (linkResult as any).rowCount || 0;
        
        results.step2_linkAppointments = {
          success: true,
          appointmentsLinked,
        };
        console.log('[Fix Missing Customers] ✅ Linked', appointmentsLinked, 'appointments to customers');
      } catch (error: any) {
        console.error('[Fix Missing Customers] ❌ Link appointments failed:', error);
        results.step2_linkAppointments = {
          success: false,
          error: error.message || 'Unknown error',
        };
      }
    } else {
      results.step2_linkAppointments = { skipped: true };
    }

    // Step 3: Run metrics backfill (triggers will recalculate)
    if (!skipMetricsBackfill) {
      console.log('[Fix Missing Customers] Step 3: Triggering metrics recalculation...');
      try {
        // Simply update appointments to trigger recalculation
        const backfillResult = await sql`
          UPDATE repcard_appointments
          SET updated_at = NOW()
          WHERE id IS NOT NULL
        `;
        const appointmentsUpdated = Array.isArray(backfillResult) ? 0 : (backfillResult as any).rowCount || 0;
        
        results.step3_metricsBackfill = {
          success: true,
          appointmentsUpdated,
          message: 'Metrics will be recalculated by database triggers',
        };
        console.log('[Fix Missing Customers] ✅ Triggered recalculation for', appointmentsUpdated, 'appointments');
      } catch (error: any) {
        console.error('[Fix Missing Customers] ❌ Metrics backfill failed:', error);
        results.step3_metricsBackfill = {
          success: false,
          error: error.message || 'Unknown error',
        };
      }
    } else {
      results.step3_metricsBackfill = { skipped: true };
    }

    // Get final stats
    const finalStats = await sql`
      SELECT 
        COUNT(*)::int as total_recent_appointments,
        COUNT(*) FILTER (WHERE c.id IS NULL)::int as appointments_without_customer,
        COUNT(*) FILTER (WHERE c.id IS NOT NULL AND c.created_at IS NULL)::int as customers_without_created_at,
        COUNT(*) FILTER (WHERE c.id IS NOT NULL AND c.created_at IS NOT NULL)::int as appointments_with_valid_customer
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const statsRow = Array.isArray(finalStats) ? finalStats[0] : (finalStats?.rows?.[0] || {});

    return NextResponse.json({
      success: true,
      message: 'Fix completed',
      results,
      finalStats: {
        totalRecentAppointments: statsRow.total_recent_appointments || 0,
        appointmentsWithoutCustomer: statsRow.appointments_without_customer || 0,
        customersWithoutCreatedAt: statsRow.customers_without_created_at || 0,
        appointmentsWithValidCustomer: statsRow.appointments_with_valid_customer || 0,
      },
    });

  } catch (error) {
    console.error('[Fix Missing Customers] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to fix missing customers',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
