import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';
export const maxDuration = 300; // 5 minutes max

/**
 * POST /api/admin/repcard/backfill-metrics
 * 
 * Backfills is_within_48_hours and has_power_bill columns for all appointments.
 * This should be run once after deployment to populate these calculated fields.
 */
export async function POST(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  try {
    logApiRequest('POST', path, { endpoint: 'backfill-repcard-metrics', requestId });

    // Authentication - admin only
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    console.log('[RepCard Backfill] Starting metrics backfill...');

    // Helper to extract rows from SQL result
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    // Step 0: Get total appointment count for diagnostics
    const totalCountResult = await sql`
      SELECT COUNT(*)::int as total FROM repcard_appointments
    `;
    const totalAppointments = getRows(totalCountResult)[0]?.total || 0;
    console.log(`[RepCard Backfill] Total appointments in database: ${totalAppointments}`);

    // Step 1: Backfill is_within_48_hours - UPDATE ALL appointments (force update)
    console.log('[RepCard Backfill] Step 1: Backfilling is_within_48_hours for all appointments...');
    const within48Result = await sql`
      UPDATE repcard_appointments a
      SET is_within_48_hours = (
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL
            AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours' 
            AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours' 
          THEN TRUE
          ELSE FALSE
        END
      )
      FROM repcard_customers c
      WHERE a.repcard_customer_id = c.repcard_customer_id
    `;
    const within48Updated = Array.isArray(within48Result) ? 0 : (within48Result as any).rowCount || 0;
    console.log(`[RepCard Backfill] Updated ${within48Updated} appointments for is_within_48_hours`);

    // Also update appointments without matching customers (set to FALSE)
    // IMPORTANT: This must update ALL appointments without customers, not just NULL ones
    const within48NoCustomerResult = await sql`
      UPDATE repcard_appointments a
      SET is_within_48_hours = FALSE
      WHERE NOT EXISTS (
        SELECT 1 FROM repcard_customers c 
        WHERE c.repcard_customer_id = a.repcard_customer_id
      )
    `;
    const within48NoCustomerUpdated = Array.isArray(within48NoCustomerResult) ? 0 : (within48NoCustomerResult as any).rowCount || 0;
    console.log(`[RepCard Backfill] Updated ${within48NoCustomerUpdated} appointments without customers for is_within_48_hours`);

    // Step 2: Backfill has_power_bill - UPDATE ALL appointments (force update)
    // SIMPLIFIED: Any attachment = power bill (for now)
    console.log('[RepCard Backfill] Step 2: Backfilling has_power_bill for all appointments...');
    const powerBillResult = await sql`
      UPDATE repcard_appointments a
      SET has_power_bill = (
        CASE
          WHEN EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca
            WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
          ) OR EXISTS (
            SELECT 1 FROM repcard_appointment_attachments aa
            WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
          ) THEN TRUE
          ELSE FALSE
        END
      )
    `;
    const powerBillUpdated = Array.isArray(powerBillResult) ? 0 : (powerBillResult as any).rowCount || 0;
    console.log(`[RepCard Backfill] Updated ${powerBillUpdated} appointments for has_power_bill`);

    // Step 3: Verify results - Check ALL appointments and last 30 days
    console.log('[RepCard Backfill] Step 3: Verifying results...');
    
    // All-time stats
    const verifyAllResult = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_pb,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb
      FROM repcard_appointments
    `;
    const verifyAll = getRows(verifyAllResult)[0] as any;
    console.log(`[RepCard Backfill] All-time verification:`, verifyAll);

    // Last 30 days stats
    const verifyRecentResult = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_pb,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb
      FROM repcard_appointments
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const verifyRecent = getRows(verifyRecentResult)[0] as any;
    console.log(`[RepCard Backfill] Recent (30 days) verification:`, verifyRecent);
    
    // Use recent for display, but include all-time in response
    const verify = verifyRecent;
    
    const duration = Date.now() - start;
    logApiResponse('POST', path, duration, { status: 200, requestId });

    return NextResponse.json({
      success: true,
      results: {
        totalAppointmentsInDatabase: totalAppointments,
        within48HoursUpdated: within48Updated + within48NoCustomerUpdated,
        powerBillUpdated: powerBillUpdated,
        verification: {
          // Recent (last 30 days) - shown in UI
          totalAppointments: verify?.total || 0,
          within48h: verify?.within_48h || 0,
          withPowerBill: verify?.with_pb || 0,
          nullWithin48h: verify?.null_48h || 0,
          nullPowerBill: verify?.null_pb || 0,
          within48hPercentage: verify?.total > 0 ? ((verify?.within_48h / verify?.total) * 100).toFixed(1) : '0.0',
          powerBillPercentage: verify?.total > 0 ? ((verify?.with_pb / verify?.total) * 100).toFixed(1) : '0.0',
        },
        verificationAllTime: {
          // All-time stats - for reference
          totalAppointments: verifyAll?.total || 0,
          within48h: verifyAll?.within_48h || 0,
          withPowerBill: verifyAll?.with_pb || 0,
          nullWithin48h: verifyAll?.null_48h || 0,
          nullPowerBill: verifyAll?.null_pb || 0,
          within48hPercentage: verifyAll?.total > 0 ? ((verifyAll?.within_48h / verifyAll?.total) * 100).toFixed(1) : '0.0',
          powerBillPercentage: verifyAll?.total > 0 ? ((verifyAll?.with_pb / verifyAll?.total) * 100).toFixed(1) : '0.0',
        }
      },
      message: 'Metrics backfill completed successfully',
      duration: duration
    });

  } catch (error) {
    const duration = Date.now() - start;
    logError('backfill-repcard-metrics', error as Error, { requestId });
    logApiResponse('POST', path, duration, { status: 500, requestId });
    
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to backfill metrics',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
