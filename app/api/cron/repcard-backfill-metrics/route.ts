import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * POST /api/cron/repcard-backfill-metrics
 * 
 * Automatic backfill of RepCard quality metrics
 * Runs periodically to ensure is_within_48_hours and has_power_bill are always correct
 * 
 * This endpoint is called by Vercel Cron and should not require authentication
 * (Vercel Cron includes a secret header)
 */
export async function POST(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `cron-${Date.now()}`;

  try {
    // Verify this is a cron request (Vercel adds Authorization header)
    const authHeader = request.headers.get('authorization');
    if (authHeader !== `Bearer ${process.env.CRON_SECRET}`) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    logApiRequest('POST', path, { endpoint: 'repcard-backfill-metrics-cron', requestId });

    // Helper to extract rows from SQL result
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    // Step 1: Backfill is_within_48_hours - UPDATE ALL appointments (recalculate to fix incorrect values)
    console.log('[RepCard Auto-Backfill] Step 1: Recalculating is_within_48_hours for all appointments...');
    const within48hResult = await sql`
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
      WHERE a.repcard_customer_id::text = c.repcard_customer_id::text
        AND a.scheduled_at IS NOT NULL
    `;
    const within48hUpdated = Array.isArray(within48hResult) ? 0 : (within48hResult as any).rowCount || 0;
    console.log(`[RepCard Auto-Backfill] Updated ${within48hUpdated} appointments for is_within_48_hours`);

    // Also set FALSE for appointments without matching customers (update ALL, not just NULL)
    const within48hNoCustomerResult = await sql`
      UPDATE repcard_appointments a
      SET is_within_48_hours = FALSE
      WHERE NOT EXISTS (
        SELECT 1 FROM repcard_customers c 
        WHERE c.repcard_customer_id::text = a.repcard_customer_id::text
      )
    `;
    const within48hNoCustomerUpdated = Array.isArray(within48hNoCustomerResult) ? 0 : (within48hNoCustomerResult as any).rowCount || 0;
    console.log(`[RepCard Auto-Backfill] Updated ${within48hNoCustomerUpdated} appointments without customers for is_within_48_hours`);

    // Step 2: Backfill has_power_bill - UPDATE ALL appointments (recalculate to fix incorrect values)
    // SIMPLIFIED: Any attachment to customer or appointment = power bill
    console.log('[RepCard Auto-Backfill] Step 2: Recalculating has_power_bill for all appointments (any attachment = PB)...');
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
    console.log(`[RepCard Auto-Backfill] Updated ${powerBillUpdated} appointments for has_power_bill`);

    // Step 3: Verify results
    const verifyResult = await sql`
      SELECT
        COUNT(*)::int as total_appointments,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_power_bill
      FROM repcard_appointments
    `;
    const verification = getRows(verifyResult)[0] || {};

    const duration = Date.now() - start;
    logApiResponse('POST', path, duration, { status: 200, requestId });

    console.log('[RepCard Auto-Backfill] Complete:', {
      within48hUpdated,
      powerBillUpdated,
      verification
    });

    return NextResponse.json({
      success: true,
      results: {
        within48HoursUpdated: within48hUpdated,
        powerBillUpdated: powerBillUpdated,
        verification: {
          totalAppointments: verification.total_appointments || 0,
          null48h: verification.null_48h || 0,
          nullPowerBill: verification.null_pb || 0,
          within48h: verification.within_48h || 0,
          withPowerBill: verification.with_power_bill || 0,
        }
      },
      duration
    });

  } catch (error) {
    const duration = Date.now() - start;
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    
    console.error('[RepCard Auto-Backfill] Error:', errorMessage, error);
    logError('repcard-backfill-metrics-cron', error as Error, { requestId });
    logApiResponse('POST', path, duration, { status: 500, requestId });
    
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to auto-backfill metrics',
        message: errorMessage,
      },
      { status: 500 }
    );
  }
}
