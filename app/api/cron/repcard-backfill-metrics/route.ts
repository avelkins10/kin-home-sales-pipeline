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

    // AUTOMATIC FIX: First ensure all appointments are linked to customers
    // This handles cases where appointments were synced before customers
    console.log('[RepCard Auto-Backfill] Step 1: Auto-linking appointments to customers...');
    try {
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
      if (appointmentsLinked > 0) {
        console.log(`[RepCard Auto-Backfill] ✅ Linked ${appointmentsLinked} appointments to customers`);
      }
    } catch (linkError) {
      console.error('[RepCard Auto-Backfill] ⚠️ Failed to link appointments (non-fatal):', linkError);
      // Continue even if linking fails
    }

    // EVENT-DRIVEN AUTO-BACKFILL: Simply update appointments to trigger recalculation
    // Triggers will automatically recalculate metrics using the same logic
    console.log('[RepCard Auto-Backfill] Step 2: Triggering metric recalculation for ALL appointments...');
    console.log('[RepCard Auto-Backfill] Using event-driven architecture - triggers calculate metrics automatically');
    
    // Update all appointments to trigger recalculation
    // The trigger will recalculate is_within_48_hours and has_power_bill automatically
    const backfillResult = await sql`
      UPDATE repcard_appointments
      SET updated_at = NOW()
      WHERE id IS NOT NULL
    `;
    const appointmentsUpdated = Array.isArray(backfillResult) ? 0 : (backfillResult as any).rowCount || 0;
    console.log(`[RepCard Auto-Backfill] ✅ Triggered recalculation for ${appointmentsUpdated} appointments`);
    
    const within48hUpdated = appointmentsUpdated; // All appointments recalculated
    const powerBillUpdated = appointmentsUpdated; // All appointments recalculated

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
