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

    // Step 1: Backfill is_within_48_hours
    console.log('[RepCard Backfill] Step 1: Backfilling is_within_48_hours...');
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
        AND (
          a.is_within_48_hours IS NULL 
          OR a.is_within_48_hours != (
            CASE
              WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL
                AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours' 
                AND (a.scheduled_at - c.created_at) >= INTERVAL '0 hours' 
              THEN TRUE
              ELSE FALSE
            END
          )
        )
    `;
    const within48Updated = Array.isArray(within48Result) ? 0 : (within48Result as any).rowCount || 0;
    console.log(`[RepCard Backfill] Updated ${within48Updated} appointments for is_within_48_hours`);

    // Step 2: Backfill has_power_bill
    console.log('[RepCard Backfill] Step 2: Backfilling has_power_bill...');
    const powerBillResult = await sql`
      UPDATE repcard_appointments a
      SET has_power_bill = (
        CASE
          WHEN EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca
            WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
              AND (
                ca.attachment_type ILIKE '%power%' 
                OR ca.attachment_type ILIKE '%bill%' 
                OR ca.file_name ILIKE '%power%' 
                OR ca.file_name ILIKE '%bill%'
                OR ca.attachment_type IS NULL
              )
          ) OR EXISTS (
            SELECT 1 FROM repcard_appointment_attachments aa
            WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
              AND (
                aa.attachment_type ILIKE '%power%' 
                OR aa.attachment_type ILIKE '%bill%' 
                OR aa.file_name ILIKE '%power%' 
                OR aa.file_name ILIKE '%bill%'
                OR aa.attachment_type IS NULL
              )
          ) THEN TRUE
          ELSE FALSE
        END
      )
      WHERE has_power_bill IS NULL 
        OR has_power_bill != (
          CASE
            WHEN EXISTS (
              SELECT 1 FROM repcard_customer_attachments ca
              WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
                AND (
                  ca.attachment_type ILIKE '%power%' 
                  OR ca.attachment_type ILIKE '%bill%' 
                  OR ca.file_name ILIKE '%power%' 
                  OR ca.file_name ILIKE '%bill%'
                  OR ca.attachment_type IS NULL
                )
            ) OR EXISTS (
              SELECT 1 FROM repcard_appointment_attachments aa
              WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
                AND (
                  aa.attachment_type ILIKE '%power%' 
                  OR aa.attachment_type ILIKE '%bill%' 
                  OR aa.file_name ILIKE '%power%' 
                  OR aa.file_name ILIKE '%bill%'
                  OR aa.attachment_type IS NULL
                )
            ) THEN TRUE
            ELSE FALSE
          END
        )
    `;
    const powerBillUpdated = Array.isArray(powerBillResult) ? 0 : (powerBillResult as any).rowCount || 0;
    console.log(`[RepCard Backfill] Updated ${powerBillUpdated} appointments for has_power_bill`);

    // Step 3: Verify results
    console.log('[RepCard Backfill] Step 3: Verifying results...');
    const verifyResult = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_pb,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb
      FROM repcard_appointments
      WHERE scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const verify = Array.from(verifyResult)[0] as any;
    
    const duration = Date.now() - start;
    logApiResponse('POST', path, duration, { status: 200, requestId });

    return NextResponse.json({
      success: true,
      results: {
        within48HoursUpdated: within48Updated,
        powerBillUpdated: powerBillUpdated,
        verification: {
          totalAppointments: verify?.total || 0,
          within48h: verify?.within_48h || 0,
          withPowerBill: verify?.with_pb || 0,
          nullWithin48h: verify?.null_48h || 0,
          nullPowerBill: verify?.null_pb || 0,
          within48hPercentage: verify?.total > 0 ? ((verify?.within_48h / verify?.total) * 100).toFixed(1) : '0.0',
          powerBillPercentage: verify?.total > 0 ? ((verify?.with_pb / verify?.total) * 100).toFixed(1) : '0.0',
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
