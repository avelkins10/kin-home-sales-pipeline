import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * GET /api/admin/repcard/debug-recent-appointments
 * 
 * Deep dive into why recent appointments show 0% metrics
 * Shows detailed breakdown of what should be TRUE vs what is stored
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    // Get last 30 days in Eastern Time
    const nowET = new Date().toLocaleString('en-US', { timeZone: 'America/New_York' });
    const thirtyDaysAgoET = new Date(Date.now() - 30 * 24 * 60 * 60 * 1000);
    const thirtyDaysAgoETStr = thirtyDaysAgoET.toLocaleString('en-US', { timeZone: 'America/New_York' });

    // Get detailed breakdown of recent appointments
    const detailed = await sql`
      SELECT 
        a.id,
        a.repcard_appointment_id,
        a.scheduled_at,
        a.is_within_48_hours,
        a.has_power_bill,
        a.repcard_customer_id,
        c.created_at as customer_created_at,
        -- Calculate hours diff using Eastern Time (matching trigger logic)
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
            EXTRACT(EPOCH FROM (
              (a.scheduled_at::timestamptz AT TIME ZONE 'America/New_York') - 
              (c.created_at::timestamptz AT TIME ZONE 'America/New_York')
            )) / 3600
          ELSE NULL
        END as hours_diff_et,
        -- Also show UTC for comparison
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
            EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600
          ELSE NULL
        END as hours_diff_utc,
        -- Attachment counts
        (
          SELECT COUNT(*)::int
          FROM repcard_customer_attachments ca
          WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
        ) as customer_att_count,
        (
          SELECT COUNT(*)::int
          FROM repcard_appointment_attachments aa
          WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
        ) as appointment_att_count,
        -- What should be TRUE
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
            EXTRACT(EPOCH FROM (
              (a.scheduled_at::timestamptz AT TIME ZONE 'America/New_York') - 
              (c.created_at::timestamptz AT TIME ZONE 'America/New_York')
            )) / 3600 BETWEEN 0 AND 48
          ELSE FALSE
        END as should_be_48h,
        CASE
          WHEN (
            (SELECT COUNT(*) FROM repcard_customer_attachments ca WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text) > 0
            OR
            (SELECT COUNT(*) FROM repcard_appointment_attachments aa WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text) > 0
          ) THEN TRUE
          ELSE FALSE
        END as should_have_pb
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= NOW() - INTERVAL '30 days'
      ORDER BY a.scheduled_at DESC
      LIMIT 50
    `;
    const details = Array.isArray(detailed) ? detailed : (detailed?.rows || []);

    // Get summary stats
    const summary = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE a.is_within_48_hours = TRUE)::int as stored_48h_true,
        COUNT(*) FILTER (WHERE a.has_power_bill = TRUE)::int as stored_pb_true,
        COUNT(*) FILTER (WHERE a.is_within_48_hours = FALSE)::int as stored_48h_false,
        COUNT(*) FILTER (WHERE a.has_power_bill = FALSE)::int as stored_pb_false,
        COUNT(*) FILTER (
          WHERE a.scheduled_at IS NOT NULL 
          AND c.created_at IS NOT NULL
          AND EXTRACT(EPOCH FROM (
            (a.scheduled_at::timestamptz AT TIME ZONE 'America/New_York') - 
            (c.created_at::timestamptz AT TIME ZONE 'America/New_York')
          )) / 3600 BETWEEN 0 AND 48
        )::int as should_be_48h_count,
        COUNT(*) FILTER (
          WHERE EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca 
            WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
          ) OR EXISTS (
            SELECT 1 FROM repcard_appointment_attachments aa 
            WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
          )
        )::int as should_have_pb_count,
        COUNT(*) FILTER (WHERE a.scheduled_at IS NULL)::int as missing_scheduled_at,
        COUNT(*) FILTER (WHERE a.repcard_customer_id IS NULL)::int as missing_customer_id,
        COUNT(*) FILTER (WHERE c.created_at IS NULL)::int as missing_customer_created_at
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= NOW() - INTERVAL '30 days'
    `;
    const summaryRow = Array.isArray(summary) ? summary[0] : (summary?.rows?.[0] || {});

    // Get audit trail for recent calculations
    const recentAudits = await sql`
      SELECT 
        appointment_id,
        metric_name,
        metric_value,
        calculation_reason,
        calculated_at,
        raw_data
      FROM repcard_metric_audit
      WHERE calculated_at >= NOW() - INTERVAL '1 hour'
      ORDER BY calculated_at DESC
      LIMIT 100
    `;
    const audits = Array.isArray(recentAudits) ? recentAudits : (recentAudits?.rows || []);

    return NextResponse.json({
      success: true,
      timezone: 'America/New_York (Eastern Time)',
      summary: {
        total: summaryRow.total || 0,
        stored: {
          within48h: summaryRow.stored_48h_true || 0,
          hasPowerBill: summaryRow.stored_pb_true || 0,
        },
        shouldBe: {
          within48h: summaryRow.should_be_48h_count || 0,
          hasPowerBill: summaryRow.should_have_pb_count || 0,
        },
        mismatches: {
          within48h: (summaryRow.should_be_48h_count || 0) - (summaryRow.stored_48h_true || 0),
          hasPowerBill: (summaryRow.should_have_pb_count || 0) - (summaryRow.stored_pb_true || 0),
        },
        missingData: {
          scheduledAt: summaryRow.missing_scheduled_at || 0,
          customerId: summaryRow.missing_customer_id || 0,
          customerCreatedAt: summaryRow.missing_customer_created_at || 0,
        }
      },
      sampleAppointments: details.map((d: any) => ({
        id: d.id,
        scheduledAt: d.scheduled_at,
        customerCreatedAt: d.customer_created_at,
        hoursDiffET: d.hours_diff_et,
        hoursDiffUTC: d.hours_diff_utc,
        stored48h: d.is_within_48_hours,
        storedPB: d.has_power_bill,
        customerAttCount: d.customer_att_count || 0,
        appointmentAttCount: d.appointment_att_count || 0,
        shouldBe48h: d.should_be_48h,
        shouldHavePB: d.should_have_pb,
        mismatch48h: d.is_within_48_hours !== d.should_be_48h,
        mismatchPB: d.has_power_bill !== d.should_have_pb,
      })),
      recentAudits: audits,
      diagnosis: {
        issue: (summaryRow.should_be_48h_count || 0) > (summaryRow.stored_48h_true || 0) || 
               (summaryRow.should_have_pb_count || 0) > (summaryRow.stored_pb_true || 0)
          ? 'Mismatch detected - stored values don\'t match calculated values'
          : 'No mismatch - all appointments correctly calculated',
        recommendation: (summaryRow.should_be_48h_count || 0) > (summaryRow.stored_48h_true || 0) || 
                       (summaryRow.should_have_pb_count || 0) > (summaryRow.stored_pb_true || 0)
          ? 'Re-run Migration 032 and then re-run backfill to recalculate with current data'
          : 'Metrics appear correct'
      }
    });

  } catch (error) {
    console.error('[Debug Recent Appointments] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to debug recent appointments',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
