import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/diagnose-metrics
 * 
 * Comprehensive diagnostic endpoint to understand why metrics show 0%
 * Checks database state, backfill status, and query results
 */
export async function GET(request: NextRequest) {
  try {
    // Authentication
    const auth = await requireRole(['super_admin', 'office_leader', 'regional']);
    if (!auth.authorized) return auth.response;

    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate') || new Date().toISOString().split('T')[0];
    const endDate = searchParams.get('endDate') || new Date().toISOString().split('T')[0];

    // 1. Check database state for date range
    const dbState = await sql`
      SELECT 
        COUNT(*)::int as total,
        COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as true_48h,
        COUNT(*) FILTER (WHERE is_within_48_hours = FALSE)::int as false_48h,
        COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
        COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as true_pb,
        COUNT(*) FILTER (WHERE has_power_bill = FALSE)::int as false_pb,
        COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb,
        COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL)::int as has_scheduled_at,
        COUNT(*) FILTER (WHERE repcard_customer_id IS NOT NULL)::int as has_customer_id
      FROM repcard_appointments
      WHERE scheduled_at >= ${startDate}::date
        AND scheduled_at <= ${endDate}::date + INTERVAL '1 day'
    `;
    const dbStateRow = Array.isArray(dbState) ? dbState[0] : (dbState?.rows?.[0] || {});

    // 2. Check sample appointments with details
    const sampleAppts = await sql`
      SELECT 
        a.id,
        a.repcard_appointment_id,
        a.scheduled_at,
        a.is_within_48_hours,
        a.has_power_bill,
        a.repcard_customer_id,
        c.created_at as customer_created_at,
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
            DATE((a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')) - DATE((c.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York'))
          ELSE NULL
        END as days_diff,
        (
          SELECT COUNT(*)::int
          FROM repcard_customer_attachments ca
          WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
        ) as customer_att_count,
        (
          SELECT COUNT(*)::int
          FROM repcard_appointment_attachments aa
          WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
        ) as appointment_att_count
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= ${startDate}::date
        AND a.scheduled_at <= ${endDate}::date + INTERVAL '1 day'
      ORDER BY a.scheduled_at DESC
      LIMIT 10
    `;
    const samples = Array.isArray(sampleAppts) ? sampleAppts : (sampleAppts?.rows || []);

    // 3. Check what unified dashboard query would return
    const dashboardQuery = await sql`
      SELECT
        COUNT(DISTINCT a.id)::int as total_appointments,
        COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h,
        COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill,
        COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE)::int as both,
        COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours IS NULL)::int as null_48h,
        COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill IS NULL)::int as null_pb
      FROM repcard_appointments a
      WHERE a.scheduled_at >= ${startDate}::timestamptz
        AND a.scheduled_at <= ${endDate}::timestamptz + INTERVAL '1 day'
    `;
    const dashboardRow = Array.isArray(dashboardQuery) ? dashboardQuery[0] : (dashboardQuery?.rows?.[0] || {});

    // 4. Calculate what should be TRUE based on actual data
    const shouldBeTrue = await sql`
      SELECT
        COUNT(*)::int as should_be_48h,
        COUNT(*) FILTER (
          WHERE EXISTS (
            SELECT 1 FROM repcard_customer_attachments ca
            WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
          ) OR EXISTS (
            SELECT 1 FROM repcard_appointment_attachments aa
            WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
          )
        )::int as should_have_pb
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
      WHERE a.scheduled_at >= ${startDate}::date
        AND a.scheduled_at <= ${endDate}::date + INTERVAL '1 day'
        AND a.scheduled_at IS NOT NULL
        AND c.created_at IS NOT NULL
        -- Within 2 calendar days: same day (0), next day (1), or day after next (2)
        AND DATE((a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')) - DATE((c.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')) >= 0
        AND DATE((a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')) - DATE((c.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')) <= 2
    `;
    const shouldBeTrueRow = Array.isArray(shouldBeTrue) ? shouldBeTrue[0] : (shouldBeTrue?.rows?.[0] || {});

    return NextResponse.json({
      success: true,
      dateRange: { startDate, endDate },
      databaseState: dbStateRow,
      dashboardQueryResult: {
        ...dashboardRow,
        calculated48hPercentage: dashboardRow.total_appointments > 0 
          ? ((dashboardRow.within_48h / dashboardRow.total_appointments) * 100).toFixed(1) + '%'
          : '0%',
        calculatedPBPercentage: dashboardRow.total_appointments > 0
          ? ((dashboardRow.with_power_bill / dashboardRow.total_appointments) * 100).toFixed(1) + '%'
          : '0%',
      },
      shouldBeTrue: shouldBeTrueRow,
      sampleAppointments: samples.map((s: any) => ({
        id: s.id,
        scheduledAt: s.scheduled_at,
        customerCreatedAt: s.customer_created_at,
        daysDiff: s.days_diff,
        isWithin48h: s.is_within_48_hours,
        hasPowerBill: s.has_power_bill,
        customerAttCount: s.customer_att_count || 0,
        appointmentAttCount: s.appointment_att_count || 0,
        shouldBe48h: s.days_diff !== null && s.days_diff >= 0 && s.days_diff <= 2,
        shouldHavePB: (s.customer_att_count || 0) > 0 || (s.appointment_att_count || 0) > 0,
        mismatch48h: s.is_within_48_hours !== (s.days_diff !== null && s.days_diff >= 0 && s.days_diff <= 2),
        mismatchPB: s.has_power_bill !== ((s.customer_att_count || 0) > 0 || (s.appointment_att_count || 0) > 0),
      })),
      diagnosis: {
        hasNullValues: (dbStateRow.null_48h || 0) > 0 || (dbStateRow.null_pb || 0) > 0,
        null48hCount: dbStateRow.null_48h || 0,
        nullPBCount: dbStateRow.null_pb || 0,
        needsBackfill: (dbStateRow.null_48h || 0) > 0 || (dbStateRow.null_pb || 0) > 0,
        dataMismatch: {
          shouldBe48hButIsnt: (shouldBeTrueRow.should_be_48h || 0) - (dbStateRow.true_48h || 0),
          shouldHavePBButDoesnt: (shouldBeTrueRow.should_have_pb || 0) - (dbStateRow.true_pb || 0),
        }
      }
    });

  } catch (error) {
    console.error('[RepCard Diagnose Metrics] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to diagnose metrics',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
