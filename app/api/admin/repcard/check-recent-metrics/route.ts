import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * GET /api/admin/repcard/check-recent-metrics
 * 
 * Quick check to see why recent appointments show 0% metrics
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    // Check last 30 days
    const thirtyDaysAgo = new Date();
    thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);

    // Get sample appointments with their calculated values
    const sample = await sql`
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
            -- Use Eastern Time for calculation to match trigger logic
            EXTRACT(EPOCH FROM (
              (a.scheduled_at::timestamptz AT TIME ZONE 'America/New_York') - 
              (c.created_at::timestamptz AT TIME ZONE 'America/New_York')
            )) / 3600
          ELSE NULL
        END as hours_diff,
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
      WHERE a.scheduled_at >= ${thirtyDaysAgo.toISOString()}
      ORDER BY a.scheduled_at DESC
      LIMIT 20
    `;
    const samples = Array.isArray(sample) ? sample : (sample?.rows || []);

    // Get audit trail for these appointments
    const auditCheck = await sql`
      SELECT 
        appointment_id,
        metric_name,
        metric_value,
        calculation_reason,
        calculated_at
      FROM repcard_metric_audit
      WHERE appointment_id IN (${sql(samples.map((s: any) => s.id))})
      ORDER BY calculated_at DESC
      LIMIT 50
    `;
    const audits = Array.isArray(auditCheck) ? auditCheck : (auditCheck?.rows || []);

    return NextResponse.json({
      success: true,
      sampleAppointments: samples.map((s: any) => ({
        id: s.id,
        scheduledAt: s.scheduled_at,
        customerCreatedAt: s.customer_created_at,
        hoursDiff: s.hours_diff,
        stored48h: s.is_within_48_hours,
        storedPB: s.has_power_bill,
        customerAttCount: s.customer_att_count || 0,
        appointmentAttCount: s.appointment_att_count || 0,
        shouldBe48h: s.hours_diff !== null && s.hours_diff >= 0 && s.hours_diff <= 48 && s.hours_diff >= 0,
        shouldHavePB: (s.customer_att_count || 0) > 0 || (s.appointment_att_count || 0) > 0,
        hasRequiredData: s.scheduled_at !== null && s.repcard_customer_id !== null && s.customer_created_at !== null,
      })),
      auditTrail: audits,
      summary: {
        totalSamples: samples.length,
        withRequiredData: samples.filter((s: any) => s.scheduled_at !== null && s.repcard_customer_id !== null && s.customer_created_at !== null).length,
        shouldBe48h: samples.filter((s: any) => {
          const hours = s.hours_diff;
          return hours !== null && hours >= 0 && hours <= 48;
        }).length,
        actually48h: samples.filter((s: any) => s.is_within_48_hours === true).length,
        shouldHavePB: samples.filter((s: any) => (s.customer_att_count || 0) > 0 || (s.appointment_att_count || 0) > 0).length,
        actuallyHasPB: samples.filter((s: any) => s.has_power_bill === true).length,
      }
    });

  } catch (error) {
    console.error('[Check Recent Metrics] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to check recent metrics',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
