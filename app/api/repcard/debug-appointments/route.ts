import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/debug-appointments
 * 
 * Debug endpoint to check specific appointments and their quality metrics
 * Helps diagnose why 48h speed and power bill show 0%
 */
export async function GET(request: NextRequest) {
  try {
    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');

    // Get detailed appointment data with quality metrics
    const appointmentsResult = startDate && endDate
      ? await sql`
          SELECT
            a.id,
            a.repcard_appointment_id,
            a.setter_user_id,
            a.repcard_customer_id,
            a.scheduled_at,
            a.is_within_48_hours,
            a.has_power_bill,
            c.created_at as customer_created_at,
            u.name as setter_name,
            -- Calculate time difference
            CASE
              WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
                EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600
              ELSE NULL
            END as hours_diff,
            -- Check for attachments
            (
              SELECT COUNT(*)::int
              FROM repcard_customer_attachments ca
              WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
            ) as customer_attachment_count,
            (
              SELECT COUNT(*)::int
              FROM repcard_appointment_attachments aa
              WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
            ) as appointment_attachment_count
          FROM repcard_appointments a
          LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
          LEFT JOIN users u ON u.repcard_user_id::text = a.setter_user_id
          WHERE a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          ORDER BY a.scheduled_at DESC
          LIMIT 50
        `
      : await sql`
          SELECT
            a.id,
            a.repcard_appointment_id,
            a.setter_user_id,
            a.repcard_customer_id,
            a.scheduled_at,
            a.is_within_48_hours,
            a.has_power_bill,
            c.created_at as customer_created_at,
            u.name as setter_name,
            -- Calculate time difference
            CASE
              WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
                EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600
              ELSE NULL
            END as hours_diff,
            -- Check for attachments
            (
              SELECT COUNT(*)::int
              FROM repcard_customer_attachments ca
              WHERE ca.repcard_customer_id::text = a.repcard_customer_id::text
            ) as customer_attachment_count,
            (
              SELECT COUNT(*)::int
              FROM repcard_appointment_attachments aa
              WHERE aa.repcard_appointment_id::text = a.repcard_appointment_id::text
            ) as appointment_attachment_count
          FROM repcard_appointments a
          LEFT JOIN repcard_customers c ON c.repcard_customer_id::text = a.repcard_customer_id::text
          LEFT JOIN users u ON u.repcard_user_id::text = a.setter_user_id
          ORDER BY a.scheduled_at DESC
          LIMIT 50
        `;

    const appointments = Array.isArray(appointmentsResult) 
      ? appointmentsResult 
      : (appointmentsResult?.rows || []);

    // Summary stats
    const summary = {
      total: appointments.length,
      with48h: appointments.filter((a: any) => a.is_within_48_hours === true).length,
      withPB: appointments.filter((a: any) => a.has_power_bill === true).length,
      withBoth: appointments.filter((a: any) => a.is_within_48_hours === true && a.has_power_bill === true).length,
      null48h: appointments.filter((a: any) => a.is_within_48_hours === null).length,
      nullPB: appointments.filter((a: any) => a.has_power_bill === null).length,
      withAttachments: appointments.filter((a: any) => 
        (a.customer_attachment_count || 0) > 0 || (a.appointment_attachment_count || 0) > 0
      ).length,
      within48hByCalc: appointments.filter((a: any) => 
        a.hours_diff !== null && a.hours_diff >= 0 && a.hours_diff <= 48
      ).length,
    };

    return NextResponse.json({
      success: true,
      summary,
      appointments: appointments.map((a: any) => ({
        id: a.id,
        repcardAppointmentId: a.repcard_appointment_id,
        setter: a.setter_name,
        scheduledAt: a.scheduled_at,
        customerCreatedAt: a.customer_created_at,
        hoursDiff: a.hours_diff,
        isWithin48h: a.is_within_48_hours,
        hasPowerBill: a.has_power_bill,
        customerAttachmentCount: a.customer_attachment_count || 0,
        appointmentAttachmentCount: a.appointment_attachment_count || 0,
        shouldBe48h: a.hours_diff !== null && a.hours_diff >= 0 && a.hours_diff <= 48,
        shouldHavePB: (a.customer_attachment_count || 0) > 0 || (a.appointment_attachment_count || 0) > 0,
      })),
      dateRange: { startDate, endDate },
    });

  } catch (error) {
    console.error('[RepCard Debug Appointments] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to fetch debug data',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
