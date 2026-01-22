import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/debug-metric
 * 
 * Enhanced debugging endpoint to understand why a specific metric is TRUE/FALSE
 * Shows calculation history, raw data, and audit trail
 */
export async function GET(request: NextRequest) {
  try {
    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    const { searchParams } = new URL(request.url);
    const appointmentId = searchParams.get('appointmentId');
    const metric = searchParams.get('metric') || 'is_within_48_hours';

    if (!appointmentId) {
      return NextResponse.json(
        { success: false, error: 'appointmentId parameter is required' },
        { status: 400 }
      );
    }

    if (!['is_within_48_hours', 'has_power_bill'].includes(metric)) {
      return NextResponse.json(
        { success: false, error: 'metric must be is_within_48_hours or has_power_bill' },
        { status: 400 }
      );
    }

    // Get appointment details
    const appointmentResult = await sql`
      SELECT 
        a.id,
        a.repcard_appointment_id,
        a.repcard_customer_id,
        a.scheduled_at,
        a.is_within_48_hours,
        a.has_power_bill,
        c.created_at as customer_created_at,
        CASE
          WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL THEN
            EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600
          ELSE NULL
        END as hours_diff,
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
      WHERE a.id = ${appointmentId}::text
         OR a.repcard_appointment_id = ${appointmentId}::text
      LIMIT 1
    `;
    const appointment = Array.isArray(appointmentResult) 
      ? appointmentResult[0] 
      : (appointmentResult?.rows?.[0] || null);

    if (!appointment) {
      return NextResponse.json(
        { success: false, error: 'Appointment not found' },
        { status: 404 }
      );
    }

    // Get audit history for this metric
    const auditResult = await sql`
      SELECT 
        id,
        metric_value,
        calculation_reason,
        raw_data,
        calculated_at,
        calculated_by
      FROM repcard_metric_audit
      WHERE appointment_id = ${appointment.id}::text
        AND metric_name = ${metric}
      ORDER BY calculated_at DESC
      LIMIT 10
    `;
    const auditHistory = Array.isArray(auditResult) 
      ? auditResult 
      : (auditResult?.rows || []);

    // Calculate what the value SHOULD be
    let shouldBeValue: boolean;
    let shouldBeReason: string;
    let rawData: any = {};

    if (metric === 'is_within_48_hours') {
      if (appointment.scheduled_at && appointment.customer_created_at && appointment.hours_diff !== null) {
        shouldBeValue = appointment.hours_diff >= 0 && appointment.hours_diff <= 48;
        shouldBeReason = `scheduled_at - customer.created_at = ${appointment.hours_diff.toFixed(1)} hours`;
        rawData = {
          scheduledAt: appointment.scheduled_at,
          customerCreatedAt: appointment.customer_created_at,
          hoursDiff: appointment.hours_diff
        };
      } else {
        shouldBeValue = false;
        shouldBeReason = appointment.scheduled_at 
          ? 'customer.created_at is NULL or customer not found'
          : 'scheduled_at is NULL';
        rawData = {
          scheduledAt: appointment.scheduled_at,
          customerCreatedAt: appointment.customer_created_at
        };
      }
    } else { // has_power_bill
      const hasAttachments = (appointment.customer_attachment_count || 0) > 0 || 
                            (appointment.appointment_attachment_count || 0) > 0;
      shouldBeValue = hasAttachments;
      shouldBeReason = hasAttachments
        ? `Found ${appointment.customer_attachment_count || 0} customer attachment(s) and ${appointment.appointment_attachment_count || 0} appointment attachment(s) (any attachment = power bill)`
        : 'No attachments found for customer or appointment';
      rawData = {
        customerAttachmentCount: appointment.customer_attachment_count || 0,
        appointmentAttachmentCount: appointment.appointment_attachment_count || 0
      };
    }

    const currentValue = metric === 'is_within_48_hours' 
      ? appointment.is_within_48_hours 
      : appointment.has_power_bill;

    const isCorrect = currentValue === shouldBeValue;

    return NextResponse.json({
      success: true,
      appointment: {
        id: appointment.id,
        repcardAppointmentId: appointment.repcard_appointment_id,
        scheduledAt: appointment.scheduled_at,
        customerCreatedAt: appointment.customer_created_at,
      },
      metric: {
        name: metric,
        currentValue: currentValue,
        shouldBeValue: shouldBeValue,
        isCorrect: isCorrect,
        shouldBeReason: shouldBeReason,
        rawData: rawData
      },
      auditHistory: auditHistory.map((a: any) => ({
        calculatedAt: a.calculated_at,
        value: a.metric_value,
        reason: a.calculation_reason,
        rawData: a.raw_data,
        calculatedBy: a.calculated_by
      })),
      diagnosis: {
        matchesExpected: isCorrect,
        issue: !isCorrect 
          ? `Stored value (${currentValue}) does not match calculated value (${shouldBeValue}). ${shouldBeReason}`
          : 'Metric value is correct',
        recommendation: !isCorrect
          ? 'Run backfill to recalculate this appointment using trigger logic'
          : 'No action needed'
      }
    });

  } catch (error) {
    console.error('[RepCard Debug Metric] Error:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to debug metric',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
