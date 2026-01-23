import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { requireRole } from '@/lib/auth/middleware';

/**
 * GET /api/repcard/appointments/[appointmentId]/history
 *
 * Returns all previous appointments for the same customer, ordered by scheduled date
 * Excludes the current appointment
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { appointmentId: string } }
) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  try {
    logApiRequest('GET', path, { endpoint: 'appointment-history', requestId });

    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    const { appointmentId } = params;

    // First, get the current appointment to find the customer
    const currentAppointmentResult = await sql`
      SELECT 
        a.repcard_customer_id,
        a.repcard_appointment_id,
        a.scheduled_at
      FROM repcard_appointments a
      WHERE a.id = ${appointmentId}::text
         OR a.repcard_appointment_id = ${appointmentId}::text
      LIMIT 1
    `;

    const currentAppointment = Array.from(currentAppointmentResult)[0];

    if (!currentAppointment) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 404, cached: false, requestId });
      return NextResponse.json(
        { error: 'Appointment not found' },
        { status: 404 }
      );
    }

    // Get all previous appointments for the same customer
    // Exclude the current appointment and only show appointments with scheduled_at
    const previousAppointmentsResult = await sql`
      SELECT
        a.id,
        a.repcard_appointment_id,
        a.status_category,
        a.disposition,
        a.scheduled_at,
        a.completed_at,
        a.duration,
        a.notes,
        a.is_reschedule,
        a.reschedule_count,
        a.created_at,
        COALESCE(NULLIF(TRIM(setter.first_name || ' ' || setter.last_name), ''), 'Unassigned') as setter_name,
        COALESCE(NULLIF(TRIM(closer.first_name || ' ' || closer.last_name), ''), 'Unassigned') as closer_name,
        CASE 
          WHEN a.status_category = 'scheduled' OR a.status_category = 'completed' THEN TRUE
          WHEN a.raw_data->>'appointment_status_title' ILIKE '%confirmed%' THEN TRUE
          WHEN a.raw_data->>'status' ILIKE '%confirmed%' THEN TRUE
          ELSE FALSE
        END as is_confirmed
      FROM repcard_appointments a
      LEFT JOIN repcard_users setter ON setter.repcard_user_id::int = a.setter_user_id::int
      LEFT JOIN repcard_users closer ON closer.repcard_user_id::int = a.closer_user_id::int
      WHERE a.repcard_customer_id = ${currentAppointment.repcard_customer_id}
        AND a.scheduled_at IS NOT NULL
        AND (
          a.id != ${appointmentId}::text 
          AND a.repcard_appointment_id::text != ${appointmentId}::text
        )
        AND (
          -- Only show appointments scheduled before the current one, or if current has no scheduled_at, show all
          ${currentAppointment.scheduled_at ? sql`a.scheduled_at < ${currentAppointment.scheduled_at}` : sql`TRUE`}
        )
      ORDER BY a.scheduled_at DESC
      LIMIT 10
    `;

    const previousAppointments = Array.from(previousAppointmentsResult);

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { 
      status: 200, 
      cached: false, 
      requestId,
      appointmentId,
      customerId: currentAppointment.repcard_customer_id,
      previousCount: previousAppointments.length
    });

    return NextResponse.json({
      previousAppointments,
      currentAppointment: {
        id: currentAppointment.repcard_appointment_id,
        scheduled_at: currentAppointment.scheduled_at
      }
    });

  } catch (error) {
    logError('repcard-appointment-history-error', {
      requestId,
      error: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    });

    return NextResponse.json(
      { error: 'Failed to fetch appointment history' },
      { status: 500 }
    );
  }
}
