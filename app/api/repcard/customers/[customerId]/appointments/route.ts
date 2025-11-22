import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/customers/[customerId]/appointments
 *
 * Returns all appointments for a specific customer, including reschedule information
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { customerId: string } }
) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  try {
    logApiRequest('GET', path, { endpoint: 'customer-appointments', requestId });

    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    const { customerId } = params;

    // Query appointments for this customer
    const appointmentsResult = await sql`
      SELECT
        a.id,
        a.repcard_appointment_id,
        a.repcard_customer_id,
        a.setter_user_id,
        a.closer_user_id,
        a.disposition,
        a.status_category,
        a.scheduled_at,
        a.completed_at,
        a.duration,
        a.notes,
        a.is_within_48_hours,
        a.is_reschedule,
        a.reschedule_count,
        a.original_appointment_id,
        a.created_at,
        a.updated_at,
        -- Get setter info
        setter.name as setter_name,
        setter.email as setter_email,
        -- Get closer info
        closer.name as closer_name,
        closer.email as closer_email,
        -- Get customer info
        c.first_name as customer_first_name,
        c.last_name as customer_last_name,
        c.email as customer_email,
        c.phone as customer_phone,
        c.address as customer_address
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON c.id = ${customerId}
      LEFT JOIN users setter ON setter.repcard_user_id::TEXT = a.setter_user_id
      LEFT JOIN users closer ON closer.repcard_user_id::TEXT = a.closer_user_id
      WHERE a.customer_id = ${customerId}
      ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
    `;

    const appointments = Array.from(appointmentsResult);

    if (appointments.length === 0) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 404, cached: false, requestId });
      return NextResponse.json(
        { error: 'Customer not found or has no appointments' },
        { status: 404 }
      );
    }

    // Get customer info from first appointment
    const customer = appointments[0] ? {
      repcardCustomerId: appointments[0].repcard_customer_id,
      firstName: appointments[0].customer_first_name,
      lastName: appointments[0].customer_last_name,
      email: appointments[0].customer_email,
      phone: appointments[0].customer_phone,
      address: appointments[0].customer_address,
    } : null;

    // Calculate summary statistics
    const totalAppointments = appointments.length;
    const originalAppointments = appointments.filter((a: any) => !a.is_reschedule).length;
    const reschedules = appointments.filter((a: any) => a.is_reschedule).length;
    const completed = appointments.filter((a: any) => a.completed_at).length;
    const cancelled = appointments.filter((a: any) =>
      a.disposition?.toLowerCase().includes('cancel')
    ).length;
    const noShow = appointments.filter((a: any) =>
      a.disposition?.toLowerCase().includes('no') && a.disposition?.toLowerCase().includes('show')
    ).length;

    // Find appointment chains (original -> reschedules)
    const chains: any[] = [];
    const originalAppts = appointments.filter((a: any) => !a.is_reschedule);

    originalAppts.forEach((original: any) => {
      const relatedReschedules = appointments.filter((a: any) =>
        a.original_appointment_id === original.id
      );

      chains.push({
        original: {
          id: original.id,
          repcardAppointmentId: original.repcard_appointment_id,
          scheduledAt: original.scheduled_at,
          disposition: original.disposition,
          statusCategory: original.status_category,
          setterName: original.setter_name,
          closerName: original.closer_name,
        },
        reschedules: relatedReschedules.map((r: any) => ({
          id: r.id,
          repcardAppointmentId: r.repcard_appointment_id,
          scheduledAt: r.scheduled_at,
          disposition: r.disposition,
          statusCategory: r.status_category,
          rescheduleCount: r.reschedule_count,
          setterName: r.setter_name,
          closerName: r.closer_name,
        })),
        totalReschedules: relatedReschedules.length,
      });
    });

    // Format appointments for response
    const formattedAppointments = appointments.map((a: any) => ({
      id: a.id,
      repcardAppointmentId: a.repcard_appointment_id,
      scheduledAt: a.scheduled_at,
      completedAt: a.completed_at,
      duration: a.duration,
      disposition: a.disposition,
      statusCategory: a.status_category,
      notes: a.notes,
      isWithin48Hours: a.is_within_48_hours,
      isReschedule: a.is_reschedule,
      rescheduleCount: a.reschedule_count,
      originalAppointmentId: a.original_appointment_id,
      setter: a.setter_user_id ? {
        id: a.setter_user_id,
        name: a.setter_name,
        email: a.setter_email,
      } : null,
      closer: a.closer_user_id ? {
        id: a.closer_user_id,
        name: a.closer_name,
        email: a.closer_email,
      } : null,
      createdAt: a.created_at,
      updatedAt: a.updated_at,
    }));

    const response = {
      customer,
      appointments: formattedAppointments,
      chains,
      summary: {
        totalAppointments,
        originalAppointments,
        reschedules,
        rescheduleRate: totalAppointments > 0 ? (reschedules / totalAppointments * 100).toFixed(1) : '0.0',
        completed,
        cancelled,
        noShow,
      },
      metadata: {
        customerId,
        count: appointments.length,
        fetchedAt: new Date().toISOString(),
      },
    };

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(response);

  } catch (error) {
    const duration = Date.now() - start;
    logError('customer-appointments', error as Error, { requestId, customerId: params.customerId });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
