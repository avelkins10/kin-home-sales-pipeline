import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getAssignedOffices } from '@/lib/quickbase/queries';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/appointments/schedule
 * 
 * Get appointment schedule with full details including setter/closer, customer, calendar, attachments, and reschedule info
 * 
 * Query params:
 * - startDate: YYYY-MM-DD (default: today)
 * - endDate: YYYY-MM-DD (default: today + 7 days)
 * - officeIds: comma-separated office IDs (for leaders)
 * - teamIds: comma-separated team IDs (for team filtering)
 * - calendarId: specific calendar ID to filter by
 * - status: filter by status_category (scheduled, completed, cancelled, rescheduled, no_show)
 * - hasPowerBill: filter by power bill presence (true/false)
 * - isReschedule: filter by reschedule status (true/false)
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  try {
    logApiRequest('GET', path, { endpoint: 'repcard-appointments-schedule', requestId });

    // Authentication
    const session = await getServerSession(authOptions);
    if (!session) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 401, cached: false, requestId });
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const userId = (session.user as any).id as string;
    const userRole = session.user.role;

    // Validate role
    const allowedRoles = ['closer', 'setter', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin', 'team_lead'];
    if (!allowedRoles.includes(userRole)) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 403, cached: false, requestId });
      return NextResponse.json({ error: 'Forbidden - insufficient permissions' }, { status: 403 });
    }

    const { searchParams } = new URL(request.url);
    
    // Date range (default: today to 7 days from now)
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const defaultEndDate = new Date(today);
    defaultEndDate.setDate(defaultEndDate.getDate() + 7);
    
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');
    const startDate = startDateParam || today.toISOString().split('T')[0];
    const endDate = endDateParam || defaultEndDate.toISOString().split('T')[0];

    // Filters
    const officeIdsParam = searchParams.get('officeIds');
    const officeIds = officeIdsParam ? officeIdsParam.split(',').map(id => parseInt(id.trim())).filter(Boolean) : undefined;
    
    const teamIdsParam = searchParams.get('teamIds');
    const teamIds = teamIdsParam ? teamIdsParam.split(',').map(id => parseInt(id.trim())).filter(Boolean) : undefined;
    
    const calendarIdParam = searchParams.get('calendarId');
    const calendarId = calendarIdParam ? parseInt(calendarIdParam) : undefined;
    
    const statusFilter = searchParams.get('status');
    const hasPowerBillFilter = searchParams.get('hasPowerBill');
    const isRescheduleFilter = searchParams.get('isReschedule');

    // Get RepCard user ID for closers (as integer)
    let repcardUserId: number | undefined;
    if (userRole === 'closer') {
      const userResult = await sql`
        SELECT repcard_user_id FROM users WHERE id = ${userId}
      `;
      const userRows = Array.from(userResult);
      if (userRows.length > 0 && userRows[0].repcard_user_id) {
        repcardUserId = parseInt(userRows[0].repcard_user_id) || undefined;
      }
    }

    // Get office IDs for leaders (if not provided)
    let effectiveOfficeIds = officeIds;
    if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(userRole)) {
      effectiveOfficeIds = await getAssignedOffices(userId);
    }

    // Build query based on role and filters - use separate queries to avoid parameter binding issues
    let result;
    
    if (userRole === 'closer' && repcardUserId) {
      // Closer query - see only their appointments
      result = await sql`
        SELECT 
          a.id,
          a.repcard_appointment_id,
          a.customer_id,
          a.repcard_customer_id,
          a.setter_user_id,
          a.closer_user_id,
          a.office_id,
          a.disposition,
          a.status_category,
          a.scheduled_at,
          a.completed_at,
          a.duration,
          a.notes,
          a.is_within_48_hours,
          a.has_power_bill,
          a.is_reschedule,
          a.reschedule_count,
          a.original_appointment_id,
          a.created_at,
          a.updated_at,
          (a.raw_data->>'calendarId')::int as calendar_id,
          setter.first_name || ' ' || setter.last_name as setter_name,
          setter.email as setter_email,
          setter.team_id as setter_team_id,
          setter.team_name as setter_team_name,
          closer.first_name || ' ' || closer.last_name as closer_name,
          closer.email as closer_email,
          closer.team_id as closer_team_id,
          closer.team_name as closer_team_name,
          c.name as customer_name,
          c.phone as customer_phone,
          c.address as customer_address,
          c.email as customer_email,
          cal.name as calendar_name,
          cal.status as calendar_status,
          COALESCE(closer_team.team_name, setter_team.team_name) as team_name,
          COALESCE(closer_team.repcard_team_id, setter_team.repcard_team_id) as team_id,
          (SELECT COUNT(*)::int FROM repcard_customer_attachments WHERE repcard_customer_id::text = a.repcard_customer_id::text) as customer_attachment_count,
          (SELECT COUNT(*)::int FROM repcard_appointment_attachments WHERE repcard_appointment_id::text = a.repcard_appointment_id::text) as appointment_attachment_count
        FROM repcard_appointments a
        LEFT JOIN repcard_users setter ON setter.repcard_user_id = a.setter_user_id
        LEFT JOIN repcard_users closer ON closer.repcard_user_id = a.closer_user_id
        LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
        LEFT JOIN repcard_calendars cal ON cal.repcard_calendar_id = (a.raw_data->>'calendarId')::int
        LEFT JOIN repcard_teams setter_team ON setter_team.repcard_team_id = setter.team_id
        LEFT JOIN repcard_teams closer_team ON closer_team.repcard_team_id = closer.team_id
        WHERE (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${startDate}::date AND a.scheduled_at::date <= ${endDate}::date)
          OR
          (a.scheduled_at IS NULL AND a.created_at::date >= ${startDate}::date AND a.created_at::date <= ${endDate}::date)
        )
        AND a.closer_user_id = ${repcardUserId}
        ${teamIds && teamIds.length > 0 ? sql`AND (setter.team_id = ANY(${teamIds}::int[]) OR closer.team_id = ANY(${teamIds}::int[]))` : sql``}
        ${calendarId ? sql`AND (a.raw_data->>'calendarId')::int = ${calendarId}` : sql``}
        ${statusFilter ? sql`AND a.status_category = ${statusFilter}` : sql``}
        ${hasPowerBillFilter === 'true' ? sql`AND a.has_power_bill = TRUE` : hasPowerBillFilter === 'false' ? sql`AND (a.has_power_bill = FALSE OR a.has_power_bill IS NULL)` : sql``}
        ${isRescheduleFilter === 'true' ? sql`AND a.is_reschedule = TRUE` : isRescheduleFilter === 'false' ? sql`AND (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)` : sql``}
        ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
      `;
    } else if (effectiveOfficeIds && effectiveOfficeIds.length > 0) {
      // Leader query with office filter
      result = await sql`
        SELECT 
          a.id,
          a.repcard_appointment_id,
          a.customer_id,
          a.repcard_customer_id,
          a.setter_user_id,
          a.closer_user_id,
          a.office_id,
          a.disposition,
          a.status_category,
          a.scheduled_at,
          a.completed_at,
          a.duration,
          a.notes,
          a.is_within_48_hours,
          a.has_power_bill,
          a.is_reschedule,
          a.reschedule_count,
          a.original_appointment_id,
          a.created_at,
          a.updated_at,
          (a.raw_data->>'calendarId')::int as calendar_id,
          setter.first_name || ' ' || setter.last_name as setter_name,
          setter.email as setter_email,
          setter.team_id as setter_team_id,
          setter.team_name as setter_team_name,
          closer.first_name || ' ' || closer.last_name as closer_name,
          closer.email as closer_email,
          closer.team_id as closer_team_id,
          closer.team_name as closer_team_name,
          c.name as customer_name,
          c.phone as customer_phone,
          c.address as customer_address,
          c.email as customer_email,
          cal.name as calendar_name,
          cal.status as calendar_status,
          COALESCE(closer_team.team_name, setter_team.team_name) as team_name,
          COALESCE(closer_team.repcard_team_id, setter_team.repcard_team_id) as team_id,
          (SELECT COUNT(*)::int FROM repcard_customer_attachments WHERE repcard_customer_id::text = a.repcard_customer_id::text) as customer_attachment_count,
          (SELECT COUNT(*)::int FROM repcard_appointment_attachments WHERE repcard_appointment_id::text = a.repcard_appointment_id::text) as appointment_attachment_count
        FROM repcard_appointments a
        LEFT JOIN repcard_users setter ON setter.repcard_user_id = a.setter_user_id
        LEFT JOIN repcard_users closer ON closer.repcard_user_id = a.closer_user_id
        LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
        LEFT JOIN repcard_calendars cal ON cal.repcard_calendar_id = (a.raw_data->>'calendarId')::int
        LEFT JOIN repcard_teams setter_team ON setter_team.repcard_team_id = setter.team_id
        LEFT JOIN repcard_teams closer_team ON closer_team.repcard_team_id = closer.team_id
        WHERE (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${startDate}::date AND a.scheduled_at::date <= ${endDate}::date)
          OR
          (a.scheduled_at IS NULL AND a.created_at::date >= ${startDate}::date AND a.created_at::date <= ${endDate}::date)
        )
        AND a.office_id = ANY(${effectiveOfficeIds}::int[])
        ${teamIds && teamIds.length > 0 ? sql`AND (setter.team_id = ANY(${teamIds}::int[]) OR closer.team_id = ANY(${teamIds}::int[]))` : sql``}
        ${calendarId ? sql`AND (a.raw_data->>'calendarId')::int = ${calendarId}` : sql``}
        ${statusFilter ? sql`AND a.status_category = ${statusFilter}` : sql``}
        ${hasPowerBillFilter === 'true' ? sql`AND a.has_power_bill = TRUE` : hasPowerBillFilter === 'false' ? sql`AND (a.has_power_bill = FALSE OR a.has_power_bill IS NULL)` : sql``}
        ${isRescheduleFilter === 'true' ? sql`AND a.is_reschedule = TRUE` : isRescheduleFilter === 'false' ? sql`AND (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)` : sql``}
        ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
      `;
    } else if (userRole === 'super_admin' || userRole === 'regional') {
      // Super admin/regional - see all appointments
      result = await sql`
        SELECT 
          a.id,
          a.repcard_appointment_id,
          a.customer_id,
          a.repcard_customer_id,
          a.setter_user_id,
          a.closer_user_id,
          a.office_id,
          a.disposition,
          a.status_category,
          a.scheduled_at,
          a.completed_at,
          a.duration,
          a.notes,
          a.is_within_48_hours,
          a.has_power_bill,
          a.is_reschedule,
          a.reschedule_count,
          a.original_appointment_id,
          a.created_at,
          a.updated_at,
          (a.raw_data->>'calendarId')::int as calendar_id,
          setter.first_name || ' ' || setter.last_name as setter_name,
          setter.email as setter_email,
          setter.team_id as setter_team_id,
          setter.team_name as setter_team_name,
          closer.first_name || ' ' || closer.last_name as closer_name,
          closer.email as closer_email,
          closer.team_id as closer_team_id,
          closer.team_name as closer_team_name,
          c.name as customer_name,
          c.phone as customer_phone,
          c.address as customer_address,
          c.email as customer_email,
          cal.name as calendar_name,
          cal.status as calendar_status,
          COALESCE(closer_team.team_name, setter_team.team_name) as team_name,
          COALESCE(closer_team.repcard_team_id, setter_team.repcard_team_id) as team_id,
          (SELECT COUNT(*)::int FROM repcard_customer_attachments WHERE repcard_customer_id::text = a.repcard_customer_id::text) as customer_attachment_count,
          (SELECT COUNT(*)::int FROM repcard_appointment_attachments WHERE repcard_appointment_id::text = a.repcard_appointment_id::text) as appointment_attachment_count
        FROM repcard_appointments a
        LEFT JOIN repcard_users setter ON setter.repcard_user_id = a.setter_user_id
        LEFT JOIN repcard_users closer ON closer.repcard_user_id = a.closer_user_id
        LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
        LEFT JOIN repcard_calendars cal ON cal.repcard_calendar_id = (a.raw_data->>'calendarId')::int
        LEFT JOIN repcard_teams setter_team ON setter_team.repcard_team_id = setter.team_id
        LEFT JOIN repcard_teams closer_team ON closer_team.repcard_team_id = closer.team_id
        WHERE (
          (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${startDate}::date AND a.scheduled_at::date <= ${endDate}::date)
          OR
          (a.scheduled_at IS NULL AND a.created_at::date >= ${startDate}::date AND a.created_at::date <= ${endDate}::date)
        )
        ${teamIds && teamIds.length > 0 ? sql`AND (setter.team_id = ANY(${teamIds}::int[]) OR closer.team_id = ANY(${teamIds}::int[]))` : sql``}
        ${calendarId ? sql`AND (a.raw_data->>'calendarId')::int = ${calendarId}` : sql``}
        ${statusFilter ? sql`AND a.status_category = ${statusFilter}` : sql``}
        ${hasPowerBillFilter === 'true' ? sql`AND a.has_power_bill = TRUE` : hasPowerBillFilter === 'false' ? sql`AND (a.has_power_bill = FALSE OR a.has_power_bill IS NULL)` : sql``}
        ${isRescheduleFilter === 'true' ? sql`AND a.is_reschedule = TRUE` : isRescheduleFilter === 'false' ? sql`AND (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)` : sql``}
        ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
      `;
    } else {
      // No access - return empty result
      result = await sql`
        SELECT 
          a.id,
          a.repcard_appointment_id,
          a.customer_id,
          a.repcard_customer_id,
          a.setter_user_id,
          a.closer_user_id,
          a.office_id,
          a.disposition,
          a.status_category,
          a.scheduled_at,
          a.completed_at,
          a.duration,
          a.notes,
          a.is_within_48_hours,
          a.has_power_bill,
          a.is_reschedule,
          a.reschedule_count,
          a.original_appointment_id,
          a.created_at,
          a.updated_at,
          (a.raw_data->>'calendarId')::int as calendar_id,
          setter.first_name || ' ' || setter.last_name as setter_name,
          setter.email as setter_email,
          setter.team_id as setter_team_id,
          setter.team_name as setter_team_name,
          closer.first_name || ' ' || closer.last_name as closer_name,
          closer.email as closer_email,
          closer.team_id as closer_team_id,
          closer.team_name as closer_team_name,
          c.name as customer_name,
          c.phone as customer_phone,
          c.address as customer_address,
          c.email as customer_email,
          cal.name as calendar_name,
          cal.status as calendar_status,
          COALESCE(closer_team.team_name, setter_team.team_name) as team_name,
          COALESCE(closer_team.repcard_team_id, setter_team.repcard_team_id) as team_id,
          (SELECT COUNT(*)::int FROM repcard_customer_attachments WHERE repcard_customer_id::text = a.repcard_customer_id::text) as customer_attachment_count,
          (SELECT COUNT(*)::int FROM repcard_appointment_attachments WHERE repcard_appointment_id::text = a.repcard_appointment_id::text) as appointment_attachment_count
        FROM repcard_appointments a
        LEFT JOIN repcard_users setter ON setter.repcard_user_id = a.setter_user_id
        LEFT JOIN repcard_users closer ON closer.repcard_user_id = a.closer_user_id
        LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
        LEFT JOIN repcard_calendars cal ON cal.repcard_calendar_id = (a.raw_data->>'calendarId')::int
        LEFT JOIN repcard_teams setter_team ON setter_team.repcard_team_id = setter.team_id
        LEFT JOIN repcard_teams closer_team ON closer_team.repcard_team_id = closer.team_id
        WHERE 1=0
        ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
      `;
    }
    const appointments = Array.from(result);

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });

    return NextResponse.json({
      appointments,
      count: appointments.length,
      filters: {
        startDate,
        endDate,
        officeIds: effectiveOfficeIds,
        teamIds,
        calendarId,
        status: statusFilter,
        hasPowerBill: hasPowerBillFilter,
        isReschedule: isRescheduleFilter
      },
      metadata: {
        userId,
        role: userRole,
        repcardUserId,
        requestId
      }
    });

  } catch (error) {
    const duration = Date.now() - start;
    logError('repcard-appointments-schedule', error as Error, { requestId });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
