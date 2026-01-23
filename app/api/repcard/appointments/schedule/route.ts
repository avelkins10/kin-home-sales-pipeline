import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { getAssignedOffices } from '@/lib/quickbase/queries';
import { toEasternStart, toEasternEnd } from '@/lib/utils/timezone';

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

  // Declare variables at function scope for error handling
  let userId: string | undefined;
  let userRole: string | undefined;
  let startDate: string | undefined;
  let endDate: string | undefined;
  let teamIds: number[] | undefined;
  let calendarId: number | undefined;
  let statusFilter: string | null = null;
  let hasPowerBillFilter: string | null = null;
  let isRescheduleFilter: string | null = null;
  let repcardUserId: number | undefined;
  let effectiveOfficeIds: number[] | undefined;

  try {
    logApiRequest('GET', path, { endpoint: 'repcard-appointments-schedule', requestId });

    // Authentication
    const session = await getServerSession(authOptions);
    if (!session) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 401, cached: false, requestId });
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    userId = (session.user as any).id as string;
    userRole = session.user.role;

    // Validate role
    const allowedRoles = ['closer', 'setter', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin', 'team_lead'];
    if (!allowedRoles.includes(userRole)) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 403, cached: false, requestId });
      return NextResponse.json({ error: 'Forbidden - insufficient permissions' }, { status: 403 });
    }

    const { searchParams } = new URL(request.url);
    
    // Date range (default: today to 7 days from now)
    // Dates come from frontend in YYYY-MM-DD format (user's local timezone)
    // We need to convert them to Eastern Time for database queries
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const defaultEndDate = new Date(today);
    defaultEndDate.setDate(defaultEndDate.getDate() + 7);
    
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');
    const startDateStr = startDateParam || today.toISOString().split('T')[0];
    const endDateStr = endDateParam || defaultEndDate.toISOString().split('T')[0];
    
    // Convert to Eastern Time for database queries
    // Store as strings for use in SQL queries with timezone conversion
    startDate = startDateStr;
    endDate = endDateStr;

    // Filters
    const officeIdsParam = searchParams.get('officeIds');
    const officeIds = officeIdsParam ? officeIdsParam.split(',').map(id => parseInt(id.trim())).filter(Boolean) : undefined;
    
    const teamIdsParam = searchParams.get('teamIds');
    teamIds = teamIdsParam ? teamIdsParam.split(',').map(id => parseInt(id.trim())).filter(Boolean) : undefined;
    
    const calendarIdParam = searchParams.get('calendarId');
    calendarId = calendarIdParam ? parseInt(calendarIdParam) : undefined;
    
    statusFilter = searchParams.get('status');
    hasPowerBillFilter = searchParams.get('hasPowerBill');
    isRescheduleFilter = searchParams.get('isReschedule');

    // Get RepCard user ID for closers (as integer)
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
    effectiveOfficeIds = officeIds;
    if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(userRole)) {
      effectiveOfficeIds = await getAssignedOffices(userId);
    }

    // Build WHERE conditions - build as single fragment only including active filters
    // This avoids creating parameters for missing conditions
    const buildAdditionalWhere = () => {
      const conditions: any[] = [];
      if (teamIds && teamIds.length > 0) {
        conditions.push(sql`AND (setter.team_id = ANY(${teamIds}::int[]) OR closer.team_id = ANY(${teamIds}::int[]))`);
      }
      if (calendarId) {
        conditions.push(sql`AND (a.raw_data->>'calendarId')::int = ${calendarId}`);
      }
      if (statusFilter) {
        conditions.push(sql`AND a.status_category = ${statusFilter}`);
      }
      if (hasPowerBillFilter === 'true') {
        conditions.push(sql`AND a.has_power_bill = TRUE`);
      } else if (hasPowerBillFilter === 'false') {
        conditions.push(sql`AND (a.has_power_bill = FALSE OR a.has_power_bill IS NULL)`);
      }
      if (isRescheduleFilter === 'true') {
        conditions.push(sql`AND a.is_reschedule = TRUE`);
      } else if (isRescheduleFilter === 'false') {
        conditions.push(sql`AND (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)`);
      }
      
      // Return empty sql fragment if no conditions, otherwise build combined fragment
      if (conditions.length === 0) {
        return sql``;
      }
      if (conditions.length === 1) {
        return conditions[0];
      }
      // For multiple conditions, build them one by one to avoid deep nesting
      let combined = conditions[0];
      for (let i = 1; i < conditions.length; i++) {
        combined = sql`${combined} ${conditions[i]}`;
      }
      return combined;
    };
    
    const additionalWhere = buildAdditionalWhere();
    
    // Log query parameters for debugging
    logInfo('repcard-appointments-schedule', {
      requestId,
      userRole,
      repcardUserId,
      filters: {
        teamIds: teamIds?.length || 0,
        calendarId,
        statusFilter,
        hasPowerBillFilter,
        isRescheduleFilter
      },
      effectiveOfficeIds: effectiveOfficeIds?.length || 0,
      dateRange: { startDate, endDate },
      additionalWhereType: additionalWhere ? 'hasConditions' : 'empty'
    });

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
        LEFT JOIN repcard_users setter ON setter.repcard_user_id::int = a.setter_user_id::int
        LEFT JOIN repcard_users closer ON closer.repcard_user_id::int = a.closer_user_id::int
        LEFT JOIN repcard_customers c ON c.repcard_customer_id::int = a.repcard_customer_id::int
        LEFT JOIN repcard_calendars cal ON cal.repcard_calendar_id = (a.raw_data->>'calendarId')::int
        LEFT JOIN repcard_teams setter_team ON setter_team.repcard_team_id = setter.team_id
        LEFT JOIN repcard_teams closer_team ON closer_team.repcard_team_id = closer.team_id
        WHERE (
          (a.scheduled_at IS NOT NULL AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date)
          OR
          (a.scheduled_at IS NULL AND (a.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (a.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date)
        )
        AND a.closer_user_id = ${repcardUserId}
        ${additionalWhere}
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
        LEFT JOIN repcard_users setter ON setter.repcard_user_id::int = a.setter_user_id::int
        LEFT JOIN repcard_users closer ON closer.repcard_user_id::int = a.closer_user_id::int
        LEFT JOIN repcard_customers c ON c.repcard_customer_id::int = a.repcard_customer_id::int
        LEFT JOIN repcard_calendars cal ON cal.repcard_calendar_id = (a.raw_data->>'calendarId')::int
        LEFT JOIN repcard_teams setter_team ON setter_team.repcard_team_id = setter.team_id
        LEFT JOIN repcard_teams closer_team ON closer_team.repcard_team_id = closer.team_id
        WHERE (
          (a.scheduled_at IS NOT NULL AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date)
          OR
          (a.scheduled_at IS NULL AND (a.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (a.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date)
        )
        AND a.office_id = ANY(${effectiveOfficeIds}::int[])
        ${additionalWhere}
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
        LEFT JOIN repcard_users setter ON setter.repcard_user_id::int = a.setter_user_id::int
        LEFT JOIN repcard_users closer ON closer.repcard_user_id::int = a.closer_user_id::int
        LEFT JOIN repcard_customers c ON c.repcard_customer_id::int = a.repcard_customer_id::int
        LEFT JOIN repcard_calendars cal ON cal.repcard_calendar_id = (a.raw_data->>'calendarId')::int
        LEFT JOIN repcard_teams setter_team ON setter_team.repcard_team_id = setter.team_id
        LEFT JOIN repcard_teams closer_team ON closer_team.repcard_team_id = closer.team_id
        WHERE (
          (a.scheduled_at IS NOT NULL AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date)
          OR
          (a.scheduled_at IS NULL AND (a.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (a.created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date)
        )
        ${additionalWhere}
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
        LEFT JOIN repcard_users setter ON setter.repcard_user_id::int = a.setter_user_id::int
        LEFT JOIN repcard_users closer ON closer.repcard_user_id::int = a.closer_user_id::int
        LEFT JOIN repcard_customers c ON c.repcard_customer_id::int = a.repcard_customer_id::int
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
    const errorDetails = error instanceof Error ? {
      message: error.message,
      stack: error.stack,
      name: error.name
    } : { error: String(error) };
    
    // Safely access variables that might not be assigned yet
    const queryParams: any = {};
    try {
      queryParams.userRole = userRole;
      queryParams.repcardUserId = repcardUserId;
      queryParams.teamIds = teamIds?.length || 0;
      queryParams.calendarId = calendarId;
      queryParams.statusFilter = statusFilter;
      queryParams.hasPowerBillFilter = hasPowerBillFilter;
      queryParams.isRescheduleFilter = isRescheduleFilter;
      queryParams.effectiveOfficeIds = effectiveOfficeIds?.length || 0;
      queryParams.startDate = startDate;
      queryParams.endDate = endDate;
    } catch (e) {
      // If accessing variables fails, just log that
      queryParams.error = 'Could not access query parameters';
    }
    
    logError('repcard-appointments-schedule', error as Error, { 
      requestId,
      errorDetails,
      queryParams
    });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      {
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
        requestId
      },
      { status: 500 }
    );
  }
}
