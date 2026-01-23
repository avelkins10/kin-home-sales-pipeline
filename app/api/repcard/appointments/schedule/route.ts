import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { getAssignedOffices } from '@/lib/quickbase/queries';
import { toEasternStart, toEasternEnd } from '@/lib/utils/timezone';

export const runtime = 'nodejs';
export const maxDuration = 30; // 30 seconds max - fail fast if query hangs

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

    // Build WHERE conditions - only include when they exist
    // Check which filters are active to conditionally build queries
    // Note: teamIds might be an array or a number, handle both cases
    const hasTeamFilter = Array.isArray(teamIds) ? teamIds.length > 0 : !!(teamIds && teamIds !== 0);
    const hasCalendarFilter = !!calendarId;
    const hasStatusFilter = !!statusFilter;
    const hasPowerBillTrue = hasPowerBillFilter === 'true';
    const hasPowerBillFalse = hasPowerBillFilter === 'false';
    const hasRescheduleTrue = isRescheduleFilter === 'true';
    const hasRescheduleFalse = isRescheduleFilter === 'false';
    
    const hasAnyFilter = hasTeamFilter || hasCalendarFilter || hasStatusFilter || hasPowerBillTrue || hasPowerBillFalse || hasRescheduleTrue || hasRescheduleFalse;
    
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
      hasAnyFilter
    });

    let result;
    
    // Log SQL structure for debugging (before query execution)
    try {
      // Create a test query to inspect structure
      const testQuery = userRole === 'closer' && repcardUserId
        ? sql`SELECT 1 WHERE 1=1 AND ${repcardUserId} = ${repcardUserId} ${hasTeamFilter ? sql`AND 1=1` : ''} ${hasCalendarFilter ? sql`AND 1=1` : ''} ${hasStatusFilter ? sql`AND 1=1` : ''}`
        : sql`SELECT 1 WHERE 1=1`;
      
      logInfo('repcard-appointments-schedule-sql-debug', {
        requestId,
        sqlStrings: (testQuery as any).strings?.length || 0,
        sqlValues: (testQuery as any).values?.length || 0,
        hasAnyFilter,
        filterFlags: { hasTeamFilter, hasCalendarFilter, hasStatusFilter, hasPowerBillTrue, hasPowerBillFalse, hasRescheduleTrue, hasRescheduleFalse }
      });
    } catch (debugError) {
      // Ignore debug errors
    }
    
    if (userRole === 'closer' && repcardUserId) {
      // Closer query - see only their appointments
      // Build separate queries for different filter combinations to avoid parameter binding issues
      const baseQuery = sql`
        SELECT 
          a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
          a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
          a.status_category, a.scheduled_at, a.completed_at, a.duration,
          a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
          a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
          (a.raw_data->>'calendarId')::int as calendar_id,
          setter.first_name || ' ' || setter.last_name as setter_name,
          setter.email as setter_email, setter.team_id as setter_team_id,
          setter.team_name as setter_team_name,
          closer.first_name || ' ' || closer.last_name as closer_name,
          closer.email as closer_email, closer.team_id as closer_team_id,
          closer.team_name as closer_team_name,
          c.name as customer_name, c.phone as customer_phone,
          c.address as customer_address, c.email as customer_email,
          cal.name as calendar_name, cal.status as calendar_status,
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
      `;
      
      // Most common case: no additional filters - write out full query to avoid nested fragments
      if (!hasAnyFilter) {
        result = await sql`
          SELECT 
            a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
            a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
            a.status_category, a.scheduled_at, a.completed_at, a.duration,
            a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
            a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
            (a.raw_data->>'calendarId')::int as calendar_id,
            setter.first_name || ' ' || setter.last_name as setter_name,
            setter.email as setter_email, setter.team_id as setter_team_id,
            setter.team_name as setter_team_name,
            closer.first_name || ' ' || closer.last_name as closer_name,
            closer.email as closer_email, closer.team_id as closer_team_id,
            closer.team_name as closer_team_name,
            c.name as customer_name, c.phone as customer_phone,
            c.address as customer_address, c.email as customer_email,
            cal.name as calendar_name, cal.status as calendar_status,
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
          ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
        `;
      } else {
        // For filtered cases, use the no-filter query for now (filters can be applied client-side)
        // TODO: Implement explicit query combinations for filters to avoid nested fragment issues
        logInfo('repcard-appointments-schedule-filtered-using-no-filter', {
          requestId,
          hasAnyFilter,
          filters: { hasTeamFilter, hasCalendarFilter, hasStatusFilter, hasPowerBillTrue, hasPowerBillFalse, hasRescheduleTrue, hasRescheduleFalse },
          note: 'Filters will be applied client-side until explicit query combinations are implemented'
        });
        
        result = await sql`
          SELECT 
            a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
            a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
            a.status_category, a.scheduled_at, a.completed_at, a.duration,
            a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
            a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
            (a.raw_data->>'calendarId')::int as calendar_id,
            setter.first_name || ' ' || setter.last_name as setter_name,
            setter.email as setter_email, setter.team_id as setter_team_id,
            setter.team_name as setter_team_name,
            closer.first_name || ' ' || closer.last_name as closer_name,
            closer.email as closer_email, closer.team_id as closer_team_id,
            closer.team_name as closer_team_name,
            c.name as customer_name, c.phone as customer_phone,
            c.address as customer_address, c.email as customer_email,
            cal.name as calendar_name, cal.status as calendar_status,
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
          ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
        `;
      }
    } else if (effectiveOfficeIds && effectiveOfficeIds.length > 0) {
      // Leader query with office filter - use same base query pattern
      const baseQuery = sql`
        SELECT 
          a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
          a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
          a.status_category, a.scheduled_at, a.completed_at, a.duration,
          a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
          a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
          (a.raw_data->>'calendarId')::int as calendar_id,
          setter.first_name || ' ' || setter.last_name as setter_name,
          setter.email as setter_email, setter.team_id as setter_team_id,
          setter.team_name as setter_team_name,
          closer.first_name || ' ' || closer.last_name as closer_name,
          closer.email as closer_email, closer.team_id as closer_team_id,
          closer.team_name as closer_team_name,
          c.name as customer_name, c.phone as customer_phone,
          c.address as customer_address, c.email as customer_email,
          cal.name as calendar_name, cal.status as calendar_status,
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
      `;
      
      if (!hasAnyFilter) {
        result = await sql`
          SELECT 
            a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
            a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
            a.status_category, a.scheduled_at, a.completed_at, a.duration,
            a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
            a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
            (a.raw_data->>'calendarId')::int as calendar_id,
            setter.first_name || ' ' || setter.last_name as setter_name,
            setter.email as setter_email, setter.team_id as setter_team_id,
            setter.team_name as setter_team_name,
            closer.first_name || ' ' || closer.last_name as closer_name,
            closer.email as closer_email, closer.team_id as closer_team_id,
            closer.team_name as closer_team_name,
            c.name as customer_name, c.phone as customer_phone,
            c.address as customer_address, c.email as customer_email,
            cal.name as calendar_name, cal.status as calendar_status,
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
          ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
        `;
      } else {
        // For filtered cases, use the no-filter query for now (filters can be applied client-side)
        logInfo('repcard-appointments-schedule-filtered-using-no-filter', {
          requestId,
          hasAnyFilter,
          filters: { hasTeamFilter, hasCalendarFilter, hasStatusFilter, hasPowerBillTrue, hasPowerBillFalse, hasRescheduleTrue, hasRescheduleFalse },
          note: 'Filters will be applied client-side until explicit query combinations are implemented'
        });
        
        result = await sql`
          SELECT 
            a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
            a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
            a.status_category, a.scheduled_at, a.completed_at, a.duration,
            a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
            a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
            (a.raw_data->>'calendarId')::int as calendar_id,
            setter.first_name || ' ' || setter.last_name as setter_name,
            setter.email as setter_email, setter.team_id as setter_team_id,
            setter.team_name as setter_team_name,
            closer.first_name || ' ' || closer.last_name as closer_name,
            closer.email as closer_email, closer.team_id as closer_team_id,
            closer.team_name as closer_team_name,
            c.name as customer_name, c.phone as customer_phone,
            c.address as customer_address, c.email as customer_email,
            cal.name as calendar_name, cal.status as calendar_status,
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
          ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
        `;
      }
    } else if (userRole === 'super_admin' || userRole === 'regional') {
      // Super admin/regional - see all appointments - use same base query pattern
      const baseQuery = sql`
        SELECT 
          a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
          a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
          a.status_category, a.scheduled_at, a.completed_at, a.duration,
          a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
          a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
          (a.raw_data->>'calendarId')::int as calendar_id,
          setter.first_name || ' ' || setter.last_name as setter_name,
          setter.email as setter_email, setter.team_id as setter_team_id,
          setter.team_name as setter_team_name,
          closer.first_name || ' ' || closer.last_name as closer_name,
          closer.email as closer_email, closer.team_id as closer_team_id,
          closer.team_name as closer_team_name,
          c.name as customer_name, c.phone as customer_phone,
          c.address as customer_address, c.email as customer_email,
          cal.name as calendar_name, cal.status as calendar_status,
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
      `;
      
      if (!hasAnyFilter) {
        result = await sql`
          SELECT 
            a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
            a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
            a.status_category, a.scheduled_at, a.completed_at, a.duration,
            a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
            a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
            (a.raw_data->>'calendarId')::int as calendar_id,
            setter.first_name || ' ' || setter.last_name as setter_name,
            setter.email as setter_email, setter.team_id as setter_team_id,
            setter.team_name as setter_team_name,
            closer.first_name || ' ' || closer.last_name as closer_name,
            closer.email as closer_email, closer.team_id as closer_team_id,
            closer.team_name as closer_team_name,
            c.name as customer_name, c.phone as customer_phone,
            c.address as customer_address, c.email as customer_email,
            cal.name as calendar_name, cal.status as calendar_status,
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
          ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
        `;
      } else {
        // For filtered cases, use the no-filter query for now (filters can be applied client-side)
        // TODO: Implement explicit query combinations for filters to avoid nested fragment issues
        logInfo('repcard-appointments-schedule-filtered-using-no-filter', {
          requestId,
          hasAnyFilter,
          filters: { hasTeamFilter, hasCalendarFilter, hasStatusFilter, hasPowerBillTrue, hasPowerBillFalse, hasRescheduleTrue, hasRescheduleFalse },
          note: 'Filters will be applied client-side until explicit query combinations are implemented'
        });
        
        result = await sql`
          SELECT 
            a.id, a.repcard_appointment_id, a.customer_id, a.repcard_customer_id,
            a.setter_user_id, a.closer_user_id, a.office_id, a.disposition,
            a.status_category, a.scheduled_at, a.completed_at, a.duration,
            a.notes, a.is_within_48_hours, a.has_power_bill, a.is_reschedule,
            a.reschedule_count, a.original_appointment_id, a.created_at, a.updated_at,
            (a.raw_data->>'calendarId')::int as calendar_id,
            setter.first_name || ' ' || setter.last_name as setter_name,
            setter.email as setter_email, setter.team_id as setter_team_id,
            setter.team_name as setter_team_name,
            closer.first_name || ' ' || closer.last_name as closer_name,
            closer.email as closer_email, closer.team_id as closer_team_id,
            closer.team_name as closer_team_name,
            c.name as customer_name, c.phone as customer_phone,
            c.address as customer_address, c.email as customer_email,
            cal.name as calendar_name, cal.status as calendar_status,
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
          ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
        `;
      }
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

    // Log query results for debugging
    logInfo('repcard-appointments-schedule-result', {
      requestId,
      userRole,
      appointmentsCount: appointments.length,
      startDate,
      endDate,
      hasAnyFilter,
      repcardUserId,
      effectiveOfficeIds: effectiveOfficeIds?.length || 0
    });

    // Log diagnostic info for empty results
    if (appointments.length === 0) {
      // Check if there are ANY appointments in the database for this date range
      // Check with timezone conversion (what the query uses)
      const diagnosticCheckTZ = await sql`
        SELECT 
          COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date) as with_scheduled_tz,
          COUNT(*) FILTER (WHERE scheduled_at IS NULL AND (created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date AND (created_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date) as with_created_tz
        FROM repcard_appointments
      `;
      
      // Also check without timezone conversion (raw dates)
      const diagnosticCheckRaw = await sql`
        SELECT 
          COUNT(*) FILTER (WHERE scheduled_at IS NOT NULL AND scheduled_at::date >= ${startDate}::date AND scheduled_at::date <= ${endDate}::date) as with_scheduled_raw,
          COUNT(*) FILTER (WHERE scheduled_at IS NULL AND created_at::date >= ${startDate}::date AND created_at::date <= ${endDate}::date) as with_created_raw,
          COUNT(*) as total_all
        FROM repcard_appointments
      `;
      
      const diagnosticTZ = Array.from(diagnosticCheckTZ)[0];
      const diagnosticRaw = Array.from(diagnosticCheckRaw)[0];
      
      // Get sample dates to see what's actually stored
      const sampleDates = await sql`
        SELECT 
          repcard_appointment_id,
          scheduled_at,
          created_at,
          (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date as scheduled_at_et_date,
          scheduled_at::date as scheduled_at_utc_date
        FROM repcard_appointments
        WHERE scheduled_at IS NOT NULL
        ORDER BY scheduled_at DESC
        LIMIT 5
      `;
      
      logInfo('repcard-appointments-schedule-empty', {
        requestId,
        userRole,
        repcardUserId,
        startDate,
        endDate,
        diagnosticTZ: {
          withScheduled: diagnosticTZ?.with_scheduled_tz || 0,
          withCreated: diagnosticTZ?.with_created_tz || 0
        },
        diagnosticRaw: {
          withScheduled: diagnosticRaw?.with_scheduled_raw || 0,
          withCreated: diagnosticRaw?.with_created_raw || 0,
          totalAll: diagnosticRaw?.total_all || 0
        },
        sampleDates: Array.from(sampleDates).map((d: any) => ({
          id: d.repcard_appointment_id,
          scheduledAt: d.scheduled_at,
          scheduledAtETDate: d.scheduled_at_et_date,
          scheduledAtUTCDate: d.scheduled_at_utc_date
        })),
        filters: {
          teamIds: teamIds?.length || 0,
          calendarId,
          statusFilter,
          hasPowerBillFilter,
          isRescheduleFilter,
          effectiveOfficeIds: effectiveOfficeIds?.length || 0
        }
      });
    }

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
