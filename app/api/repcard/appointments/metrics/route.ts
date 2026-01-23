import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { getAssignedOffices } from '@/lib/quickbase/queries';

export const runtime = 'nodejs';
export const maxDuration = 30;

// Simple in-memory cache for metrics
const metricsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 30000; // 30 seconds
const MAX_CACHE_SIZE = 50;

function getCacheKey(userId: string, role: string, params: URLSearchParams): string {
  return `${userId}:${role}:${params.toString()}`;
}

function getCachedMetrics(key: string): any | null {
  const cached = metricsCache.get(key);
  if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
    return cached.data;
  }
  metricsCache.delete(key);
  return null;
}

function setCachedMetrics(key: string, data: any): void {
  // Evict oldest entries if cache is full
  if (metricsCache.size >= MAX_CACHE_SIZE) {
    const oldestKey = metricsCache.keys().next().value;
    metricsCache.delete(oldestKey);
  }
  metricsCache.set(key, { data, timestamp: Date.now() });
}

/**
 * GET /api/repcard/appointments/metrics
 *
 * Get appointment metrics with aggregated statistics
 *
 * Query params:
 * - startDate: YYYY-MM-DD (default: today)
 * - endDate: YYYY-MM-DD (default: today + 7 days)
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  let userId: string | undefined;
  let userRole: string | undefined;
  let startDate: string | undefined;
  let endDate: string | undefined;
  let repcardUserId: number | undefined;
  let effectiveOfficeIds: number[] | undefined;

  try {
    logApiRequest('GET', path, { endpoint: 'repcard-appointments-metrics', requestId });

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

    // Check cache
    const cacheKey = getCacheKey(userId, userRole, searchParams);
    const cached = getCachedMetrics(cacheKey);
    if (cached) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: true, requestId });
      return NextResponse.json(cached);
    }

    // Date range (default: today to 7 days from now)
    const today = new Date();
    today.setHours(0, 0, 0, 0);
    const defaultEndDate = new Date(today);
    defaultEndDate.setDate(defaultEndDate.getDate() + 7);

    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');
    const startDateStr = startDateParam || today.toISOString().split('T')[0];
    const endDateStr = endDateParam || defaultEndDate.toISOString().split('T')[0];

    startDate = startDateStr;
    endDate = endDateStr;

    // Get RepCard user ID for closers
    if (userRole === 'closer') {
      const userResult = await sql`
        SELECT repcard_user_id FROM users WHERE id = ${userId}
      `;
      const userRows = Array.from(userResult);
      if (userRows.length > 0 && userRows[0].repcard_user_id) {
        repcardUserId = parseInt(userRows[0].repcard_user_id) || undefined;
      }
    }

    // Get office IDs for leaders
    const officeIdsParam = searchParams.get('officeIds');
    const officeIds = officeIdsParam ? officeIdsParam.split(',').map(id => parseInt(id.trim())).filter(Boolean) : undefined;

    effectiveOfficeIds = officeIds;
    if (!effectiveOfficeIds && ['office_leader', 'area_director', 'divisional', 'regional'].includes(userRole)) {
      effectiveOfficeIds = await getAssignedOffices(userId);
    }

    // Normalize empty array to undefined
    if ((userRole === 'super_admin' || userRole === 'regional') && Array.isArray(effectiveOfficeIds) && effectiveOfficeIds.length === 0) {
      effectiveOfficeIds = undefined;
    }

    logInfo('repcard-appointments-metrics', {
      requestId,
      userRole,
      repcardUserId,
      effectiveOfficeIds: effectiveOfficeIds?.length || 0,
      dateRange: { startDate, endDate }
    });

    let result;
    let queryDescription = '';

    // Execute appropriate query based on role
    try {
      if (userRole === 'closer' && repcardUserId) {
        queryDescription = 'closer-specific';
        // Closer - see only their appointments
        result = await sql`
        SELECT
          COUNT(*) as total_appointments,
          COUNT(*) FILTER (WHERE a.has_power_bill = TRUE) as appointments_with_power_bill,
          COUNT(*) FILTER (WHERE a.closer_user_id IS NULL) as unassigned_appointments,
          COUNT(*) FILTER (WHERE
            a.status_category = 'scheduled' OR a.status_category = 'completed' OR
            a.raw_data->>'appointment_status_title' ILIKE '%confirmed%' OR
            a.raw_data->>'status' ILIKE '%confirmed%'
          ) as confirmed_appointments,
          COUNT(*) FILTER (WHERE a.is_reschedule = TRUE) as rescheduled_appointments,
          AVG(EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600)
            FILTER (WHERE a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL)
            as avg_schedule_out_hours
        FROM repcard_appointments a
        LEFT JOIN repcard_customers c ON c.repcard_customer_id::int = a.repcard_customer_id::int
        WHERE
          a.scheduled_at IS NOT NULL
          AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date
          AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date
          AND a.closer_user_id = ${repcardUserId}
      `;
    } else if (effectiveOfficeIds && effectiveOfficeIds.length > 0) {
      queryDescription = 'leader-office-filtered';
      // Leader - see office appointments
      result = await sql`
        SELECT
          COUNT(*) as total_appointments,
          COUNT(*) FILTER (WHERE a.has_power_bill = TRUE) as appointments_with_power_bill,
          COUNT(*) FILTER (WHERE a.closer_user_id IS NULL) as unassigned_appointments,
          COUNT(*) FILTER (WHERE
            a.status_category = 'scheduled' OR a.status_category = 'completed' OR
            a.raw_data->>'appointment_status_title' ILIKE '%confirmed%' OR
            a.raw_data->>'status' ILIKE '%confirmed%'
          ) as confirmed_appointments,
          COUNT(*) FILTER (WHERE a.is_reschedule = TRUE) as rescheduled_appointments,
          AVG(EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600)
            FILTER (WHERE a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL)
            as avg_schedule_out_hours
        FROM repcard_appointments a
        LEFT JOIN repcard_customers c ON c.repcard_customer_id::int = a.repcard_customer_id::int
        WHERE
          a.scheduled_at IS NOT NULL
          AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date
          AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date
          AND a.office_id = ANY(${effectiveOfficeIds}::int[])
      `;
    } else if (userRole === 'super_admin' || userRole === 'regional') {
      queryDescription = 'super-admin-all-appointments';
      // Super admin/regional - see all appointments
      result = await sql`
        SELECT
          COUNT(*) as total_appointments,
          COUNT(*) FILTER (WHERE a.has_power_bill = TRUE) as appointments_with_power_bill,
          COUNT(*) FILTER (WHERE a.closer_user_id IS NULL) as unassigned_appointments,
          COUNT(*) FILTER (WHERE
            a.status_category = 'scheduled' OR a.status_category = 'completed' OR
            a.raw_data->>'appointment_status_title' ILIKE '%confirmed%' OR
            a.raw_data->>'status' ILIKE '%confirmed%'
          ) as confirmed_appointments,
          COUNT(*) FILTER (WHERE a.is_reschedule = TRUE) as rescheduled_appointments,
          AVG(EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600)
            FILTER (WHERE a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL)
            as avg_schedule_out_hours
        FROM repcard_appointments a
        LEFT JOIN repcard_customers c ON c.repcard_customer_id::int = a.repcard_customer_id::int
        WHERE
          a.scheduled_at IS NOT NULL
          AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDate}::date
          AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDate}::date
      `;
    } else {
      queryDescription = 'no-access-empty';
      // No access - return empty metrics
      result = await sql`
        SELECT
          0::bigint as total_appointments,
          0::bigint as appointments_with_power_bill,
          0::bigint as unassigned_appointments,
          0::bigint as confirmed_appointments,
          0::bigint as rescheduled_appointments,
          NULL::numeric as avg_schedule_out_hours
      `;
      }
    } catch (queryError) {
      logError('repcard-appointments-metrics-query-execution-error', queryError as Error, {
        requestId,
        queryType: queryDescription,
        userRole,
        repcardUserId,
        effectiveOfficeIds,
        dateRange: { startDate, endDate }
      });
      throw queryError; // Re-throw to be caught by outer catch
    }

    // Parse result
    const rows = Array.from(result);

    // Log detailed query result for debugging
    logInfo('repcard-appointments-metrics-query-result', {
      requestId,
      queryType: queryDescription,
      rowCount: rows.length,
      hasRows: rows.length > 0,
      firstRow: rows[0] || null,
      resultType: typeof result,
      isArray: Array.isArray(result),
      resultConstructor: result?.constructor?.name,
      queryParams: {
        userRole,
        repcardUserId,
        effectiveOfficeIds,
        startDate,
        endDate
      }
    });

    if (!rows || rows.length === 0) {
      logError('repcard-appointments-metrics-empty-result', new Error('Query returned no rows'), {
        requestId,
        queryType: queryDescription,
        userRole,
        repcardUserId,
        effectiveOfficeIds,
        dateRange: { startDate, endDate }
      });

      // Return zero metrics instead of erroring
      const emptyResponse = {
        totalAppointments: 0,
        powerBillCoverage: { count: 0, percentage: 0 },
        averageScheduleOutTime: null,
        unassignedAppointments: 0,
        confirmationRate: { count: 0, total: 0, percentage: 0 },
        rescheduleStats: { count: 0, percentage: 0 },
        filters: { startDate, endDate, officeIds: effectiveOfficeIds },
        metadata: { userId, role: userRole, repcardUserId, requestId }
      };

      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
      return NextResponse.json(emptyResponse);
    }

    const metrics = rows[0];

    const totalAppointments = parseInt(metrics.total_appointments) || 0;
    const appointmentsWithPowerBill = parseInt(metrics.appointments_with_power_bill) || 0;
    const unassignedAppointments = parseInt(metrics.unassigned_appointments) || 0;
    const confirmedAppointments = parseInt(metrics.confirmed_appointments) || 0;
    const rescheduledAppointments = parseInt(metrics.rescheduled_appointments) || 0;
    const avgScheduleOutHours = metrics.avg_schedule_out_hours ? parseFloat(metrics.avg_schedule_out_hours) : null;

    // Calculate percentages and format output
    const powerBillPercentage = totalAppointments > 0
      ? Math.round((appointmentsWithPowerBill / totalAppointments) * 100)
      : 0;

    const confirmationPercentage = totalAppointments > 0
      ? Math.round((confirmedAppointments / totalAppointments) * 100)
      : 0;

    const reschedulePercentage = totalAppointments > 0
      ? Math.round((rescheduledAppointments / totalAppointments) * 100)
      : 0;

    // Format average schedule out time
    let averageScheduleOutTime = null;
    if (avgScheduleOutHours !== null && !isNaN(avgScheduleOutHours)) {
      const days = avgScheduleOutHours / 24;
      const formatted = days >= 1
        ? `${days.toFixed(1)} days`
        : `${avgScheduleOutHours.toFixed(1)} hours`;

      averageScheduleOutTime = {
        hours: Math.round(avgScheduleOutHours * 10) / 10,
        days: Math.round(days * 10) / 10,
        formatted
      };
    }

    const response = {
      totalAppointments,
      powerBillCoverage: {
        count: appointmentsWithPowerBill,
        percentage: powerBillPercentage
      },
      averageScheduleOutTime,
      unassignedAppointments,
      confirmationRate: {
        count: confirmedAppointments,
        total: totalAppointments,
        percentage: confirmationPercentage
      },
      rescheduleStats: {
        count: rescheduledAppointments,
        percentage: reschedulePercentage
      },
      filters: {
        startDate,
        endDate,
        officeIds: effectiveOfficeIds
      },
      metadata: {
        userId,
        role: userRole,
        repcardUserId,
        requestId
      }
    };

    // Cache the response
    setCachedMetrics(cacheKey, response);

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });

    return NextResponse.json(response);

  } catch (error) {
    const duration = Date.now() - start;
    const errorDetails = error instanceof Error ? {
      message: error.message,
      stack: error.stack,
      name: error.name
    } : { error: String(error) };

    logError('repcard-appointments-metrics', error as Error, {
      requestId,
      errorDetails,
      queryParams: {
        userRole,
        repcardUserId,
        effectiveOfficeIds: effectiveOfficeIds?.length || 0,
        startDate,
        endDate
      }
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
