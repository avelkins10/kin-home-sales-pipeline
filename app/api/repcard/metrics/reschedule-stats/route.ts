import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/metrics/reschedule-stats
 *
 * Returns reschedule metrics including:
 * - Total appointments and reschedules
 * - Reschedule rate
 * - Customers with reschedules
 * - Average reschedules per customer
 * - Top reschedulers
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  try {
    logApiRequest('GET', path, { endpoint: 'reschedule-stats', requestId });

    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const userId = searchParams.get('userId');
    const officeIdsParam = searchParams.get('officeIds');
    const officeIds = officeIdsParam ? officeIdsParam.split(',').map(Number) : [];
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');

    // Helper to extract rows
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    // Query reschedule metrics with conditional filters
    let metricsResult;

    if (userId && officeIds.length > 0 && startDate && endDate) {
      metricsResult = await sql`
        SELECT
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          COUNT(DISTINCT a.customer_id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as customers_with_reschedules,
          AVG(a.reschedule_count) FILTER (WHERE a.is_reschedule = TRUE) as avg_reschedules_per_customer
        FROM repcard_appointments a
        WHERE (a.setter_user_id::text = ${userId} OR a.closer_user_id::text = ${userId})
          AND a.office_id::text = ANY(${officeIds.map(id => id.toString())}::text[])
          AND a.scheduled_at >= ${startDate}::timestamptz
          AND a.scheduled_at <= ${endDate}::timestamptz
      `;
    } else if (userId && startDate && endDate) {
      metricsResult = await sql`
        SELECT
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          COUNT(DISTINCT a.customer_id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as customers_with_reschedules,
          AVG(a.reschedule_count) FILTER (WHERE a.is_reschedule = TRUE) as avg_reschedules_per_customer
        FROM repcard_appointments a
        WHERE (a.setter_user_id::text = ${userId} OR a.closer_user_id::text = ${userId})
          AND a.scheduled_at >= ${startDate}::timestamptz
          AND a.scheduled_at <= ${endDate}::timestamptz
      `;
    } else if (startDate && endDate) {
      metricsResult = await sql`
        SELECT
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          COUNT(DISTINCT a.customer_id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as customers_with_reschedules,
          AVG(a.reschedule_count) FILTER (WHERE a.is_reschedule = TRUE) as avg_reschedules_per_customer
        FROM repcard_appointments a
        WHERE a.scheduled_at >= ${startDate}::timestamptz
          AND a.scheduled_at <= ${endDate}::timestamptz
      `;
    } else {
      // No date filter - query all data
      metricsResult = await sql`
        SELECT
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          COUNT(DISTINCT a.customer_id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as customers_with_reschedules,
          AVG(a.reschedule_count) FILTER (WHERE a.is_reschedule = TRUE) as avg_reschedules_per_customer
        FROM repcard_appointments a
      `;
    }

    const metricsRows = getRows(metricsResult);
    const metrics = metricsRows[0] as any;

    const totalAppointments = parseInt(metrics?.total_appointments || '0');
    const totalReschedules = parseInt(metrics?.total_reschedules || '0');
    const customersWithReschedules = parseInt(metrics?.customers_with_reschedules || '0');
    const avgReschedulesPerCustomer = parseFloat(metrics?.avg_reschedules_per_customer || '0');
    const rescheduleRate = totalAppointments > 0 ? (totalReschedules / totalAppointments) * 100 : 0;

    // Query top reschedulers with same filters
    let topReschedulersResult;

    if (userId && officeIds.length > 0 && startDate && endDate) {
      topReschedulersResult = await sql`
        SELECT
          u.repcard_user_id,
          u.name as user_name,
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          CASE
            WHEN COUNT(DISTINCT a.id) > 0 THEN
              (COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::float / COUNT(DISTINCT a.id)::float) * 100
            ELSE 0
          END as reschedule_rate
        FROM repcard_appointments a
        LEFT JOIN users u ON u.repcard_user_id::TEXT = a.setter_user_id OR u.repcard_user_id::TEXT = a.closer_user_id
        WHERE (a.setter_user_id::text = ${userId} OR a.closer_user_id::text = ${userId})
          AND a.office_id::text = ANY(${officeIds.map(id => id.toString())}::text[])
          AND a.scheduled_at >= ${startDate}::timestamptz
          AND a.scheduled_at <= ${endDate}::timestamptz
        GROUP BY u.repcard_user_id, u.name
        HAVING COUNT(DISTINCT a.id) >= 5
        ORDER BY reschedule_rate DESC, total_reschedules DESC
        LIMIT 10
      `;
    } else if (userId && startDate && endDate) {
      topReschedulersResult = await sql`
        SELECT
          u.repcard_user_id,
          u.name as user_name,
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          CASE
            WHEN COUNT(DISTINCT a.id) > 0 THEN
              (COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::float / COUNT(DISTINCT a.id)::float) * 100
            ELSE 0
          END as reschedule_rate
        FROM repcard_appointments a
        LEFT JOIN users u ON u.repcard_user_id::TEXT = a.setter_user_id OR u.repcard_user_id::TEXT = a.closer_user_id
        WHERE (a.setter_user_id::text = ${userId} OR a.closer_user_id::text = ${userId})
          AND a.scheduled_at >= ${startDate}::timestamptz
          AND a.scheduled_at <= ${endDate}::timestamptz
        GROUP BY u.repcard_user_id, u.name
        HAVING COUNT(DISTINCT a.id) >= 5
        ORDER BY reschedule_rate DESC, total_reschedules DESC
        LIMIT 10
      `;
    } else if (startDate && endDate) {
      topReschedulersResult = await sql`
        SELECT
          u.repcard_user_id,
          u.name as user_name,
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          CASE
            WHEN COUNT(DISTINCT a.id) > 0 THEN
              (COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::float / COUNT(DISTINCT a.id)::float) * 100
            ELSE 0
          END as reschedule_rate
        FROM repcard_appointments a
        LEFT JOIN users u ON u.repcard_user_id::TEXT = a.setter_user_id OR u.repcard_user_id::TEXT = a.closer_user_id
        WHERE a.scheduled_at >= ${startDate}::timestamptz
          AND a.scheduled_at <= ${endDate}::timestamptz
        GROUP BY u.repcard_user_id, u.name
        HAVING COUNT(DISTINCT a.id) >= 5
        ORDER BY reschedule_rate DESC, total_reschedules DESC
        LIMIT 10
      `;
    } else {
      topReschedulersResult = await sql`
        SELECT
          u.repcard_user_id,
          u.name as user_name,
          COUNT(DISTINCT a.id)::bigint as total_appointments,
          COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::bigint as total_reschedules,
          CASE
            WHEN COUNT(DISTINCT a.id) > 0 THEN
              (COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::float / COUNT(DISTINCT a.id)::float) * 100
            ELSE 0
          END as reschedule_rate
        FROM repcard_appointments a
        LEFT JOIN users u ON u.repcard_user_id::TEXT = a.setter_user_id OR u.repcard_user_id::TEXT = a.closer_user_id
        GROUP BY u.repcard_user_id, u.name
        HAVING COUNT(DISTINCT a.id) >= 5
        ORDER BY reschedule_rate DESC, total_reschedules DESC
        LIMIT 10
      `;
    }

    const topReschedulersRows = getRows(topReschedulersResult);
    const topReschedulers = topReschedulersRows.map((row: any) => ({
      repcardUserId: row.repcard_user_id,
      userName: row.user_name,
      totalAppointments: parseInt(row.total_appointments || '0'),
      totalReschedules: parseInt(row.total_reschedules || '0'),
      rescheduleRate: parseFloat(row.reschedule_rate || '0'),
    }));

    const response = {
      success: true,
      totalAppointments,
      totalReschedules,
      rescheduleRate,
      customersWithReschedules,
      avgReschedulesPerCustomer,
      topReschedulers,
      metadata: {
        filters: {
          userId: userId || null,
          officeIds: officeIds || null,
          startDate: startDate || null,
          endDate: endDate || null,
        },
        fetchedAt: new Date().toISOString(),
      },
    };

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(response);

  } catch (error) {
    const duration = Date.now() - start;
    logError('reschedule-stats', error as Error, { requestId });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      {
        success: false,
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
      },
      { status: 500 }
    );
  }
}
