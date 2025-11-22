import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/repcard/unified-dashboard
 *
 * Consolidated endpoint returning all data needed for the 4-tile dashboard:
 * 1. Quality metrics (48hr, reschedules, power bills)
 * 2. Office performance comparisons
 * 3. Rep leaderboards (doors, converters, closers)
 * 4. Canvassing activity trends
 */
export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;

  try {
    logApiRequest('GET', path, { endpoint: 'unified-dashboard', requestId });

    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');

    // Helper to extract rows
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    // ========================================
    // 1. QUALITY METRICS
    // ========================================
    const qualityResult = startDate && endDate
      ? await sql`
          SELECT
            COUNT(DISTINCT a.id)::int as total_appointments,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::int as reschedules,
            COUNT(DISTINCT CASE
              WHEN EXISTS (
                SELECT 1 FROM repcard_appointment_attachments att
                WHERE att.repcard_appointment_id::TEXT = a.repcard_appointment_id
                  AND (att.attachment_type ILIKE '%power%' OR att.attachment_type ILIKE '%bill%')
              ) OR EXISTS (
                SELECT 1 FROM repcard_customer_attachments catt
                WHERE catt.repcard_customer_id::TEXT = a.repcard_customer_id
                  AND (catt.attachment_type ILIKE '%power%' OR catt.attachment_type ILIKE '%bill%')
              )
              THEN a.id
            END)::int as with_power_bill
          FROM repcard_appointments a
          WHERE a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
        `
      : await sql`
          SELECT
            COUNT(DISTINCT a.id)::int as total_appointments,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::int as reschedules,
            COUNT(DISTINCT CASE
              WHEN EXISTS (
                SELECT 1 FROM repcard_appointment_attachments att
                WHERE att.repcard_appointment_id::TEXT = a.repcard_appointment_id
                  AND (att.attachment_type ILIKE '%power%' OR att.attachment_type ILIKE '%bill%')
              ) OR EXISTS (
                SELECT 1 FROM repcard_customer_attachments catt
                WHERE catt.repcard_customer_id::TEXT = a.repcard_customer_id
                  AND (catt.attachment_type ILIKE '%power%' OR catt.attachment_type ILIKE '%bill%')
              )
              THEN a.id
            END)::int as with_power_bill
          FROM repcard_appointments a
        `;

    const qualityData = getRows(qualityResult)[0] as any;
    const totalAppts = qualityData?.total_appointments || 0;

    const qualityMetrics = {
      totalAppointments: totalAppts,
      appointmentSpeed: totalAppts > 0 ? ((qualityData.within_48h / totalAppts) * 100) : 0,
      rescheduleRate: totalAppts > 0 ? ((qualityData.reschedules / totalAppts) * 100) : 0,
      powerBillRate: totalAppts > 0 ? ((qualityData.with_power_bill / totalAppts) * 100) : 0,
    };

    // Top reschedulers
    const topReschedulersResult = startDate && endDate
      ? await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            COUNT(DISTINCT a.id)::int as total_appointments,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::int as reschedules,
            CASE
              WHEN COUNT(DISTINCT a.id) > 0 THEN
                (COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::float / COUNT(DISTINCT a.id)::float) * 100
              ELSE 0
            END as reschedule_rate
          FROM repcard_appointments a
          LEFT JOIN users u ON u.repcard_user_id::TEXT = a.setter_user_id
          WHERE u.repcard_user_id IS NOT NULL
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          GROUP BY u.repcard_user_id, u.name
          HAVING COUNT(DISTINCT a.id) >= 5
          ORDER BY reschedule_rate DESC
          LIMIT 5
        `
      : await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            COUNT(DISTINCT a.id)::int as total_appointments,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::int as reschedules,
            CASE
              WHEN COUNT(DISTINCT a.id) > 0 THEN
                (COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::float / COUNT(DISTINCT a.id)::float) * 100
              ELSE 0
            END as reschedule_rate
          FROM repcard_appointments a
          LEFT JOIN users u ON u.repcard_user_id::TEXT = a.setter_user_id
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name
          HAVING COUNT(DISTINCT a.id) >= 5
          ORDER BY reschedule_rate DESC
          LIMIT 5
        `;

    const topReschedulers = getRows(topReschedulersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      totalAppointments: row.total_appointments,
      reschedules: row.reschedules,
      rescheduleRate: parseFloat(row.reschedule_rate || '0'),
    }));

    // ========================================
    // 2. OFFICE PERFORMANCE
    // ========================================
    const officeResult = startDate && endDate
      ? await sql`
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_set,
            COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::int as sales_closed,
            COUNT(DISTINCT u.repcard_user_id)::int as active_reps,
            CASE
              WHEN COUNT(DISTINCT c.repcard_customer_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id)::float / COUNT(DISTINCT c.repcard_customer_id)::float) * 100
              ELSE 0
            END as conversion_rate,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::float /
                 COUNT(DISTINCT a.repcard_appointment_id)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          LEFT JOIN repcard_customers c ON c.office_id = o.repcard_office_id::TEXT
          LEFT JOIN repcard_appointments a ON a.repcard_customer_id = c.repcard_customer_id
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          LEFT JOIN users u ON u.repcard_user_id::TEXT = c.setter_user_id
          GROUP BY o.repcard_office_id, o.name
          HAVING COUNT(DISTINCT c.repcard_customer_id) > 0
          ORDER BY doors_knocked DESC
          LIMIT 10
        `
      : await sql`
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_set,
            COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::int as sales_closed,
            COUNT(DISTINCT u.repcard_user_id)::int as active_reps,
            CASE
              WHEN COUNT(DISTINCT c.repcard_customer_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id)::float / COUNT(DISTINCT c.repcard_customer_id)::float) * 100
              ELSE 0
            END as conversion_rate,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::float /
                 COUNT(DISTINCT a.repcard_appointment_id)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          LEFT JOIN repcard_customers c ON c.office_id = o.repcard_office_id::TEXT
          LEFT JOIN repcard_appointments a ON a.repcard_customer_id = c.repcard_customer_id
          LEFT JOIN users u ON u.repcard_user_id::TEXT = c.setter_user_id
          GROUP BY o.repcard_office_id, o.name
          HAVING COUNT(DISTINCT c.repcard_customer_id) > 0
          ORDER BY doors_knocked DESC
          LIMIT 10
        `;

    const officePerformance = getRows(officeResult).map((row: any) => ({
      officeId: row.repcard_office_id,
      officeName: row.office_name,
      doorsKnocked: row.doors_knocked,
      appointmentsSet: row.appointments_set,
      salesClosed: row.sales_closed,
      activeReps: row.active_reps,
      conversionRate: parseFloat(row.conversion_rate || '0'),
      closeRate: parseFloat(row.close_rate || '0'),
    }));

    // ========================================
    // 3. REP LEADERBOARDS
    // ========================================

    // Top doors knocked
    const topDoorsResult = startDate && endDate
      ? await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_set,
            CASE
              WHEN COUNT(DISTINCT c.repcard_customer_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id)::float / COUNT(DISTINCT c.repcard_customer_id)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM users u
          LEFT JOIN repcard_customers c ON c.setter_user_id = u.repcard_user_id::TEXT
          LEFT JOIN repcard_appointments a ON a.repcard_customer_id = c.repcard_customer_id
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT c.repcard_customer_id) > 0
          ORDER BY doors_knocked DESC
          LIMIT 10
        `
      : await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_set,
            CASE
              WHEN COUNT(DISTINCT c.repcard_customer_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id)::float / COUNT(DISTINCT c.repcard_customer_id)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM users u
          LEFT JOIN repcard_customers c ON c.setter_user_id = u.repcard_user_id::TEXT
          LEFT JOIN repcard_appointments a ON a.repcard_customer_id = c.repcard_customer_id
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT c.repcard_customer_id) > 0
          ORDER BY doors_knocked DESC
          LIMIT 10
        `;

    const topDoors = getRows(topDoorsResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      doorsKnocked: row.doors_knocked,
      appointmentsSet: row.appointments_set,
      conversionRate: parseFloat(row.conversion_rate || '0'),
    }));

    // Top appointment setters (with quality metrics)
    const topAppointmentSettersResult = startDate && endDate
      ? await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT a.id)::int as appointments_set,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h_count,
            COUNT(DISTINCT CASE
              WHEN EXISTS (
                SELECT 1 FROM repcard_appointment_attachments att
                WHERE att.repcard_appointment_id::TEXT = a.repcard_appointment_id
                  AND (att.attachment_type ILIKE '%power%' OR att.attachment_type ILIKE '%bill%')
              ) OR EXISTS (
                SELECT 1 FROM repcard_customer_attachments catt
                WHERE catt.repcard_customer_id::TEXT = a.repcard_customer_id
                  AND (catt.attachment_type ILIKE '%power%' OR catt.attachment_type ILIKE '%bill%')
              )
              THEN a.id
            END)::int as with_power_bill_count,
            COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
            CASE
              WHEN COUNT(DISTINCT c.repcard_customer_id) > 0 THEN
                (COUNT(DISTINCT a.id)::float / COUNT(DISTINCT c.repcard_customer_id)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM users u
          LEFT JOIN repcard_appointments a ON a.setter_user_id = u.repcard_user_id::TEXT
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT a.id) > 0
          ORDER BY appointments_set DESC
          LIMIT 10
        `
      : await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT a.id)::int as appointments_set,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h_count,
            COUNT(DISTINCT CASE
              WHEN EXISTS (
                SELECT 1 FROM repcard_appointment_attachments att
                WHERE att.repcard_appointment_id::TEXT = a.repcard_appointment_id
                  AND (att.attachment_type ILIKE '%power%' OR att.attachment_type ILIKE '%bill%')
              ) OR EXISTS (
                SELECT 1 FROM repcard_customer_attachments catt
                WHERE catt.repcard_customer_id::TEXT = a.repcard_customer_id
                  AND (catt.attachment_type ILIKE '%power%' OR catt.attachment_type ILIKE '%bill%')
              )
              THEN a.id
            END)::int as with_power_bill_count,
            COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
            CASE
              WHEN COUNT(DISTINCT c.repcard_customer_id) > 0 THEN
                (COUNT(DISTINCT a.id)::float / COUNT(DISTINCT c.repcard_customer_id)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM users u
          LEFT JOIN repcard_appointments a ON a.setter_user_id = u.repcard_user_id::TEXT
          LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT a.id) > 0
          ORDER BY appointments_set DESC
          LIMIT 10
        `;

    const topAppointmentSetters = getRows(topAppointmentSettersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      appointmentsSet: row.appointments_set,
      within48hCount: row.within_48h_count,
      withPowerBillCount: row.with_power_bill_count,
      doorsKnocked: row.doors_knocked,
      conversionRate: parseFloat(row.conversion_rate || '0'),
    }));

    // Top closers
    const topClosersResult = startDate && endDate
      ? await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::int as sales_closed,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::float /
                 COUNT(DISTINCT a.repcard_appointment_id)::float) * 100
              ELSE 0
            END as close_rate
          FROM users u
          LEFT JOIN repcard_appointments a ON a.closer_user_id = u.repcard_user_id::TEXT
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT a.repcard_appointment_id) > 0
          ORDER BY sales_closed DESC
          LIMIT 10
        `
      : await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::int as sales_closed,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT CASE WHEN a.disposition ILIKE '%closed%' THEN a.repcard_appointment_id END)::float /
                 COUNT(DISTINCT a.repcard_appointment_id)::float) * 100
              ELSE 0
            END as close_rate
          FROM users u
          LEFT JOIN repcard_appointments a ON a.closer_user_id = u.repcard_user_id::TEXT
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT a.repcard_appointment_id) > 0
          ORDER BY sales_closed DESC
          LIMIT 10
        `;

    const topClosers = getRows(topClosersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      appointmentsRun: row.appointments_run,
      salesClosed: row.sales_closed,
      closeRate: parseFloat(row.close_rate || '0'),
    }));

    // ========================================
    // 4. CANVASSING ACTIVITY
    // ========================================
    const canvassingResult = await sql`
      SELECT
        DATE(c.created_at) as date,
        COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
        COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_set
      FROM repcard_customers c
      LEFT JOIN repcard_appointments a ON a.repcard_customer_id = c.repcard_customer_id
        AND DATE(a.scheduled_at) = DATE(c.created_at)
      WHERE c.created_at >= NOW() - INTERVAL '30 days'
      GROUP BY DATE(c.created_at)
      ORDER BY date DESC
      LIMIT 30
    `;

    const canvassingTrends = getRows(canvassingResult).map((row: any) => ({
      date: row.date,
      doorsKnocked: row.doors_knocked,
      appointmentsSet: row.appointments_set,
      conversionRate: row.doors_knocked > 0 ? (row.appointments_set / row.doors_knocked) * 100 : 0,
    }));

    // Calculate totals
    const totalDoors = canvassingTrends.reduce((sum, day) => sum + day.doorsKnocked, 0);
    const totalAppointments = canvassingTrends.reduce((sum, day) => sum + day.appointmentsSet, 0);

    const canvassingActivity = {
      last30Days: {
        totalDoors,
        totalAppointments,
        avgDoorsPerDay: canvassingTrends.length > 0 ? totalDoors / canvassingTrends.length : 0,
        avgConversionRate: totalDoors > 0 ? (totalAppointments / totalDoors) * 100 : 0,
      },
      dailyTrends: canvassingTrends,
    };

    // ========================================
    // RESPONSE
    // ========================================
    const response = {
      success: true,
      quality: qualityMetrics,
      topReschedulers,
      officePerformance,
      leaderboards: {
        topDoors,
        topAppointmentSetters,
        topClosers,
      },
      canvassing: canvassingActivity,
      metadata: {
        fetchedAt: new Date().toISOString(),
        dateRange: { startDate, endDate },
      },
    };

    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(response);

  } catch (error) {
    const duration = Date.now() - start;
    logError('unified-dashboard', error as Error, { requestId });
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
