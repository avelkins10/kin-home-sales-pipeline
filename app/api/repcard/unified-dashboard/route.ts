import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

// Cache implementation
const unifiedDashboardCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 30000; // 30 seconds for real-time updates (reduced from 5 minutes for webhook responsiveness)
const MAX_CACHE_ENTRIES = 50;

// Clean up expired cache entries
function cleanCache() {
  const now = Date.now();
  for (const [key, value] of Array.from(unifiedDashboardCache.entries())) {
    if (now - value.timestamp > CACHE_TTL) {
      unifiedDashboardCache.delete(key);
    }
  }
  
  // LRU eviction if cache is too large
  if (unifiedDashboardCache.size > MAX_CACHE_ENTRIES) {
    const entries = Array.from(unifiedDashboardCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    const toDelete = entries.slice(0, unifiedDashboardCache.size - MAX_CACHE_ENTRIES);
    toDelete.forEach(([key]) => unifiedDashboardCache.delete(key));
  }
}

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
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');

    // Convert date strings to proper timestamps in Eastern Time (per project rules)
    // When a date like "2026-01-22" is provided, interpret it as the start/end of that day in Eastern Time
    let startDate: string | null = null;
    let endDate: string | null = null;
    
    if (startDateParam) {
      // Parse as Eastern Time start of day (00:00:00 ET)
      const date = new Date(startDateParam + 'T00:00:00-05:00'); // EST/EDT offset
      startDate = date.toISOString();
    }
    
    if (endDateParam) {
      // Parse as Eastern Time end of day (23:59:59.999 ET)
      const date = new Date(endDateParam + 'T23:59:59.999-05:00'); // EST/EDT offset
      endDate = date.toISOString();
    }

    // Build cache key (use original params for cache key to match frontend)
    const cacheKey = `unified-dashboard:${startDateParam || 'all'}:${endDateParam || 'all'}`;
    
    // Check cache
    cleanCache();
    const cached = unifiedDashboardCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: true, requestId });
      return NextResponse.json({
        ...cached.data,
        metadata: {
          ...cached.data.metadata,
          cached: true
        }
      });
    }

    // Helper to extract rows
    const getRows = (result: any): any[] => {
      if (Array.isArray(result)) return result;
      if (result?.rows && Array.isArray(result.rows)) return result.rows;
      return Array.from(result);
    };

    // ========================================
    // 1. QUALITY METRICS
    // Track 4 separate metrics:
    // 1. Appointments with power bill (regardless of 48h)
    // 2. Appointments within 48h (regardless of power bill)
    // 3. Appointments with BOTH (power bill AND 48h) - HIGH QUALITY
    // 4. Appointments with NEITHER (no power bill AND not 48h) - LOW QUALITY
    // ========================================
    const qualityResult = startDate && endDate
      ? await sql`
          SELECT
            COUNT(DISTINCT a.id)::int as total_appointments,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE)::int as both,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE)::int as neither,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::int as reschedules,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours IS NULL)::int as null_48h,
            COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill IS NULL)::int as null_pb
          FROM repcard_appointments a
          WHERE a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
        `
      : await sql`
          SELECT
            COUNT(DISTINCT a.id)::int as total_appointments,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE)::int as both,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE)::int as neither,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = TRUE)::int as reschedules,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours IS NULL)::int as null_48h,
            COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill IS NULL)::int as null_pb
          FROM repcard_appointments a
        `;

    const qualityData = getRows(qualityResult)[0] as any;
    const totalAppts = qualityData?.total_appointments || 0;
    const within48h = qualityData?.within_48h || 0;
    const withPowerBill = qualityData?.with_power_bill || 0;
    const both = qualityData?.both || 0;
    const neither = qualityData?.neither || 0;
    const reschedules = qualityData?.reschedules || 0;
    const null48h = qualityData?.null_48h || 0;
    const nullPB = qualityData?.null_pb || 0;

    // Log warning if there are NULL values
    if (null48h > 0 || nullPB > 0) {
      console.warn(`[RepCard Unified Dashboard] WARNING: Found ${null48h} appointments with NULL is_within_48_hours and ${nullPB} with NULL has_power_bill. Run backfill to populate.`);
    }

    // Calculate percentages for all 4 metrics
    const qualityMetrics = {
      totalAppointments: totalAppts,
      // Individual metrics (percentages and counts)
      appointmentSpeed: totalAppts > 0 ? ((within48h / totalAppts) * 100) : 0,
      within48h: within48h, // Add count for display
      powerBillRate: totalAppts > 0 ? ((withPowerBill / totalAppts) * 100) : 0,
      withPowerBill: withPowerBill, // Add count for display
      // Combined metrics (counts and percentages)
      withBoth: {
        count: both,
        percentage: totalAppts > 0 ? ((both / totalAppts) * 100) : 0
      },
      withNeither: {
        count: neither,
        percentage: totalAppts > 0 ? ((neither / totalAppts) * 100) : 0
      },
      rescheduleRate: totalAppts > 0 ? ((reschedules / totalAppts) * 100) : 0,
      reschedules: reschedules, // Add count for display
      // Add warnings for NULL values
      hasNullValues: null48h > 0 || nullPB > 0,
      null48hCount: null48h,
      nullPBCount: nullPB,
    };

    // Debug logging - ENHANCED to help diagnose issues
    console.log(`[RepCard Unified Dashboard] Quality metrics:`, {
      totalAppts,
      within48h: `${within48h} (${qualityMetrics.appointmentSpeed.toFixed(1)}%)`,
      withPowerBill: `${withPowerBill} (${qualityMetrics.powerBillRate.toFixed(1)}%)`,
      withBoth: `${both} (${qualityMetrics.withBoth.percentage.toFixed(1)}%)`,
      withNeither: `${neither} (${qualityMetrics.withNeither.percentage.toFixed(1)}%)`,
      reschedules: `${reschedules} (${qualityMetrics.rescheduleRate.toFixed(1)}%)`,
      null48h,
      nullPB,
      dateRange: { startDate: startDateParam, endDate: endDateParam },
      rawData: {
        total_appointments: qualityData?.total_appointments,
        within_48h: qualityData?.within_48h,
        with_power_bill: qualityData?.with_power_bill,
        both: qualityData?.both,
        neither: qualityData?.neither,
        null_48h: qualityData?.null_48h,
        null_pb: qualityData?.null_pb
      }
    });

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
            AND c.created_at >= ${startDate}::timestamptz
            AND c.created_at <= ${endDate}::timestamptz
          LEFT JOIN repcard_appointments a ON a.repcard_customer_id = c.repcard_customer_id
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          LEFT JOIN users u ON u.repcard_user_id::TEXT = c.setter_user_id
          GROUP BY o.repcard_office_id, o.name
          HAVING COUNT(DISTINCT c.repcard_customer_id) > 0
          ORDER BY doors_knocked DESC
          LIMIT 50
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
          LIMIT 50
        `;

    const topDoors = getRows(topDoorsResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      doorsKnocked: row.doors_knocked,
      appointmentsSet: row.appointments_set,
      conversionRate: parseFloat(row.conversion_rate || '0'),
    }));

    // Top appointment setters (with quality metrics + hours on doors)
    // Track: total, within 48h, with PB, both, neither, doors, hours on doors
    // FIXED: Count doors separately (all customers created by setter in date range)
    // FIXED: Count appointments separately (all appointments set by setter in date range)
    // FIXED: Join customers to appointments for quality metrics, but don't restrict doors by appointment date
    const topAppointmentSettersResult = startDate && endDate
      ? await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            -- Appointments set in date range
            COUNT(DISTINCT a.id)::int as appointments_set,
            -- Quality metrics for appointments in date range
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE)::int as within_48h_count,
            COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill_count,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE)::int as both_count,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE)::int as neither_count,
            -- Doors knocked: all customers created by this setter in date range (regardless of appointments)
            COALESCE(doors_subquery.doors_knocked, 0)::int as doors_knocked,
            -- Hours on doors: count unique days with door knocks * 4 hours per day (average)
            COALESCE(doors_subquery.estimated_hours_on_doors, 0)::int as estimated_hours_on_doors,
            CASE
              WHEN COALESCE(doors_subquery.doors_knocked, 0) > 0 THEN
                (COUNT(DISTINCT a.id)::float / doors_subquery.doors_knocked::float) * 100
              ELSE 0
            END as conversion_rate
          FROM users u
          -- Appointments set by this user in date range
          LEFT JOIN repcard_appointments a ON a.setter_user_id = u.repcard_user_id::TEXT
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          -- Doors knocked: subquery to count all customers created by this setter in date range
          LEFT JOIN LATERAL (
            SELECT
              COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
              COUNT(DISTINCT DATE(c.created_at))::int * 4 as estimated_hours_on_doors
            FROM repcard_customers c
            WHERE c.setter_user_id = u.repcard_user_id::TEXT
              AND c.created_at >= ${startDate}::timestamptz
              AND c.created_at <= ${endDate}::timestamptz
          ) doors_subquery ON true
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role, doors_subquery.doors_knocked, doors_subquery.estimated_hours_on_doors
          HAVING COUNT(DISTINCT a.id) > 0 OR doors_subquery.doors_knocked > 0
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
            COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill_count,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE)::int as both_count,
            COUNT(DISTINCT a.id) FILTER (WHERE a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE)::int as neither_count,
            -- Doors knocked: all customers created by this setter (regardless of appointments)
            COALESCE(doors_subquery.doors_knocked, 0)::int as doors_knocked,
            -- Hours on doors: count unique days with door knocks * 4 hours per day (average)
            COALESCE(doors_subquery.estimated_hours_on_doors, 0)::int as estimated_hours_on_doors,
            CASE
              WHEN COALESCE(doors_subquery.doors_knocked, 0) > 0 THEN
                (COUNT(DISTINCT a.id)::float / doors_subquery.doors_knocked::float) * 100
              ELSE 0
            END as conversion_rate
          FROM users u
          -- Appointments set by this user
          LEFT JOIN repcard_appointments a ON a.setter_user_id = u.repcard_user_id::TEXT
          -- Doors knocked: subquery to count all customers created by this setter
          LEFT JOIN LATERAL (
            SELECT
              COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
              COUNT(DISTINCT DATE(c.created_at))::int * 4 as estimated_hours_on_doors
            FROM repcard_customers c
            WHERE c.setter_user_id = u.repcard_user_id::TEXT
          ) doors_subquery ON true
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role, doors_subquery.doors_knocked, doors_subquery.estimated_hours_on_doors
          HAVING COUNT(DISTINCT a.id) > 0 OR doors_subquery.doors_knocked > 0
          ORDER BY appointments_set DESC
          LIMIT 50
        `;

    const topAppointmentSetters = getRows(topAppointmentSettersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      appointmentsSet: row.appointments_set,
      within48hCount: row.within_48h_count,
      withPowerBillCount: row.with_power_bill_count,
      bothCount: row.both_count || 0, // High quality: both PB and 48h
      neitherCount: row.neither_count || 0, // Low quality: neither PB nor 48h
      doorsKnocked: row.doors_knocked,
      hoursOnDoors: row.estimated_hours_on_doors || 0,
      conversionRate: parseFloat(row.conversion_rate || '0'),
    }));

    // Top closers - with appointment outcomes breakdown
    const topClosersResult = startDate && endDate
      ? await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::int as sat_closed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_no_close')::int as sat_no_close,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'no_show')::int as no_show,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'cancelled')::int as cancelled,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'rescheduled')::int as rescheduled,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'completed')::int as completed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category IN ('sat_closed', 'sat_no_close', 'completed'))::int as appointments_sat,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::float /
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
          ORDER BY appointments_run DESC
          LIMIT 10
        `
      : await sql`
          SELECT
            u.repcard_user_id,
            u.name,
            u.role,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::int as sat_closed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_no_close')::int as sat_no_close,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'no_show')::int as no_show,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'cancelled')::int as cancelled,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'rescheduled')::int as rescheduled,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'completed')::int as completed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category IN ('sat_closed', 'sat_no_close', 'completed'))::int as appointments_sat,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::float /
                 COUNT(DISTINCT a.repcard_appointment_id)::float) * 100
              ELSE 0
            END as close_rate
          FROM users u
          LEFT JOIN repcard_appointments a ON a.closer_user_id = u.repcard_user_id::TEXT
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT a.repcard_appointment_id) > 0
          ORDER BY appointments_run DESC
          LIMIT 50
        `;

    const topClosers = getRows(topClosersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      appointmentsRun: row.appointments_run,
      satClosed: row.sat_closed || 0,
      satNoClose: row.sat_no_close || 0,
      noShow: row.no_show || 0,
      cancelled: row.cancelled || 0,
      rescheduled: row.rescheduled || 0,
      completed: row.completed || 0,
      appointmentsSat: row.appointments_sat || 0,
      closeRate: parseFloat(row.close_rate || '0'),
    }));

    // ========================================
    // 4. CANVASSING ACTIVITY
    // ========================================
    const canvassingResult = startDate && endDate
      ? await sql`
          SELECT
            DATE(c.created_at) as date,
            COUNT(DISTINCT c.repcard_customer_id)::int as doors_knocked,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_set
          FROM repcard_customers c
          LEFT JOIN repcard_appointments a ON a.repcard_customer_id = c.repcard_customer_id
            AND DATE(a.scheduled_at) = DATE(c.created_at)
          WHERE c.created_at >= ${startDate}::timestamptz
            AND c.created_at <= ${endDate}::timestamptz
          GROUP BY DATE(c.created_at)
          ORDER BY date DESC
        `
      : await sql`
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
    // SYNC STATUS
    // Get last successful sync time for appointments (most relevant for dashboard)
    // ========================================
    let lastSyncTime: Date | null = null;
    try {
      const lastSyncResult = await sql`
        SELECT completed_at, last_record_date
        FROM repcard_sync_log
        WHERE entity_type = 'appointments'
          AND status = 'completed'
        ORDER BY completed_at DESC
        LIMIT 1
      `;
      const lastSyncRows = getRows(lastSyncResult);
      if (lastSyncRows.length > 0 && lastSyncRows[0].completed_at) {
        lastSyncTime = new Date(lastSyncRows[0].completed_at);
      }
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error fetching sync status:', error);
      // Continue without sync status if query fails
    }

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
        dateRange: { startDate: startDateParam, endDate: endDateParam },
        cached: false,
        lastSyncTime: lastSyncTime ? lastSyncTime.toISOString() : null,
      },
    };

    // Cache result
    unifiedDashboardCache.set(cacheKey, {
      data: response,
      timestamp: Date.now()
    });

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
