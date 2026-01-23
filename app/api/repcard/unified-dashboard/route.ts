import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { toEasternStart, toEasternEnd } from '@/lib/utils/timezone';

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
    // The date string comes from the user's local timezone, but we convert it to Eastern for database queries
    let startDate: string | null = null;
    let endDate: string | null = null;
    
    if (startDateParam) {
      try {
        // Convert to start of day in Eastern Time (handles DST automatically)
        startDate = toEasternStart(startDateParam);
      } catch (error) {
        logError('unified-dashboard', error as Error, { requestId, context: 'date conversion', startDateParam });
        return NextResponse.json(
          { error: 'Invalid startDate parameter', message: error instanceof Error ? error.message : 'Unknown error' },
          { status: 400 }
        );
      }
    }
    
    if (endDateParam) {
      try {
        // Convert to end of day in Eastern Time (handles DST automatically)
        endDate = toEasternEnd(endDateParam);
      } catch (error) {
        logError('unified-dashboard', error as Error, { requestId, context: 'date conversion', endDateParam });
        return NextResponse.json(
          { error: 'Invalid endDate parameter', message: error instanceof Error ? error.message : 'Unknown error' },
          { status: 400 }
        );
      }
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
    let qualityResult;
    try {
      qualityResult = startDate && endDate
      ? await sql`
          SELECT
            -- Match RepCard dashboard: exclude reschedules AND cancelled/no-show appointments
            COUNT(*) FILTER (
              WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              AND status_category NOT IN ('cancelled', 'no_show')
              AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
            )::int as total_appointments,
            COUNT(*) FILTER (
              WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              AND status_category NOT IN ('cancelled', 'no_show')
              AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              AND is_within_48_hours = TRUE
            )::int as within_48h,
            COUNT(*) FILTER (
              WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              AND status_category NOT IN ('cancelled', 'no_show')
              AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              AND has_power_bill = TRUE
            )::int as with_power_bill,
            COUNT(*) FILTER (
              WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              AND status_category NOT IN ('cancelled', 'no_show')
              AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              AND is_within_48_hours = TRUE AND has_power_bill = TRUE
            )::int as both,
            COUNT(*) FILTER (
              WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              AND status_category NOT IN ('cancelled', 'no_show')
              AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              AND is_within_48_hours = FALSE AND has_power_bill = FALSE
            )::int as neither,
            COUNT(*) FILTER (WHERE is_reschedule = TRUE)::int as reschedules,
            COUNT(*) FILTER (
              WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              AND status_category NOT IN ('cancelled', 'no_show')
              AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              AND is_within_48_hours IS NULL
            )::int as null_48h,
            COUNT(*) FILTER (
              WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              AND status_category NOT IN ('cancelled', 'no_show')
              AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              AND has_power_bill IS NULL
            )::int as null_pb
          FROM repcard_appointments a
          WHERE (
            a.scheduled_at IS NOT NULL 
            AND a.scheduled_at IS NOT NULL
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
          )
        `
      : await sql`
          SELECT
            COUNT(*)::int as total_appointments,
            COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
            COUNT(*) FILTER (WHERE has_power_bill = TRUE)::int as with_power_bill,
            COUNT(*) FILTER (WHERE is_within_48_hours = TRUE AND has_power_bill = TRUE)::int as both,
            COUNT(*) FILTER (WHERE is_within_48_hours = FALSE AND has_power_bill = FALSE)::int as neither,
            COUNT(*) FILTER (WHERE is_reschedule = TRUE)::int as reschedules,
            COUNT(*) FILTER (WHERE is_within_48_hours IS NULL)::int as null_48h,
            COUNT(*) FILTER (WHERE has_power_bill IS NULL)::int as null_pb
          FROM repcard_appointments a
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in qualityResult query:', error);
      throw new Error(`Quality metrics query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }

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
    let topReschedulersResult;
    try {
      topReschedulersResult = startDate && endDate
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
          LEFT JOIN users u ON u.repcard_user_id = a.setter_user_id::int
          WHERE u.repcard_user_id IS NOT NULL
            AND a.scheduled_at IS NOT NULL
            AND a.scheduled_at IS NOT NULL
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
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
          LEFT JOIN users u ON u.repcard_user_id = a.setter_user_id::int
          WHERE u.repcard_user_id IS NOT NULL
          GROUP BY u.repcard_user_id, u.name
          HAVING COUNT(DISTINCT a.id) >= 5
          ORDER BY reschedule_rate DESC
          LIMIT 5
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in topReschedulersResult query:', error);
      throw new Error(`Top reschedulers query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }

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
    let officeResult;
    try {
      officeResult = startDate && endDate
      ? await sql`
          WITH office_door_knocks AS (
            SELECT 
              office_id,
              COUNT(DISTINCT id)::int as doors_knocked,
              COUNT(DISTINCT setter_user_id)::int as active_reps
            FROM repcard_door_knocks
            WHERE door_knocked_at >= ${startDate}::timestamptz
              AND door_knocked_at <= ${endDate}::timestamptz
            GROUP BY office_id
          ),
          office_appointments AS (
            SELECT 
              office_id,
              -- Match RepCard dashboard: exclude reschedules AND cancelled/no-show appointments
              COUNT(DISTINCT repcard_appointment_id) FILTER (
                WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
                AND status_category NOT IN ('cancelled', 'no_show')
                AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              )::int as appointments_set,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE disposition ILIKE '%closed%' AND (is_reschedule = FALSE OR is_reschedule IS NULL))::int as sales_closed,
              COUNT(DISTINCT repcard_appointment_id)::int as total_appointments
            FROM repcard_appointments
            WHERE scheduled_at IS NOT NULL
              AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
              AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
            GROUP BY office_id
          )
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            COALESCE(odk.doors_knocked, 0)::int as doors_knocked,
            COALESCE(oa.appointments_set, 0)::int as appointments_set,
            COALESCE(oa.sales_closed, 0)::int as sales_closed,
            COALESCE(odk.active_reps, 0)::int as active_reps,
            CASE
              WHEN COALESCE(odk.doors_knocked, 0) > 0 THEN
                (COALESCE(oa.appointments_set, 0)::float / COALESCE(odk.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate,
            CASE
              WHEN COALESCE(oa.total_appointments, 0) > 0 THEN
                (COALESCE(oa.sales_closed, 0)::float / COALESCE(oa.total_appointments, 0)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          LEFT JOIN office_door_knocks odk ON odk.office_id = o.repcard_office_id
          LEFT JOIN office_appointments oa ON oa.office_id = o.repcard_office_id
          WHERE COALESCE(odk.doors_knocked, 0) > 0 OR COALESCE(oa.appointments_set, 0) > 0
          ORDER BY doors_knocked DESC
          LIMIT 50
        `
      : await sql`
          WITH office_door_knocks AS (
            SELECT 
              office_id,
              COUNT(DISTINCT id)::int as doors_knocked,
              COUNT(DISTINCT setter_user_id)::int as active_reps
            FROM repcard_door_knocks
            GROUP BY office_id
          ),
          office_appointments AS (
            SELECT 
              office_id,
              -- Match RepCard dashboard: exclude reschedules AND cancelled/no-show appointments
              COUNT(DISTINCT repcard_appointment_id) FILTER (
                WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
                AND status_category NOT IN ('cancelled', 'no_show')
                AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
              )::int as appointments_set,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE disposition ILIKE '%closed%' AND (is_reschedule = FALSE OR is_reschedule IS NULL))::int as sales_closed,
              COUNT(DISTINCT repcard_appointment_id)::int as total_appointments
            FROM repcard_appointments
            GROUP BY office_id
          )
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            COALESCE(odk.doors_knocked, 0)::int as doors_knocked,
            COALESCE(oa.appointments_set, 0)::int as appointments_set,
            COALESCE(oa.sales_closed, 0)::int as sales_closed,
            COALESCE(odk.active_reps, 0)::int as active_reps,
            CASE
              WHEN COALESCE(odk.doors_knocked, 0) > 0 THEN
                (COALESCE(oa.appointments_set, 0)::float / COALESCE(odk.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate,
            CASE
              WHEN COALESCE(oa.total_appointments, 0) > 0 THEN
                (COALESCE(oa.sales_closed, 0)::float / COALESCE(oa.total_appointments, 0)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          LEFT JOIN office_door_knocks odk ON odk.office_id = o.repcard_office_id
          LEFT JOIN office_appointments oa ON oa.office_id = o.repcard_office_id
          WHERE COALESCE(odk.doors_knocked, 0) > 0 OR COALESCE(oa.appointments_set, 0) > 0
          ORDER BY doors_knocked DESC
          LIMIT 10
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in officeResult query:', error);
      throw new Error(`Office performance query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }

    // Get office-level setters and closers breakdown
    let officeSettersResult;
    try {
      officeSettersResult = startDate && endDate
      ? await sql`
          SELECT
            o.repcard_office_id,
            COUNT(DISTINCT dk.id)::int as doors_knocked,
            -- Gross appointments set (for gamification) - exclude ONLY reschedules
            -- Disposition doesn't matter - we want to gamify TODAY's door efforts
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
            )::int as appointments_set,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.has_power_bill = TRUE)::int as with_power_bill,
            COUNT(DISTINCT u.repcard_user_id) FILTER (WHERE u.role = 'setter')::int as setters_count
          FROM repcard_offices o
          LEFT JOIN repcard_door_knocks dk ON dk.office_id::int = o.repcard_office_id
            AND dk.door_knocked_at >= ${startDate}::timestamptz
            AND dk.door_knocked_at <= ${endDate}::timestamptz
          LEFT JOIN repcard_appointments a ON a.office_id::int = o.repcard_office_id
            AND a.scheduled_at IS NOT NULL
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
          LEFT JOIN users u ON u.repcard_user_id::text = dk.setter_user_id::text
          GROUP BY o.repcard_office_id
        `
      : await sql`
          SELECT
            o.repcard_office_id,
            COUNT(DISTINCT dk.id)::int as doors_knocked,
            -- Gross appointments set (for gamification) - exclude ONLY reschedules
            -- Disposition doesn't matter - we want to gamify TODAY's door efforts
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
            )::int as appointments_set,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.has_power_bill = TRUE)::int as with_power_bill,
            COUNT(DISTINCT u.repcard_user_id) FILTER (WHERE u.role = 'setter')::int as setters_count
          FROM repcard_offices o
          LEFT JOIN repcard_door_knocks dk ON dk.office_id::int = o.repcard_office_id
          LEFT JOIN repcard_appointments a ON a.office_id::int = o.repcard_office_id
          LEFT JOIN users u ON u.repcard_user_id::text = dk.setter_user_id::text
          GROUP BY o.repcard_office_id
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in officeSettersResult query:', error);
      throw new Error(`Office setters query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
    
    let officeClosersResult;
    try {
      officeClosersResult = startDate && endDate
      ? await sql`
          SELECT
            o.repcard_office_id,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::int as sat_closed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_no_close')::int as sat_no_close,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'no_show')::int as no_show,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'cancelled')::int as cancelled,
            COUNT(DISTINCT u.repcard_user_id) FILTER (WHERE u.role = 'closer')::int as closers_count
          FROM repcard_offices o
          LEFT JOIN repcard_appointments a ON NULLIF(a.office_id::text, '')::int = o.repcard_office_id
            AND a.scheduled_at IS NOT NULL
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
          LEFT JOIN users u ON u.repcard_user_id = a.closer_user_id::int
          GROUP BY o.repcard_office_id
        `
      : await sql`
          SELECT
            o.repcard_office_id,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::int as sat_closed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_no_close')::int as sat_no_close,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'no_show')::int as no_show,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'cancelled')::int as cancelled,
            COUNT(DISTINCT u.repcard_user_id) FILTER (WHERE u.role = 'closer')::int as closers_count
          FROM repcard_offices o
          LEFT JOIN repcard_appointments a ON NULLIF(a.office_id::text, '')::int = o.repcard_office_id
          LEFT JOIN users u ON u.repcard_user_id = a.closer_user_id::int
          GROUP BY o.repcard_office_id
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in officeClosersResult query:', error);
      throw new Error(`Office closers query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
    
    const officeSetters = new Map<string, any>();
    getRows(officeSettersResult).forEach((row: any) => {
      officeSetters.set(row.repcard_office_id, row);
    });
    
    const officeClosers = new Map<string, any>();
    getRows(officeClosersResult).forEach((row: any) => {
      officeClosers.set(row.repcard_office_id, row);
    });

    const officePerformance = getRows(officeResult).map((row: any) => {
      const setterMetrics = officeSetters.get(row.repcard_office_id) || {};
      const closerMetrics = officeClosers.get(row.repcard_office_id) || {};
      
      return {
        officeId: row.repcard_office_id,
        officeName: row.office_name,
        doorsKnocked: row.doors_knocked,
        appointmentsSet: row.appointments_set,
        salesClosed: row.sales_closed,
        activeReps: row.active_reps,
        conversionRate: parseFloat(row.conversion_rate || '0'),
        closeRate: parseFloat(row.close_rate || '0'),
        // Setter metrics by office
        settersCount: setterMetrics.setters_count || 0,
        setterWithin48h: setterMetrics.within_48h || 0,
        setterWithPowerBill: setterMetrics.with_power_bill || 0,
        // Closer metrics by office
        closersCount: closerMetrics.closers_count || 0,
        closerAppointmentsRun: closerMetrics.appointments_run || 0,
        closerSatClosed: closerMetrics.sat_closed || 0,
        closerSatNoClose: closerMetrics.sat_no_close || 0,
        closerNoShow: closerMetrics.no_show || 0,
        closerCancelled: closerMetrics.cancelled || 0,
      };
    });

    // ========================================
    // 3. REP LEADERBOARDS
    // ========================================

    // Top doors knocked
    // OPTIMIZED: Use CTEs for better performance, avoid multiple DISTINCT operations
    let topDoorsResult;
    try {
      topDoorsResult = startDate && endDate
      ? await sql`
          WITH door_knock_counts AS (
            SELECT 
              setter_user_id,
              COUNT(DISTINCT id)::int as doors_knocked
            FROM repcard_door_knocks
            WHERE door_knocked_at >= ${startDate}::timestamptz
              AND door_knocked_at <= ${endDate}::timestamptz
            GROUP BY setter_user_id
          ),
          appointment_counts AS (
            SELECT 
              setter_user_id::int as setter_user_id,
              -- Gross appointments set (for gamification) - exclude ONLY reschedules
              -- Disposition doesn't matter - we want to gamify TODAY's door efforts
              -- Use date comparison (not timestamp) to match schedule route pattern
              COUNT(DISTINCT repcard_appointment_id) FILTER (
                WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              )::int as appointments_set
            FROM repcard_appointments
            WHERE scheduled_at IS NOT NULL
              AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
              AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
            GROUP BY setter_user_id::int
          )
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            COALESCE(dkc.doors_knocked, 0)::int as doors_knocked,
            COALESCE(ac.appointments_set, 0)::int as appointments_set,
            CASE
              WHEN COALESCE(dkc.doors_knocked, 0) > 0 THEN
                (COALESCE(ac.appointments_set, 0)::float / COALESCE(dkc.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN door_knock_counts dkc ON dkc.setter_user_id = ru.repcard_user_id
          LEFT JOIN appointment_counts ac ON ac.setter_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'setter' OR ru.role IS NULL)
            AND (COALESCE(dkc.doors_knocked, 0) > 0 OR COALESCE(ac.appointments_set, 0) > 0)
          ORDER BY doors_knocked DESC
          LIMIT 10
        `
      : await sql`
          WITH door_knock_counts AS (
            SELECT 
              setter_user_id,
              COUNT(DISTINCT id)::int as doors_knocked
            FROM repcard_door_knocks
            GROUP BY setter_user_id
          ),
          appointment_counts AS (
            SELECT 
              setter_user_id::int as setter_user_id,
              -- Gross appointments set (for gamification) - exclude ONLY reschedules
              -- Disposition doesn't matter - we want to gamify TODAY's door efforts
              COUNT(DISTINCT repcard_appointment_id) FILTER (
                WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
              )::int as appointments_set
            FROM repcard_appointments
            GROUP BY setter_user_id::int
          )
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            COALESCE(dkc.doors_knocked, 0)::int as doors_knocked,
            COALESCE(ac.appointments_set, 0)::int as appointments_set,
            CASE
              WHEN COALESCE(dkc.doors_knocked, 0) > 0 THEN
                (COALESCE(ac.appointments_set, 0)::float / COALESCE(dkc.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN door_knock_counts dkc ON dkc.setter_user_id = ru.repcard_user_id
          LEFT JOIN appointment_counts ac ON ac.setter_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'setter' OR ru.role IS NULL)
            AND (COALESCE(dkc.doors_knocked, 0) > 0 OR COALESCE(ac.appointments_set, 0) > 0)
          ORDER BY doors_knocked DESC
          LIMIT 50
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in topDoorsResult query:', error);
      throw new Error(`Top doors query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }

    const topDoors = getRows(topDoorsResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      team: row.team || 'No Team',
      doorsKnocked: row.doors_knocked,
      appointmentsSet: row.appointments_set,
      conversionRate: parseFloat(row.conversion_rate || '0'),
    }));

    // Top appointment setters (with quality metrics + hours on doors)
    // Track: total, within 48h, with PB, both, neither, doors, hours on doors
    // FIXED: Start from repcard_users to ensure all RepCard users are included (even if not linked to users table)
    // FIXED: Count doors separately (all customers created by setter in date range)
    // FIXED: Count appointments separately (all appointments set by setter in date range)
    // FIXED: Join customers to appointments for quality metrics, but don't restrict doors by appointment date
    let topAppointmentSettersResult;
    try {
      topAppointmentSettersResult = startDate && endDate
      ? await sql`
          WITH door_knock_stats AS (
            -- Pre-calculate door knocks and hours per setter (more efficient than LATERAL)
            SELECT
              dk.setter_user_id,
              COUNT(DISTINCT dk.id)::int as doors_knocked,
              COALESCE((
                SELECT SUM(EXTRACT(EPOCH FROM (day_max - day_min)) / 3600)::int
                FROM (
                  SELECT 
                    DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York') as day,
                    MIN(dk2.door_knocked_at) as day_min,
                    MAX(dk2.door_knocked_at) as day_max
                  FROM repcard_door_knocks dk2
                  WHERE dk2.setter_user_id = dk.setter_user_id
                    AND dk2.door_knocked_at >= ${startDate}::timestamptz
                    AND dk2.door_knocked_at <= ${endDate}::timestamptz
                  GROUP BY DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York')
                ) daily_ranges
              ), 0)::int as estimated_hours_on_doors
            FROM repcard_door_knocks dk
            WHERE dk.door_knocked_at >= ${startDate}::timestamptz
              AND dk.door_knocked_at <= ${endDate}::timestamptz
            GROUP BY dk.setter_user_id
          )
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            -- Gross appointments set today (for gamification) - exclude ONLY reschedules
            -- Disposition doesn't matter - we want to gamify TODAY's door efforts
            -- Quality metrics (48h, power bill) are shown separately on the same row
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
            )::int as appointments_set,
            -- Quality metrics for appointments in date range (exclude cancelled/no-show to match RepCard)
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.status_category NOT IN ('cancelled', 'no_show')
              AND (a.disposition IS NULL OR a.disposition NOT ILIKE '%cancel%' AND a.disposition NOT ILIKE '%no.show%' AND a.disposition NOT ILIKE '%no_show%')
              AND a.is_within_48_hours = TRUE
            )::int as within_48h_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.status_category NOT IN ('cancelled', 'no_show')
              AND (a.disposition IS NULL OR a.disposition NOT ILIKE '%cancel%' AND a.disposition NOT ILIKE '%no.show%' AND a.disposition NOT ILIKE '%no_show%')
              AND a.has_power_bill = TRUE
            )::int as with_power_bill_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.status_category NOT IN ('cancelled', 'no_show')
              AND (a.disposition IS NULL OR a.disposition NOT ILIKE '%cancel%' AND a.disposition NOT ILIKE '%no.show%' AND a.disposition NOT ILIKE '%no_show%')
              AND a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE
            )::int as both_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.status_category NOT IN ('cancelled', 'no_show')
              AND (a.disposition IS NULL OR a.disposition NOT ILIKE '%cancel%' AND a.disposition NOT ILIKE '%no.show%' AND a.disposition NOT ILIKE '%no_show%')
              AND a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE
            )::int as neither_count,
            -- Doors knocked from pre-calculated CTE
            COALESCE(dks.doors_knocked, 0)::int as doors_knocked,
            -- Hours on doors from pre-calculated CTE
            COALESCE(dks.estimated_hours_on_doors, 0)::int as estimated_hours_on_doors,
            CASE
              WHEN COALESCE(dks.doors_knocked, 0) > 0 THEN
                (COUNT(DISTINCT a.id)::float / COALESCE(dks.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          -- Appointments set by this user in date range
          -- Convert scheduled_at to Eastern Time date for accurate day-boundary comparison (matches schedule route)
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = ru.repcard_user_id
            AND a.scheduled_at IS NOT NULL
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
          -- Door knock stats from CTE (more efficient than LATERAL)
          LEFT JOIN door_knock_stats dks ON dks.setter_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'setter' OR ru.role IS NULL)
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team'), COALESCE(dks.doors_knocked, 0)::int, COALESCE(dks.estimated_hours_on_doors, 0)::int
          HAVING COUNT(DISTINCT a.id) > 0 OR COALESCE(dks.doors_knocked, 0) > 0
          ORDER BY appointments_set DESC
          LIMIT 10
        `
      : await sql`
          WITH door_knock_stats AS (
            -- Pre-calculate door knocks and hours per setter (more efficient than LATERAL)
            SELECT
              dk.setter_user_id,
              COUNT(DISTINCT dk.id)::int as doors_knocked,
              COALESCE((
                SELECT SUM(EXTRACT(EPOCH FROM (day_max - day_min)) / 3600)::int
                FROM (
                  SELECT 
                    DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York') as day,
                    MIN(dk2.door_knocked_at) as day_min,
                    MAX(dk2.door_knocked_at) as day_max
                  FROM repcard_door_knocks dk2
                  WHERE dk2.setter_user_id = dk.setter_user_id
                  GROUP BY DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York')
                ) daily_ranges
              ), 0)::int as estimated_hours_on_doors
            FROM repcard_door_knocks dk
            GROUP BY dk.setter_user_id
          )
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            -- Match RepCard dashboard: exclude reschedules AND cancelled/no-show appointments
            -- Gross appointments set (for gamification) - exclude ONLY reschedules
            -- Disposition doesn't matter - we want to gamify TODAY's door efforts
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
            )::int as appointments_set,
            -- Quality metrics shown separately (can exclude cancelled/no-show for quality)
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.is_within_48_hours = TRUE
            )::int as within_48h_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.has_power_bill = TRUE
            )::int as with_power_bill_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.is_within_48_hours = TRUE AND a.has_power_bill = TRUE
            )::int as both_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE
            )::int as neither_count,
            -- Doors knocked from pre-calculated CTE
            COALESCE(dks.doors_knocked, 0)::int as doors_knocked,
            -- Hours on doors from pre-calculated CTE
            COALESCE(dks.estimated_hours_on_doors, 0)::int as estimated_hours_on_doors,
            CASE
              WHEN COALESCE(dks.doors_knocked, 0) > 0 THEN
                (COUNT(DISTINCT a.id)::float / COALESCE(dks.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          -- Appointments set by this user
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = ru.repcard_user_id
          -- Door knock stats from CTE (more efficient than LATERAL)
          LEFT JOIN door_knock_stats dks ON dks.setter_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'setter' OR ru.role IS NULL)
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team'), COALESCE(dks.doors_knocked, 0)::int, COALESCE(dks.estimated_hours_on_doors, 0)::int
          HAVING COUNT(DISTINCT a.id) > 0 OR COALESCE(dks.doors_knocked, 0) > 0
          ORDER BY appointments_set DESC
          LIMIT 50
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in topAppointmentSettersResult query:', error);
      console.error('[RepCard Unified Dashboard] Error details:', error instanceof Error ? error.stack : error);
      throw new Error(`Top appointment setters query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }

    const topAppointmentSetters = getRows(topAppointmentSettersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      team: row.team || 'No Team',
      appointmentsSet: row.appointments_set || 0,
      within48hCount: row.within_48h_count || 0,
      withPowerBillCount: row.with_power_bill_count || 0,
      bothCount: row.both_count || 0, // High quality: both PB and 48h
      neitherCount: row.neither_count || 0, // Low quality: neither PB nor 48h
      doorsKnocked: row.doors_knocked || 0,
      hoursOnDoors: row.estimated_hours_on_doors || 0,
      conversionRate: parseFloat(row.conversion_rate || '0'),
      // Calculate 48h speed percentage
      speed48hPercent: row.appointments_set > 0 
        ? ((row.within_48h_count || 0) / row.appointments_set) * 100 
        : 0,
    }));

    // Top closers - with appointment outcomes breakdown
    // FIXED: Start from repcard_users to ensure all RepCard users are included
    let topClosersResult;
    try {
      topClosersResult = startDate && endDate
      ? await sql`
          WITH closer_appointment_stats AS (
            SELECT
              closer_user_id::int as closer_user_id,
              COUNT(DISTINCT repcard_appointment_id)::int as appointments_run,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'sat_closed')::int as sat_closed,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'sat_no_close')::int as sat_no_close,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'no_show')::int as no_show,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'cancelled')::int as cancelled,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'rescheduled')::int as rescheduled,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'completed')::int as completed,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category IN ('sat_closed', 'sat_no_close', 'completed'))::int as appointments_sat
            FROM repcard_appointments
            WHERE closer_user_id IS NOT NULL
              AND scheduled_at IS NOT NULL
              AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
              AND (scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
            GROUP BY closer_user_id::int
          )
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            COALESCE(cas.appointments_run, 0)::int as appointments_run,
            COALESCE(cas.sat_closed, 0)::int as sat_closed,
            COALESCE(cas.sat_no_close, 0)::int as sat_no_close,
            COALESCE(cas.no_show, 0)::int as no_show,
            COALESCE(cas.cancelled, 0)::int as cancelled,
            COALESCE(cas.rescheduled, 0)::int as rescheduled,
            COALESCE(cas.completed, 0)::int as completed,
            COALESCE(cas.appointments_sat, 0)::int as appointments_sat,
            CASE
              WHEN COALESCE(cas.appointments_run, 0) > 0 THEN
                (COALESCE(cas.sat_closed, 0)::float / COALESCE(cas.appointments_run, 0)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN closer_appointment_stats cas ON cas.closer_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'closer' OR ru.role IS NULL)
            AND COALESCE(cas.appointments_run, 0) > 0
          ORDER BY appointments_run DESC
          LIMIT 10
        `
      : await sql`
          WITH closer_appointment_stats AS (
            SELECT
              closer_user_id::int as closer_user_id,
              COUNT(DISTINCT repcard_appointment_id)::int as appointments_run,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'sat_closed')::int as sat_closed,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'sat_no_close')::int as sat_no_close,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'no_show')::int as no_show,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'cancelled')::int as cancelled,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'rescheduled')::int as rescheduled,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category = 'completed')::int as completed,
              COUNT(DISTINCT repcard_appointment_id) FILTER (WHERE status_category IN ('sat_closed', 'sat_no_close', 'completed'))::int as appointments_sat
            FROM repcard_appointments
            WHERE closer_user_id IS NOT NULL
            GROUP BY closer_user_id::int
          )
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            COALESCE(cas.appointments_run, 0)::int as appointments_run,
            COALESCE(cas.sat_closed, 0)::int as sat_closed,
            COALESCE(cas.sat_no_close, 0)::int as sat_no_close,
            COALESCE(cas.no_show, 0)::int as no_show,
            COALESCE(cas.cancelled, 0)::int as cancelled,
            COALESCE(cas.rescheduled, 0)::int as rescheduled,
            COALESCE(cas.completed, 0)::int as completed,
            COALESCE(cas.appointments_sat, 0)::int as appointments_sat,
            CASE
              WHEN COALESCE(cas.appointments_run, 0) > 0 THEN
                (COALESCE(cas.sat_closed, 0)::float / COALESCE(cas.appointments_run, 0)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN closer_appointment_stats cas ON cas.closer_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'closer' OR ru.role IS NULL)
            AND COALESCE(cas.appointments_run, 0) > 0
          ORDER BY appointments_run DESC
          LIMIT 50
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in topClosersResult query:', error);
      throw new Error(`Top closers query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }

    // Get all unique dispositions for closers (to track all outcomes, not just the 7 categories)
    let allOutcomesResult;
    try {
      allOutcomesResult = startDate && endDate
      ? await sql`
          SELECT
            u.repcard_user_id,
            a.disposition,
            COUNT(DISTINCT a.repcard_appointment_id)::int as count
          FROM users u
          INNER JOIN repcard_appointments a ON a.closer_user_id::int = u.repcard_user_id
            AND a.scheduled_at IS NOT NULL
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
          WHERE u.repcard_user_id IS NOT NULL
            AND a.disposition IS NOT NULL
          GROUP BY u.repcard_user_id, a.disposition
        `
      : await sql`
          SELECT
            u.repcard_user_id,
            a.disposition,
            COUNT(DISTINCT a.repcard_appointment_id)::int as count
          FROM users u
          INNER JOIN repcard_appointments a ON a.closer_user_id::int = u.repcard_user_id
          WHERE u.repcard_user_id IS NOT NULL
            AND a.disposition IS NOT NULL
          GROUP BY u.repcard_user_id, a.disposition
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in allOutcomesResult query:', error);
      throw new Error(`All outcomes query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
    
    const allOutcomes = getRows(allOutcomesResult);
    const outcomesByUser = new Map<string, Array<{ disposition: string; count: number }>>();
    allOutcomes.forEach((outcome: any) => {
      const userId = outcome.repcard_user_id;
      if (!outcomesByUser.has(userId)) {
        outcomesByUser.set(userId, []);
      }
      outcomesByUser.get(userId)!.push({
        disposition: outcome.disposition,
        count: outcome.count
      });
    });

    const topClosers = getRows(topClosersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      team: row.team || 'No Team',
      appointmentsRun: row.appointments_run,
      satClosed: row.sat_closed || 0,
      satNoClose: row.sat_no_close || 0,
      noShow: row.no_show || 0,
      cancelled: row.cancelled || 0,
      rescheduled: row.rescheduled || 0,
      completed: row.completed || 0,
      appointmentsSat: row.appointments_sat || 0,
      closeRate: parseFloat(row.close_rate || '0'),
      allOutcomes: outcomesByUser.get(row.repcard_user_id) || [], // All unique dispositions with counts
    }));

    // ========================================
    // 3.5. OFFICE SETTERS AND CLOSERS BREAKDOWN
    // Detailed breakdown of setters and closers by office for expandable view
    // ========================================
    let officeSettersBreakdownResult;
    try {
      officeSettersBreakdownResult = startDate && endDate
      ? await sql`
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            u.repcard_user_id,
            u.name as rep_name,
            u.role,
            COUNT(DISTINCT dk.id)::int as doors_knocked,
            -- Gross appointments set (for gamification) - exclude ONLY reschedules
            -- Disposition doesn't matter - we want to gamify TODAY's door efforts
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
            )::int as appointments_set,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.has_power_bill = TRUE)::int as with_power_bill,
            COALESCE((
              SELECT SUM(EXTRACT(EPOCH FROM (day_max - day_min)) / 3600)::int
              FROM (
                SELECT 
                  DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York') as day,
                  MIN(dk2.door_knocked_at) as day_min,
                  MAX(dk2.door_knocked_at) as day_max
                FROM repcard_door_knocks dk2
                WHERE dk2.setter_user_id::int = u.repcard_user_id
                  AND dk2.office_id::int = o.repcard_office_id
                  AND dk2.door_knocked_at >= ${startDate}::timestamptz
                  AND dk2.door_knocked_at <= ${endDate}::timestamptz
                GROUP BY DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York')
              ) daily_ranges
            ), 0)::int as hours_on_doors
          FROM repcard_offices o
          INNER JOIN repcard_door_knocks dk ON dk.office_id::int = o.repcard_office_id
            AND dk.door_knocked_at >= ${startDate}::timestamptz
            AND dk.door_knocked_at <= ${endDate}::timestamptz
          INNER JOIN users u ON u.repcard_user_id::text = dk.setter_user_id::text
            AND u.role = 'setter'
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = u.repcard_user_id
            AND a.scheduled_at >= ${startDate}::timestamptz
            AND a.scheduled_at <= ${endDate}::timestamptz
          GROUP BY o.repcard_office_id, o.name, u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT dk.id) > 0
          ORDER BY o.repcard_office_id, doors_knocked DESC
        `
      : await sql`
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            u.repcard_user_id,
            u.name as rep_name,
            u.role,
            COUNT(DISTINCT dk.id)::int as doors_knocked,
            -- Gross appointments set (for gamification) - exclude ONLY reschedules
            -- Disposition doesn't matter - we want to gamify TODAY's door efforts
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
            )::int as appointments_set,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.is_within_48_hours = TRUE)::int as within_48h,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND a.has_power_bill = TRUE)::int as with_power_bill,
            COALESCE((
              SELECT SUM(EXTRACT(EPOCH FROM (day_max - day_min)) / 3600)::int
              FROM (
                SELECT 
                  DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York') as day,
                  MIN(dk2.door_knocked_at) as day_min,
                  MAX(dk2.door_knocked_at) as day_max
                FROM repcard_door_knocks dk2
                WHERE dk2.setter_user_id::int = u.repcard_user_id
                  AND dk2.office_id::int = o.repcard_office_id
                GROUP BY DATE(dk2.door_knocked_at AT TIME ZONE 'America/New_York')
              ) daily_ranges
            ), 0)::int as hours_on_doors
          FROM repcard_offices o
          INNER JOIN repcard_door_knocks dk ON dk.office_id::int = o.repcard_office_id
          INNER JOIN users u ON u.repcard_user_id::text = dk.setter_user_id::text
            AND u.role = 'setter'
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = u.repcard_user_id
          GROUP BY o.repcard_office_id, o.name, u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT dk.id) > 0
          ORDER BY o.repcard_office_id, doors_knocked DESC
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in officeSettersBreakdownResult query:', error);
      throw new Error(`Office setters breakdown query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
    
    let officeClosersBreakdownResult;
    try {
      officeClosersBreakdownResult = startDate && endDate
      ? await sql`
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            u.repcard_user_id,
            u.name as rep_name,
            u.role,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::int as sat_closed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_no_close')::int as sat_no_close,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'no_show')::int as no_show,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'cancelled')::int as cancelled,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::float /
                 COUNT(DISTINCT a.repcard_appointment_id)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          INNER JOIN repcard_appointments a ON NULLIF(a.office_id::text, '')::int = o.repcard_office_id
            AND a.scheduled_at IS NOT NULL
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date >= ${startDateParam}::date
            AND (a.scheduled_at AT TIME ZONE 'UTC' AT TIME ZONE 'America/New_York')::date <= ${endDateParam}::date
          INNER JOIN users u ON u.repcard_user_id = a.closer_user_id::int
            AND u.role = 'closer'
          GROUP BY o.repcard_office_id, o.name, u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT a.repcard_appointment_id) > 0
          ORDER BY o.repcard_office_id, appointments_run DESC
        `
      : await sql`
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            u.repcard_user_id,
            u.name as rep_name,
            u.role,
            COUNT(DISTINCT a.repcard_appointment_id)::int as appointments_run,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::int as sat_closed,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_no_close')::int as sat_no_close,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'no_show')::int as no_show,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'cancelled')::int as cancelled,
            CASE
              WHEN COUNT(DISTINCT a.repcard_appointment_id) > 0 THEN
                (COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE a.status_category = 'sat_closed')::float /
                 COUNT(DISTINCT a.repcard_appointment_id)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          INNER JOIN repcard_appointments a ON NULLIF(a.office_id::text, '')::int = o.repcard_office_id
          INNER JOIN users u ON u.repcard_user_id = a.closer_user_id::int
            AND u.role = 'closer'
          GROUP BY o.repcard_office_id, o.name, u.repcard_user_id, u.name, u.role
          HAVING COUNT(DISTINCT a.repcard_appointment_id) > 0
          ORDER BY o.repcard_office_id, appointments_run DESC
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in officeClosersBreakdownResult query:', error);
      throw new Error(`Office closers breakdown query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }
    
    const officeSettersBreakdown = getRows(officeSettersBreakdownResult).map((row: any) => ({
      officeId: row.repcard_office_id,
      officeName: row.office_name,
      userId: row.repcard_user_id,
      name: row.rep_name,
      role: row.role,
      doorsKnocked: row.doors_knocked,
      appointmentsSet: row.appointments_set,
      within48h: row.within_48h,
      withPowerBill: row.with_power_bill,
      hoursOnDoors: row.hours_on_doors || 0,
    }));
    
    const officeClosersBreakdown = getRows(officeClosersBreakdownResult).map((row: any) => ({
      officeId: row.repcard_office_id,
      officeName: row.office_name,
      userId: row.repcard_user_id,
      name: row.rep_name,
      role: row.role,
      appointmentsRun: row.appointments_run,
      satClosed: row.sat_closed,
      satNoClose: row.sat_no_close,
      noShow: row.no_show,
      cancelled: row.cancelled,
      closeRate: parseFloat(row.close_rate || '0'),
    }));

    // ========================================
    // 4. CANVASSING ACTIVITY
    // ========================================
    let canvassingResult;
    try {
      canvassingResult = startDate && endDate
      ? await sql`
          SELECT
            DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York') as date,
            COUNT(DISTINCT dk.id)::int as doors_knocked,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND DATE(a.scheduled_at AT TIME ZONE 'America/New_York') = DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York'))::int as appointments_set
          FROM repcard_door_knocks dk
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = dk.setter_user_id::int
            AND DATE(a.scheduled_at AT TIME ZONE 'America/New_York') = DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York')
          WHERE dk.door_knocked_at >= ${startDate}::timestamptz
            AND dk.door_knocked_at <= ${endDate}::timestamptz
          GROUP BY DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York')
          ORDER BY date DESC
        `
      : await sql`
          SELECT
            DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York') as date,
            COUNT(DISTINCT dk.id)::int as doors_knocked,
            COUNT(DISTINCT a.repcard_appointment_id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL) AND DATE(a.scheduled_at AT TIME ZONE 'America/New_York') = DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York'))::int as appointments_set
          FROM repcard_door_knocks dk
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = dk.setter_user_id::int
            AND DATE(a.scheduled_at AT TIME ZONE 'America/New_York') = DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York')
          WHERE dk.door_knocked_at >= NOW() - INTERVAL '30 days'
          GROUP BY DATE(dk.door_knocked_at AT TIME ZONE 'America/New_York')
          ORDER BY date DESC
          LIMIT 30
        `;
    } catch (error) {
      console.error('[RepCard Unified Dashboard] Error in canvassingResult query:', error);
      throw new Error(`Canvassing query failed: ${error instanceof Error ? error.message : 'Unknown error'}`);
    }

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
      officeSettersBreakdown,
      officeClosersBreakdown,
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
