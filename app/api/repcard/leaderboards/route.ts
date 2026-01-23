import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { sql } from '@/lib/db/client';
import { toEasternStart, toEasternEnd } from '@/lib/utils/timezone';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

// Cache implementation
const leaderboardCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 300000; // 5 minutes
const MAX_CACHE_ENTRIES = 100;

function cleanCache() {
  const now = Date.now();
  for (const [key, value] of Array.from(leaderboardCache.entries())) {
    if (now - value.timestamp > CACHE_TTL) {
      leaderboardCache.delete(key);
    }
  }
  
  if (leaderboardCache.size > MAX_CACHE_ENTRIES) {
    const entries = Array.from(leaderboardCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    const toDelete = entries.slice(0, leaderboardCache.size - MAX_CACHE_ENTRIES);
    toDelete.forEach(([key]) => leaderboardCache.delete(key));
  }
}

export async function GET(request: NextRequest) {
  const requestId = `leaderboards-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('repcard-leaderboards', request, { requestId });

    const session = await getServerSession(authOptions);
    if (!session?.user) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    // Check authorization (same roles as analytics)
    const authorizedRoles = ['team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'];
    if (!session.user.role || !authorizedRoles.includes(session.user.role)) {
      return NextResponse.json({ error: 'Forbidden' }, { status: 403 });
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');
    const officeIdsParam = searchParams.get('officeIds');

    // Convert date strings to proper timestamps in Eastern Time
    let startDate: string | null = null;
    let endDate: string | null = null;
    
    if (startDateParam) {
      try {
        startDate = toEasternStart(startDateParam);
      } catch (error) {
        logError('repcard-leaderboards', error as Error, { requestId, context: 'date conversion', startDateParam });
        return NextResponse.json(
          { error: 'Invalid startDate parameter', message: error instanceof Error ? error.message : 'Unknown error' },
          { status: 400 }
        );
      }
    }
    
    if (endDateParam) {
      try {
        endDate = toEasternEnd(endDateParam);
      } catch (error) {
        logError('repcard-leaderboards', error as Error, { requestId, context: 'date conversion', endDateParam });
        return NextResponse.json(
          { error: 'Invalid endDate parameter', message: error instanceof Error ? error.message : 'Unknown error' },
          { status: 400 }
        );
      }
    }

    // Build cache key
    const cacheKey = `leaderboards:${startDateParam || 'all'}:${endDateParam || 'all'}:${officeIdsParam || 'all'}`;
    cleanCache();
    
    const cached = leaderboardCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      logApiResponse('repcard-leaderboards', 200, { requestId, cached: true });
      return NextResponse.json(cached.data);
    }

    // Parse office IDs
    const officeIds = officeIdsParam ? officeIdsParam.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id)) : null;

    // Build queries
    const hasDateFilter = !!(startDateParam && endDateParam);
    const hasOfficeFilter = !!(officeIds && officeIds.length > 0);
    
    // Validate date parameters if date filter is requested
    if (hasDateFilter && (!startDateParam || !endDateParam)) {
      return NextResponse.json(
        { error: 'Both startDate and endDate are required for date filtering' },
        { status: 400 }
      );
    }

    // ========================================
    // SETTERS LEADERBOARD
    // ========================================
    let settersResult;
    try {
      settersResult = hasDateFilter
        ? await sql`
          WITH door_knock_stats AS (
            SELECT 
              dk.setter_user_id,
              COUNT(DISTINCT dk.id)::int as doors_knocked,
              COALESCE(
                SUM(
                  EXTRACT(EPOCH FROM (
                    SELECT MAX(daily_ranges.day_max) - MIN(daily_ranges.day_min)
                    FROM (
                      SELECT 
                        DATE(dk2.door_knocked_at) as day,
                        MIN(dk2.door_knocked_at) as day_min,
                        MAX(dk2.door_knocked_at) as day_max
                      FROM repcard_door_knocks dk2
                      WHERE dk2.setter_user_id = dk.setter_user_id
                        AND dk2.door_knocked_at >= ${startDate}::timestamptz
                        AND dk2.door_knocked_at <= ${endDate}::timestamptz
                      GROUP BY DATE(dk2.door_knocked_at)
                    ) daily_ranges
                  )) / 3600
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
            -- Gross appointments set (for gamification) - exclude ONLY reschedules
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
            )::int as appointments_set,
            -- Quality metrics (exclude cancelled/no-show)
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
            )::int as high_quality_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.status_category NOT IN ('cancelled', 'no_show')
              AND (a.disposition IS NULL OR a.disposition NOT ILIKE '%cancel%' AND a.disposition NOT ILIKE '%no.show%' AND a.disposition NOT ILIKE '%no_show%')
              AND a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE
            )::int as low_quality_count,
            COALESCE(dks.doors_knocked, 0)::int as doors_knocked,
            COALESCE(dks.estimated_hours_on_doors, 0)::int as estimated_hours_on_doors,
            CASE
              WHEN COALESCE(dks.doors_knocked, 0) > 0 THEN
                (COUNT(DISTINCT a.id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL))::float / COALESCE(dks.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = ru.repcard_user_id
            AND a.scheduled_at IS NOT NULL
            AND a.scheduled_at::date >= ${startDateParam}::date 
            AND a.scheduled_at::date <= ${endDateParam}::date
          LEFT JOIN door_knock_stats dks ON dks.setter_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'setter' OR ru.role IS NULL)
          ${hasOfficeFilter ? sql`AND EXISTS (
            SELECT 1 FROM offices o
            WHERE o.name = COALESCE(u.sales_office[1], ru.office_name)
            AND o.quickbase_office_id = ANY(${officeIds}::int[])
          )` : sql``}
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team'), COALESCE(dks.doors_knocked, 0)::int, COALESCE(dks.estimated_hours_on_doors, 0)::int
          HAVING COUNT(DISTINCT a.id) > 0 OR COALESCE(dks.doors_knocked, 0) > 0
          ORDER BY appointments_set DESC
        `
        : await sql`
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            COUNT(DISTINCT a.id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL))::int as appointments_set,
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
            )::int as high_quality_count,
            COUNT(DISTINCT a.id) FILTER (
              WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              AND a.status_category NOT IN ('cancelled', 'no_show')
              AND (a.disposition IS NULL OR a.disposition NOT ILIKE '%cancel%' AND a.disposition NOT ILIKE '%no.show%' AND a.disposition NOT ILIKE '%no_show%')
              AND a.is_within_48_hours = FALSE AND a.has_power_bill = FALSE
            )::int as low_quality_count,
            COUNT(DISTINCT dk.id)::int as doors_knocked,
            0::int as estimated_hours_on_doors,
            CASE
              WHEN COUNT(DISTINCT dk.id) > 0 THEN
                (COUNT(DISTINCT a.id) FILTER (WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL))::float / COUNT(DISTINCT dk.id)::float) * 100
              ELSE 0
            END as conversion_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN repcard_appointments a ON a.setter_user_id::int = ru.repcard_user_id
            AND a.scheduled_at IS NOT NULL
          LEFT JOIN repcard_door_knocks dk ON dk.setter_user_id = ru.repcard_user_id
          WHERE ru.status = 1 AND (ru.role = 'setter' OR ru.role IS NULL)
          ${hasOfficeFilter ? sql`AND EXISTS (
            SELECT 1 FROM offices o
            WHERE o.name = COALESCE(u.sales_office[1], ru.office_name)
            AND o.quickbase_office_id = ANY(${officeIds}::int[])
          )` : sql``}
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team')
          HAVING COUNT(DISTINCT a.id) > 0 OR COUNT(DISTINCT dk.id) > 0
          ORDER BY appointments_set DESC
        `;
    } catch (error) {
      logError('repcard-leaderboards', error as Error, { requestId, context: 'setters query' });
      throw error;
    }

    // ========================================
    // CLOSERS LEADERBOARD
    // ========================================
    let closersResult;
    try {
      closersResult = hasDateFilter
        ? await sql`
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            COUNT(DISTINCT a.id) FILTER (
              WHERE a.closer_user_id IS NOT NULL
            )::int as appointments_run,
            COUNT(DISTINCT a.id) FILTER (
              WHERE a.status_category IN ('sat_closed', 'completed')
            )::int as sat_closed,
            COUNT(DISTINCT a.id) FILTER (
              WHERE a.status_category = 'sat_no_close'
            )::int as sat_no_close,
            COUNT(DISTINCT a.id) FILTER (
              WHERE a.status_category = 'no_show'
            )::int as no_show,
            COUNT(DISTINCT a.id) FILTER (
              WHERE a.status_category = 'cancelled'
            )::int as cancelled,
            CASE
              WHEN COUNT(DISTINCT a.id) FILTER (WHERE a.closer_user_id IS NOT NULL) > 0 THEN
                (COUNT(DISTINCT a.id) FILTER (WHERE a.status_category IN ('sat_closed', 'completed'))::float / 
                 COUNT(DISTINCT a.id) FILTER (WHERE a.closer_user_id IS NOT NULL)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN repcard_appointments a ON a.closer_user_id::int = ru.repcard_user_id
            AND a.scheduled_at IS NOT NULL
            AND a.scheduled_at::date >= ${startDateParam}::date 
            AND a.scheduled_at::date <= ${endDateParam}::date
          WHERE ru.status = 1 AND (ru.role = 'closer' OR ru.role IS NULL)
          ${hasOfficeFilter ? sql`AND EXISTS (
            SELECT 1 FROM offices o
            WHERE o.name = COALESCE(u.sales_office[1], ru.office_name)
            AND o.quickbase_office_id = ANY(${officeIds}::int[])
          )` : sql``}
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team')
          HAVING COUNT(DISTINCT a.id) > 0
          ORDER BY sat_closed DESC
        `
        : await sql`
          SELECT
            ru.repcard_user_id,
            COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as name,
            COALESCE(u.role, ru.role) as role,
            COALESCE(ru.team, 'No Team') as team,
            COUNT(DISTINCT a.id) FILTER (WHERE a.closer_user_id IS NOT NULL)::int as appointments_run,
            COUNT(DISTINCT a.id) FILTER (WHERE a.status_category IN ('sat_closed', 'completed'))::int as sat_closed,
            COUNT(DISTINCT a.id) FILTER (WHERE a.status_category = 'sat_no_close')::int as sat_no_close,
            COUNT(DISTINCT a.id) FILTER (WHERE a.status_category = 'no_show')::int as no_show,
            COUNT(DISTINCT a.id) FILTER (WHERE a.status_category = 'cancelled')::int as cancelled,
            CASE
              WHEN COUNT(DISTINCT a.id) FILTER (WHERE a.closer_user_id IS NOT NULL) > 0 THEN
                (COUNT(DISTINCT a.id) FILTER (WHERE a.status_category IN ('sat_closed', 'completed'))::float / 
                 COUNT(DISTINCT a.id) FILTER (WHERE a.closer_user_id IS NOT NULL)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_users ru
          LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
          LEFT JOIN repcard_appointments a ON a.closer_user_id::int = ru.repcard_user_id
            AND a.scheduled_at IS NOT NULL
          WHERE ru.status = 1 AND (ru.role = 'closer' OR ru.role IS NULL)
          ${hasOfficeFilter ? sql`AND EXISTS (
            SELECT 1 FROM offices o
            WHERE o.name = COALESCE(u.sales_office[1], ru.office_name)
            AND o.quickbase_office_id = ANY(${officeIds}::int[])
          )` : sql``}
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team')
          HAVING COUNT(DISTINCT a.id) > 0
          ORDER BY sat_closed DESC
        `;
    } catch (error) {
      logError('repcard-leaderboards', error as Error, { requestId, context: 'closers query' });
      throw error;
    }

    // ========================================
    // OFFICES LEADERBOARD
    // ========================================
    let officesResult;
    try {
      officesResult = hasDateFilter
        ? await sql`
          WITH office_setters AS (
            SELECT
              o.repcard_office_id,
              COUNT(DISTINCT ru.repcard_user_id) FILTER (WHERE ru.role = 'setter' OR ru.role IS NULL)::int as setters_count,
              COUNT(DISTINCT dk.id)::int as doors_knocked,
              COUNT(DISTINCT a.id) FILTER (
                WHERE a.setter_user_id IS NOT NULL
                AND (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              )::int as appointments_set
            FROM repcard_offices o
            LEFT JOIN repcard_users ru ON ru.office_id = o.repcard_office_id
            LEFT JOIN repcard_door_knocks dk ON dk.setter_user_id = ru.repcard_user_id
              AND dk.door_knocked_at >= ${startDate}::timestamptz
              AND dk.door_knocked_at <= ${endDate}::timestamptz
            LEFT JOIN repcard_appointments a ON a.setter_user_id::int = ru.repcard_user_id
              AND a.scheduled_at IS NOT NULL
              AND a.scheduled_at::date >= ${startDateParam}::date 
              AND a.scheduled_at::date <= ${endDateParam}::date
            ${hasOfficeFilter ? sql`WHERE o.repcard_office_id = ANY(${officeIds}::int[])` : sql``}
            GROUP BY o.repcard_office_id
          ),
          office_closers AS (
            SELECT
              o.repcard_office_id,
              COUNT(DISTINCT ru.repcard_user_id) FILTER (WHERE ru.role = 'closer' OR ru.role IS NULL)::int as closers_count,
              COUNT(DISTINCT a.id) FILTER (WHERE a.closer_user_id IS NOT NULL)::int as appointments_run,
              COUNT(DISTINCT a.id) FILTER (WHERE a.status_category IN ('sat_closed', 'completed'))::int as sales_closed
            FROM repcard_offices o
            LEFT JOIN repcard_users ru ON ru.office_id = o.repcard_office_id
            LEFT JOIN repcard_appointments a ON a.closer_user_id::int = ru.repcard_user_id
              AND a.scheduled_at IS NOT NULL
              AND a.scheduled_at::date >= ${startDateParam}::date 
              AND a.scheduled_at::date <= ${endDateParam}::date
            ${hasOfficeFilter ? sql`WHERE o.repcard_office_id = ANY(${officeIds}::int[])` : sql``}
            GROUP BY o.repcard_office_id
          )
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            COALESCE(os.doors_knocked, 0)::int as doors_knocked,
            COALESCE(os.appointments_set, 0)::int as appointments_set,
            COALESCE(oc.sales_closed, 0)::int as sales_closed,
            COALESCE(os.setters_count, 0)::int as setters_count,
            COALESCE(oc.closers_count, 0)::int as closers_count,
            (COALESCE(os.setters_count, 0) + COALESCE(oc.closers_count, 0))::int as total_reps,
            CASE
              WHEN COALESCE(os.doors_knocked, 0) > 0 THEN
                (COALESCE(os.appointments_set, 0)::float / COALESCE(os.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate,
            CASE
              WHEN COALESCE(oc.appointments_run, 0) > 0 THEN
                (COALESCE(oc.sales_closed, 0)::float / COALESCE(oc.appointments_run, 0)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          LEFT JOIN office_setters os ON os.repcard_office_id = o.repcard_office_id
          LEFT JOIN office_closers oc ON oc.repcard_office_id = o.repcard_office_id
          ${hasOfficeFilter ? sql`WHERE o.repcard_office_id = ANY(${officeIds}::int[])` : sql``}
          GROUP BY o.repcard_office_id, o.name, os.doors_knocked, os.appointments_set, oc.sales_closed, os.setters_count, oc.closers_count, oc.appointments_run
          HAVING COALESCE(os.doors_knocked, 0) > 0 OR COALESCE(os.appointments_set, 0) > 0 OR COALESCE(oc.sales_closed, 0) > 0
          ORDER BY sales_closed DESC, appointments_set DESC
        `
        : await sql`
          WITH office_setters AS (
            SELECT
              o.repcard_office_id,
              COUNT(DISTINCT ru.repcard_user_id) FILTER (WHERE ru.role = 'setter' OR ru.role IS NULL)::int as setters_count,
              COUNT(DISTINCT dk.id)::int as doors_knocked,
              COUNT(DISTINCT a.id) FILTER (
                WHERE a.setter_user_id IS NOT NULL
                AND (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
              )::int as appointments_set
            FROM repcard_offices o
            LEFT JOIN repcard_users ru ON ru.office_id = o.repcard_office_id
            LEFT JOIN repcard_door_knocks dk ON dk.setter_user_id = ru.repcard_user_id
            LEFT JOIN repcard_appointments a ON a.setter_user_id::int = ru.repcard_user_id
              AND a.scheduled_at IS NOT NULL
            ${hasOfficeFilter ? sql`WHERE o.repcard_office_id = ANY(${officeIds}::int[])` : sql``}
            GROUP BY o.repcard_office_id
          ),
          office_closers AS (
            SELECT
              o.repcard_office_id,
              COUNT(DISTINCT ru.repcard_user_id) FILTER (WHERE ru.role = 'closer' OR ru.role IS NULL)::int as closers_count,
              COUNT(DISTINCT a.id) FILTER (WHERE a.closer_user_id IS NOT NULL)::int as appointments_run,
              COUNT(DISTINCT a.id) FILTER (WHERE a.status_category IN ('sat_closed', 'completed'))::int as sales_closed
            FROM repcard_offices o
            LEFT JOIN repcard_users ru ON ru.office_id = o.repcard_office_id
            LEFT JOIN repcard_appointments a ON a.closer_user_id::int = ru.repcard_user_id
              AND a.scheduled_at IS NOT NULL
            ${hasOfficeFilter ? sql`WHERE o.repcard_office_id = ANY(${officeIds}::int[])` : sql``}
            GROUP BY o.repcard_office_id
          )
          SELECT
            o.repcard_office_id,
            o.name as office_name,
            COALESCE(os.doors_knocked, 0)::int as doors_knocked,
            COALESCE(os.appointments_set, 0)::int as appointments_set,
            COALESCE(oc.sales_closed, 0)::int as sales_closed,
            COALESCE(os.setters_count, 0)::int as setters_count,
            COALESCE(oc.closers_count, 0)::int as closers_count,
            (COALESCE(os.setters_count, 0) + COALESCE(oc.closers_count, 0))::int as total_reps,
            CASE
              WHEN COALESCE(os.doors_knocked, 0) > 0 THEN
                (COALESCE(os.appointments_set, 0)::float / COALESCE(os.doors_knocked, 0)::float) * 100
              ELSE 0
            END as conversion_rate,
            CASE
              WHEN COALESCE(oc.appointments_run, 0) > 0 THEN
                (COALESCE(oc.sales_closed, 0)::float / COALESCE(oc.appointments_run, 0)::float) * 100
              ELSE 0
            END as close_rate
          FROM repcard_offices o
          LEFT JOIN office_setters os ON os.repcard_office_id = o.repcard_office_id
          LEFT JOIN office_closers oc ON oc.repcard_office_id = o.repcard_office_id
          ${hasOfficeFilter ? sql`WHERE o.repcard_office_id = ANY(${officeIds}::int[])` : sql``}
          GROUP BY o.repcard_office_id, o.name, os.doors_knocked, os.appointments_set, oc.sales_closed, os.setters_count, oc.closers_count, oc.appointments_run
          HAVING COALESCE(os.doors_knocked, 0) > 0 OR COALESCE(os.appointments_set, 0) > 0 OR COALESCE(oc.sales_closed, 0) > 0
          ORDER BY sales_closed DESC, appointments_set DESC
        `;
    } catch (error) {
      logError('repcard-leaderboards', error as Error, { requestId, context: 'offices query' });
      throw error;
    }

    // Format results
    const getRows = (result: any) => {
      if (Array.isArray(result)) return result;
      if (result?.rows) return result.rows;
      return [];
    };

    const setters = getRows(settersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      team: row.team,
      appointmentsSet: row.appointments_set || 0,
      within48hCount: row.within_48h_count || 0,
      withPowerBillCount: row.with_power_bill_count || 0,
      highQualityCount: row.high_quality_count || 0,
      lowQualityCount: row.low_quality_count || 0,
      doorsKnocked: row.doors_knocked || 0,
      estimatedHoursOnDoors: row.estimated_hours_on_doors || 0,
      conversionRate: parseFloat(row.conversion_rate || '0'),
    }));

    const closers = getRows(closersResult).map((row: any) => ({
      userId: row.repcard_user_id,
      name: row.name,
      role: row.role,
      team: row.team,
      appointmentsRun: row.appointments_run || 0,
      satClosed: row.sat_closed || 0,
      satNoClose: row.sat_no_close || 0,
      noShow: row.no_show || 0,
      cancelled: row.cancelled || 0,
      closeRate: parseFloat(row.close_rate || '0'),
    }));

    const offices = getRows(officesResult).map((row: any) => ({
      officeId: row.repcard_office_id,
      officeName: row.office_name,
      doorsKnocked: row.doors_knocked || 0,
      appointmentsSet: row.appointments_set || 0,
      salesClosed: row.sales_closed || 0,
      settersCount: row.setters_count || 0,
      closersCount: row.closers_count || 0,
      totalReps: row.total_reps || 0,
      conversionRate: parseFloat(row.conversion_rate || '0'),
      closeRate: parseFloat(row.close_rate || '0'),
    }));

    // Get last sync time for metadata
    let lastSyncTime: Date | null = null;
    try {
      const lastSyncResult = await sql`
        SELECT completed_at
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
      console.error('[RepCard Leaderboards] Error fetching sync status:', error);
      // Continue without sync status if query fails
    }

    const responseData = {
      setters,
      closers,
      offices,
      metadata: {
        startDate: startDateParam,
        endDate: endDateParam,
        officeIds: officeIds || [],
        generatedAt: new Date().toISOString(),
        lastSyncTime: lastSyncTime ? lastSyncTime.toISOString() : null,
      },
    };

    // Cache the response
    leaderboardCache.set(cacheKey, {
      data: responseData,
      timestamp: Date.now(),
    });

    logApiResponse('repcard-leaderboards', 200, { requestId, settersCount: setters.length, closersCount: closers.length, officesCount: offices.length });
    return NextResponse.json(responseData);

  } catch (error) {
    logError('repcard-leaderboards', error as Error, { requestId: requestId });
    return NextResponse.json(
      { error: 'Internal server error', message: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
