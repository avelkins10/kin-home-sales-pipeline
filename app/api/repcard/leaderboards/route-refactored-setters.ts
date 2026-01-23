    // ========================================
    // SETTERS LEADERBOARD
    // ========================================
    let settersResult;
    try {
      if (hasDateFilter && hasOfficeFilter) {
        settersResult = await sql`
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
          AND EXISTS (
            SELECT 1 FROM offices o
            WHERE o.name = COALESCE(u.sales_office[1], ru.office_name)
            AND o.quickbase_office_id = ANY(${officeIds}::int[])
          )
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team'), COALESCE(dks.doors_knocked, 0)::int, COALESCE(dks.estimated_hours_on_doors, 0)::int
          HAVING COUNT(DISTINCT a.id) > 0 OR COALESCE(dks.doors_knocked, 0) > 0
          ORDER BY appointments_set DESC
        `;
      } else if (hasDateFilter && !hasOfficeFilter) {
        settersResult = await sql`
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
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team'), COALESCE(dks.doors_knocked, 0)::int, COALESCE(dks.estimated_hours_on_doors, 0)::int
          HAVING COUNT(DISTINCT a.id) > 0 OR COALESCE(dks.doors_knocked, 0) > 0
          ORDER BY appointments_set DESC
        `;
      } else if (!hasDateFilter && hasOfficeFilter) {
        settersResult = await sql`
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
          AND EXISTS (
            SELECT 1 FROM offices o
            WHERE o.name = COALESCE(u.sales_office[1], ru.office_name)
            AND o.quickbase_office_id = ANY(${officeIds}::int[])
          )
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team')
          HAVING COUNT(DISTINCT a.id) > 0 OR COUNT(DISTINCT dk.id) > 0
          ORDER BY appointments_set DESC
        `;
      } else {
        settersResult = await sql`
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
          GROUP BY ru.repcard_user_id, COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)), COALESCE(u.role, ru.role), COALESCE(ru.team, 'No Team')
          HAVING COUNT(DISTINCT a.id) > 0 OR COUNT(DISTINCT dk.id) > 0
          ORDER BY appointments_set DESC
        `;
      }
    } catch (error) {
      logError('repcard-leaderboards', error as Error, { requestId, context: 'setters query' });
      throw error;
    }
