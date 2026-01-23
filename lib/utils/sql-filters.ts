/**
 * SQL filter building utilities for appointments queries
 * Avoids conditional template fragment issues by building filter arrays
 */

interface FilterConditions {
  hasTeamFilter: boolean;
  teamIds?: number[];
  hasCalendarFilter: boolean;
  calendarId?: number;
  hasStatusFilter: boolean;
  statusFilter?: string;
  hasPowerBillFilter: boolean;
  powerBillValue?: boolean;
  hasRescheduleFilter: boolean;
  rescheduleValue?: boolean;
}

/**
 * Build additional WHERE conditions for appointments queries
 * Returns array of SQL condition strings and their parameter values
 */
export function buildAppointmentFilters(filters: FilterConditions): {
  conditions: string[];
  params: any[];
} {
  const conditions: string[] = [];
  const params: any[] = [];
  let paramIndex = 1;

  if (filters.hasTeamFilter && filters.teamIds && filters.teamIds.length > 0) {
    conditions.push(`COALESCE(closer_team.repcard_team_id, setter_team.repcard_team_id) = ANY($${paramIndex})`);
    params.push(filters.teamIds);
    paramIndex++;
  }

  if (filters.hasCalendarFilter && filters.calendarId) {
    conditions.push(`(a.raw_data->>'calendarId')::int = $${paramIndex}`);
    params.push(filters.calendarId);
    paramIndex++;
  }

  if (filters.hasStatusFilter && filters.statusFilter) {
    conditions.push(`a.status_category = $${paramIndex}`);
    params.push(filters.statusFilter);
    paramIndex++;
  }

  if (filters.hasPowerBillFilter) {
    conditions.push(`a.has_power_bill = $${paramIndex}`);
    params.push(filters.powerBillValue);
    paramIndex++;
  }

  if (filters.hasRescheduleFilter) {
    conditions.push(`a.is_reschedule = $${paramIndex}`);
    params.push(filters.rescheduleValue);
    paramIndex++;
  }

  return { conditions, params };
}
