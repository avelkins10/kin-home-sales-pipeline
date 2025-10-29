/**
 * RepCard Metrics Calculations
 * 
 * Helper functions for calculating key metrics from synced RepCard data
 * All calculations use database queries for performance
 */

import { sql } from '@/lib/db/client';

export interface AppointmentMetrics {
  total: number;
  within48Hours: number;
  withPowerBill: number;
  cancelled: number;
  rescheduled: number;
  noShow: number;
  satClosed: number;
  satNoClose: number;
  completed: number;
  scheduled: number;
}

export interface AttributionMetrics {
  bySetter: Map<number, AppointmentMetrics>;
  byCloser: Map<number, AppointmentMetrics>;
  byOffice: Map<number, AppointmentMetrics>;
}

/**
 * Get appointment metrics for a user (as setter or closer)
 */
export async function getAppointmentMetricsForUser(
  repcardUserId: number,
  startDate?: string,
  endDate?: string,
  role: 'setter' | 'closer' | 'both' = 'both'
): Promise<AppointmentMetrics> {
  let query = sql`
    SELECT 
      COUNT(*) as total,
      COUNT(*) FILTER (WHERE is_within_48_hours = TRUE) as within48Hours,
      COUNT(*) FILTER (WHERE has_power_bill = TRUE) as withPowerBill,
      COUNT(*) FILTER (WHERE status_category = 'cancelled') as cancelled,
      COUNT(*) FILTER (WHERE status_category = 'rescheduled') as rescheduled,
      COUNT(*) FILTER (WHERE status_category = 'no_show') as noShow,
      COUNT(*) FILTER (WHERE status_category = 'sat_closed') as satClosed,
      COUNT(*) FILTER (WHERE status_category = 'sat_no_close') as satNoClose,
      COUNT(*) FILTER (WHERE status_category = 'completed') as completed,
      COUNT(*) FILTER (WHERE status_category = 'scheduled') as scheduled
    FROM repcard_appointments
    WHERE 
  `;

  if (role === 'setter') {
    query = sql`${query} setter_user_id = ${repcardUserId}`;
  } else if (role === 'closer') {
    query = sql`${query} closer_user_id = ${repcardUserId}`;
  } else {
    query = sql`${query} (setter_user_id = ${repcardUserId} OR closer_user_id = ${repcardUserId})`;
  }

  if (startDate) {
    query = sql`${query} AND created_at >= ${startDate}::timestamp`;
  }
  if (endDate) {
    query = sql`${query} AND created_at <= ${endDate}::timestamp`;
  }

  const result = await query;
  const row = result.rows?.[0] || result[0];

  return {
    total: parseInt(row?.total || '0', 10),
    within48Hours: parseInt(row?.within48Hours || '0', 10),
    withPowerBill: parseInt(row?.withPowerBill || '0', 10),
    cancelled: parseInt(row?.cancelled || '0', 10),
    rescheduled: parseInt(row?.rescheduled || '0', 10),
    noShow: parseInt(row?.noShow || '0', 10),
    satClosed: parseInt(row?.satClosed || '0', 10),
    satNoClose: parseInt(row?.satNoClose || '0', 10),
    completed: parseInt(row?.completed || '0', 10),
    scheduled: parseInt(row?.scheduled || '0', 10),
  };
}

/**
 * Get appointments set within 48 hours for a setter
 */
export async function getAppointmentsWithin48Hours(
  repcardUserId: number,
  startDate?: string,
  endDate?: string
): Promise<number> {
  let query = sql`
    SELECT COUNT(*) as count
    FROM repcard_appointments
    WHERE setter_user_id = ${repcardUserId}
      AND is_within_48_hours = TRUE
  `;

  if (startDate) {
    query = sql`${query} AND created_at >= ${startDate}::timestamp`;
  }
  if (endDate) {
    query = sql`${query} AND created_at <= ${endDate}::timestamp`;
  }

  const result = await query;
  const row = result.rows?.[0] || result[0];
  return parseInt(row?.count || '0', 10);
}

/**
 * Get appointments with power bill for a setter
 */
export async function getAppointmentsWithPowerBill(
  repcardUserId: number,
  startDate?: string,
  endDate?: string
): Promise<number> {
  let query = sql`
    SELECT COUNT(*) as count
    FROM repcard_appointments
    WHERE setter_user_id = ${repcardUserId}
      AND has_power_bill = TRUE
  `;

  if (startDate) {
    query = sql`${query} AND created_at >= ${startDate}::timestamp`;
  }
  if (endDate) {
    query = sql`${query} AND created_at <= ${endDate}::timestamp`;
  }

  const result = await query;
  const row = result.rows?.[0] || result[0];
  return parseInt(row?.count || '0', 10);
}

/**
 * Get all dispositions/statuses for a user
 */
export async function getAllDispositionsForUser(
  repcardUserId: number,
  startDate?: string,
  endDate?: string,
  role: 'setter' | 'closer' = 'setter'
): Promise<{ disposition: string; count: number }[]> {
  let query = sql`
    SELECT 
      COALESCE(disposition, 'Unknown') as disposition,
      COUNT(*) as count
    FROM repcard_appointments
    WHERE ${role === 'setter' ? sql`setter_user_id` : sql`closer_user_id`} = ${repcardUserId}
  `;

  if (startDate) {
    query = sql`${query} AND created_at >= ${startDate}::timestamp`;
  }
  if (endDate) {
    query = sql`${query} AND created_at <= ${endDate}::timestamp`;
  }

  query = sql`${query} GROUP BY disposition ORDER BY count DESC`;

  const result = await query;
  const rows = result.rows || result;
  
  return rows.map((row: any) => ({
    disposition: row.disposition || 'Unknown',
    count: parseInt(row.count || '0', 10),
  }));
}

/**
 * Get office-level metrics
 */
export async function getOfficeMetrics(
  officeId: number,
  startDate?: string,
  endDate?: string
): Promise<AppointmentMetrics> {
  let query = sql`
    SELECT 
      COUNT(*) as total,
      COUNT(*) FILTER (WHERE is_within_48_hours = TRUE) as within48Hours,
      COUNT(*) FILTER (WHERE has_power_bill = TRUE) as withPowerBill,
      COUNT(*) FILTER (WHERE status_category = 'cancelled') as cancelled,
      COUNT(*) FILTER (WHERE status_category = 'rescheduled') as rescheduled,
      COUNT(*) FILTER (WHERE status_category = 'no_show') as noShow,
      COUNT(*) FILTER (WHERE status_category = 'sat_closed') as satClosed,
      COUNT(*) FILTER (WHERE status_category = 'sat_no_close') as satNoClose,
      COUNT(*) FILTER (WHERE status_category = 'completed') as completed,
      COUNT(*) FILTER (WHERE status_category = 'scheduled') as scheduled
    FROM repcard_appointments
    WHERE office_id = ${officeId}
  `;

  if (startDate) {
    query = sql`${query} AND created_at >= ${startDate}::timestamp`;
  }
  if (endDate) {
    query = sql`${query} AND created_at <= ${endDate}::timestamp`;
  }

  const result = await query;
  const row = result.rows?.[0] || result[0];

  return {
    total: parseInt(row?.total || '0', 10),
    within48Hours: parseInt(row?.within48Hours || '0', 10),
    withPowerBill: parseInt(row?.withPowerBill || '0', 10),
    cancelled: parseInt(row?.cancelled || '0', 10),
    rescheduled: parseInt(row?.rescheduled || '0', 10),
    noShow: parseInt(row?.noShow || '0', 10),
    satClosed: parseInt(row?.satClosed || '0', 10),
    satNoClose: parseInt(row?.satNoClose || '0', 10),
    completed: parseInt(row?.completed || '0', 10),
    scheduled: parseInt(row?.scheduled || '0', 10),
  };
}

