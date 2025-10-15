// lib/utils/timezone-helpers.ts
/**
 * Timezone-aware date utilities for analytics APIs
 * 
 * This module provides functions to handle dates in the user's timezone
 * instead of UTC, ensuring accurate date filtering and calculations.
 */

/**
 * Convert a date to the user's timezone and return as YYYY-MM-DD string
 * This is used for date filtering in Quickbase queries
 * 
 * @param date - Date object to convert
 * @param timezone - User's timezone (e.g., 'America/New_York')
 * @returns Date string in YYYY-MM-DD format in the user's timezone
 */
export function formatDateForTimezone(date: Date, timezone: string = 'America/New_York'): string {
  try {
    return new Intl.DateTimeFormat('en-CA', {
      timeZone: timezone,
      year: 'numeric',
      month: '2-digit',
      day: '2-digit',
    }).format(date);
  } catch (error) {
    console.error('Error formatting date for timezone:', error);
    // Fallback to local date
    return date.toISOString().split('T')[0];
  }
}

/**
 * Get the start of a time period in the user's timezone
 *
 * @param period - Time period type
 * @param timezone - User's timezone
 * @returns Date object representing the start of the period in user's timezone
 */
export function getPeriodStart(period: 'ytd' | 'month' | 'week' | 'last_30' | 'last_90' | 'last_12_months', timezone: string = 'America/New_York'): Date {
  const now = new Date();

  switch (period) {
    case 'ytd':
      // January 1st of current year in user's timezone
      return new Date(now.getFullYear(), 0, 1);

    case 'month':
      // 1st of current month in user's timezone
      return new Date(now.getFullYear(), now.getMonth(), 1);

    case 'week':
      // 7 days ago in user's timezone
      const weekStart = new Date(now);
      weekStart.setDate(now.getDate() - 7);
      return weekStart;

    case 'last_30':
      // 30 days ago
      const last30Start = new Date(now);
      last30Start.setDate(now.getDate() - 30);
      return last30Start;

    case 'last_90':
      // 90 days ago
      const last90Start = new Date(now);
      last90Start.setDate(now.getDate() - 90);
      return last90Start;

    case 'last_12_months':
      // 12 months ago (365 days)
      const last12MonthsStart = new Date(now);
      last12MonthsStart.setDate(now.getDate() - 365);
      return last12MonthsStart;

    default:
      return now;
  }
}

/**
 * Get the end of a time period (typically "now") in the user's timezone
 * 
 * @param timezone - User's timezone
 * @returns Date object representing "now" in user's timezone
 */
export function getPeriodEnd(timezone: string = 'America/New_York'): Date {
  return new Date();
}

/**
 * Calculate the difference between two dates in days, accounting for timezone
 * 
 * @param startDate - Start date
 * @param endDate - End date
 * @param timezone - User's timezone
 * @returns Number of days between dates
 */
export function calculateDaysDifference(
  startDate: Date | string, 
  endDate: Date | string, 
  timezone: string = 'America/New_York'
): number {
  try {
    const start = typeof startDate === 'string' ? new Date(startDate) : startDate;
    const end = typeof endDate === 'string' ? new Date(endDate) : endDate;
    
    // Calculate difference in milliseconds and convert to days
    const diffMs = end.getTime() - start.getTime();
    return Math.ceil(diffMs / (1000 * 60 * 60 * 24));
  } catch (error) {
    console.error('Error calculating days difference:', error);
    return 0;
  }
}

/**
 * Parse a Quickbase date string and return a Date object in the user's timezone
 * 
 * @param dateString - Date string from Quickbase
 * @param timezone - User's timezone
 * @returns Date object in user's timezone, or null if invalid
 */
export function parseQuickbaseDateInTimezone(
  dateString: string | null | undefined, 
  timezone: string = 'America/New_York'
): Date | null {
  if (!dateString || typeof dateString !== 'string') {
    return null;
  }

  try {
    // Extract just the date portion (YYYY-MM-DD)
    const dateMatch = dateString.match(/^(\d{4}-\d{2}-\d{2})/);
    if (!dateMatch) {
      return null;
    }

    const datePart = dateMatch[1]; // e.g., "2025-01-15"
    const [year, month, day] = datePart.split('-').map(Number);

    // Create date in local timezone (month is 0-indexed in JS)
    const localDate = new Date(year, month - 1, day);

    // Validate the date is valid
    if (isNaN(localDate.getTime())) {
      return null;
    }

    return localDate;
  } catch (error) {
    console.error('Error parsing Quickbase date:', error);
    return null;
  }
}

/**
 * Build date filters for Quickbase queries using user's timezone
 *
 * @param timeRange - Time range type
 * @param timezone - User's timezone
 * @param fieldId - Quickbase field ID to filter on
 * @param customStartDate - Custom start date (for 'custom' timeRange)
 * @param customEndDate - Custom end date (for 'custom' timeRange)
 * @returns Array of date filter objects for Quickbase query
 */
export function buildTimezoneAwareDateFilters(
  timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months',
  timezone: string = 'America/New_York',
  fieldId: number,
  customStartDate?: string,
  customEndDate?: string
): any[] {
  const dateFilters: any[] = [];

  switch (timeRange) {
    case 'lifetime':
      // No date filter for lifetime
      break;

    case 'ytd':
      const ytdStart = getPeriodStart('ytd', timezone);
      const ytdEnd = getPeriodEnd(timezone);
      dateFilters.push(
        {
          field: fieldId,
          operator: 'gte',
          value: formatDateForTimezone(ytdStart, timezone)
        },
        {
          field: fieldId,
          operator: 'lte',
          value: formatDateForTimezone(ytdEnd, timezone)
        }
      );
      break;

    case 'month':
      const monthStart = getPeriodStart('month', timezone);
      const monthEnd = getPeriodEnd(timezone);
      dateFilters.push(
        {
          field: fieldId,
          operator: 'gte',
          value: formatDateForTimezone(monthStart, timezone)
        },
        {
          field: fieldId,
          operator: 'lte',
          value: formatDateForTimezone(monthEnd, timezone)
        }
      );
      break;

    case 'week':
      const weekStart = getPeriodStart('week', timezone);
      const weekEnd = getPeriodEnd(timezone);
      dateFilters.push(
        {
          field: fieldId,
          operator: 'gte',
          value: formatDateForTimezone(weekStart, timezone)
        },
        {
          field: fieldId,
          operator: 'lte',
          value: formatDateForTimezone(weekEnd, timezone)
        }
      );
      break;

    case 'last_30':
      const last30Start = getPeriodStart('last_30', timezone);
      const last30End = getPeriodEnd(timezone);
      dateFilters.push(
        {
          field: fieldId,
          operator: 'gte',
          value: formatDateForTimezone(last30Start, timezone)
        },
        {
          field: fieldId,
          operator: 'lte',
          value: formatDateForTimezone(last30End, timezone)
        }
      );
      break;

    case 'last_90':
      const last90Start = getPeriodStart('last_90', timezone);
      const last90End = getPeriodEnd(timezone);
      dateFilters.push(
        {
          field: fieldId,
          operator: 'gte',
          value: formatDateForTimezone(last90Start, timezone)
        },
        {
          field: fieldId,
          operator: 'lte',
          value: formatDateForTimezone(last90End, timezone)
        }
      );
      break;

    case 'last_12_months':
      const last12MonthsStart = getPeriodStart('last_12_months', timezone);
      const last12MonthsEnd = getPeriodEnd(timezone);
      dateFilters.push(
        {
          field: fieldId,
          operator: 'gte',
          value: formatDateForTimezone(last12MonthsStart, timezone)
        },
        {
          field: fieldId,
          operator: 'lte',
          value: formatDateForTimezone(last12MonthsEnd, timezone)
        }
      );
      break;

    case 'custom':
      if (customStartDate && customEndDate) {
        dateFilters.push(
          {
            field: fieldId,
            operator: 'gte',
            value: customStartDate
          },
          {
            field: fieldId,
            operator: 'lte',
            value: customEndDate
          }
        );
      }
      break;
  }

  return dateFilters;
}
