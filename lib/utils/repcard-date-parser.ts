/**
 * Parse RepCard date/time strings with timezone information
 * 
 * RepCard API returns dates in various formats:
 * - startAt: "2026-01-19 22:00:00" (local time in timezone, no timezone indicator)
 * - startAtTimezone: "America/New_York"
 * - createdAt: "2026-01-16T23:11:40.000000Z" (UTC ISO)
 */

import { isDSTInEffect } from './timezone';

/**
 * Parse a RepCard datetime string with timezone and convert to UTC ISO string
 * @param dateTimeString - e.g., "2026-01-19 22:00:00"
 * @param timezone - e.g., "America/New_York" (default)
 * @returns ISO string in UTC, or null if invalid
 */
export function parseRepCardDateTime(dateTimeString: string | null | undefined, timezone: string = 'America/New_York'): string | null {
  if (!dateTimeString) return null;
  
  try {
    // If it's already an ISO string with timezone, parse it directly
    if (dateTimeString.includes('T') && (dateTimeString.includes('Z') || dateTimeString.includes('+') || dateTimeString.match(/\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}-\d{2}:\d{2}/))) {
      return new Date(dateTimeString).toISOString();
    }
    
    // Parse "YYYY-MM-DD HH:mm:ss" format (e.g., "2026-01-19 22:00:00")
    const match = dateTimeString.match(/(\d{4})-(\d{2})-(\d{2})\s+(\d{2}):(\d{2}):(\d{2})/);
    if (!match) {
      // Try to parse as-is (might be ISO without timezone)
      const date = new Date(dateTimeString);
      if (!isNaN(date.getTime())) {
        return date.toISOString();
      }
      return null;
    }
    
    const [, yearStr, monthStr, dayStr, hourStr, minuteStr, secondStr] = match;
    const year = parseInt(yearStr, 10);
    const month = parseInt(monthStr, 10);
    const day = parseInt(dayStr, 10);
    const hours = parseInt(hourStr, 10);
    const minutes = parseInt(minuteStr, 10);
    const seconds = parseInt(secondStr, 10);
    
    // Check if DST is in effect for this date in the specified timezone
    // For America/New_York: DST is 2nd Sunday in March to 1st Sunday in November
    const isDST = isDSTInEffect(year, month, day);
    const offsetHours = isDST ? -4 : -5; // EDT is UTC-4, EST is UTC-5
    
    // Create UTC date by subtracting the timezone offset
    // The datetime string represents local time in the timezone, so we add the offset to get UTC
    const utcDate = new Date(Date.UTC(
      year,
      month - 1, // JavaScript months are 0-indexed
      day,
      hours - offsetHours, // Subtract offset (negative offset means add hours to get UTC)
      minutes,
      seconds || 0
    ));
    
    if (isNaN(utcDate.getTime())) {
      console.error(`[RepCard Date Parser] Invalid date: ${dateTimeString}`);
      return null;
    }
    
    return utcDate.toISOString();
  } catch (error) {
    console.error(`[RepCard Date Parser] Error parsing date: ${dateTimeString}`, error);
    return null;
  }
}
