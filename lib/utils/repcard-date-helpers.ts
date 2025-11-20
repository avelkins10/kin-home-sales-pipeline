/**
 * RepCard Date Utilities
 * Handles proper parsing and timezone conversion for RepCard API dates
 */

/**
 * Parse a RepCard API date string and convert to UTC timestamp for database storage
 * RepCard API returns dates in ISO format, potentially with timezone info
 * 
 * @param dateString - ISO date string from RepCard API (e.g., "2025-01-15T10:30:00Z" or "2025-01-15T10:30:00-07:00")
 * @returns Date object in UTC, or null if invalid
 */
export function parseRepCardDate(dateString: string | null | undefined): Date | null {
  if (!dateString || typeof dateString !== 'string') {
    return null;
  }

  try {
    // Parse the ISO string - JavaScript Date handles timezone conversion automatically
    const date = new Date(dateString);
    
    // Validate the date is valid
    if (isNaN(date.getTime())) {
      console.warn(`[RepCard Date] Invalid date string: ${dateString}`);
      return null;
    }

    return date;
  } catch (error) {
    console.warn(`[RepCard Date] Error parsing date: ${dateString}`, error);
    return null;
  }
}

/**
 * Format a date for display with proper timezone handling
 * Uses the user's local timezone for display
 * 
 * @param date - Date object (assumed to be UTC from database)
 * @param options - Formatting options
 * @returns Formatted date string
 */
export function formatRepCardDate(
  date: Date | string | null | undefined,
  options: {
    includeTime?: boolean;
    includeTimezone?: boolean;
  } = {}
): string {
  if (!date) return 'N/A';

  try {
    const dateObj = typeof date === 'string' ? new Date(date) : date;
    
    if (isNaN(dateObj.getTime())) {
      return 'Invalid Date';
    }

    const { includeTime = true, includeTimezone = false } = options;

    if (includeTime) {
      return dateObj.toLocaleString('en-US', {
        timeZone: 'America/Denver', // Default to Mountain Time (most common for sales team)
        year: 'numeric',
        month: 'short',
        day: 'numeric',
        hour: 'numeric',
        minute: '2-digit',
        ...(includeTimezone && { timeZoneName: 'short' })
      });
    } else {
      return dateObj.toLocaleDateString('en-US', {
        timeZone: 'America/Denver',
        year: 'numeric',
        month: 'short',
        day: 'numeric'
      });
    }
  } catch (error) {
    console.warn(`[RepCard Date] Error formatting date:`, error);
    return 'Invalid Date';
  }
}

/**
 * Format a date range for display
 */
export function formatRepCardDateRange(
  startDate: Date | string | null | undefined,
  endDate: Date | string | null | undefined
): string {
  if (!startDate || !endDate) return 'N/A';
  
  const start = formatRepCardDate(startDate, { includeTime: false });
  const end = formatRepCardDate(endDate, { includeTime: false });
  
  return `${start} - ${end}`;
}

/**
 * Convert a date to ISO string for database storage (UTC)
 */
export function toUTCISOString(date: Date | string | null | undefined): string | null {
  if (!date) return null;
  
  try {
    const dateObj = typeof date === 'string' ? new Date(date) : date;
    
    if (isNaN(dateObj.getTime())) {
      return null;
    }

    // Return ISO string in UTC
    return dateObj.toISOString();
  } catch (error) {
    console.warn(`[RepCard Date] Error converting to UTC:`, error);
    return null;
  }
}

