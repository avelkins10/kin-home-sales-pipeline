/**
 * Timezone utilities for RepCard
 * All dates should be converted to America/New_York timezone for database queries
 * Frontend can use local timezone for display, but dates sent to API should be in YYYY-MM-DD format
 * API will convert to Eastern Time for database queries
 */

/**
 * Get today's date in the user's local timezone as YYYY-MM-DD
 * This is what the frontend should use when "Today" is clicked
 */
export function getTodayLocal(): string {
  const now = new Date();
  const year = now.getFullYear();
  const month = String(now.getMonth() + 1).padStart(2, '0');
  const day = String(now.getDate()).padStart(2, '0');
  return `${year}-${month}-${day}`;
}

/**
 * Check if DST is in effect for a given date in America/New_York
 * DST: Second Sunday in March to First Sunday in November
 */
export function isDSTInEffect(year: number, month: number, day: number): boolean {
  // DST starts: Second Sunday in March
  // DST ends: First Sunday in November
  
  if (month < 3 || month > 11) return false; // Definitely not DST
  if (month > 3 && month < 11) return true; // Definitely DST (April-October)
  
  if (month === 3) {
    // March: DST starts on second Sunday
    const secondSunday = getNthSunday(year, 3, 2);
    return day >= secondSunday;
  }
  
  if (month === 11) {
    // November: DST ends on first Sunday
    const firstSunday = getNthSunday(year, 11, 1);
    return day < firstSunday;
  }
  
  return false;
}

/**
 * Get the day of month for the Nth Sunday in a given month/year
 */
export function getNthSunday(year: number, month: number, n: number): number {
  // Find the first day of the month
  const firstDay = new Date(year, month - 1, 1);
  const firstDayOfWeek = firstDay.getDay(); // 0 = Sunday, 1 = Monday, etc.
  
  // Calculate how many days until the first Sunday
  const daysUntilFirstSunday = (7 - firstDayOfWeek) % 7;
  const firstSunday = 1 + daysUntilFirstSunday;
  
  // Nth Sunday = first Sunday + (n-1) * 7
  return firstSunday + (n - 1) * 7;
}

/**
 * Convert YYYY-MM-DD to start of day in America/New_York timezone
 * Properly handles DST (EDT = -04:00, EST = -05:00)
 * Returns ISO string for database queries
 */
export function toEasternStart(dateString: string): string {
  const [year, month, day] = dateString.split('-').map(Number);
  
  // Check if DST is in effect
  const isDST = isDSTInEffect(year, month, day);
  const offset = isDST ? '-04:00' : '-05:00';
  
  // Create date string with explicit Eastern Time offset
  const dateStr = `${year}-${String(month).padStart(2, '0')}-${String(day).padStart(2, '0')}T00:00:00${offset}`;
  const date = new Date(dateStr);
  
  return date.toISOString();
}

/**
 * Convert YYYY-MM-DD to end of day in America/New_York timezone
 * Properly handles DST (EDT = -04:00, EST = -05:00)
 * Returns ISO string for database queries
 */
export function toEasternEnd(dateString: string): string {
  const [year, month, day] = dateString.split('-').map(Number);

  // Check if DST is in effect
  const isDST = isDSTInEffect(year, month, day);
  const offset = isDST ? '-04:00' : '-05:00';

  // Create date string with explicit Eastern Time offset for end of day
  const dateStr = `${year}-${String(month).padStart(2, '0')}-${String(day).padStart(2, '0')}T23:59:59.999${offset}`;
  const date = new Date(dateStr);

  return date.toISOString();
}

/**
 * Sync user timezone (stub function for compatibility)
 * NOTE: Timezone conversion is handled server-side in API routes
 * This function exists to prevent import errors but doesn't need to do anything
 */
export function syncUserTimezone(timezone: string): void {
  // No-op: All timezone conversion is handled server-side
  // This function exists for backward compatibility
  return;
}

/**
 * Format a date/time in its local timezone for display
 * This shows the time as it was originally set, not converted to viewer's timezone
 *
 * @param date - Date object or ISO string
 * @param formatStr - Format string: 'date', 'time', 'datetime', 'full'
 * @param timezone - IANA timezone identifier (e.g., 'America/New_York', 'America/Los_Angeles')
 * @returns Formatted string in the specified timezone with timezone suffix
 */
export function formatInTimezone(
  date: Date | string | null | undefined,
  formatStr: 'date' | 'time' | 'datetime' | 'full' = 'datetime',
  timezone: string = 'America/New_York'
): string {
  if (!date) return 'Not set';

  const dateObj = typeof date === 'string' ? new Date(date) : date;

  if (isNaN(dateObj.getTime())) return 'Invalid date';

  const options: Intl.DateTimeFormatOptions = {
    timeZone: timezone,
  };

  switch (formatStr) {
    case 'date':
      options.weekday = 'short';
      options.month = 'short';
      options.day = 'numeric';
      options.year = 'numeric';
      break;
    case 'time':
      options.hour = 'numeric';
      options.minute = '2-digit';
      options.hour12 = true;
      break;
    case 'datetime':
      options.weekday = 'short';
      options.month = 'short';
      options.day = 'numeric';
      options.hour = 'numeric';
      options.minute = '2-digit';
      options.hour12 = true;
      break;
    case 'full':
      options.weekday = 'short';
      options.month = 'short';
      options.day = 'numeric';
      options.year = 'numeric';
      options.hour = 'numeric';
      options.minute = '2-digit';
      options.hour12 = true;
      break;
  }

  const formatted = new Intl.DateTimeFormat('en-US', options).format(dateObj);

  // Add timezone abbreviation for time formats
  if (formatStr === 'time' || formatStr === 'datetime' || formatStr === 'full') {
    // Get timezone abbreviation (PT, ET, CT, MT, etc.)
    const tzAbbr = getTimezoneAbbreviation(timezone, dateObj);
    return `${formatted} ${tzAbbr}`;
  }

  return formatted;
}

/**
 * Get timezone abbreviation (PT, ET, CT, MT, etc.) for a given timezone and date
 */
function getTimezoneAbbreviation(timezone: string, date: Date): string {
  try {
    // Get the short timezone name
    const formatter = new Intl.DateTimeFormat('en-US', {
      timeZone: timezone,
      timeZoneName: 'short'
    });

    const parts = formatter.formatToParts(date);
    const timeZonePart = parts.find(part => part.type === 'timeZoneName');

    if (timeZonePart && timeZonePart.value) {
      return timeZonePart.value;
    }
  } catch (error) {
    // Fallback for invalid timezones
  }

  // Fallback: derive from timezone name
  if (timezone.includes('New_York') || timezone.includes('Eastern')) return 'ET';
  if (timezone.includes('Chicago') || timezone.includes('Central')) return 'CT';
  if (timezone.includes('Denver') || timezone.includes('Mountain')) return 'MT';
  if (timezone.includes('Los_Angeles') || timezone.includes('Pacific')) return 'PT';

  return 'Local';
}

/**
 * Legacy alias - kept for backwards compatibility
 * @deprecated Use formatInTimezone instead
 */
export function formatInEasternTime(
  date: Date | string | null | undefined,
  formatStr: 'date' | 'time' | 'datetime' | 'full' = 'datetime'
): string {
  return formatInTimezone(date, formatStr, 'America/New_York');
}

/**
 * Calculate time difference between two dates (for schedule out time)
 * Returns the raw difference in hours without any timezone conversion
 * This is used to calculate how long between when appointment was created and when it's scheduled
 */
export function getTimeDifferenceHours(
  endDate: Date | string | null,
  startDate: Date | string | null
): number | null {
  if (!endDate || !startDate) return null;

  const end = typeof endDate === 'string' ? new Date(endDate) : endDate;
  const start = typeof startDate === 'string' ? new Date(startDate) : startDate;

  if (isNaN(end.getTime()) || isNaN(start.getTime())) return null;

  const diffMs = end.getTime() - start.getTime();
  const diffHours = diffMs / (1000 * 60 * 60);

  return diffHours;
}

/**
 * Format schedule out time in a human-readable way
 */
export function formatScheduleOutTime(hours: number | null): string {
  if (hours === null || hours < 0) return 'N/A';

  const days = hours / 24;

  if (days >= 1) {
    return `${days.toFixed(1)} days`;
  } else {
    return `${hours.toFixed(1)} hours`;
  }
}
