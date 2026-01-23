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
