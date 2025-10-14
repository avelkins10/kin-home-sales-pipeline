// lib/utils/date-helpers.ts
/**
 * Date utilities for handling QuickBase date fields correctly
 *
 * QuickBase returns dates in ISO format, but we need to handle them carefully:
 * - Date-only fields (Sales Date, Install Date): Should ignore timezone and use the date as-is
 * - DateTime fields (timestamps): Should preserve time but may need timezone conversion
 */

import { format, parseISO } from 'date-fns';

/**
 * Parse a QuickBase date field as a local date, ignoring timezone
 *
 * This is critical for date-only fields like Sales Date, Install Date, etc.
 *
 * Example:
 * - QuickBase value: "2025-01-15T00:00:00Z" (midnight UTC)
 * - Desired display: "Jan 15, 2025" (NOT "Jan 14, 2025")
 *
 * The function extracts only the date portion (YYYY-MM-DD) and creates
 * a Date object in local timezone, preventing the UTC->local conversion bug.
 *
 * @param dateString - ISO date string from QuickBase (e.g., "2025-01-15" or "2025-01-15T00:00:00Z")
 * @returns Date object representing the date in local timezone, or null if invalid
 */
export function parseQuickbaseDate(dateString: string | null | undefined): Date | null {
  if (!dateString || typeof dateString !== 'string') {
    return null;
  }

  try {
    // Extract just the date portion (YYYY-MM-DD)
    // This handles both "2025-01-15" and "2025-01-15T00:00:00Z" formats
    const dateMatch = dateString.match(/^(\d{4}-\d{2}-\d{2})/);
    if (!dateMatch) {
      return null;
    }

    const datePart = dateMatch[1]; // e.g., "2025-01-15"

    // Parse as local date by providing time component in local timezone
    // This prevents the UTC conversion that causes off-by-one day errors
    const [year, month, day] = datePart.split('-').map(Number);

    // Create date in local timezone (month is 0-indexed in JS)
    const localDate = new Date(year, month - 1, day);

    // Validate the date is valid
    if (isNaN(localDate.getTime())) {
      return null;
    }

    return localDate;
  } catch {
    return null;
  }
}

/**
 * Parse a QuickBase datetime field with proper timezone handling
 *
 * Use this for fields that include time components and need timezone conversion
 * (e.g., survey scheduled time, install appointment time)
 *
 * @param dateTimeString - ISO datetime string from QuickBase
 * @returns Date object with proper timezone, or null if invalid
 */
export function parseQuickbaseDateTime(dateTimeString: string | null | undefined): Date | null {
  if (!dateTimeString || typeof dateTimeString !== 'string') {
    return null;
  }

  try {
    // Use parseISO from date-fns which handles timezone properly
    const date = parseISO(dateTimeString);

    // Validate the date is valid
    if (isNaN(date.getTime())) {
      return null;
    }

    return date;
  } catch {
    return null;
  }
}

/**
 * Format a QuickBase date field for display
 *
 * @param dateString - ISO date string from QuickBase
 * @param formatString - Optional format string (default: 'MMM d, yyyy')
 * @returns Formatted date string, or empty string if invalid
 */
export function formatQuickbaseDate(
  dateString: string | null | undefined,
  formatString: string = 'MMM d, yyyy'
): string {
  const date = parseQuickbaseDate(dateString);
  if (!date) {
    return '';
  }

  try {
    return format(date, formatString);
  } catch {
    return '';
  }
}

/**
 * Format a QuickBase datetime field for display
 *
 * @param dateTimeString - ISO datetime string from QuickBase
 * @param formatString - Optional format string (default: 'MMM d, yyyy h:mm a')
 * @returns Formatted datetime string, or empty string if invalid
 */
export function formatQuickbaseDateTime(
  dateTimeString: string | null | undefined,
  formatString: string = 'MMM d, yyyy h:mm a'
): string {
  const date = parseQuickbaseDateTime(dateTimeString);
  if (!date) {
    return '';
  }

  try {
    return format(date, formatString);
  } catch {
    return '';
  }
}

/**
 * Calculate age in days from a QuickBase date field
 *
 * @param dateString - ISO date string from QuickBase
 * @returns Age in days, or 0 if invalid/future date
 */
export function calculateDaysFromDate(dateString: string | null | undefined): number {
  const date = parseQuickbaseDate(dateString);
  if (!date) {
    return 0;
  }

  try {
    const now = new Date();
    // Set time to midnight for consistent day calculation
    now.setHours(0, 0, 0, 0);
    date.setHours(0, 0, 0, 0);

    const diffTime = now.getTime() - date.getTime();
    const diffDays = Math.floor(diffTime / (1000 * 60 * 60 * 24));

    // Return 0 if negative (future date)
    return diffDays >= 0 ? diffDays : 0;
  } catch {
    return 0;
  }
}

/**
 * Get the date portion only from a QuickBase date string
 * Returns in YYYY-MM-DD format for consistent storage/comparison
 *
 * @param dateString - ISO date string from QuickBase
 * @returns Date string in YYYY-MM-DD format, or empty string if invalid
 */
export function getDateOnly(dateString: string | null | undefined): string {
  if (!dateString || typeof dateString !== 'string') {
    return '';
  }

  const dateMatch = dateString.match(/^(\d{4}-\d{2}-\d{2})/);
  return dateMatch ? dateMatch[1] : '';
}

/**
 * Check if a QuickBase date string represents a valid date
 *
 * @param dateString - ISO date string from QuickBase
 * @returns true if valid date, false otherwise
 */
export function isValidQuickbaseDate(dateString: string | null | undefined): boolean {
  const date = parseQuickbaseDate(dateString);
  return date !== null;
}
