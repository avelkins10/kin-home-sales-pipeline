// lib/utils/quickbase-field-helpers.ts
// Helper functions for extracting values from QuickBase field objects

import { parseQuickbaseDate } from './date-helpers';

/**
 * QuickBase returns fields as wrapped objects: { id, type, value }
 * These helpers safely extract the actual values
 */

/**
 * Extract string value from QuickBase field
 * Returns empty string if field is null, undefined, or an empty object
 */
export function extractFieldValue(field: any): string {
  if (field === null || field === undefined) {
    return '';
  }

  // If it's already a string, return it
  if (typeof field === 'string') {
    return field;
  }

  // If it's a number or boolean, convert to string
  if (typeof field === 'number' || typeof field === 'boolean') {
    return String(field);
  }

  // If it's an object with a value property
  if (typeof field === 'object') {
    if ('value' in field && field.value !== undefined && field.value !== null) {
      // Recursively extract if value is also an object
      if (typeof field.value === 'object') {
        return extractFieldValue(field.value);
      }
      return String(field.value);
    }
    // Empty object or object without value
    return '';
  }

  // Fallback
  return '';
}

/**
 * Extract numeric value from QuickBase field
 * Returns null if field is empty or invalid
 */
export function extractNumericField(field: any): number | null {
  const value = extractFieldValue(field);
  if (!value || value.trim() === '') {
    return null;
  }

  const parsed = parseFloat(value);
  if (isNaN(parsed)) {
    return null;
  }

  return parsed;
}

/**
 * Extract boolean value from QuickBase field
 * Handles: true, false, 1, 0, "1", "0", "true", "false", checkboxes
 */
export function extractBooleanField(field: any): boolean {
  if (field === true || field === 1 || field === '1') {
    return true;
  }

  if (field === false || field === 0 || field === '0') {
    return false;
  }

  // Check object with value
  if (typeof field === 'object' && field !== null && 'value' in field) {
    const value = field.value;
    if (value === true || value === 1 || value === '1') {
      return true;
    }
    if (value === false || value === 0 || value === '0') {
      return false;
    }
  }

  // String comparison (case-insensitive)
  const strValue = extractFieldValue(field).toLowerCase();
  return strValue === 'true' || strValue === 'yes' || strValue === '1';
}

/**
 * Extract date value from QuickBase field
 * Returns Date object or null if invalid
 */
export function extractDateField(field: any): Date | null {
  const value = extractFieldValue(field);
  if (!value || value.trim() === '') {
    return null;
  }

  return parseQuickbaseDate(value);
}

/**
 * Extract array field from QuickBase
 * Some fields return arrays (e.g., multi-select, related records)
 */
export function extractArrayField(field: any): string[] {
  if (Array.isArray(field)) {
    return field.map(item => extractFieldValue(item)).filter(v => v !== '');
  }

  // If it's an object with a value that's an array
  if (typeof field === 'object' && field !== null && 'value' in field) {
    if (Array.isArray(field.value)) {
      return field.value.map(item => extractFieldValue(item)).filter(v => v !== '');
    }
  }

  // Single value - return as array
  const value = extractFieldValue(field);
  return value ? [value] : [];
}

/**
 * Extract email from QuickBase field
 * Some email fields return objects with email property
 */
export function extractEmailField(field: any): string | null {
  if (!field) return null;

  // Direct email string
  if (typeof field === 'string' && field.includes('@')) {
    return field.trim().toLowerCase();
  }

  // Object with email property
  if (typeof field === 'object' && field !== null) {
    if ('email' in field && field.email) {
      return String(field.email).trim().toLowerCase();
    }
    if ('value' in field && field.value) {
      if (typeof field.value === 'object' && 'email' in field.value) {
        return String(field.value.email).trim().toLowerCase();
      }
      if (typeof field.value === 'string' && field.value.includes('@')) {
        return field.value.trim().toLowerCase();
      }
    }
  }

  // Fallback to standard extraction
  const value = extractFieldValue(field);
  return value.includes('@') ? value.trim().toLowerCase() : null;
}

/**
 * Extract user object from QuickBase field
 * User fields often return { id, name, email }
 */
export function extractUserField(field: any): { id?: string; name?: string; email?: string } | null {
  if (!field || typeof field !== 'object') return null;

  // Direct user object
  if ('id' in field || 'name' in field || 'email' in field) {
    return {
      id: field.id ? String(field.id) : undefined,
      name: field.name ? String(field.name) : undefined,
      email: field.email ? String(field.email).trim().toLowerCase() : undefined,
    };
  }

  // Wrapped in value property
  if ('value' in field && typeof field.value === 'object' && field.value !== null) {
    return extractUserField(field.value);
  }

  return null;
}

/**
 * Extract price/currency field
 * Returns number formatted as price or null
 */
export function extractPriceField(field: any): number | null {
  const value = extractNumericField(field);
  if (value === null) return null;

  // Round to 2 decimal places
  return Math.round(value * 100) / 100;
}

/**
 * Extract phone number field
 * Returns formatted phone or null
 */
export function extractPhoneField(field: any): string | null {
  const value = extractFieldValue(field);
  if (!value || value.trim() === '') return null;

  // Remove all non-numeric characters
  const digits = value.replace(/\D/g, '');

  // Must have at least 10 digits
  if (digits.length < 10) return null;

  return digits;
}

/**
 * Extract address field
 * Some address fields are multi-line or structured
 */
export function extractAddressField(field: any): string {
  const value = extractFieldValue(field);
  if (!value) return '';

  // Replace newlines with comma-space
  return value.replace(/\n/g, ', ').trim();
}

/**
 * Check if field has a value (not empty, null, or undefined)
 */
export function hasFieldValue(field: any): boolean {
  const value = extractFieldValue(field);
  return value !== '' && value !== null && value !== undefined;
}

/**
 * Extract record ID field (always numeric)
 */
export function extractRecordId(field: any): number | null {
  const value = extractNumericField(field);
  if (value === null || value < 1) return null;
  return Math.floor(value); // Ensure integer
}

/**
 * Safe field extractor that never throws
 * Returns default value if extraction fails
 */
export function safeExtract<T>(
  extractor: () => T,
  defaultValue: T
): T {
  try {
    const result = extractor();
    return result !== null && result !== undefined ? result : defaultValue;
  } catch (error) {
    console.warn('Field extraction error:', error);
    return defaultValue;
  }
}
