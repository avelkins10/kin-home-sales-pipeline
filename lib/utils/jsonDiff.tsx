/**
 * JSON diff utility for rendering audit log changes
 * 
 * These utilities provide consistent diff rendering across the audit logs interface.
 */

import React from 'react';

/**
 * Convert value to readable string format
 */
export function formatDiffValue(value: any): string {
  if (value === null || value === undefined) {
    return 'null';
  }
  
  if (typeof value === 'boolean') {
    return value ? 'true' : 'false';
  }
  
  if (typeof value === 'string') {
    // Truncate long strings with ellipsis
    if (value.length > 100) {
      return value.substring(0, 100) + '...';
    }
    return value;
  }
  
  if (typeof value === 'number') {
    return value.toString();
  }
  
  if (typeof value === 'object' || Array.isArray(value)) {
    return JSON.stringify(value, null, 2);
  }
  
  return String(value);
}

/**
 * Determine color for diff display
 */
export function getDiffColor(hasOldValue: boolean, hasNewValue: boolean): string {
  if (hasOldValue && !hasNewValue) {
    return 'text-red-600'; // deleted
  }
  
  if (!hasOldValue && hasNewValue) {
    return 'text-green-600'; // added
  }
  
  if (hasOldValue && hasNewValue) {
    return 'text-blue-600'; // changed
  }
  
  return 'text-gray-600'; // no change
}

/**
 * Render field change as React element
 */
export function renderFieldDiff(field: string, oldValue: any, newValue: any): JSX.Element {
  const hasOldValue = oldValue !== null && oldValue !== undefined;
  const hasNewValue = newValue !== null && newValue !== undefined;
  
  // Handle special cases
  if (field.toLowerCase().includes('password') || field.toLowerCase().includes('token')) {
    return (
      <div className="space-y-1">
        <div className="font-medium text-sm">{field}</div>
        <div className="text-sm text-gray-500">*** (sensitive data)</div>
      </div>
    );
  }
  
  // Handle large objects
  const oldStr = formatDiffValue(oldValue);
  const newStr = formatDiffValue(newValue);
  
  if (oldStr.length > 200 || newStr.length > 200) {
    return (
      <div className="space-y-1">
        <div className="font-medium text-sm">{field}</div>
        <div className="text-sm text-gray-500">Object changed (large content)</div>
      </div>
    );
  }
  
  return (
    <div className="space-y-1">
      <div className="font-medium text-sm">{field}</div>
      <div className="flex items-center space-x-2 text-sm">
        {hasOldValue && (
          <span className="text-red-600 line-through">
            {formatDiffValue(oldValue)}
          </span>
        )}
        {hasOldValue && hasNewValue && (
          <span className="text-gray-400">→</span>
        )}
        {hasNewValue && (
          <span className="text-green-600">
            {formatDiffValue(newValue)}
          </span>
        )}
      </div>
    </div>
  );
}

/**
 * Generate human-readable summary of changes
 */
export function calculateChangeSummary(changes: Record<string, { old: any; new: any }>): string {
  const fieldCount = Object.keys(changes).length;
  
  if (fieldCount === 0) {
    return '0 fields changed';
  }
  
  if (fieldCount === 1) {
    return '1 field changed';
  }
  
  return `${fieldCount} fields changed`;
}

/**
 * Determine if change is significant (not just whitespace, case, etc.)
 */
export function hasSignificantChange(oldValue: any, newValue: any): boolean {
  // Handle null/undefined
  if (oldValue === null || oldValue === undefined) {
    return newValue !== null && newValue !== undefined;
  }
  
  if (newValue === null || newValue === undefined) {
    return oldValue !== null && oldValue !== undefined;
  }
  
  // Handle strings (trim whitespace)
  if (typeof oldValue === 'string' && typeof newValue === 'string') {
    return oldValue.trim() !== newValue.trim();
  }
  
  // Handle numbers
  if (typeof oldValue === 'number' && typeof newValue === 'number') {
    return oldValue !== newValue;
  }
  
  // Handle booleans
  if (typeof oldValue === 'boolean' && typeof newValue === 'boolean') {
    return oldValue !== newValue;
  }
  
  // Handle objects (deep equality)
  if (typeof oldValue === 'object' && typeof newValue === 'object') {
    return JSON.stringify(oldValue) !== JSON.stringify(newValue);
  }
  
  // Default comparison
  return oldValue !== newValue;
}