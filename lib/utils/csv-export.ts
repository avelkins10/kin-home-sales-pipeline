/**
 * CSV Export Utility Functions
 * 
 * Provides reusable functions for converting data to CSV format and triggering downloads.
 * Follows the pattern established in components/settings/AuditLogsTab.tsx
 */

import { format } from 'date-fns';

/**
 * Type definition for CSV headers mapping
 * Maps field names to display names for CSV column headers
 */
export type CSVHeaders = Record<string, string>;

/**
 * Escape special characters in CSV values
 * @param value - The value to escape
 * @returns Escaped CSV value
 */
export function escapeCSVValue(value: any): string {
  if (value === null || value === undefined) {
    return '';
  }
  
  const stringValue = String(value);
  
  // If contains comma, quote, or newline, wrap in double quotes
  if (stringValue.includes(',') || stringValue.includes('"') || stringValue.includes('\n')) {
    // Escape existing double quotes by doubling them
    const escapedValue = stringValue.replace(/"/g, '""');
    return `"${escapedValue}"`;
  }
  
  return stringValue;
}

/**
 * Format dates consistently for CSV export
 * @param date - Date to format
 * @returns Formatted date string or empty string if null/undefined
 */
export function formatDateForCSV(date: string | Date | null): string {
  if (!date) {
    return '';
  }
  
  try {
    const dateObj = typeof date === 'string' ? new Date(date) : date;
    return format(dateObj, 'yyyy-MM-dd HH:mm:ss');
  } catch (error) {
    return '';
  }
}

/**
 * Convert array of objects to CSV string
 * @param data - Array of data objects
 * @param headers - Array of header strings (field names)
 * @returns CSV string
 */
export function convertToCSV(data: any[], headers: string[]): string {
  if (!data || data.length === 0) {
    return headers.join(',');
  }
  
  // Create CSV header row
  const headerRow = headers.map(header => escapeCSVValue(header)).join(',');
  
  // Create CSV data rows
  const dataRows = data.map(row => {
    return headers.map(header => {
      const value = row[header];
      return escapeCSVValue(value);
    }).join(',');
  });
  
  // Combine header and data rows
  return [headerRow, ...dataRows].join('\n');
}

/**
 * Trigger CSV download in browser
 * @param csvContent - CSV string content
 * @param filename - Filename for download
 */
export function downloadCSV(csvContent: string, filename: string): void {
  // Create Blob from CSV content
  const blob = new Blob([csvContent], { 
    type: 'text/csv;charset=utf-8;' 
  });
  
  // Create object URL from blob
  const url = URL.createObjectURL(blob);
  
  // Create temporary anchor element
  const link = document.createElement('a');
  link.href = url;
  link.download = filename;
  
  // Programmatically click to trigger download
  document.body.appendChild(link);
  link.click();
  
  // Clean up
  document.body.removeChild(link);
  URL.revokeObjectURL(url);
}

/**
 * High-level function to export analytics data to CSV
 * @param data - Array of data objects to export
 * @param filename - Filename for the CSV file
 * @param fieldToHeader - Object mapping field names to column display names
 */
export function exportAnalyticsToCSV(data: any[], filename: string, fieldToHeader: CSVHeaders): void {
  try {
    // Map data to use display names as keys
    const mappedData = data.map(row => {
      const mappedRow: any = {};
      Object.keys(fieldToHeader).forEach(fieldName => {
        const displayName = fieldToHeader[fieldName];
        mappedRow[displayName] = row[fieldName];
      });
      return mappedRow;
    });
    
    // Generate CSV with display names as headers
    const displayNames = Object.values(fieldToHeader);
    const csvContent = convertToCSV(mappedData, displayNames);
    
    // Trigger download
    downloadCSV(csvContent, filename);
  } catch (error) {
    throw new Error(`Failed to export CSV: ${error instanceof Error ? error.message : 'Unknown error'}`);
  }
}
