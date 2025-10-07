// lib/utils/project-helpers.ts

import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';

/**
 * Parse customer name into first and last name
 * @param fullName - Full customer name
 * @returns object with firstName and lastName
 */
export function parseCustomerName(fullName: string): { firstName: string; lastName: string } {
  if (!fullName || fullName.trim() === '') {
    return { firstName: 'Unknown', lastName: '' };
  }

  const parts = fullName.trim().split(/\s+/);

  if (parts.length === 1) {
    return { firstName: parts[0], lastName: '' };
  }

  if (parts.length === 2) {
    return { firstName: parts[0], lastName: parts[1] };
  }

  // Multiple words: last word is lastName, rest is firstName
  const lastName = parts[parts.length - 1];
  const firstName = parts.slice(0, -1).join(' ');

  return { firstName, lastName };
}

/**
 * Format address into line1 and line2
 * @param address - Full address string
 * @returns object with line1 and line2
 */
export function formatAddress(address: string): { line1: string; line2: string } {
  if (!address || address.trim() === '') {
    return { line1: '', line2: '' };
  }

  const parts = address.split(',').map(part => part.trim());
  
  if (parts.length === 1) {
    return { line1: parts[0], line2: '' };
  }
  
  if (parts.length === 2) {
    return { line1: parts[0], line2: parts[1] };
  }
  
  // Multiple parts: first is line1, rest is line2
  const line1 = parts[0];
  const line2 = parts.slice(1).join(', ');
  
  return { line1, line2 };
}

/**
 * Get project age in days
 * @param project - Quickbase project object
 * @returns age in days, 0 if missing
 */
export function getProjectAge(project: QuickbaseProject): number {
  const ageValue = project[PROJECT_FIELDS.PROJECT_AGE]?.value;
  const age = parseInt(ageValue || '0');
  return isNaN(age) ? 0 : age;
}

/**
 * Get install date from project (priority order)
 * @param project - Quickbase project object
 * @returns install date string or null
 */
export function getInstallDate(project: QuickbaseProject): string | null {
  // Check fields in priority order
  const installCompletedDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
  if (installCompletedDate) return installCompletedDate;

  const installScheduledDate = project[PROJECT_FIELDS.INSTALL_SCHEDULED_DATE_CAPTURE]?.value;
  if (installScheduledDate) return installScheduledDate;

  const installScheduledStartDate = project[PROJECT_FIELDS.INSTALL_SCHEDULED_START_DATE]?.value;
  if (installScheduledStartDate) return installScheduledStartDate;

  const estimatedInstallDate = project[PROJECT_FIELDS.ESTIMATED_INSTALL_DATE]?.value;
  if (estimatedInstallDate) return estimatedInstallDate;

  return null;
}

/**
 * Get reschedule history count
 * @param project - Quickbase project object
 * @returns number of reschedules, 0 if missing
 */
export function getRescheduleHistory(project: QuickbaseProject): number {
  // Note: RESCHEDULE_COUNT field may not exist in fieldIds.ts
  // This is a placeholder for when the field is added
  const rescheduleCount = project[PROJECT_FIELDS.RESCHEDULE_COUNT]?.value;
  const count = parseInt(rescheduleCount || '0');
  return isNaN(count) ? 0 : count;
}
