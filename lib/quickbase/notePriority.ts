import 'server-only';
// Server-only module. Do not import from client components.
export const __isServerOnly = true as const;

import { NOTE_CATEGORIES } from '@/lib/constants/noteFieldIds';
import type { NotificationPriority } from '@/lib/types/notification';

/**
 * Configurable mapping of note categories to notification priorities.
 *
 * Priority levels:
 * - critical: Urgent issues requiring immediate attention (red badge)
 * - normal: Standard updates and information (blue badge)
 * - info: Low-priority informational notes (gray badge)
 *
 * Configuration:
 * Adjust these mappings based on business requirements and user feedback.
 * Categories not listed here will default to 'normal' priority.
 */
export const NOTE_PRIORITY_CONFIG: Record<string, NotificationPriority> = {
  // CRITICAL - Issues blocking project progress
  [NOTE_CATEGORIES.SALES]: 'critical',          // Sales-related issues often need immediate action
  [NOTE_CATEGORIES.ACCEPTANCE]: 'critical',     // Acceptance blockers are time-sensitive
  [NOTE_CATEGORIES.INSTALLATION]: 'critical',   // Installation issues impact customer satisfaction
  [NOTE_CATEGORIES.INSPECTION]: 'critical',     // Inspection failures require quick response
  [NOTE_CATEGORIES.PTO]: 'critical',            // PTO issues delay project completion

  // NORMAL - Standard project updates
  [NOTE_CATEGORIES.SURVEY]: 'normal',           // Survey updates are routine
  [NOTE_CATEGORIES.DESIGN]: 'normal',           // Design notes are informational
  [NOTE_CATEGORIES.NEM]: 'normal',              // NEM updates are expected workflow
  [NOTE_CATEGORIES.PERMITTING]: 'normal',       // Permit updates are routine
  [NOTE_CATEGORIES.HOA]: 'normal',              // HOA updates are expected workflow
  [NOTE_CATEGORIES.VERIFICATION]: 'normal',     // Verification is standard process
  [NOTE_CATEGORIES.COMMISSIONING]: 'normal',    // Commissioning is final step

  // Add custom categories here as needed
  // 'Custom Category': 'info',
};

/**
 * Get notification priority for a note category.
 *
 * Uses configurable mapping from NOTE_PRIORITY_CONFIG.
 * Falls back to 'normal' priority for unknown categories.
 *
 * @param category - Note category from QuickBase
 * @returns Notification priority level
 *
 * @example
 * ```ts
 * const priority = getNotePriority('Sales');
 * // Returns: 'critical'
 *
 * const priority = getNotePriority('Unknown Category');
 * // Returns: 'normal' (default)
 * ```
 */
export function getNotePriority(category: string): NotificationPriority {
  const priority = NOTE_PRIORITY_CONFIG[category];

  if (priority) {
    console.log('[notePriority] Category mapped to priority:', { category, priority });
    return priority;
  }

  // Default to 'normal' for unknown categories
  console.log('[notePriority] Unknown category, defaulting to normal:', category);
  return 'normal';
}

/**
 * Get all categories mapped to a specific priority level.
 * Useful for admin UI or documentation.
 *
 * @param priority - Priority level to filter by
 * @returns Array of category names
 *
 * @example
 * ```ts
 * const criticalCategories = getCategoriesByPriority('critical');
 * // Returns: ['Sales', 'Acceptance', 'Installation', ...]
 * ```
 */
export function getCategoriesByPriority(priority: NotificationPriority): string[] {
  return Object.entries(NOTE_PRIORITY_CONFIG)
    .filter(([_, p]) => p === priority)
    .map(([category]) => category);
}

/**
 * Get the full priority configuration.
 * Useful for admin UI to display/edit priority mappings.
 *
 * @returns Complete priority configuration map
 */
export function getPriorityConfig(): Record<string, NotificationPriority> {
  return { ...NOTE_PRIORITY_CONFIG };
}
