/**
 * Escalation Helper Functions
 *
 * Shared utilities for escalation urgency calculations and metadata.
 * Can be used in both client and server components.
 */

/**
 * Calculate urgency level for a sales aid request based on deadline
 * @param deadline - ISO date string for the deadline
 * @returns Urgency level: 'critical' | 'high' | 'normal'
 */
export function calculateSalesAidUrgency(deadline: string): 'critical' | 'high' | 'normal' {
  if (!deadline) return 'normal';

  const deadlineDate = new Date(deadline);
  const now = new Date();

  const hoursUntilDeadline = (deadlineDate.getTime() - now.getTime()) / (1000 * 60 * 60);

  if (hoursUntilDeadline < 0) return 'critical'; // Past deadline
  if (hoursUntilDeadline < 24) return 'high'; // Within 24 hours
  return 'normal';
}
