// lib/utils/task-urgency.ts
/**
 * Task Urgency System
 *
 * Determines urgency level based on:
 * - Task age (how long it's been waiting)
 * - Project status
 * - Submission status
 */

import { Task, TaskSubmission } from '@/lib/types/task';

export type UrgencyLevel = 'critical' | 'urgent' | 'normal';

export interface TaskUrgency {
  level: UrgencyLevel;
  reason: string;
  daysWaiting: number;
}

/**
 * Calculate days between two dates
 */
function calculateDaysWaiting(dateCreated: string | null): number {
  if (!dateCreated) return 0;

  const created = new Date(dateCreated);
  const now = new Date();
  const diffTime = Math.abs(now.getTime() - created.getTime());
  const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));

  return diffDays;
}

/**
 * Determine task urgency level
 *
 * Rules:
 * - CRITICAL (Red): Task >7 days old OR project in "Rejected" status
 * - URGENT (Yellow): Task 3-7 days old OR latest submission "Needs Revision"
 * - NORMAL (Gray): Task <3 days old
 */
export function getTaskUrgency(
  task: Task,
  projectStatus?: string
): TaskUrgency {
  const daysWaiting = calculateDaysWaiting(task.dateCreated);

  // Check if project is rejected
  const isProjectRejected = projectStatus?.toLowerCase().includes('reject') || false;

  // Check if latest submission needs revision
  const latestSubmission = task.submissions?.[0]; // Assume sorted by date desc
  const needsRevision = latestSubmission?.opsDisposition === 'Needs Revision';

  // CRITICAL: >7 days OR project rejected
  if (daysWaiting > 7) {
    return {
      level: 'critical',
      reason: `Task waiting ${daysWaiting} days`,
      daysWaiting
    };
  }

  if (isProjectRejected) {
    return {
      level: 'critical',
      reason: 'Project rejected - urgent action required',
      daysWaiting
    };
  }

  // URGENT: 3-7 days OR needs revision
  if (daysWaiting >= 3 && daysWaiting <= 7) {
    return {
      level: 'urgent',
      reason: `Task waiting ${daysWaiting} days`,
      daysWaiting
    };
  }

  if (needsRevision) {
    return {
      level: 'urgent',
      reason: 'Needs revision from ops',
      daysWaiting
    };
  }

  // NORMAL: <3 days
  return {
    level: 'normal',
    reason: daysWaiting === 0 ? 'Assigned today' : `Assigned ${daysWaiting} ${daysWaiting === 1 ? 'day' : 'days'} ago`,
    daysWaiting
  };
}

/**
 * Get urgency color classes for badges
 */
export function getUrgencyColors(level: UrgencyLevel): {
  bg: string;
  text: string;
  border: string;
} {
  switch (level) {
    case 'critical':
      return {
        bg: 'bg-red-50',
        text: 'text-red-700',
        border: 'border-red-200'
      };
    case 'urgent':
      return {
        bg: 'bg-yellow-50',
        text: 'text-yellow-700',
        border: 'border-yellow-200'
      };
    case 'normal':
      return {
        bg: 'bg-gray-50',
        text: 'text-gray-600',
        border: 'border-gray-200'
      };
  }
}

/**
 * Get urgency label for display
 */
export function getUrgencyLabel(level: UrgencyLevel): string {
  switch (level) {
    case 'critical':
      return 'Critical';
    case 'urgent':
      return 'Urgent';
    case 'normal':
      return 'Normal';
  }
}

/**
 * Sort tasks by urgency (critical first, then urgent, then normal)
 * Within same urgency level, sort by age (oldest first)
 */
export function sortTasksByUrgency<T extends Task>(
  tasks: T[],
  projectStatuses?: Map<number, string>
): T[] {
  return [...tasks].sort((a, b) => {
    const urgencyA = getTaskUrgency(a, projectStatuses?.get(a.recordId));
    const urgencyB = getTaskUrgency(b, projectStatuses?.get(b.recordId));

    // Sort by urgency level first
    const urgencyOrder = { critical: 0, urgent: 1, normal: 2 };
    const urgencyDiff = urgencyOrder[urgencyA.level] - urgencyOrder[urgencyB.level];

    if (urgencyDiff !== 0) return urgencyDiff;

    // Within same urgency, sort by age (oldest first)
    return urgencyB.daysWaiting - urgencyA.daysWaiting;
  });
}
