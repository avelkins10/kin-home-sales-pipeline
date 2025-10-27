/**
 * Daily Digest Email Generator
 * Aggregates user activity data and sends daily summary emails
 */

import { getUnreadCounts, getNotificationsForUser } from '@/lib/db/notifications';
import { getDailyDigestEmailTemplate } from './email-templates';
import { sendCustomEmail } from './email-helpers';
import { logInfo, logError } from '@/lib/logging/logger';

interface DigestData {
  unreadNotifications: number;
  criticalNotifications: number;
  tasksCompleted: number;
  tasksNeedingAttention: number;
  projectsUpdated: number;
  newProjects: number;
  topNotifications: Array<{
    title: string;
    message: string;
    projectName: string;
    priority: 'critical' | 'normal' | 'info';
  }>;
}

/**
 * Extract user's first name from email
 */
function getUserNameFromEmail(email: string): string {
  try {
    const localPart = email.split('@')[0];
    const namePart = localPart.split(/[._]/)[0];
    return namePart.charAt(0).toUpperCase() + namePart.slice(1).toLowerCase();
  } catch {
    return 'there';
  }
}

/**
 * Aggregate digest data for a user
 */
async function aggregateDigestData(userId: string): Promise<DigestData> {
  try {
    // Get unread counts
    const unreadCounts = await getUnreadCounts(userId);

    // Get recent notifications (last 24 hours, unread only)
    const yesterday = new Date();
    yesterday.setDate(yesterday.getDate() - 1);

    const recentNotifications = await getNotificationsForUser(userId, {
      limit: 10,
      unreadOnly: true
    });

    // Filter to last 24 hours and count by type
    const last24Hours = recentNotifications.filter(n => {
      const createdAt = new Date(n.created_at);
      return createdAt >= yesterday;
    });

    // Count task-related notifications
    const taskNotifications = last24Hours.filter(n =>
      ['task_approved', 'task_revision_needed', 'all_tasks_complete'].includes(n.type)
    );

    const tasksCompleted = last24Hours.filter(n =>
      n.type === 'task_approved' || n.type === 'all_tasks_complete'
    ).length;

    const tasksNeedingAttention = last24Hours.filter(n =>
      n.type === 'task_revision_needed'
    ).length;

    // Extract project info from top 5 critical notifications
    const topNotifications = last24Hours
      .filter(n => n.priority === 'critical' || n.priority === 'normal')
      .slice(0, 5)
      .map(n => ({
        title: n.title,
        message: n.message || '',
        projectName: `Project #${n.project_id}`, // Simplified - could fetch actual project name
        priority: n.priority
      }));

    // Estimate projects updated (based on notification activity)
    const uniqueProjects = new Set(last24Hours.map(n => n.project_id));
    const projectsUpdated = uniqueProjects.size;

    // For now, set newProjects to 0 (would need project creation tracking)
    const newProjects = 0;

    return {
      unreadNotifications: unreadCounts.total,
      criticalNotifications: unreadCounts.by_priority.critical,
      tasksCompleted,
      tasksNeedingAttention,
      projectsUpdated,
      newProjects,
      topNotifications
    };
  } catch (error) {
    logError('Failed to aggregate digest data', error as Error, { userId });

    // Return empty digest data on error
    return {
      unreadNotifications: 0,
      criticalNotifications: 0,
      tasksCompleted: 0,
      tasksNeedingAttention: 0,
      projectsUpdated: 0,
      newProjects: 0,
      topNotifications: []
    };
  }
}

/**
 * Send daily digest email to a user
 */
export async function sendDailyDigest(userId: string): Promise<{
  success: boolean;
  error?: string;
  skipped?: boolean;
}> {
  try {
    // Check if email is enabled
    if (process.env.EMAIL_ENABLED !== 'true') {
      logInfo('Daily digest skipped - email disabled', { userId });
      return { success: false, error: 'email_disabled', skipped: true };
    }

    // Aggregate digest data
    const digestData = await aggregateDigestData(userId);

    // Skip if no activity (no notifications)
    if (digestData.unreadNotifications === 0 && digestData.topNotifications.length === 0) {
      logInfo('Daily digest skipped - no activity', { userId });
      return { success: false, error: 'no_activity', skipped: true };
    }

    // Generate email
    const recipientName = getUserNameFromEmail(userId);
    const dashboardUrl = process.env.NEXT_PUBLIC_APP_URL || 'https://dashboard.kinhome.com';

    const html = getDailyDigestEmailTemplate(recipientName, digestData, dashboardUrl);

    // Send email
    await sendCustomEmail(
      userId,
      'Daily Digest - Kin Home Sales Dashboard',
      html
    );

    logInfo('Daily digest sent successfully', { userId, digestData });
    return { success: true };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    logError('Failed to send daily digest', error as Error, { userId });

    return {
      success: false,
      error: errorMessage
    };
  }
}

/**
 * Send daily digest to all users with dailyDigest enabled
 */
export async function sendDailyDigestToAllUsers(): Promise<{
  total: number;
  sent: number;
  skipped: number;
  failed: number;
}> {
  const summary = {
    total: 0,
    sent: 0,
    skipped: 0,
    failed: 0
  };

  try {
    // Get all users from database
    const { sql } = await import('@/lib/db/client');

    // Get users who have daily digest enabled
    const users = await sql`
      SELECT email
      FROM users
      WHERE daily_digest = true
        AND email IS NOT NULL
        AND email != ''
    `;

    summary.total = users.length;
    logInfo(`Starting daily digest for ${summary.total} users`);

    // Send digest to each user
    for (const user of users) {
      try {
        const result = await sendDailyDigest(user.email);

        if (result.success) {
          summary.sent++;
        } else if (result.skipped) {
          summary.skipped++;
        } else {
          summary.failed++;
        }
      } catch (error) {
        logError('Failed to send digest to user', error as Error, { email: user.email });
        summary.failed++;
      }
    }

    logInfo('Daily digest batch completed', summary);
    return summary;

  } catch (error) {
    logError('Failed to send daily digests', error as Error);
    return summary;
  }
}
