import { sql } from '@vercel/postgres';
import type {
  Notification,
  CreateNotificationInput,
  UnreadCounts,
} from '@/lib/types/notification';

/**
 * Create a new notification in the database
 */
export async function createNotification(
  input: CreateNotificationInput
): Promise<Notification> {
  const {
    user_id,
    project_id,
    type,
    priority,
    source,
    title,
    message = null,
    metadata = {},
    sender_id = null,
    sender_name = null,
    sender_role = null,
    icon = 'bell',
    color = 'blue',
    action_url = null,
  } = input;

  const result = await sql<Notification>`
    INSERT INTO notifications (
      user_id,
      project_id,
      type,
      priority,
      source,
      title,
      message,
      metadata,
      sender_id,
      sender_name,
      sender_role,
      icon,
      color,
      action_url
    ) VALUES (
      ${user_id},
      ${project_id},
      ${type},
      ${priority},
      ${source},
      ${title},
      ${message},
      ${JSON.stringify(metadata)},
      ${sender_id},
      ${sender_name},
      ${sender_role},
      ${icon},
      ${color},
      ${action_url}
    )
    RETURNING *
  `;

  return result.rows[0];
}

/**
 * Get all notifications for a user, ordered by priority and date
 * Supports pagination with limit and offset
 *
 * FIXED: Properly builds WHERE clause without nesting sql template literals
 */
export async function getNotificationsForUser(
  userId: string,
  options: {
    limit?: number;
    offset?: number;
    unreadOnly?: boolean;
    projectId?: number;
  } = {}
): Promise<Notification[]> {
  const {
    limit = 50,
    offset = 0,
    unreadOnly = false,
    projectId,
  } = options;

  // Build query based on filter combinations
  let result;

  if (projectId !== undefined && unreadOnly) {
    // Filter by project AND unread
    result = await sql<Notification>`
      SELECT *
      FROM notifications
      WHERE user_id = ${userId}
        AND project_id = ${projectId}
        AND is_read = false
      ORDER BY
        CASE priority
          WHEN 'critical' THEN 1
          WHEN 'normal' THEN 2
          WHEN 'info' THEN 3
        END,
        created_at DESC
      LIMIT ${limit}
      OFFSET ${offset}
    `;
  } else if (projectId !== undefined) {
    // Filter by project only
    result = await sql<Notification>`
      SELECT *
      FROM notifications
      WHERE user_id = ${userId}
        AND project_id = ${projectId}
      ORDER BY
        CASE priority
          WHEN 'critical' THEN 1
          WHEN 'normal' THEN 2
          WHEN 'info' THEN 3
        END,
        created_at DESC
      LIMIT ${limit}
      OFFSET ${offset}
    `;
  } else if (unreadOnly) {
    // Filter by unread only
    result = await sql<Notification>`
      SELECT *
      FROM notifications
      WHERE user_id = ${userId}
        AND is_read = false
      ORDER BY
        CASE priority
          WHEN 'critical' THEN 1
          WHEN 'normal' THEN 2
          WHEN 'info' THEN 3
        END,
        created_at DESC
      LIMIT ${limit}
      OFFSET ${offset}
    `;
  } else {
    // No filters, just user
    result = await sql<Notification>`
      SELECT *
      FROM notifications
      WHERE user_id = ${userId}
      ORDER BY
        CASE priority
          WHEN 'critical' THEN 1
          WHEN 'normal' THEN 2
          WHEN 'info' THEN 3
        END,
        created_at DESC
      LIMIT ${limit}
      OFFSET ${offset}
    `;
  }

  return result.rows;
}

/**
 * Mark a single notification as read
 */
export async function markNotificationAsRead(
  notificationId: number,
  userId: string
): Promise<Notification | null> {
  const result = await sql<Notification>`
    UPDATE notifications
    SET
      is_read = true,
      read_at = NOW()
    WHERE id = ${notificationId}
      AND user_id = ${userId}
    RETURNING *
  `;

  return result.rows[0] || null;
}

/**
 * Mark all notifications for a user as read
 * Optionally filter by project
 */
export async function markAllNotificationsAsRead(
  userId: string,
  projectId?: number
): Promise<number> {
  let result;

  if (projectId !== undefined) {
    result = await sql`
      UPDATE notifications
      SET
        is_read = true,
        read_at = NOW()
      WHERE user_id = ${userId}
        AND project_id = ${projectId}
        AND is_read = false
    `;
  } else {
    result = await sql`
      UPDATE notifications
      SET
        is_read = true,
        read_at = NOW()
      WHERE user_id = ${userId}
        AND is_read = false
    `;
  }

  return result.rowCount || 0;
}

/**
 * Get unread notification counts for a user
 * Returns total count, counts by priority, and counts by project
 */
export async function getUnreadCounts(userId: string): Promise<UnreadCounts> {
  // Get total unread count
  const totalResult = await sql<{ count: string }>`
    SELECT COUNT(*) as count
    FROM notifications
    WHERE user_id = ${userId}
      AND is_read = false
  `;

  // Get counts by priority
  const priorityResult = await sql<{ priority: string; count: string }>`
    SELECT priority, COUNT(*) as count
    FROM notifications
    WHERE user_id = ${userId}
      AND is_read = false
    GROUP BY priority
  `;

  // Get counts by project
  const projectResult = await sql<{ project_id: number; count: string }>`
    SELECT project_id, COUNT(*) as count
    FROM notifications
    WHERE user_id = ${userId}
      AND is_read = false
    GROUP BY project_id
  `;

  // Get counts by notification type (for task notifications)
  const typeResult = await sql<{ type: string; count: string }>`
    SELECT type, COUNT(*) as count
    FROM notifications
    WHERE user_id = ${userId}
      AND is_read = false
      AND type IN ('task_submitted', 'task_approved', 'task_revision_needed', 'all_tasks_complete')
    GROUP BY type
  `;

  // Build the response object
  const by_priority = {
    critical: 0,
    normal: 0,
    info: 0,
  };

  priorityResult.rows.forEach((row) => {
    const priority = row.priority as 'critical' | 'normal' | 'info';
    by_priority[priority] = parseInt(row.count);
  });

  const by_project: Record<number, number> = {};
  projectResult.rows.forEach((row) => {
    by_project[row.project_id] = parseInt(row.count);
  });

  // Build task notification counts
  const by_type: Record<string, number> = {};
  typeResult.rows.forEach((row) => {
    by_type[row.type] = parseInt(row.count);
  });

  // Calculate total task notifications
  const task_notifications = Object.keys(by_type)
    .filter(type => ['task_submitted', 'task_approved', 'task_revision_needed', 'all_tasks_complete'].includes(type))
    .reduce((sum, type) => sum + by_type[type], 0);

  return {
    total: parseInt(totalResult.rows[0]?.count || '0'),
    by_priority,
    by_project,
    by_type,
    task_notifications,
  };
}

/**
 * Get unread count for a specific project
 */
export async function getUnreadCountForProject(
  userId: string,
  projectId: number
): Promise<number> {
  const result = await sql<{ count: string }>`
    SELECT COUNT(*) as count
    FROM notifications
    WHERE user_id = ${userId}
      AND project_id = ${projectId}
      AND is_read = false
  `;

  return parseInt(result.rows[0]?.count || '0');
}

/**
 * Delete old read notifications (cleanup)
 * Removes notifications read more than X days ago
 */
export async function deleteOldNotifications(daysOld: number = 30): Promise<number> {
  const result = await sql`
    DELETE FROM notifications
    WHERE is_read = true
      AND read_at < NOW() - INTERVAL '${daysOld} days'
  `;

  return result.rowCount || 0;
}
