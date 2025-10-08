import 'server-only';
// Server-only module. Do not import from client components.
export const __isServerOnly = true as const;

import { sql } from '@vercel/postgres';
import { logError } from '@/lib/logging/logger';
import type { ProjectMessage, CreateMessageInput } from '@/lib/types/message';

/**
 * Get messages for a specific project
 *
 * @param projectId - QuickBase project record ID
 * @param options - Query options (limit, offset)
 * @returns Array of messages, newest first
 */
export async function getMessagesForProject(
  projectId: number,
  options: {
    limit?: number;
    offset?: number;
  } = {}
): Promise<ProjectMessage[]> {
  const { limit = 50, offset = 0 } = options;

  try {
    const result = await sql<ProjectMessage>`
      SELECT *
      FROM project_messages
      WHERE project_id = ${projectId}
        AND is_deleted = false
      ORDER BY created_at DESC
      LIMIT ${limit}
      OFFSET ${offset}
    `;

    return result.rows;
  } catch (error) {
    logError('Failed to get messages for project', error as Error, { projectId });
    throw error;
  }
}

/**
 * Create a new project message
 *
 * @param input - Message data
 * @returns Created message
 */
export async function createMessage(
  input: CreateMessageInput
): Promise<ProjectMessage> {
  const {
    project_id,
    sender_id,
    sender_name,
    sender_role,
    message,
    is_system_message = false,
    metadata = {},
  } = input;

  try {
    const result = await sql<ProjectMessage>`
      INSERT INTO project_messages (
        project_id,
        sender_id,
        sender_name,
        sender_role,
        message,
        is_system_message,
        metadata
      )
      VALUES (
        ${project_id},
        ${sender_id},
        ${sender_name},
        ${sender_role},
        ${message},
        ${is_system_message},
        ${JSON.stringify(metadata)}
      )
      RETURNING *
    `;

    if (result.rows.length === 0) {
      throw new Error('Failed to create message');
    }

    return result.rows[0];
  } catch (error) {
    logError('Failed to create message', error as Error, { projectId: project_id, sender_id });
    throw error;
  }
}

/**
 * Soft delete a message (for audit trail)
 *
 * @param messageId - Message ID to delete
 * @param deletedBy - User ID performing the deletion
 * @returns true if successful
 */
export async function deleteMessage(
  messageId: number,
  deletedBy: string
): Promise<boolean> {
  try {
    const result = await sql`
      UPDATE project_messages
      SET
        is_deleted = true,
        deleted_at = NOW(),
        deleted_by = ${deletedBy}
      WHERE id = ${messageId}
        AND is_deleted = false
    `;

    return result.rowCount ? result.rowCount > 0 : false;
  } catch (error) {
    logError('Failed to delete message', error as Error, { messageId, deletedBy });
    throw error;
  }
}

/**
 * Get message count for a project
 *
 * @param projectId - QuickBase project record ID
 * @returns Total number of non-deleted messages
 */
export async function getMessageCount(projectId: number): Promise<number> {
  try {
    const result = await sql<{ count: string }>`
      SELECT COUNT(*) as count
      FROM project_messages
      WHERE project_id = ${projectId}
        AND is_deleted = false
    `;

    return parseInt(result.rows[0]?.count || '0');
  } catch (error) {
    logError('Failed to get message count', error as Error, { projectId });
    return 0;
  }
}

/**
 * Get recent messages across all projects for a user (for admin dashboard)
 *
 * @param limit - Number of messages to return
 * @returns Array of recent messages
 */
export async function getRecentMessages(limit: number = 20): Promise<ProjectMessage[]> {
  try {
    const result = await sql<ProjectMessage>`
      SELECT *
      FROM project_messages
      WHERE is_deleted = false
      ORDER BY created_at DESC
      LIMIT ${limit}
    `;

    return result.rows;
  } catch (error) {
    logError('Failed to get recent messages', error as Error);
    return [];
  }
}
