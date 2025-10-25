// lib/db/messageReadReceipts.ts

import { sql } from './client';

export interface MessageReadReceipt {
  id: string;
  messageId: number;
  userEmail: string;
  readAt: Date;
  createdAt: Date;
  updatedAt: Date;
}

/**
 * Mark messages as read for a specific user
 */
export async function markMessagesAsRead(
  messageIds: number[],
  userEmail: string
): Promise<void> {
  if (messageIds.length === 0) return;

  // Use INSERT ... ON CONFLICT to handle duplicate receipts gracefully
  const values = messageIds.map(messageId => `(${messageId}, '${userEmail}')`).join(',');
  
  await sql`
    INSERT INTO message_read_receipts (message_id, user_email)
    VALUES ${sql.unsafe(values)}
    ON CONFLICT (message_id, user_email) 
    DO UPDATE SET 
      read_at = NOW(),
      updated_at = NOW()
  `;
}

/**
 * Get read receipts for specific messages and user
 */
export async function getReadReceipts(
  messageIds: number[],
  userEmail: string
): Promise<MessageReadReceipt[]> {
  if (messageIds.length === 0) return [];

  const result = await sql`
    SELECT id, message_id, user_email, read_at, created_at, updated_at
    FROM message_read_receipts
    WHERE message_id = ANY(${messageIds}) 
    AND user_email = ${userEmail}
  `;

  return result.rows.map(row => ({
    id: row.id,
    messageId: row.message_id,
    userEmail: row.user_email,
    readAt: row.read_at,
    createdAt: row.created_at,
    updatedAt: row.updated_at
  }));
}

/**
 * Check if a message is read by a specific user
 */
export async function isMessageRead(
  messageId: number,
  userEmail: string
): Promise<boolean> {
  const result = await sql`
    SELECT 1 FROM message_read_receipts
    WHERE message_id = ${messageId} 
    AND user_email = ${userEmail}
    LIMIT 1
  `;

  return result.rows.length > 0;
}

/**
 * Get read status for multiple messages
 */
export async function getMessagesReadStatus(
  messageIds: number[],
  userEmail: string
): Promise<Record<number, boolean>> {
  if (messageIds.length === 0) return {};

  const receipts = await getReadReceipts(messageIds, userEmail);
  const readMessageIds = new Set(receipts.map(r => r.messageId));
  
  const status: Record<number, boolean> = {};
  messageIds.forEach(messageId => {
    status[messageId] = readMessageIds.has(messageId);
  });
  
  return status;
}
