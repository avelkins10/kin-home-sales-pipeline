import { sql } from '@/lib/db/client';

export interface ParsedMention {
  user_id: string;
  user_name: string;
  user_email: string;
}

/**
 * Parse @mentions from message text
 * Supports formats:
 * - @[User Name](user-id)
 * - This is the format used when composing with autocomplete
 */
export function extractMentionIds(text: string): string[] {
  // Match mentions in format: @[Name](id)
  const mentionRegex = /@\[([^\]]+)\]\(([^)]+)\)/g;
  const matches = Array.from(text.matchAll(mentionRegex));
  return matches.map(match => match[2]); // Extract user ID from parentheses
}

/**
 * Look up mentioned users from database
 * Returns user details for each valid mention
 */
export async function resolveMentions(userIds: string[]): Promise<ParsedMention[]> {
  if (userIds.length === 0) {
    return [];
  }

  try {
    // Build IN clause with explicit UUID casting for each ID
    const placeholders = userIds.map(id => `'${id}'::uuid`).join(', ');

    const result = await sql.query(
      `SELECT id, name, email FROM users WHERE id IN (${placeholders}) AND is_active = true`
    );

    return result.rows.map(row => ({
      user_id: row.id,
      user_name: row.name,
      user_email: row.email,
    }));
  } catch (error) {
    console.error('Error resolving mentions:', error);
    return [];
  }
}

/**
 * Parse message text and resolve all mentions
 */
export async function parseMentions(text: string): Promise<ParsedMention[]> {
  const userIds = extractMentionIds(text);
  return await resolveMentions(userIds);
}

/**
 * Format message text for display
 * Converts @[Name](id) to clickable mentions
 */
export function formatMentionsForDisplay(text: string): string {
  // Replace @[Name](id) with @Name for display
  return text.replace(/@\[([^\]]+)\]\([^)]+\)/g, '@$1');
}

/**
 * Check if a message mentions a specific user
 */
export function isUserMentioned(text: string, userId: string): boolean {
  const mentionIds = extractMentionIds(text);
  return mentionIds.includes(userId);
}
