/**
 * ⚠️ CENTRALIZED USER IDENTITY MODULE
 *
 * This module defines HOW users are identified throughout the application.
 *
 * IMPORTANT: If you change how users are identified (e.g., switching from
 * email to another field), update this file ONLY. All authorization logic
 * uses these functions, so changes here propagate everywhere.
 *
 * Current Strategy: EMAIL-BASED identification
 * - Users are identified by their email address
 * - QuickBase User ID may be null for some users (admin/ops)
 * - Email is the single source of truth for user identity
 */

import type { Session } from 'next-auth';

/**
 * Get the primary identifier for a user
 * @param session - User session
 * @returns User's primary identifier (email)
 */
export function getUserIdentifier(session: Session): string | null {
  return session.user.email || null;
}

/**
 * Get user's email (normalized to lowercase for comparison)
 * @param session - User session
 * @returns Lowercase email or null
 */
export function getUserEmail(session: Session): string | null {
  const email = session.user.email;
  return email ? email.toLowerCase() : null;
}

/**
 * Get user's role
 * @param session - User session
 * @returns User role
 */
export function getUserRole(session: Session): string {
  return session.user.role as string;
}

/**
 * Get user's database ID
 * @param session - User session
 * @returns User's database UUID
 */
export function getUserDatabaseId(session: Session): string {
  return session.user.id as string;
}

/**
 * Get user's QuickBase User ID (may be null)
 * @param session - User session
 * @returns QuickBase User ID or null
 */
export function getUserQuickbaseId(session: Session): string | null {
  return session.user.quickbaseUserId || null;
}

/**
 * Check if user is an admin role
 * @param session - User session
 * @returns True if user has admin privileges
 */
export function isAdmin(session: Session): boolean {
  const role = getUserRole(session);
  return role === 'super_admin' || role === 'regional';
}

/**
 * Check if user is an office leader
 * @param session - User session
 * @returns True if user is office leader
 */
export function isOfficeLeader(session: Session): boolean {
  return getUserRole(session) === 'office_leader';
}

/**
 * Check if user is a rep (closer/setter)
 * @param session - User session
 * @returns True if user is a rep
 */
export function isRep(session: Session): boolean {
  const role = getUserRole(session);
  return role === 'closer' || role === 'setter';
}
