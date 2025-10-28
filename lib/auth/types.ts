// lib/auth/types.ts
/**
 * TypeScript type definitions for authentication and authorization
 */

import { UserRole } from './guards';

/**
 * Extended user object in session
 * Includes all fields available in the session token
 */
export interface SessionUser {
  id: string;
  email: string;
  name?: string | null;
  role: UserRole;
  quickbaseUserId?: string | null;
  timezone?: string | null;
  image?: string | null;
}

/**
 * Typed session object
 * Extends NextAuth Session with our custom user type
 */
export interface TypedSession {
  user: SessionUser;
  expires: string;
}

