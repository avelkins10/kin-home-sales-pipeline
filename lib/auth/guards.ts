// lib/auth/guards.ts
/**
 * ⚠️ AUTHORIZATION GUARDS
 *
 * This file provides guard functions for API routes.
 * All guards use the centralized authorization modules:
 * - userIdentity.ts: Defines how users are identified
 * - projectAuthorization.ts: Defines project access rules
 *
 * DO NOT duplicate authorization logic here. Use the centralized modules.
 */

import { getServerSession } from 'next-auth';
import { NextResponse } from 'next/server';
import { authOptions } from './next-auth.config';
import type { Session } from 'next-auth';
import {
  canUserAccessProject,
  hasUnrestrictedAccess,
} from './projectAuthorization';

export type UserRole =
  | 'closer'
  | 'setter'
  | 'coordinator'
  | 'team_lead'
  | 'office_leader'
  | 'area_director'
  | 'divisional'
  | 'regional'
  | 'super_admin';

export type AuthGuardResult = 
  | { authorized: true; session: Session }
  | { authorized: false; response: NextResponse };

/**
 * Validates that user is authenticated
 * @returns Promise<AuthGuardResult> - Either authorized session or 401 response
 */
export async function requireAuth(): Promise<AuthGuardResult> {
  const session = await getServerSession(authOptions);
  
  if (!session) {
    return {
      authorized: false,
      response: NextResponse.json({ error: 'Unauthorized' }, { status: 401 })
    };
  }
  
  return { authorized: true, session };
}

/**
 * Validates that user has one of the allowed roles
 * @param allowedRoles - Array of roles that are permitted
 * @returns Promise<AuthGuardResult> - Either authorized session or 401/403 response
 */
export async function requireRole(allowedRoles: UserRole[]): Promise<AuthGuardResult> {
  const auth = await requireAuth();
  
  if (!auth.authorized) {
    return auth; // Return 401 if not authenticated
  }
  
  const userRole = auth.session.user.role as UserRole;
  
  if (!allowedRoles.includes(userRole)) {
    return {
      authorized: false,
      response: NextResponse.json(
        { error: 'Forbidden - insufficient permissions' }, 
        { status: 403 }
      )
    };
  }
  
  return { authorized: true, session: auth.session };
}

/**
 * Validates that user can access a specific project
 * Uses centralized projectAuthorization module
 * @param session - Authenticated user session
 * @param project - Project data from QuickBase
 * @returns boolean - Whether user has access to the project
 */
export function requireProjectAccess(session: Session, project: Record<string, any>): boolean {
  return canUserAccessProject(session, project);
}

/**
 * Ensures the current session has access to the given project id.
 * Uses centralized projectAuthorization module.
 */
export async function requireProjectAccessById(projectId: number | string): Promise<AuthGuardResult> {
  const auth = await requireAuth();
  if (!auth.authorized) return auth;

  const session = auth.session;

  // Optimization: Admins and office leaders have unrestricted access
  if (hasUnrestrictedAccess(session)) {
    return { authorized: true, session };
  }

  // For reps, load the project and check access
  try {
    const { getProjectById } = await import('@/lib/quickbase/queries');
    const numericId = typeof projectId === 'string' ? parseInt(projectId, 10) : projectId;
    if (Number.isNaN(numericId)) {
      return {
        authorized: false,
        response: NextResponse.json({ error: 'Invalid project ID' }, { status: 400 })
      };
    }
    const project: any = await getProjectById(numericId as number);
    if (!project) {
      return {
        authorized: false,
        response: NextResponse.json({ error: 'Project not found' }, { status: 404 })
      };
    }

    // Use centralized authorization check
    const hasAccess = canUserAccessProject(session, project);

    if (!hasAccess) {
      return {
        authorized: false,
        response: NextResponse.json({ error: 'Forbidden' }, { status: 403 })
      };
    }
    return { authorized: true, session };
  } catch {
    return {
      authorized: false,
      response: NextResponse.json({ error: 'Forbidden' }, { status: 403 })
    };
  }
}

/**
 * Extract and return typed user object from session
 * @param session - Authenticated user session
 * @returns Typed user object with all properties
 */
export function extractUserFromSession(session: Session) {
  return {
    id: session.user.id,
    email: session.user.email,
    name: session.user.name,
    role: session.user.role as UserRole,
    quickbaseUserId: session.user.quickbaseUserId,
  };
}
