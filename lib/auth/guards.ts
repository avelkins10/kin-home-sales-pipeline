// lib/auth/guards.ts

import { getServerSession } from 'next-auth';
import { NextResponse } from 'next/server';
import { authOptions } from './next-auth.config';
import type { Session } from 'next-auth';

export type UserRole = 'closer' | 'setter' | 'office_leader' | 'regional' | 'super_admin';

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
 * @param session - Authenticated user session
 * @param projectOwnerId - ID of the project owner (closer/setter)
 * @returns boolean - Whether user has access to the project
 */
export function requireProjectAccess(session: Session, projectOwnerId: string): boolean {
  const userRole = session.user.role as UserRole;
  const userId = session.user.quickbaseUserId;
  
  // Super admin and regional have full access
  if (userRole === 'super_admin' || userRole === 'regional') {
    return true;
  }
  
  // Office leaders can see all projects in their office
  if (userRole === 'office_leader') {
    return true;
  }
  
  // Closers and setters can only access their own projects
  if (userRole === 'closer' || userRole === 'setter') {
    return userId === projectOwnerId;
  }
  
  return false;
}

/**
 * Ensures the current session has access to the given project id.
 * Loads the project owner (closer/setter) and checks by role per business rules.
 */
export async function requireProjectAccessById(projectId: number | string): Promise<AuthGuardResult> {
  const auth = await requireAuth();
  if (!auth.authorized) return auth;

  const session = auth.session;
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

    const ownerCloserId = project[301]?.value || project[516]?.value || '';
    const ownerSetterId = project[329]?.value || '';

    const allowed = requireProjectAccess(session, ownerCloserId || ownerSetterId);
    if (!allowed) {
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
