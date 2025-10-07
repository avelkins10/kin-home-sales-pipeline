// lib/auth/withAuth.ts

import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from './next-auth.config';
import { logWarn } from '@/lib/logging/logger';
import { rateLimit } from './rateLimit';
import { UserRole } from './guards';
import type { Session } from 'next-auth';

type AuthenticatedHandler = (
  request: NextRequest,
  params: any,
  session: Session
) => Promise<NextResponse>;

type RouteParams = Record<string, string>;

/**
 * Higher-order function that wraps API route handlers with authentication
 * @param handler - The API route handler function
 * @returns Wrapped handler with authentication check
 */
export function withAuth(handler: AuthenticatedHandler) {
  return async function(
    request: NextRequest,
    { params }: { params: RouteParams }
  ): Promise<NextResponse> {
    try {
      const ipHeader = request.headers.get('x-forwarded-for') || request.headers.get('x-real-ip') || ''
      const ipAddress = ipHeader.split(',')[0]?.trim() || 'unknown'

      if (request.nextUrl.pathname.startsWith('/api/auth') && request.method === 'POST') {
        let email: string | undefined
        try {
          const cloned = request.clone()
          const body = await cloned.json().catch(() => undefined)
          email = typeof (body as any)?.email === 'string' ? (body as any).email.toLowerCase() : undefined
        } catch {}

        const allowedIp = rateLimit(['login', ipAddress], 10, 15 * 60_000)
        const allowedEmail = email ? rateLimit(['login', email], 5, 15 * 60_000) : true
        if (!allowedIp || !allowedEmail) {
          logWarn('Rate limit exceeded for login attempt', {
            path: request.nextUrl.pathname,
            method: request.method,
            ipAddress,
            email: email ? '[redacted]' : undefined,
          })
          return NextResponse.json({ error: 'Too Many Requests' }, { status: 429 })
        }
      }
      // Check authentication
      const session = await getServerSession(authOptions);
      
      if (!session) {
        logWarn('Unauthorized access attempt', { 
          path: request.nextUrl.pathname,
          method: request.method 
        });
        return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
      }

      // Call the original handler with session
      return await handler(request, params, session);
      
    } catch (error) {
      logWarn('Error in withAuth wrapper', { 
        error: error instanceof Error ? error.message : 'Unknown error',
        path: request.nextUrl.pathname 
      });
      return NextResponse.json(
        { error: 'Internal server error' },
        { status: 500 }
      );
    }
  };
}

/**
 * Higher-order function that wraps API route handlers with role-based authorization
 * @param allowedRoles - Array of roles that are permitted
 * @param handler - The API route handler function
 * @returns Wrapped handler with role-based authorization
 */
export function withRole(allowedRoles: UserRole[], handler: AuthenticatedHandler) {
  return withAuth(async (request, params, session) => {
    const userRole = session.user.role as UserRole;
    
    if (!allowedRoles.includes(userRole)) {
      logWarn('Insufficient permissions', { 
        userId: session.user.id,
        userRole,
        allowedRoles,
        path: request.nextUrl.pathname 
      });
      return NextResponse.json(
        { error: 'Forbidden - insufficient permissions' },
        { status: 403 }
      );
    }

    // User has required role, call the handler
    return await handler(request, params, session);
  });
}

/**
 * Usage examples:
 * 
 * // Simple authentication check
 * export const POST = withAuth(async (request, params, session) => {
 *   // Handler code here - session is guaranteed to exist
 *   return NextResponse.json({ success: true })
 * })
 * 
 * // Role-based authorization
 * export const POST = withRole(['super_admin'], async (request, params, session) => {
 *   // Handler code here - user is guaranteed to be super_admin
 *   return NextResponse.json({ success: true })
 * })
 * 
 * // Multiple roles allowed
 * export const POST = withRole(['closer', 'setter'], async (request, params, session) => {
 *   // Handler code here - user is guaranteed to be closer or setter
 *   return NextResponse.json({ success: true })
 * })
 */
