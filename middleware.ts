import { NextResponse } from 'next/server';
import type { NextRequest } from 'next/server';
import { getToken } from 'next-auth/jwt';
import { canAccessSalesApp, canAccessOperationsApp, canAccessBothApps, getDefaultAppForRole } from '@/lib/utils/app-helpers';

export async function middleware(request: NextRequest) {
  const token = await getToken({ req: request });
  const { pathname } = request.nextUrl;

  // Allow unauthenticated access to login, auth routes, and accept-invite
  if (pathname.startsWith('/login') || pathname.startsWith('/api/auth/') || pathname.startsWith('/accept-invite')) {
    return NextResponse.next();
  }

  // Allow static assets and Next.js internals
  if (
    pathname.startsWith('/_next/') ||
    pathname.startsWith('/favicon.ico') ||
    pathname.startsWith('/icon.svg') ||
    pathname.startsWith('/manifest.json') ||
    pathname.startsWith('/sw.js') ||
    pathname.startsWith('/robots.txt') ||
    pathname.startsWith('/sitemap.xml')
  ) {
    return NextResponse.next();
  }

  // Redirect unauthenticated users to login
  if (!token) {
    return NextResponse.redirect(new URL('/login', request.url));
  }

  const userRole = token.role as string;

  // Handle app-specific redirects
  if (pathname.startsWith('/operations')) {
    // User trying to access operations app
    if (!canAccessOperationsApp(userRole)) {
      // Redirect to their default app
      const defaultApp = getDefaultAppForRole(userRole);
      return NextResponse.redirect(new URL(defaultApp === 'operations' ? '/operations' : '/', request.url));
    }
  } else if (!pathname.startsWith('/operations')) {
    // User trying to access sales app (any path not starting with /operations)
    if (!canAccessSalesApp(userRole)) {
      // Redirect to their default app
      const defaultApp = getDefaultAppForRole(userRole);
      return NextResponse.redirect(new URL(defaultApp === 'operations' ? '/operations' : '/', request.url));
    }
  }

  return NextResponse.next();
}

export const config = {
  matcher: [
    /*
     * Match all request paths except for the ones starting with:
     * - api/auth (NextAuth.js routes)
     * - _next/static (static files)
     * - _next/image (image optimization files)
     * - favicon.ico (favicon file)
     * - public folder files and common static assets
     */
    '/((?!api/auth|_next/static|_next/image|favicon.ico|.*\\.(?:svg|png|jpg|jpeg|gif|webp|woff|woff2|ttf|eot|ico|json|css|js)$).*)',
  ],
};
