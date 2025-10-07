export const runtime = 'nodejs'

// app/api/projects/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/projects', undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const { searchParams } = new URL(req.url);
    // Defense-in-depth: reject if client attempts to set userId/role
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }
    const view = searchParams.get('view') || undefined;
    const search = searchParams.get('search')?.trim().slice(0, 100) || undefined; // Clamp to 100 chars and normalize whitespace

    // Validate view parameter against allowed values
    const allowedViews = ['all', 'active', 'on-hold', 'install-ready', 'install-scheduled', 'install-completed', 'pending-cancel', 'cancelled', 'needs-attention']
    if (view && !allowedViews.includes(view)) {
      return NextResponse.json({ error: 'Invalid view parameter' }, { status: 400 });
    }

    const { quickbaseUserId, role } = auth.session.user as any;
    const userId = quickbaseUserId as string;

    const { getProjectsForUser } = await import('@/lib/quickbase/queries');
    const projects = await getProjectsForUser(userId, role, view, search);

    logApiResponse('GET', '/api/projects', Date.now() - startedAt, { count: Array.isArray(projects) ? projects.length : 0 }, reqId);
    return NextResponse.json(projects, { status: 200 });
  } catch (error) {
    logError('Failed to fetch projects', error as Error, {});
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}


