export const runtime = 'nodejs'

// app/api/dashboard/recent/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/dashboard/recent', undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { searchParams } = new URL(req.url);
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }

    const { id, role, salesOffice } = auth.session.user as any;
    const userId = id as string;

    const { getRecentProjects } = await import('@/lib/quickbase/queries');
    const recent = await getRecentProjects(userId, role, salesOffice);

    logApiResponse('GET', '/api/dashboard/recent', Date.now() - startedAt, { count: Array.isArray(recent) ? recent.length : 0 }, reqId);
    return NextResponse.json(recent, { status: 200 });
  } catch (error) {
    logError('Failed to fetch recent projects', error as Error, {});
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}


