export const runtime = 'nodejs'

// app/api/dashboard/team-activity/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { isManagerRole } from '@/lib/utils/role-helpers';

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/dashboard/team-activity', undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { searchParams } = new URL(req.url);
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }

    // Parse pagination parameters
    const limit = Math.min(parseInt(searchParams.get('limit') || '10'), 50); // Max 50 items per page
    const offset = Math.max(parseInt(searchParams.get('offset') || '0'), 0);

    const { id, role, salesOffice } = auth.session.user as any;
    const userId = id as string;

    // Check if user is a manager
    if (!isManagerRole(role)) {
      logApiResponse('GET', '/api/dashboard/team-activity', Date.now() - startedAt, { count: 0 }, reqId);
      return NextResponse.json({ activities: [], totalCount: 0, hasMore: false }, { status: 200 });
    }

    const { getTeamActivityFeed } = await import('@/lib/quickbase/queries');
    const result = await getTeamActivityFeed(userId, role, salesOffice, limit, offset);

    logApiResponse('GET', '/api/dashboard/team-activity', Date.now() - startedAt, { 
      count: result.activities.length,
      totalCount: result.totalCount,
      hasMore: result.hasMore,
      limit,
      offset
    }, reqId);
    
    return NextResponse.json(result, { status: 200 });
  } catch (error) {
    logError('Failed to fetch team activity feed', error as Error, {});
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
