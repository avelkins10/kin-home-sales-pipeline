export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getUnreadCounts } from '@/lib/db/notifications';

/**
 * GET /api/notifications/unread-counts
 * Get unread notification counts for the current user
 *
 * Returns:
 *   - total: total unread count
 *   - by_priority: breakdown by priority level
 *   - by_project: breakdown by project ID
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/notifications/unread-counts', undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const { user } = auth.session as any;

    // Fetch unread counts
    const counts = await getUnreadCounts(user.email || user.id);

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/notifications/unread-counts', duration, {
      total: counts.total,
      criticalCount: counts.by_priority.critical,
    }, reqId);

    return NextResponse.json(counts, { status: 200 });
  } catch (error) {
    logError('Failed to fetch unread counts', error as Error);
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
