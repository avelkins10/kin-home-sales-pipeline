export const runtime = 'nodejs'

// app/api/dashboard/notifications-summary/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getNotificationsForUser, getUnreadCounts } from '@/lib/db/notifications';

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/dashboard/notifications-summary', undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { quickbaseUserId } = auth.session.user as any;
    const userId = quickbaseUserId as string;

    // Fetch top 5 unread notifications for dashboard display
    const notifications = await getNotificationsForUser(userId, {
      limit: 5,
      unreadOnly: true,
      offset: 0,
    });

    // Get unread counts for summary
    const unreadCounts = await getUnreadCounts(userId);
    const totalUnread = unreadCounts.by_priority.critical + unreadCounts.by_priority.normal + unreadCounts.by_priority.info;
    const criticalCount = unreadCounts.by_priority.critical;
    const hasMore = totalUnread > 5;

    const response = {
      notifications,
      totalUnread,
      criticalCount,
      hasMore,
    };

    logApiResponse('GET', '/api/dashboard/notifications-summary', Date.now() - startedAt, { 
      notificationCount: notifications.length,
      totalUnread,
      criticalCount 
    }, reqId);

    return NextResponse.json(response, { status: 200 });
  } catch (error) {
    console.error('[/api/dashboard/notifications-summary] ERROR:', error);
    logError('Failed to fetch notifications summary', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
    }, { status: 500 });
  }
}
