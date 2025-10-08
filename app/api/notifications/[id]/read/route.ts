export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { markNotificationAsRead } from '@/lib/db/notifications';

/**
 * POST /api/notifications/[id]/read
 * Mark a single notification as read
 */
export async function POST(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', `/api/notifications/${params.id}/read`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const { user } = auth.session as any;
    const notificationId = parseInt(params.id);

    // Validate notification ID
    if (isNaN(notificationId) || notificationId < 1) {
      return NextResponse.json({ error: 'Invalid notification ID' }, { status: 400 });
    }

    // Mark as read
    const notification = await markNotificationAsRead(
      notificationId,
      user.email || user.id
    );

    if (!notification) {
      return NextResponse.json(
        { error: 'Notification not found or unauthorized' },
        { status: 404 }
      );
    }

    const duration = Date.now() - startedAt;
    logApiResponse('POST', `/api/notifications/${params.id}/read`, duration, {
      notificationId,
      success: true,
    }, reqId);

    return NextResponse.json({ success: true, notification }, { status: 200 });
  } catch (error) {
    logError('Failed to mark notification as read', error as Error, {
      notificationId: params.id,
    });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
