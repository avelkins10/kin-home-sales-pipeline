export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import {
  getNotificationsForUser,
  createNotification,
} from '@/lib/db/notifications';
import type { CreateNotificationInput } from '@/lib/types/notification';

/**
 * GET /api/notifications
 * Fetch notifications for the current user
 *
 * Query params:
 *   - limit: number (default: 50)
 *   - offset: number (default: 0)
 *   - unreadOnly: boolean (default: false)
 *   - projectId: number (optional)
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/notifications', undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const { user } = auth.session as any;
    const { searchParams } = new URL(req.url);

    // Parse query parameters
    const limit = parseInt(searchParams.get('limit') || '50');
    const offset = parseInt(searchParams.get('offset') || '0');
    const unreadOnly = searchParams.get('unreadOnly') === 'true';
    const projectIdParam = searchParams.get('projectId');
    const projectId = projectIdParam ? parseInt(projectIdParam) : undefined;

    // Validate parameters
    if (limit < 1 || limit > 100) {
      return NextResponse.json({ error: 'Limit must be between 1 and 100' }, { status: 400 });
    }

    if (offset < 0) {
      return NextResponse.json({ error: 'Offset must be non-negative' }, { status: 400 });
    }

    // Fetch notifications
    const notifications = await getNotificationsForUser(user.email || user.id, {
      limit,
      offset,
      unreadOnly,
      projectId,
    });

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/notifications', duration, {
      count: notifications.length,
      unreadOnly,
      projectId,
    }, reqId);

    return NextResponse.json({
      notifications,
      count: notifications.length,
      has_more: notifications.length === limit,
    }, { status: 200 });
  } catch (error) {
    logError('Failed to fetch notifications', error as Error);
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}

/**
 * POST /api/notifications
 * Create a new notification (for internal messages and system alerts)
 *
 * Body should match CreateNotificationInput type
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', '/api/notifications', undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const { user } = auth.session as any;
    const body = await req.json();

    // Validate required fields
    const requiredFields = ['user_id', 'project_id', 'type', 'priority', 'source', 'title'];
    for (const field of requiredFields) {
      if (!body[field]) {
        return NextResponse.json(
          { error: `Missing required field: ${field}` },
          { status: 400 }
        );
      }
    }

    // Validate type
    const validTypes = ['quickbase_note', 'internal_message', 'system_alert'];
    if (!validTypes.includes(body.type)) {
      return NextResponse.json(
        { error: 'Invalid notification type' },
        { status: 400 }
      );
    }

    // Validate priority
    const validPriorities = ['critical', 'normal', 'info'];
    if (!validPriorities.includes(body.priority)) {
      return NextResponse.json(
        { error: 'Invalid priority level' },
        { status: 400 }
      );
    }

    // Validate source
    const validSources = ['quickbase', 'internal', 'system'];
    if (!validSources.includes(body.source)) {
      return NextResponse.json(
        { error: 'Invalid notification source' },
        { status: 400 }
      );
    }

    // Add sender information for internal messages
    const notificationInput: CreateNotificationInput = {
      ...body,
      sender_id: body.sender_id || user.email || user.id,
      sender_name: body.sender_name || user.name,
      sender_role: body.sender_role || user.role,
    };

    // Create notification
    const notification = await createNotification(notificationInput);

    const duration = Date.now() - startedAt;
    logApiResponse('POST', '/api/notifications', duration, {
      notificationId: notification.id,
      type: notification.type,
    }, reqId);

    return NextResponse.json({ success: true, notification }, { status: 201 });
  } catch (error) {
    logError('Failed to create notification', error as Error);
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
