/**
 * API Route: GET /api/operations/notifications
 * Fetch PC-specific notifications for operations dashboard
 */

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getNotificationsForUser } from '@/lib/db/notifications';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  const startTime = Date.now();
  
  try {
    // Verify authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const { user } = auth.session as any;

    // Check user role for operations access
    const allowedRoles = [
      'operations_coordinator', 
      'operations_manager', 
      'office_leader', 
      'regional', 
      'super_admin'
    ];
    
    if (!allowedRoles.includes(user.role)) {
      return NextResponse.json({ error: 'Forbidden' }, { status: 403 });
    }

    // Extract PC email from session
    const pcEmail = user.email;
    if (!pcEmail) {
      return NextResponse.json({ error: 'User email not found' }, { status: 400 });
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const limit = parseInt(searchParams.get('limit') || '50');
    const offset = parseInt(searchParams.get('offset') || '0');
    const unreadOnly = searchParams.get('unreadOnly') === 'true';
    const type = searchParams.get('type') || null;
    const source = searchParams.get('source') || null;
    const search = searchParams.get('search') || null;

    logApiRequest('GET', '/api/operations/notifications', {
      pcEmail,
      limit,
      offset,
      unreadOnly,
      type,
      source,
      search
    });

    // Get all notifications for the PC
    const allNotifications = await getNotificationsForUser(pcEmail, {
      limit,
      offset,
      unreadOnly
    });

    // Filter to only include PC milestone and Arrivy field notification types
    const pcNotificationTypes = [
      'milestone_survey_late',
      'milestone_install_late', 
      'milestone_nem_overdue',
      'milestone_pto_overdue',
      'milestone_unresponsive_escalation',
      'arrivy_task_late',
      'arrivy_task_noshow',
      'arrivy_task_exception',
      'arrivy_task_cancelled'
    ];

    let filteredNotifications = allNotifications.filter(notification => 
      pcNotificationTypes.includes(notification.type)
    );

    // Apply type filter if provided
    if (type) {
      filteredNotifications = filteredNotifications.filter(notification => 
        notification.type === type
      );
    }

    // Apply source filter if provided
    if (source && source !== 'all') {
      filteredNotifications = filteredNotifications.filter(notification => 
        notification.source === source
      );
    }

    // Apply search filter if provided
    if (search) {
      const searchLower = search.toLowerCase();
      filteredNotifications = filteredNotifications.filter(notification => 
        notification.title.toLowerCase().includes(searchLower) ||
        (notification.message && notification.message.toLowerCase().includes(searchLower))
      );
    }

    // Calculate unread count for PC notifications
    const unreadCount = filteredNotifications.filter(n => !n.is_read).length;

    const response = {
      notifications: filteredNotifications,
      count: filteredNotifications.length,
      has_more: allNotifications.length === limit,
      unread_count: unreadCount
    };

    const executionTime = Date.now() - startTime;
    logApiResponse('GET', '/api/operations/notifications', 200, {
      notificationCount: filteredNotifications.length,
      unreadCount,
      executionTimeMs: executionTime
    });

    return NextResponse.json(response);

  } catch (error) {
    logError('Failed to fetch PC notifications', error, {
      url: request.url,
      method: 'GET'
    });

    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
