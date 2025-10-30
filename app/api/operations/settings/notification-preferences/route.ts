// app/api/operations/settings/notification-preferences/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getPCNotificationPreferences, updatePCNotificationPreferences } from '@/lib/db/pcNotificationPreferences';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

/**
 * GET /api/operations/settings/notification-preferences
 * Get user's notification preferences
 */
export async function GET(request: NextRequest) {
  const requestId = crypto.randomUUID();
  
  try {
    logApiRequest('GET', '/api/operations/settings/notification-preferences', requestId);

    // Require authentication
    const { session } = await requireAuth();
    const pcEmail = session?.user?.email;

    if (!pcEmail) {
      return NextResponse.json(
        { error: 'User email not found in session' },
        { status: 401 }
      );
    }

    // Verify operations role
    const userRole = session?.user?.role;
    const operationsRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!userRole || !operationsRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden - operations role required' },
        { status: 403 }
      );
    }

    // Get preferences
    const preferences = await getPCNotificationPreferences(pcEmail);

    logApiResponse(requestId, 200, { preferences });

    return NextResponse.json({ preferences });

  } catch (error) {
    logError('Failed to get notification preferences', error as Error, { requestId });
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/operations/settings/notification-preferences
 * Update user's notification preferences
 */
export async function POST(request: NextRequest) {
  const requestId = crypto.randomUUID();
  
  try {
    logApiRequest('POST', '/api/operations/settings/notification-preferences', requestId);

    // Require authentication
    const { session } = await requireAuth();
    const pcEmail = session?.user?.email;

    if (!pcEmail) {
      return NextResponse.json(
        { error: 'User email not found in session' },
        { status: 401 }
      );
    }

    // Verify operations role
    const userRole = session?.user?.role;
    const operationsRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!userRole || !operationsRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden - operations role required' },
        { status: 403 }
      );
    }

    // Parse request body
    const body = await request.json();

    // Validate preferences
    const { email_enabled, email_frequency, notification_types, quiet_hours_start, quiet_hours_end } = body;

    // Validate email_frequency
    if (email_frequency && !['immediate', 'daily', 'weekly'].includes(email_frequency)) {
      return NextResponse.json(
        { error: 'Invalid email_frequency. Must be immediate, daily, or weekly.' },
        { status: 400 }
      );
    }

    // Validate notification_types
    if (notification_types && !Array.isArray(notification_types)) {
      return NextResponse.json(
        { error: 'Invalid notification_types. Must be an array.' },
        { status: 400 }
      );
    }

    // Validate quiet_hours format (HH:MM:SS)
    const timeRegex = /^([01]\d|2[0-3]):([0-5]\d):([0-5]\d)$/;
    if (quiet_hours_start && !timeRegex.test(quiet_hours_start)) {
      return NextResponse.json(
        { error: 'Invalid quiet_hours_start format. Must be HH:MM:SS.' },
        { status: 400 }
      );
    }
    if (quiet_hours_end && !timeRegex.test(quiet_hours_end)) {
      return NextResponse.json(
        { error: 'Invalid quiet_hours_end format. Must be HH:MM:SS.' },
        { status: 400 }
      );
    }

    // Update preferences
    const updatedPreferences = await updatePCNotificationPreferences(pcEmail, body);

    logApiResponse(requestId, 200, { success: true, preferences: updatedPreferences });

    return NextResponse.json({ success: true, preferences: updatedPreferences });

  } catch (error) {
    logError('Failed to update notification preferences', error as Error, { requestId });
    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}



