// app/api/operations/field-tracking/events/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getArrivyEvents } from '@/lib/db/arrivy';

/**
 * GET - Query field tracking events (activity feed)
 */
export async function GET(req: NextRequest) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Operations role required.' 
      }, { status: 403 });
    }

    // Parse query parameters
    const searchParams = req.nextUrl.searchParams;
    const taskId = searchParams.get('task_id');
    const eventType = searchParams.get('event_type');
    const limit = parseInt(searchParams.get('limit') || '50', 10);
    const offset = parseInt(searchParams.get('offset') || '0', 10);

    // Parse date range
    const dateRange = searchParams.get('date_range');
    let startDate: Date | undefined;
    let endDate: Date | undefined;

    if (dateRange) {
      const now = new Date();
      const startOfDay = new Date(now.getFullYear(), now.getMonth(), now.getDate());
      
      switch (dateRange) {
        case 'today':
          startDate = startOfDay;
          endDate = new Date(startOfDay.getTime() + 24 * 60 * 60 * 1000);
          break;
        case 'this_week':
          const startOfWeek = new Date(startOfDay);
          startOfWeek.setDate(startOfDay.getDate() - startOfDay.getDay());
          startDate = startOfWeek;
          endDate = new Date(startOfWeek.getTime() + 7 * 24 * 60 * 60 * 1000);
          break;
        case 'this_month':
          startDate = new Date(now.getFullYear(), now.getMonth(), 1);
          endDate = new Date(now.getFullYear(), now.getMonth() + 1, 0);
          break;
      }
    }

    // Fetch events with filters
    const events = await getArrivyEvents({
      taskId: taskId ? parseInt(taskId, 10) : undefined,
      eventType: eventType || undefined,
      startDate,
      endDate,
      limit,
      offset,
    });

    // Map events to include timestamp field
    const mappedEvents = events.map(event => ({
      ...event,
      timestamp: event.event_time,
    }));

    logApiResponse('GET', '/api/operations/field-tracking/events', Date.now() - startedAt, { 
      count: events.length,
    }, reqId);

    return NextResponse.json({ events: mappedEvents }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch field tracking events', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to load events' 
    }, { status: 500 });
  }
}

