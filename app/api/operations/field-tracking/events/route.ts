// app/api/operations/field-tracking/events/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getArrivyEvents, getArrivyEventsCount } from '@/lib/db/arrivy';

/**
 * GET - Query field tracking events (activity feed) with comprehensive filtering
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
    const eventType = searchParams.get('eventType');
    const reporterName = searchParams.get('reporterName');
    const taskType = searchParams.get('taskType');
    const search = searchParams.get('search');
    const limit = Math.min(parseInt(searchParams.get('limit') || '20', 10), 100); // Max 100
    const offset = Math.max(parseInt(searchParams.get('offset') || '0', 10), 0);

    // Parse date range
    const startDateStr = searchParams.get('startDate');
    const endDateStr = searchParams.get('endDate');
    let startDate: Date | undefined;
    let endDate: Date | undefined;

    if (startDateStr) {
      startDate = new Date(startDateStr);
      if (isNaN(startDate.getTime())) {
        return NextResponse.json(
          { error: 'Invalid startDate format' },
          { status: 400 }
        );
      }
    }

    if (endDateStr) {
      endDate = new Date(endDateStr);
      if (isNaN(endDate.getTime())) {
        return NextResponse.json(
          { error: 'Invalid endDate format' },
          { status: 400 }
        );
      }
    }

    // Build filters object
    const filters = {
      taskId: taskId ? parseInt(taskId, 10) : undefined,
      eventType: eventType || undefined,
      startDate,
      endDate,
      reporterName: reporterName || undefined,
      taskType: taskType || undefined,
      search: search || undefined,
      limit,
      offset,
    };

    // Fetch events with filters
    const events = await getArrivyEvents(filters);

    // Count total events with same filters (without limit/offset)
    const countFilters = {
      taskId: filters.taskId,
      eventType: filters.eventType,
      startDate: filters.startDate,
      endDate: filters.endDate,
      reporterName: filters.reporterName,
      taskType: filters.taskType,
      search: filters.search,
    };
    
    const total = await getArrivyEventsCount(countFilters);

    // Map events to include timestamp field
    const mappedEvents = events.map(event => ({
      ...event,
      timestamp: event.event_time,
    }));

    // Calculate hasMore flag
    const hasMore = (offset + limit) < total;

    logApiResponse('GET', '/api/operations/field-tracking/events', Date.now() - startedAt, { 
      count: events.length,
      total,
      hasMore,
      offset,
      limit,
    }, reqId);

    return NextResponse.json(
      { 
        events: mappedEvents,
        total,
        hasMore,
        offset,
        limit,
      },
      { 
        status: 200,
        headers: {
          'Cache-Control': 'private, max-age=10', // 10-second cache
        },
      }
    );

  } catch (error) {
    logError('Failed to fetch field tracking events', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to load events' 
    }, { status: 500 });
  }
}
