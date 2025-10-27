import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getPCCalendarEvents } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

// In-memory cache for calendar events
const cache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 300000; // 5 minutes

interface CalendarQueryParams {
  pcEmail: string;
  startDate: string;
  endDate: string;
}

export async function GET(request: NextRequest) {
  const reqId = `calendar-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest(reqId, 'GET', '/api/operations/calendar', request);

    // Require authentication
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    // Check user role for operations access
    const allowedRoles = [
      'operations_coordinator',
      'operations_manager', 
      'office_leader',
      'regional',
      'super_admin'
    ];
    
    if (!allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions for calendar access' },
        { status: 403 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const pcEmail = searchParams.get('pcEmail') || session.user.email;
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');

    if (!startDate || !endDate) {
      return NextResponse.json(
        { error: 'startDate and endDate parameters are required' },
        { status: 400 }
      );
    }

    // Validate date range (max 90 days)
    const start = new Date(startDate);
    const end = new Date(endDate);
    const daysDiff = Math.ceil((end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24));
    
    if (daysDiff > 90) {
      return NextResponse.json(
        { error: 'Date range cannot exceed 90 days' },
        { status: 400 }
      );
    }

    if (start >= end) {
      return NextResponse.json(
        { error: 'startDate must be before endDate' },
        { status: 400 }
      );
    }

    // Check cache first
    const cacheKey = `pc-calendar:${pcEmail}:${startDate}:${endDate}`;
    const cached = cache.get(cacheKey);
    
    if (cached && (Date.now() - cached.timestamp) < CACHE_TTL) {
      logApiResponse(reqId, 200, { 
        events: cached.data.events,
        count: cached.data.events.length,
        cached: true 
      });
      
      return NextResponse.json({
        events: cached.data.events,
        count: cached.data.events.length,
        dateRange: { start: startDate, end: endDate },
        cached: true
      });
    }

    // Fetch calendar events from QuickBase
    const events = await getPCCalendarEvents(
      pcEmail,
      session.user.name || '',
      startDate,
      endDate,
      reqId
    );

    // Cache the results
    cache.set(cacheKey, {
      data: { events },
      timestamp: Date.now()
    });

    // Clean up old cache entries (keep only last 100 entries)
    if (cache.size > 100) {
      const entries = Array.from(cache.entries());
      entries.sort((a, b) => b[1].timestamp - a[1].timestamp);
      const toDelete = entries.slice(100);
      toDelete.forEach(([key]) => cache.delete(key));
    }

    const response = {
      events,
      count: events.length,
      dateRange: { start: startDate, end: endDate },
      cached: false
    };

    logApiResponse(reqId, 200, response);
    return NextResponse.json(response);

  } catch (error) {
    logError(reqId, 'Calendar API Error', error);
    
    if (error instanceof Error) {
      return NextResponse.json(
      { 
        error: 'Failed to fetch calendar events',
        details: error.message 
      },
      { status: 500 }
    );
    }

    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

