export const runtime = 'nodejs'
export const dynamic = 'force-dynamic';

// app/api/calendar/events/route.ts
import { NextResponse } from 'next/server';
import { requireAuth, extractUserFromSession } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { getCachedProjects, setCachedProjects, getCacheTTL, getCacheStats } from '@/lib/cache/projectsCache';
import { getCalendarDates } from '@/lib/utils/calendar-helpers';

// Request counter for periodic logging
let requestCount = 0;

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/calendar/events', undefined, reqId);
  
  // Increment request counter
  requestCount++;

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const { searchParams } = new URL(req.url);
    
    // Defense-in-depth: reject if client attempts to set userId/role
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }

    // Extract and validate query parameters
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');
    const ownership = searchParams.get('ownership') || 'all';
    const sort = searchParams.get('sort') || 'newest';

    // Validate ownership parameter - align with backend expectations
    const validOwnershipValues = ['all', 'my-projects', 'team-projects'];
    if (!validOwnershipValues.includes(ownership)) {
      return NextResponse.json({ error: 'Invalid ownership parameter. Must be one of: all, my-projects, team-projects' }, { status: 400 });
    }

    // Validate sort parameter - align with /api/projects/route.ts
    const validSortValues = ['default', 'newest', 'oldest', 'age-desc', 'customer-asc', 'customer-desc'];
    if (!validSortValues.includes(sort)) {
      return NextResponse.json({ error: 'Invalid sort parameter. Must be one of: default, newest, oldest, age-desc, customer-asc, customer-desc' }, { status: 400 });
    }

    // Extract user from session using proper function
    const { id: userId, role } = extractUserFromSession(auth.session);

    // Validate team-projects access for non-managers
    if (ownership === 'team-projects' && !['office_leader', 'regional', 'area_director', 'divisional', 'team_lead'].includes(role)) {
      logInfo('[OWNERSHIP_FILTER] Non-manager attempted to access team-projects filter', { userId, role, ownership, reqId });
      return NextResponse.json({ error: 'Team projects filter is only available for managers' }, { status: 403 });
    }

    // Check cache first
    const cacheKey = `${userId}:${role}:calendar:${startDate || ''}:${endDate || ''}:${ownership}:${sort}`;
    const cached = getCachedProjects(cacheKey);
    if (cached) {
      const duration = Date.now() - startedAt;
      logApiResponse('GET', '/api/calendar/events', duration, {
        cached: true,
        count: Array.isArray(cached.data) ? cached.data.length : 0,
        cacheStats: getCacheStats()
      }, reqId);
      
      // Periodic logging every 100 requests
      if (requestCount % 100 === 0) {
        logApiResponse('GET', '/api/calendar/events', duration, { cacheStats: getCacheStats() }, reqId);
      }
      
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Fetch projects using the same logic as /api/projects
    const { getProjectsForUserList } = await import('@/lib/quickbase/queries');
    logInfo('[CALENDAR_EVENTS] Fetching projects for calendar events', { userId, role, ownership, reqId });
    
    const projects = await getProjectsForUserList(
      userId, 
      role, 
      undefined, // view
      undefined, // search
      sort, // sort
      undefined, // salesOffice
      undefined, // memberEmail
      ownership, // ownership
      undefined, // office
      undefined, // setter
      undefined, // closer
      reqId
    );

    // Convert projects to calendar events
    const allEvents = [];
    for (const project of projects) {
      const events = getCalendarDates(project);
      
      // Filter by date range if provided
      if (startDate && endDate) {
        const filteredEvents = events.filter(event => {
          const eventDate = new Date(event.date);
          const start = new Date(startDate);
          const end = new Date(endDate);
          return eventDate >= start && eventDate <= end;
        });
        allEvents.push(...filteredEvents);
      } else {
        allEvents.push(...events);
      }
    }

    // Sort events by date
    allEvents.sort((a, b) => new Date(a.date).getTime() - new Date(b.date).getTime());

    // Cache the result
    setCachedProjects(cacheKey, allEvents);

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/calendar/events', duration, { 
      cached: false, 
      count: allEvents.length,
      cacheStats: getCacheStats()
    }, reqId);
    
    // Periodic logging every 100 requests
    if (requestCount % 100 === 0) {
      logApiResponse('GET', '/api/calendar/events', duration, { cacheStats: getCacheStats() }, reqId);
    }
    
    return NextResponse.json(allEvents, { status: 200 });
  } catch (error) {
    logError('Failed to fetch calendar events', error as Error, {});
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
