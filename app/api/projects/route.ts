export const runtime = 'nodejs'
export const dynamic = 'force-dynamic';
// Increase timeout to 60 seconds for complex project queries with QuickBase API calls
export const maxDuration = 60

// app/api/projects/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { getCachedProjects, setCachedProjects, getCacheTTL, cacheStats } from '@/lib/cache/projectsCache';

// Request counter for periodic logging
let requestCount = 0;

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  const TIMEOUT_WARNING_MS = 50000; // Warn if we're approaching 60s timeout
  const TIMEOUT_ERROR_MS = 58000; // Error out at 58s to avoid Vercel 504

  logApiRequest('GET', '/api/projects', undefined, reqId);
  
  // Increment request counter
  requestCount++;

  // Helper to check if we're running out of time
  const checkTimeout = () => {
    const elapsed = Date.now() - startedAt;
    if (elapsed > TIMEOUT_ERROR_MS) {
      throw new Error(`Request timeout: operation took ${elapsed}ms, exceeding ${TIMEOUT_ERROR_MS}ms limit`);
    }
    if (elapsed > TIMEOUT_WARNING_MS) {
      logInfo('[PROJECTS_API] Approaching timeout', { elapsed, reqId });
    }
    return elapsed;
  };

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
    const view = searchParams.get('view') || undefined;
    const search = searchParams.get('search')?.trim().slice(0, 100) || undefined; // Clamp to 100 chars for performance and QuickBase query limits
    const sort = searchParams.get('sort') || undefined;
    const memberEmail = searchParams.get('memberEmail') || undefined;
    const ownership = searchParams.get('ownership') || 'all';
    const office = searchParams.get('office') || undefined; // Can be comma-separated for multiple offices
    const state = searchParams.get('state') || undefined;
    const setter = searchParams.get('setter') || undefined;
    const closer = searchParams.get('closer') || undefined;
    const withTasks = searchParams.get('withTasks') === 'true';
    const dateFilter = searchParams.get('dateFilter') || undefined;
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;

    // Validate view parameter against allowed values
    const allowedViews = ['all', 'active', 'pending-kca', 'rejected', 'on-hold', 'install-ready', 'install-scheduled', 'install-completed', 'pending-cancel', 'cancelled', 'needs-attention']
    if (view && !allowedViews.includes(view)) {
      return NextResponse.json({ error: 'Invalid view parameter' }, { status: 400 });
    }

    // Validate sort parameter
    const allowedSorts = ['default', 'newest', 'oldest', 'age-desc', 'customer-asc', 'customer-desc']
    if (sort && !allowedSorts.includes(sort)) {
      return NextResponse.json({ error: 'Invalid sort parameter' }, { status: 400 });
    }

    // Validate date filter parameter
    const allowedDateFilters = ['this-week', 'last-week', 'this-month', 'last-month', 'custom']
    if (dateFilter && !allowedDateFilters.includes(dateFilter)) {
      return NextResponse.json({ error: 'Invalid dateFilter parameter' }, { status: 400 });
    }

    // Validate custom date range
    if (dateFilter === 'custom') {
      if (!startDate || !endDate) {
        return NextResponse.json({ error: 'startDate and endDate are required for custom date filter' }, { status: 400 });
      }
      // Validate date format (YYYY-MM-DD)
      const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
      if (!dateRegex.test(startDate) || !dateRegex.test(endDate)) {
        return NextResponse.json({ error: 'Invalid date format. Use YYYY-MM-DD' }, { status: 400 });
      }
    }

    const { id, role } = auth.session.user as any;
    const userId = id as string;

    // Validate ownership parameter
    const validOwnershipValues = ['all', 'my-projects', 'team-projects'];
    if (!validOwnershipValues.includes(ownership)) {
      return NextResponse.json({ error: 'Invalid ownership parameter' }, { status: 400 });
    }

    // Validate team-projects access for non-managers
    if (ownership === 'team-projects' && !['office_leader', 'regional', 'area_director', 'divisional', 'team_lead', 'super_admin'].includes(role)) {
      logInfo('[OWNERSHIP_FILTER] Non-manager attempted to access team-projects filter', { userId, role, ownership, reqId });
      return NextResponse.json({ error: 'Team projects filter is only available for managers' }, { status: 403 });
    }

    // For super_admin with withTasks, override ownership to 'all' to see all teams' tasks
    let effectiveOwnership = ownership;
    if (role === 'super_admin' && withTasks) {
      effectiveOwnership = 'all';
      logInfo('[OWNERSHIP_FILTER] Super admin with tasks filter - showing all teams tasks', { userId, role, withTasks, reqId });
    }

    // Check cache first
    const cacheKey = `${userId}:${role}:${view || 'all'}:${search || ''}:${sort || 'default'}:${memberEmail || ''}:${ownership}:${office || ''}:${state || ''}:${setter || ''}:${closer || ''}:${withTasks}:${dateFilter || ''}:${startDate || ''}:${endDate || ''}`;
    const cached = getCachedProjects(cacheKey);
    if (cached) {
      const duration = Date.now() - startedAt;
      logApiResponse('GET', '/api/projects', duration, {
        cached: true,
        count: Array.isArray(cached.data) ? cached.data.length : 0,
        cacheStats: { ...cacheStats }
      }, reqId);
      
      // Periodic logging every 100 requests
      if (requestCount % 100 === 0) {
        logApiResponse('GET', '/api/projects', duration, { cacheStats }, reqId);
      }
      
      return NextResponse.json(cached.data, { status: 200 });
    }

    checkTimeout(); // Check before expensive query
    const { getProjectsForUserList } = await import('@/lib/quickbase/queries');
    // Note: We do NOT pass salesOffice from session to ensure fresh data.
    // For office-based roles, getProjectsForUserList() will fetch offices
    // from the office_assignments table, ensuring immediate visibility
    // of newly assigned offices without requiring logout/login.
    logInfo('[OFFICE_RESOLUTION] Fetching offices from database for user', { userId, role, reqId });
    const projects = await getProjectsForUserList(userId, role, view, search, sort, undefined, memberEmail, effectiveOwnership, office, state, setter, closer, reqId, withTasks, dateFilter, startDate, endDate);
    checkTimeout(); // Check after query completes

    // Cache the result
    setCachedProjects(cacheKey, projects);

    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/projects', duration, { 
      cached: false, 
      count: Array.isArray(projects) ? projects.length : 0,
      withTasks,
      cacheStats: { ...cacheStats }
    }, reqId);
    
    // Periodic logging every 100 requests
    if (requestCount % 100 === 0) {
      logApiResponse('GET', '/api/projects', duration, { cacheStats }, reqId);
    }
    
    return NextResponse.json(projects, { status: 200 });
  } catch (error) {
    const elapsed = Date.now() - startedAt;
    const errorMessage = (error as Error).message || 'Unknown error';
    const isTimeout = errorMessage.includes('timeout') || elapsed > TIMEOUT_ERROR_MS;

    // CRITICAL: Console.error to ensure it shows in Vercel logs
    console.error('[PROJECTS_API] CRITICAL ERROR:', {
      message: errorMessage,
      name: (error as Error).name,
      stack: (error as Error).stack,
      elapsed,
      isTimeout,
      reqId
    });

    logError('Failed to fetch projects', error as Error, { elapsed, isTimeout, reqId });

    // Return 504 Gateway Timeout for timeout errors
    if (isTimeout) {
      return NextResponse.json({
        error: 'Gateway Timeout',
        message: 'The request took too long to complete. This can happen when there are many projects to process.',
        details: `Operation took ${elapsed}ms, exceeding the ${TIMEOUT_ERROR_MS}ms limit.`,
        suggestion: 'Try filtering by a specific view, office, or date range, or contact support if this persists.',
        elapsed,
        reqId
      }, { status: 504 });
    }

    return NextResponse.json({ 
      error: 'Internal Server Error',
      details: errorMessage,
      elapsed,
      reqId
    }, { status: 500 });
  }
}


