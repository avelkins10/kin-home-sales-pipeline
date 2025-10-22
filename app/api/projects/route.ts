export const runtime = 'nodejs'

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
  logApiRequest('GET', '/api/projects', undefined, reqId);
  
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
    const view = searchParams.get('view') || undefined;
    const search = searchParams.get('search')?.trim().slice(0, 100) || undefined; // Clamp to 100 chars for performance and QuickBase query limits
    const sort = searchParams.get('sort') || undefined;
    const memberEmail = searchParams.get('memberEmail') || undefined;
    const ownership = searchParams.get('ownership') || 'all';
    const office = searchParams.get('office') || undefined;
    const setter = searchParams.get('setter') || undefined;
    const closer = searchParams.get('closer') || undefined;
    const withTasks = searchParams.get('withTasks') === 'true';

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
    const cacheKey = `${userId}:${role}:${view || 'all'}:${search || ''}:${sort || 'default'}:${memberEmail || ''}:${ownership}:${office || ''}:${setter || ''}:${closer || ''}:${withTasks}`;
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

    const { getProjectsForUserList } = await import('@/lib/quickbase/queries');
    // Note: We do NOT pass salesOffice from session to ensure fresh data.
    // For office-based roles, getProjectsForUserList() will fetch offices
    // from the office_assignments table, ensuring immediate visibility
    // of newly assigned offices without requiring logout/login.
    logInfo('[OFFICE_RESOLUTION] Fetching offices from database for user', { userId, role, reqId });
    const projects = await getProjectsForUserList(userId, role, view, search, sort, undefined, memberEmail, effectiveOwnership, office, setter, closer, reqId, withTasks);

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
    logError('Failed to fetch projects', error as Error, {});
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}


