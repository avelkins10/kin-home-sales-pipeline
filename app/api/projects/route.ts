export const runtime = 'nodejs'

// app/api/projects/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

// Simple in-memory cache for projects with configurable TTL
const projectsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = parseInt(process.env.PROJECTS_CACHE_TTL_MS || '60000'); // Default 60 seconds
const MAX_CACHE_ENTRIES = parseInt(process.env.PROJECTS_CACHE_MAX || '100'); // Default 100 entries

// Cache statistics for monitoring
let cacheStats = {
  hits: 0,
  misses: 0,
  evictions: 0,
  expiredRemovals: 0,
  currentSize: 0
};

// Export cache stats for debug endpoint
export function getCacheStats() {
  return {
    ...cacheStats,
    currentSize: projectsCache.size,
    ttl: CACHE_TTL,
    maxEntries: MAX_CACHE_ENTRIES
  };
}

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/projects', undefined, reqId);

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

    // Validate view parameter against allowed values
    const allowedViews = ['all', 'active', 'on-hold', 'install-ready', 'install-scheduled', 'install-completed', 'pending-cancel', 'cancelled', 'needs-attention']
    if (view && !allowedViews.includes(view)) {
      return NextResponse.json({ error: 'Invalid view parameter' }, { status: 400 });
    }

    // Validate sort parameter
    const allowedSorts = ['default', 'newest', 'oldest', 'age-desc', 'customer-asc', 'customer-desc']
    if (sort && !allowedSorts.includes(sort)) {
      return NextResponse.json({ error: 'Invalid sort parameter' }, { status: 400 });
    }

    const { quickbaseUserId, role, salesOffice } = auth.session.user as any;
    const userId = quickbaseUserId as string;

    // Check cache first
    const officeKey = salesOffice ? salesOffice.sort().join(',') : '';
    const cacheKey = `${userId}:${role}:${officeKey}:${view || 'all'}:${search || ''}:${sort || 'default'}`;
    const cached = projectsCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      // Refresh timestamp to approximate LRU behavior
      projectsCache.set(cacheKey, { data: cached.data, timestamp: Date.now() });
      cacheStats.hits++;
      cacheStats.currentSize = projectsCache.size;
      logApiResponse('GET', '/api/projects', Date.now() - startedAt, { 
        cached: true, 
        count: Array.isArray(cached.data) ? cached.data.length : 0,
        cacheStats: { ...cacheStats }
      }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }
    
    cacheStats.misses++;

    const { getProjectsForUserList } = await import('@/lib/quickbase/queries');
    const projects = await getProjectsForUserList(userId, role, view, search, sort, salesOffice);

    // Cache the result
    projectsCache.set(cacheKey, { data: projects, timestamp: Date.now() });

    // Enforce strict 100-entry cap after every cache set
    const now = Date.now();
    const entries = Array.from(projectsCache.entries());
    
    // First, remove expired entries
    let expiredCount = 0;
    for (const [key, value] of entries) {
      if (now - value.timestamp > CACHE_TTL) {
        projectsCache.delete(key);
        expiredCount++;
      }
    }
    cacheStats.expiredRemovals += expiredCount;
    
    // If still over MAX_CACHE_ENTRIES, evict oldest entries by timestamp
    if (projectsCache.size > MAX_CACHE_ENTRIES) {
      const remainingEntries = Array.from(projectsCache.entries());
      const toEvict = projectsCache.size - MAX_CACHE_ENTRIES;
      
      // Sort by timestamp (oldest first) and remove excess
      remainingEntries
        .sort((a, b) => a[1].timestamp - b[1].timestamp)
        .slice(0, toEvict)
        .forEach(([key]) => projectsCache.delete(key));
      
      cacheStats.evictions += toEvict;
    }
    
    cacheStats.currentSize = projectsCache.size;

    logApiResponse('GET', '/api/projects', Date.now() - startedAt, { 
      cached: false, 
      count: Array.isArray(projects) ? projects.length : 0,
      cacheStats: { ...cacheStats }
    }, reqId);
    return NextResponse.json(projects, { status: 200 });
  } catch (error) {
    logError('Failed to fetch projects', error as Error, {});
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}


