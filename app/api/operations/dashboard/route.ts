export const runtime = 'nodejs'

// app/api/operations/dashboard/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { 
  getPCDashboardMetrics, 
  getPCPriorityQueue, 
  getPCProjectPipeline, 
  getPCActivityFeed 
} from '@/lib/quickbase/queries';
import type { PCDashboardData } from '@/lib/types/operations';

// Simple in-memory cache for PC dashboard data
const pcDashboardCache = new Map<string, { data: PCDashboardData; timestamp: number; ttl: number }>();

// PC dashboard cache TTL (30 seconds - operations data changes more frequently)
const PC_CACHE_TTL = 30 * 1000;

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/operations/dashboard', undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    // Check if user has operations role
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Operations role required.' 
      }, { status: 403 });
    }

    // Extract PC email and name from session
    const pcEmail = auth.session.user.email as string;
    const pcName = auth.session.user.name as string;
    const cacheKey = `pc-dashboard:${pcEmail}`;

    // Check cache first
    const cached = pcDashboardCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < PC_CACHE_TTL) {
      logApiResponse('GET', '/api/operations/dashboard', Date.now() - startedAt, { cached: true }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Fetch all PC dashboard data in parallel
    const [metrics, priorityQueue, pipeline, activityFeed] = await Promise.all([
      getPCDashboardMetrics(pcEmail, pcName, reqId),
      getPCPriorityQueue(pcEmail, pcName, 10, reqId),
      getPCProjectPipeline(pcEmail, pcName, reqId),
      getPCActivityFeed(pcEmail, pcName, 20, reqId)
    ]);

    // Combine into dashboard data
    const dashboardData: PCDashboardData = {
      metrics,
      priorityQueue,
      pipeline,
      activityFeed
    };

    // Cache the result
    pcDashboardCache.set(cacheKey, { 
      data: dashboardData, 
      timestamp: Date.now(), 
      ttl: PC_CACHE_TTL 
    });

    // Clean up old cache entries (smaller cache than sales dashboard)
    if (pcDashboardCache.size > 50) {
      const now = Date.now();
      const entries = Array.from(pcDashboardCache.entries());
      
      // Remove expired entries
      for (const [key, value] of entries) {
        if (now - value.timestamp > value.ttl) {
          pcDashboardCache.delete(key);
        }
      }
      
      // If still over 50 entries, evict oldest
      if (pcDashboardCache.size > 50) {
        const remainingEntries = Array.from(pcDashboardCache.entries());
        remainingEntries
          .sort((a, b) => a[1].timestamp - b[1].timestamp)
          .slice(0, pcDashboardCache.size - 50)
          .forEach(([key]) => pcDashboardCache.delete(key));
      }
    }

    logApiResponse('GET', '/api/operations/dashboard', Date.now() - startedAt, { cached: false }, reqId);
    return NextResponse.json(dashboardData, { status: 200 });

  } catch (error) {
    console.error('[/api/operations/dashboard] ERROR:', error);
    logError('Failed to fetch PC dashboard data', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    }, { status: 500 });
  }
}
