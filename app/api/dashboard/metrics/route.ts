export const runtime = 'nodejs'

// app/api/dashboard/metrics/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

// Simple in-memory cache for metrics (30s TTL)
const metricsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 30 * 1000; // 30 seconds

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/dashboard/metrics', undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { searchParams } = new URL(req.url);
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }

    const { quickbaseUserId, role } = auth.session.user as any;
    const userId = quickbaseUserId as string;

    // Check cache first
    const cacheKey = `${userId}:${role}`;
    const cached = metricsCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      logApiResponse('GET', '/api/dashboard/metrics', Date.now() - startedAt, { cached: true }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    const { getDashboardMetricsOptimized } = await import('@/lib/quickbase/queries');
    const metrics = await getDashboardMetricsOptimized(userId, role);

    // Cache the result
    metricsCache.set(cacheKey, { data: metrics, timestamp: Date.now() });

    // Clean up old cache entries with strict size cap
    if (metricsCache.size > 100) {
      const now = Date.now();
      const entries = Array.from(metricsCache.entries());
      
      // First, remove expired entries
      for (const [key, value] of entries) {
        if (now - value.timestamp > CACHE_TTL) {
          metricsCache.delete(key);
        }
      }
      
      // If still over 100 entries, evict oldest (FIFO) until size == 100
      if (metricsCache.size > 100) {
        const remainingEntries = Array.from(metricsCache.entries());
        // Sort by timestamp (oldest first) and remove excess
        remainingEntries
          .sort((a, b) => a[1].timestamp - b[1].timestamp)
          .slice(0, metricsCache.size - 100)
          .forEach(([key]) => metricsCache.delete(key));
      }
    }

    logApiResponse('GET', '/api/dashboard/metrics', Date.now() - startedAt, { cached: false }, reqId);
    return NextResponse.json(metrics, { status: 200 });
  } catch (error) {
    console.error('[/api/dashboard/metrics] ERROR:', error);
    logError('Failed to fetch dashboard metrics', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    }, { status: 500 });
  }
}


