export const runtime = 'nodejs'

// app/api/dashboard/metrics/route.ts
import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

// Simple in-memory cache for metrics with differentiated TTL
const metricsCache = new Map<string, { data: any; timestamp: number; ttl: number }>();

// Determine TTL based on timeRange
function getCacheTTL(timeRange: 'lifetime' | 'month' | 'week'): number {
  switch (timeRange) {
    case 'lifetime':
      return 120 * 1000; // 2 minutes for lifetime data (changes less frequently)
    case 'month':
      return 60 * 1000; // 1 minute for monthly data
    case 'week':
      return 30 * 1000; // 30 seconds for weekly data (changes more frequently)
    default:
      return 30 * 1000; // Default 30 seconds
  }
}

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

    // Extract time range parameter with validation
    const timeRange = searchParams.get('timeRange') as 'lifetime' | 'month' | 'week' | null;
    const validTimeRanges = ['lifetime', 'month', 'week'];
    if (timeRange && !validTimeRanges.includes(timeRange)) {
      return NextResponse.json({ error: 'Invalid timeRange parameter. Must be one of: lifetime, month, week' }, { status: 400 });
    }
    const effectiveTimeRange = timeRange || 'lifetime';

    const { id, role, salesOffice } = auth.session.user as any;
    const userId = id as string;

    // Check cache first - include timeRange in cache key
    const officeKey = salesOffice ? salesOffice.sort().join(',') : '';
    const cacheKey = `${userId}:${role}:${officeKey}:${effectiveTimeRange}`;
    const cached = metricsCache.get(cacheKey);
    const cacheTTL = getCacheTTL(effectiveTimeRange);
    
    if (cached && Date.now() - cached.timestamp < cacheTTL) {
      logApiResponse('GET', '/api/dashboard/metrics', Date.now() - startedAt, { cached: true, timeRange: effectiveTimeRange, ttl: cacheTTL }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Use enhanced metrics function with time range support
    const { getEnhancedDashboardMetrics } = await import('@/lib/quickbase/queries');
    const metrics = await getEnhancedDashboardMetrics(userId, role, effectiveTimeRange, salesOffice);

    // Cache the result with TTL
    metricsCache.set(cacheKey, { data: metrics, timestamp: Date.now(), ttl: cacheTTL });

    // Clean up old cache entries with strict size cap
    if (metricsCache.size > 100) {
      const now = Date.now();
      const entries = Array.from(metricsCache.entries());
      
      // First, remove expired entries using their individual TTL
      for (const [key, value] of entries) {
        if (now - value.timestamp > value.ttl) {
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

    logApiResponse('GET', '/api/dashboard/metrics', Date.now() - startedAt, { cached: false, timeRange: effectiveTimeRange }, reqId);
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


