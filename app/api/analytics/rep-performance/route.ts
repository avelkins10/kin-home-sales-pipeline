export const runtime = 'nodejs'

// app/api/analytics/rep-performance/route.ts
import { NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getRepPerformance } from '@/lib/quickbase/queries';
import type { RepPerformance } from '@/lib/types/analytics';

// Simple in-memory cache for rep performance metrics with differentiated TTL
const repPerformanceCache = new Map<string, { data: any; timestamp: number; ttl: number }>();

// Determine TTL based on timeRange
function getCacheTTL(timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months'): number {
  switch (timeRange) {
    case 'lifetime':
      return 120 * 1000; // 2 minutes for lifetime data (changes less frequently)
    case 'ytd':
      return 60 * 1000; // 1 minute for YTD data (changes daily)
    case 'month':
      return 60 * 1000; // 1 minute for monthly data
    case 'week':
      return 60 * 1000; // 60 seconds for weekly data (rep performance queries are expensive)
    case 'last_30':
      return 60 * 1000; // 1 minute for last 30 days
    case 'last_90':
      return 60 * 1000; // 1 minute for last 90 days
    case 'last_12_months':
      return 90 * 1000; // 90 seconds for last 12 months
    case 'custom':
      return 60 * 1000; // 1 minute for custom ranges
    default:
      return 60 * 1000; // Default 60 seconds (rep performance queries are expensive)
  }
}

/**
 * GET /api/analytics/rep-performance
 * 
 * Fetch per-rep performance metrics for analytics dashboard.
 * Only accessible by office_leader, regional, and super_admin roles.
 * 
 * Query Parameters:
 * - timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' (default: 'ytd')
 * - officeIds: comma-separated list of office IDs for filtering (optional)
 * - repEmail: email address to filter for a specific rep's performance (optional)
 * - startDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 * - endDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 * 
 * Response:
 * {
 *   metrics: RepPerformance[],
 *   metadata: {
 *     timeRange: string,
 *     startDate?: string,
 *     endDate?: string,
 *     repCount: number,
 *     filteredByRep?: string,
 *     cached: boolean
 *   }
 * }
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/analytics/rep-performance', undefined, reqId);

  // Restrict access to managers only
  const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
  if (!auth.authorized) return auth.response;

  try {
    const { searchParams } = new URL(req.url);
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }

    // Extract time range parameter with validation
    const timeRange = searchParams.get('timeRange') as 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | null;
    const validTimeRanges = ['lifetime', 'ytd', 'month', 'week', 'custom', 'last_30', 'last_90', 'last_12_months'];
    if (timeRange && !validTimeRanges.includes(timeRange)) {
      return NextResponse.json({ error: 'Invalid timeRange parameter. Must be one of: lifetime, ytd, month, week, custom' }, { status: 400 });
    }
    const effectiveTimeRange = timeRange || 'ytd'; // Default to YTD for analytics

    // Extract office IDs parameter
    const officeIdsParam = searchParams.get('officeIds');
    let officeIds: number[] | undefined;
    if (officeIdsParam) {
      try {
        officeIds = officeIdsParam.split(',').map(id => parseInt(id.trim(), 10)).filter(id => !isNaN(id));
        if (officeIds.length === 0) {
          return NextResponse.json({ error: 'Invalid officeIds parameter. Must be comma-separated numbers' }, { status: 400 });
        }
      } catch (error) {
        return NextResponse.json({ error: 'Invalid officeIds parameter. Must be comma-separated numbers' }, { status: 400 });
      }
    }

    // Extract rep email filter parameter
    const repEmail = searchParams.get('repEmail');
    if (repEmail && !repEmail.includes('@')) {
      return NextResponse.json({ error: 'Invalid repEmail parameter. Must be a valid email address' }, { status: 400 });
    }

    // Extract custom date range if provided
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');

    // Validate custom date range
    if (effectiveTimeRange === 'custom') {
      if (!startDate || !endDate) {
        return NextResponse.json({ error: 'startDate and endDate are required for custom time range' }, { status: 400 });
      }

      // Validate date format (YYYY-MM-DD)
      const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
      if (!dateRegex.test(startDate) || !dateRegex.test(endDate)) {
        return NextResponse.json({ error: 'Invalid date format. Use YYYY-MM-DD' }, { status: 400 });
      }

      // Validate date range is logical
      const start = new Date(startDate);
      const end = new Date(endDate);
      if (start > end) {
        return NextResponse.json({ error: 'startDate must be before or equal to endDate' }, { status: 400 });
      }
    }

    const { id, role, salesOffice, timezone } = auth.session.user as any;
    const userId = id as string;
    const userTimezone = timezone || 'America/New_York';

    // Check cache first - include timeRange, officeIds, repEmail, and custom dates in cache key
    const officeKey = officeIds ? officeIds.sort().join(',') : (salesOffice ? salesOffice.sort().join(',') : '');
    const customRangeKey = effectiveTimeRange === 'custom' ? `${startDate}:${endDate}` : '';
    const repEmailKey = repEmail || 'all';
    const cacheKey = `${userId}:${role}:${officeKey}:${effectiveTimeRange}:${customRangeKey}:${repEmailKey}`;
    const cached = repPerformanceCache.get(cacheKey);
    const cacheTTL = getCacheTTL(effectiveTimeRange);
    
    if (cached && Date.now() - cached.timestamp < cacheTTL) {
      logApiResponse('GET', '/api/analytics/rep-performance', Date.now() - startedAt, { cached: true, timeRange: effectiveTimeRange, repCount: cached.data.metrics?.length || 0 }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Fetch rep performance metrics with timezone awareness
    const allMetrics = await getRepPerformance(
      userId,
      role,
      effectiveTimeRange,
      officeIds,
      effectiveTimeRange === 'custom' ? { startDate: startDate!, endDate: endDate! } : undefined,
      reqId,
      userTimezone
    );

    // Filter by rep email if provided
    let metrics = allMetrics;
    if (repEmail) {
      metrics = allMetrics.filter(rep => rep.repEmail === repEmail);
    }

    const response = {
      metrics,
      metadata: {
        timeRange: effectiveTimeRange,
        ...(effectiveTimeRange === 'custom' && { startDate, endDate }),
        repCount: metrics.length,
        ...(repEmail && { filteredByRep: repEmail }),
        cached: false,
      },
    };

    // Cache the result with TTL
    repPerformanceCache.set(cacheKey, { data: response, timestamp: Date.now(), ttl: cacheTTL });

    // Clean up old cache entries with strict size cap
    if (repPerformanceCache.size > 100) {
      const now = Date.now();
      const entries = Array.from(repPerformanceCache.entries());
      
      // First, remove expired entries using their individual TTL
      for (const [key, value] of entries) {
        if (now - value.timestamp > value.ttl) {
          repPerformanceCache.delete(key);
        }
      }
      
      // If still over 100 entries, evict oldest (FIFO) until size == 100
      if (repPerformanceCache.size > 100) {
        const remainingEntries = Array.from(repPerformanceCache.entries());
        // Sort by timestamp (oldest first) and remove excess
        remainingEntries
          .sort((a, b) => a[1].timestamp - b[1].timestamp)
          .slice(0, repPerformanceCache.size - 100)
          .forEach(([key]) => repPerformanceCache.delete(key));
      }
    }

    logApiResponse('GET', '/api/analytics/rep-performance', Date.now() - startedAt, { cached: false, timeRange: effectiveTimeRange, repCount: metrics.length }, reqId);
    return NextResponse.json(response, { status: 200 });
  } catch (error) {
    console.error('[/api/analytics/rep-performance] ERROR:', error);
    logError('Failed to fetch rep performance metrics', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    }, { status: 500 });
  }
}
