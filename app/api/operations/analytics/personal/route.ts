import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { 
  getPCPersonalMetrics, 
  getPCOutreachTrend, 
  getPCStageDistribution, 
  getPCResponseBreakdown 
} from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCAnalyticsData } from '@/lib/types/operations';

export const runtime = 'nodejs';

// In-memory cache for analytics data
const cache = new Map<string, { data: PCAnalyticsData; timestamp: number }>();
const CACHE_TTL = 60 * 1000; // 60 seconds
const MAX_CACHE_SIZE = 50;

interface CacheEntry {
  data: PCAnalyticsData;
  timestamp: number;
}

export async function GET(request: NextRequest) {
  const reqId = `pc-analytics-personal-${Date.now()}`;
  
  try {
    logApiRequest('GET', '/api/operations/analytics/personal', {}, reqId);

    // Require authentication
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    // Check user role
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    if (!session.user?.role || !allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Unauthorized - PC analytics access required' },
        { status: 403 }
      );
    }

    // Extract PC email and name from session
    const pcEmail = session.user.email;
    const pcName = session.user.name || 'Unknown PC';
    
    if (!pcEmail) {
      return NextResponse.json(
        { error: 'PC email not found in session' },
        { status: 400 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const timeRange = (searchParams.get('timeRange') as '7days' | '30days' | '90days' | 'all') || '30days';
    
    // Validate timeRange
    const validTimeRanges = ['7days', '30days', '90days', 'all'];
    if (!validTimeRanges.includes(timeRange)) {
      return NextResponse.json(
        { error: 'Invalid timeRange parameter' },
        { status: 400 }
      );
    }

    // Check cache
    const cacheKey = `pc-analytics-personal:${pcEmail}:${timeRange}`;
    const cached = cache.get(cacheKey);
    
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      logApiResponse('GET', '/api/operations/analytics/personal', { cached: true }, reqId);
      return NextResponse.json({
        ...cached.data,
        cached: true
      });
    }

    // Fetch fresh data
    const [personalMetrics, outreachTrend, stageDistribution, responseBreakdown] = await Promise.all([
      getPCPersonalMetrics(pcEmail, pcName, timeRange, reqId),
      getPCOutreachTrend(pcEmail, pcName, 30, reqId),
      getPCStageDistribution(pcEmail, pcName, reqId),
      getPCResponseBreakdown(pcEmail, pcName, timeRange, reqId)
    ]);

    const analyticsData: PCAnalyticsData = {
      personalMetrics,
      outreachTrend,
      stageDistribution,
      responseBreakdown
    };

    // Cache the result
    cache.set(cacheKey, {
      data: analyticsData,
      timestamp: Date.now()
    });

    // Cleanup cache if it gets too large
    if (cache.size > MAX_CACHE_SIZE) {
      const oldestKey = cache.keys().next().value;
      cache.delete(oldestKey);
    }

    logApiResponse('GET', '/api/operations/analytics/personal', { 
      recordCount: outreachTrend.length,
      cached: false 
    }, reqId);

    return NextResponse.json({
      ...analyticsData,
      cached: false
    });

  } catch (error) {
    logError('PC Analytics Personal API Error', error, reqId);
    
    return NextResponse.json(
      { 
        error: 'Failed to fetch PC analytics data',
        details: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
