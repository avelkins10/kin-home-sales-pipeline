import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getPCTeamMetrics } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { isManagerRole } from '@/lib/utils/role-helpers';
import { PCTeamMetrics } from '@/lib/types/operations';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

// In-memory cache for team analytics data
const cache = new Map<string, { data: PCTeamMetrics; timestamp: number }>();
const CACHE_TTL = 120 * 1000; // 120 seconds (longer due to expensive queries)
const MAX_CACHE_SIZE = 20; // Fewer managers

interface CacheEntry {
  data: PCTeamMetrics;
  timestamp: number;
}

export async function GET(request: NextRequest) {
  const reqId = `pc-analytics-team-${Date.now()}`;
  
  try {
    logApiRequest('GET', '/api/operations/analytics/team', {}, reqId);

    // Require authentication
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    // Check user role - only managers can access team metrics
    if (!session.user?.role || !isManagerRole(session.user.role)) {
      return NextResponse.json(
        { error: 'Team metrics are only available to Operations Managers' },
        { status: 403 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const timeRange = (searchParams.get('timeRange') as '7days' | '30days' | '90days' | 'all') || '30days';
    const coordinatorEmail = searchParams.get('coordinatorEmail') || 'all';
    
    // Validate timeRange
    const validTimeRanges = ['7days', '30days', '90days', 'all'];
    if (!validTimeRanges.includes(timeRange)) {
      return NextResponse.json(
        { error: 'Invalid timeRange parameter' },
        { status: 400 }
      );
    }

    // Check cache
    const cacheKey = `pc-analytics-team:${timeRange}:${coordinatorEmail}`;
    const cached = cache.get(cacheKey);
    
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      logApiResponse('GET', '/api/operations/analytics/team', { cached: true }, reqId);
      return NextResponse.json({
        teamMetrics: cached.data,
        cached: true
      });
    }

    // Fetch fresh team metrics
    const teamMetrics = await getPCTeamMetrics(timeRange, reqId);

    // Filter by coordinator if specified
    if (coordinatorEmail !== 'all') {
      teamMetrics.coordinators = teamMetrics.coordinators.filter(
        coordinator => coordinator.coordinatorEmail === coordinatorEmail
      );
    }

    // Cache the result
    cache.set(cacheKey, {
      data: teamMetrics,
      timestamp: Date.now()
    });

    // Cleanup cache if it gets too large
    if (cache.size > MAX_CACHE_SIZE) {
      const oldestKey = cache.keys().next().value;
      cache.delete(oldestKey);
    }

    logApiResponse('GET', '/api/operations/analytics/team', { 
      coordinatorCount: teamMetrics.coordinators.length,
      cached: false 
    }, reqId);

    return NextResponse.json({
      teamMetrics,
      cached: false
    });

  } catch (error) {
    logError('PC Analytics Team API Error', error, reqId);
    
    return NextResponse.json(
      { 
        error: 'Failed to fetch PC team analytics data',
        details: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
