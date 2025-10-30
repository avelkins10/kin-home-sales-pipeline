import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { 
  getCrewPerformanceMetrics, 
  getCrewTeamAverages, 
  getCrewLeaderboard 
} from '@/lib/db/arrivy';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { CrewPerformanceDashboardData } from '@/lib/types/operations';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

// In-memory cache for crew performance data
const cache = new Map<string, { data: CrewPerformanceDashboardData; timestamp: number }>();
const CACHE_TTL = 60 * 1000; // 60 seconds
const MAX_CACHE_SIZE = 50;

interface CacheEntry {
  data: CrewPerformanceDashboardData;
  timestamp: number;
}

export async function GET(request: NextRequest) {
  const reqId = `crew-performance-${Date.now()}`;
  
  try {
    logApiRequest('GET', '/api/operations/crew-performance', {}, reqId);

    // Require authentication
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    // Check user role
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    if (!session.user?.role || !allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Unauthorized - Crew performance access required' },
        { status: 403 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const timeRange = (searchParams.get('timeRange') as '7days' | '30days' | '90days' | 'all') || '30days';
    const crewIdParam = searchParams.get('crewId');
    const crewId = crewIdParam && crewIdParam !== 'all' ? parseInt(crewIdParam, 10) : undefined;
    const taskType = searchParams.get('taskType') || 'all';
    
    // Validate timeRange
    const validTimeRanges = ['7days', '30days', '90days', 'all'];
    if (!validTimeRanges.includes(timeRange)) {
      return NextResponse.json(
        { error: 'Invalid timeRange parameter' },
        { status: 400 }
      );
    }

    // Validate crewId if provided
    if (crewId !== undefined && isNaN(crewId)) {
      return NextResponse.json(
        { error: 'Invalid crewId parameter - must be a number' },
        { status: 400 }
      );
    }

    // Validate taskType
    const validTaskTypes = ['all', 'survey', 'install', 'inspection', 'service'];
    if (!validTaskTypes.includes(taskType)) {
      return NextResponse.json(
        { error: 'Invalid taskType parameter' },
        { status: 400 }
      );
    }

    // Check cache
    const cacheKey = `crew-performance:${timeRange}:${crewId || 'all'}:${taskType}`;
    const cached = cache.get(cacheKey);
    
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      logApiResponse('GET', '/api/operations/crew-performance', { cached: true }, reqId);
      return NextResponse.json({
        ...cached.data,
        cached: true
      });
    }

    // Fetch fresh data (parallel)
    const filters = {
      timeRange,
      crewId,
      taskType: taskType !== 'all' ? taskType : undefined
    };

    const [crewMetrics, teamAverages, leaderboard] = await Promise.all([
      getCrewPerformanceMetrics(filters),
      getCrewTeamAverages({ timeRange, taskType: taskType !== 'all' ? taskType : undefined }),
      getCrewLeaderboard({ timeRange, taskType: taskType !== 'all' ? taskType : undefined })
    ]);

    const dashboardData: CrewPerformanceDashboardData = {
      crewMetrics,
      teamAverages,
      leaderboard
    };

    // Cache the result
    cache.set(cacheKey, {
      data: dashboardData,
      timestamp: Date.now()
    });

    // Cleanup cache if it gets too large
    if (cache.size > MAX_CACHE_SIZE) {
      const oldestKey = cache.keys().next().value;
      if (oldestKey) {
        cache.delete(oldestKey);
      }
    }

    logApiResponse('GET', '/api/operations/crew-performance', { 
      crewCount: crewMetrics.length,
      cached: false 
    }, reqId);

    return NextResponse.json({
      ...dashboardData,
      cached: false
    }, {
      headers: {
        'Cache-Control': 'private, max-age=60'
      }
    });

  } catch (error) {
    logError('Crew Performance API Error', error, reqId);
    
    return NextResponse.json(
      { 
        error: 'Failed to fetch crew performance data',
        details: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

