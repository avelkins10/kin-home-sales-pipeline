export const runtime = 'nodejs'

// app/api/analytics/milestone-timings/route.ts
import { NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getMilestoneTimings } from '@/lib/quickbase/queries';
import type { MilestoneTimings } from '@/lib/types/analytics';

// Simple in-memory cache for milestone timings with differentiated TTL
const milestoneTimingsCache = new Map<string, { data: any; timestamp: number; ttl: number }>();

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
      return 30 * 1000; // 30 seconds for weekly data (changes more frequently)
    case 'last_30':
      return 60 * 1000; // 1 minute for last 30 days
    case 'last_90':
      return 60 * 1000; // 1 minute for last 90 days
    case 'last_12_months':
      return 90 * 1000; // 90 seconds for last 12 months
    case 'custom':
      return 60 * 1000; // 1 minute for custom ranges
    default:
      return 30 * 1000; // Default 30 seconds
  }
}

/**
 * GET /api/analytics/milestone-timings
 * 
 * Calculate average time per milestone for process optimization.
 * Only accessible by office_leader, regional, and super_admin roles.
 * 
 * Analyzes duration between milestone dates to identify bottlenecks:
 * - Intake: SALES_DATE → INTAKE_FIRST_PASS_COMPLETE
 * - Survey: INTAKE_FIRST_PASS_COMPLETE → SURVEY_APPROVED
 * - Design: SURVEY_APPROVED → DESIGN_COMPLETED
 * - Permit: DESIGN_COMPLETED → PERMIT_APPROVED
 * - Install: PERMIT_APPROVED → INSTALL_COMPLETED_DATE
 * - PTO: INSTALL_COMPLETED_DATE → PTO_APPROVED
 * 
 * Outliers (>365 days) are filtered out to prevent skewing results.
 * 
 * Query Parameters:
 * - timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' (default: 'ytd')
 * - officeIds: comma-separated list of office IDs for filtering (optional)
 * - startDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 * - endDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 * 
 * Response:
 * {
 *   timings: {
 *     overallCycleTime: MilestoneTiming,
 *     milestones: MilestoneTiming[]  // Array of 6 milestones: Intake, Survey, Design, Permit, Install, PTO
 *   },
 *   metadata: {
 *     timeRange: string,
 *     startDate?: string,
 *     endDate?: string,
 *     projectCount: number,
 *     completedProjectCount: number,
 *     cached: boolean
 *   }
 * }
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/analytics/milestone-timings', undefined, reqId);

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

    // Check cache first - include timeRange, officeIds, and custom dates in cache key
    const officeKey = officeIds ? officeIds.sort().join(',') : (salesOffice ? salesOffice.sort().join(',') : '');
    const customRangeKey = effectiveTimeRange === 'custom' ? `${startDate}:${endDate}` : '';
    const cacheKey = `${userId}:${role}:${officeKey}:${effectiveTimeRange}:${customRangeKey}`;
    const cached = milestoneTimingsCache.get(cacheKey);
    const cacheTTL = getCacheTTL(effectiveTimeRange);
    
    if (cached && Date.now() - cached.timestamp < cacheTTL) {
      logApiResponse('GET', '/api/analytics/milestone-timings', Date.now() - startedAt, { cached: true, timeRange: effectiveTimeRange }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Fetch milestone timings with timezone awareness
    const timingsData = await getMilestoneTimings(
      userId,
      role,
      effectiveTimeRange,
      officeIds,
      effectiveTimeRange === 'custom' ? { startDate: startDate!, endDate: endDate! } : undefined,
      reqId,
      userTimezone
    );

    // Calculate metadata
    const totalProjects = timingsData.totalProjects;
    const completedProjects = timingsData.completedProjects;

    const response = {
      timings: timingsData,
      metadata: {
        timeRange: effectiveTimeRange,
        ...(effectiveTimeRange === 'custom' && { startDate, endDate }),
        projectCount: totalProjects,
        completedProjectCount: completedProjects,
        bottleneck: timingsData.bottleneck,
        cached: false,
      },
    };

    // Cache the result with TTL
    milestoneTimingsCache.set(cacheKey, { data: response, timestamp: Date.now(), ttl: cacheTTL });

    // Clean up old cache entries with strict size cap
    if (milestoneTimingsCache.size > 100) {
      const now = Date.now();
      const entries = Array.from(milestoneTimingsCache.entries());
      
      // First, remove expired entries using their individual TTL
      for (const [key, value] of entries) {
        if (now - value.timestamp > value.ttl) {
          milestoneTimingsCache.delete(key);
        }
      }
      
      // If still over 100 entries, evict oldest (FIFO) until size == 100
      if (milestoneTimingsCache.size > 100) {
        const remainingEntries = Array.from(milestoneTimingsCache.entries());
        // Sort by timestamp (oldest first) and remove excess
        remainingEntries
          .sort((a, b) => a[1].timestamp - b[1].timestamp)
          .slice(0, milestoneTimingsCache.size - 100)
          .forEach(([key]) => milestoneTimingsCache.delete(key));
      }
    }

    logApiResponse('GET', '/api/analytics/milestone-timings', Date.now() - startedAt, { cached: false, timeRange: effectiveTimeRange, projectCount: totalProjects }, reqId);
    return NextResponse.json(response, { status: 200 });
  } catch (error) {
    console.error('[/api/analytics/milestone-timings] ERROR:', error);
    logError('Failed to fetch milestone timings', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    }, { status: 500 });
  }
}
