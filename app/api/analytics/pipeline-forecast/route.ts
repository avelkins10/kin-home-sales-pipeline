export const runtime = 'nodejs'

// app/api/analytics/pipeline-forecast/route.ts
import { NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getPipelineForecast } from '@/lib/quickbase/queries';
import type { PipelineForecast } from '@/lib/types/analytics';

// Simple in-memory cache for pipeline forecast with shorter TTL
const pipelineForecastCache = new Map<string, { data: any; timestamp: number; ttl: number }>();

// Use shorter TTL for forecast data since it changes frequently
function getCacheTTL(): number {
  return 30 * 1000; // 30 seconds - forecast data changes frequently as install dates are scheduled
}

/**
 * GET /api/analytics/pipeline-forecast
 *
 * Install Tracker: Shows current 3-week window of install activity
 * Independent of page date filters - always displays last week, this week, next week
 * Only accessible by office_leader, regional, and super_admin roles.
 *
 * Uses actual scheduled/completed dates (not estimates):
 * - Last Week: INSTALL_COMPLETED_DATE (Field 534) - actual completions
 * - This/Next Week: INSTALL_SCHEDULED_START_DATE (Field 178) or INSTALL_SCHEDULED_DATE_CAPTURE (Field 710)
 *
 * Query Parameters:
 * - officeIds: comma-separated list of office IDs for filtering (optional)
 * - includeDetails: boolean to control whether to return detailed project list (default: false)
 *
 * Response:
 * {
 *   forecast: {
 *     lastWeek: number,
 *     thisWeek: number,
 *     nextWeek: number,
 *     projects?: Array<{
 *       recordId: number,
 *       projectId: string,
 *       customerName: string,
 *       systemSize: number,
 *       forecastDate: string,
 *       scheduledStartDate: string | null,
 *       scheduledCaptureDate: string | null,
 *       completedDate: string | null,
 *       dateSource: 'completed' | 'scheduled-primary' | 'scheduled-capture',
 *       weekBucket: 'lastWeek' | 'thisWeek' | 'nextWeek'
 *     }>
 *   },
 *   metadata: {
 *     officeCount: number,
 *     totalActiveProjects: number,
 *     forecastCoverage: number,
 *     cached: boolean
 *   }
 * }
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/analytics/pipeline-forecast', undefined, reqId);

  // Restrict access to managers only
  const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
  if (!auth.authorized) return auth.response;

  try {
    const { searchParams } = new URL(req.url);
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }

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

    // Extract includeDetails parameter
    const includeDetailsParam = searchParams.get('includeDetails');
    const includeDetails = includeDetailsParam === 'true';

    const { id, role, salesOffice } = auth.session.user as any;
    const userId = id as string;

    // Check cache first - include officeIds and includeDetails in cache key
    const officeKey = officeIds ? officeIds.sort().join(',') : (salesOffice ? salesOffice.sort().join(',') : '');
    const cacheKey = `${userId}:${role}:${officeKey}:forecast:${includeDetails}`;
    const cached = pipelineForecastCache.get(cacheKey);
    const cacheTTL = getCacheTTL();

    if (cached && Date.now() - cached.timestamp < cacheTTL) {
      logApiResponse('GET', '/api/analytics/pipeline-forecast', Date.now() - startedAt, { cached: true, includeDetails }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Fetch pipeline forecast - always shows current 3-week window
    const forecastData = await getPipelineForecast(
      userId,
      role,
      officeIds,
      includeDetails,
      reqId
    );

    // Calculate metadata
    const totalActiveProjects = forecastData.totalActiveProjects;
    const projectsWithForecast = forecastData.projectsWithForecast;
    const forecastCoverage = totalActiveProjects > 0 ? (projectsWithForecast / totalActiveProjects) * 100 : 0;

    const response = {
      forecast: {
        lastWeek: forecastData.lastWeek,
        thisWeek: forecastData.thisWeek,
        nextWeek: forecastData.nextWeek,
        ...(includeDetails && { projects: forecastData.projects }),
      },
      metadata: {
        officeCount: officeIds ? officeIds.length : (salesOffice ? salesOffice.length : 0),
        totalActiveProjects,
        forecastCoverage: Math.round(forecastCoverage * 100) / 100,
        cached: false,
      },
    };

    // Cache the result with TTL
    pipelineForecastCache.set(cacheKey, { data: response, timestamp: Date.now(), ttl: cacheTTL });

    // Clean up old cache entries with strict size cap
    if (pipelineForecastCache.size > 100) {
      const now = Date.now();
      const entries = Array.from(pipelineForecastCache.entries());

      // First, remove expired entries using their individual TTL
      for (const [key, value] of entries) {
        if (now - value.timestamp > value.ttl) {
          pipelineForecastCache.delete(key);
        }
      }

      // If still over 100 entries, evict oldest (FIFO) until size == 100
      if (pipelineForecastCache.size > 100) {
        const remainingEntries = Array.from(pipelineForecastCache.entries());
        // Sort by timestamp (oldest first) and remove excess
        remainingEntries
          .sort((a, b) => a[1].timestamp - b[1].timestamp)
          .slice(0, pipelineForecastCache.size - 100)
          .forEach(([key]) => pipelineForecastCache.delete(key));
      }
    }

    logApiResponse('GET', '/api/analytics/pipeline-forecast', Date.now() - startedAt, { cached: false, includeDetails, forecastCoverage }, reqId);
    return NextResponse.json(response, { status: 200 });
  } catch (error) {
    console.error('[/api/analytics/pipeline-forecast] ERROR:', error);
    logError('Failed to fetch pipeline forecast', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    }, { status: 500 });
  }
}
