export const runtime = 'nodejs'

// app/api/analytics/rep-details/route.ts
import { NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getRepPerformance } from '@/lib/quickbase/queries';
import { qbClient } from '@/lib/quickbase/client';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import type { RepDetailMetrics, RepPerformance } from '@/lib/types/analytics';

// Simple in-memory cache for rep details with differentiated TTL
const repDetailsCache = new Map<string, { data: any; timestamp: number; ttl: number }>();

// Determine TTL based on timeRange
function getCacheTTL(timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months'): number {
  switch (timeRange) {
    case 'lifetime':
      return 120 * 1000; // 2 minutes for lifetime data
    case 'ytd':
      return 60 * 1000; // 1 minute for YTD data
    case 'month':
      return 60 * 1000; // 1 minute for monthly data
    case 'week':
      return 30 * 1000; // 30 seconds for weekly data
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
 * GET /api/analytics/rep-details
 *
 * Fetch detailed metrics for a specific rep, including performance data and project list.
 * Only accessible by office_leader, regional, and super_admin roles.
 *
 * Query Parameters:
 * - id: rep email address (required)
 * - timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months' (default: 'ytd')
 * - startDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 * - endDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 *
 * Response:
 * {
 *   rep: RepPerformance,
 *   projects: Array<ProjectSummary>,
 *   monthlyTrends: Array<MonthlyTrend>,
 *   metadata: {
 *     timeRange: string,
 *     startDate?: string,
 *     endDate?: string,
 *     cached: boolean
 *   }
 * }
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/analytics/rep-details', undefined, reqId);

  // Restrict access to managers only
  const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
  if (!auth.authorized) return auth.response;

  try {
    const { searchParams } = new URL(req.url);

    // Extract rep identifier (email)
    const repId = searchParams.get('id');
    if (!repId) {
      return NextResponse.json({ error: 'Missing required parameter: id (rep email)' }, { status: 400 });
    }

    // Extract time range parameter with validation
    const timeRange = searchParams.get('timeRange') as 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months' | null;
    const validTimeRanges = ['lifetime', 'ytd', 'month', 'week', 'custom', 'last_30', 'last_90', 'last_12_months'];
    if (timeRange && !validTimeRanges.includes(timeRange)) {
      return NextResponse.json({ error: 'Invalid timeRange parameter' }, { status: 400 });
    }
    const effectiveTimeRange = timeRange || 'ytd';

    // Extract custom date range if provided
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');

    // Validate custom date range
    if (effectiveTimeRange === 'custom') {
      if (!startDate || !endDate) {
        return NextResponse.json({ error: 'startDate and endDate are required for custom time range' }, { status: 400 });
      }

      const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
      if (!dateRegex.test(startDate) || !dateRegex.test(endDate)) {
        return NextResponse.json({ error: 'Invalid date format. Use YYYY-MM-DD' }, { status: 400 });
      }

      const start = new Date(startDate);
      const end = new Date(endDate);
      if (start > end) {
        return NextResponse.json({ error: 'startDate must be before or equal to endDate' }, { status: 400 });
      }
    }

    const { id, role, salesOffice, timezone } = auth.session.user as any;
    const userId = id as string;
    const userTimezone = timezone || 'America/New_York';

    // Check cache first
    const customRangeKey = effectiveTimeRange === 'custom' ? `${startDate}:${endDate}` : '';
    const cacheKey = `${userId}:${role}:${repId}:${effectiveTimeRange}:${customRangeKey}`;
    const cached = repDetailsCache.get(cacheKey);
    const cacheTTL = getCacheTTL(effectiveTimeRange);

    if (cached && Date.now() - cached.timestamp < cacheTTL) {
      logApiResponse('GET', '/api/analytics/rep-details', Date.now() - startedAt, { cached: true, timeRange: effectiveTimeRange }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Fetch rep performance metrics
    const allReps = await getRepPerformance(
      userId,
      role,
      effectiveTimeRange,
      undefined,
      effectiveTimeRange === 'custom' ? { startDate: startDate!, endDate: endDate! } : undefined,
      reqId,
      userTimezone
    );

    // Find the specific rep
    const rep = allReps.find(r => r.repEmail === repId);
    if (!rep) {
      return NextResponse.json({ error: 'Rep not found' }, { status: 404 });
    }

    // Fetch projects for this rep
    const projects = await fetchRepProjects(
      rep.repEmail!,
      rep.role,
      effectiveTimeRange,
      effectiveTimeRange === 'custom' ? { startDate: startDate!, endDate: endDate! } : undefined,
      userId,
      role,
      salesOffice
    );

    // Calculate monthly trends
    const monthlyTrends = calculateMonthlyTrends(projects, effectiveTimeRange);

    const response: RepDetailMetrics & { metadata: any } = {
      rep,
      projects,
      monthlyTrends,
      metadata: {
        timeRange: effectiveTimeRange,
        ...(effectiveTimeRange === 'custom' && { startDate, endDate }),
        cached: false,
      },
    };

    // Cache the result
    repDetailsCache.set(cacheKey, { data: response, timestamp: Date.now(), ttl: cacheTTL });

    // Clean up old cache entries
    if (repDetailsCache.size > 100) {
      const now = Date.now();
      const entries = Array.from(repDetailsCache.entries());

      for (const [key, value] of entries) {
        if (now - value.timestamp > value.ttl) {
          repDetailsCache.delete(key);
        }
      }

      if (repDetailsCache.size > 100) {
        const remainingEntries = Array.from(repDetailsCache.entries());
        remainingEntries
          .sort((a, b) => a[1].timestamp - b[1].timestamp)
          .slice(0, repDetailsCache.size - 100)
          .forEach(([key]) => repDetailsCache.delete(key));
      }
    }

    logApiResponse('GET', '/api/analytics/rep-details', Date.now() - startedAt, { cached: false, timeRange: effectiveTimeRange, projectCount: projects.length }, reqId);
    return NextResponse.json(response, { status: 200 });
  } catch (error) {
    console.error('[/api/analytics/rep-details] ERROR:', error);
    logError('Failed to fetch rep details', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    }, { status: 500 });
  }
}

/**
 * Fetch projects for a specific rep
 */
async function fetchRepProjects(
  repEmail: string,
  repRole: 'closer' | 'setter',
  timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | 'last_30' | 'last_90' | 'last_12_months',
  customDateRange?: { startDate: string; endDate: string },
  userId?: string,
  role?: string,
  salesOffice?: number[]
): Promise<RepDetailMetrics['projects']> {
  // Build time range filter
  let timeFilter = '';
  if (timeRange === 'custom' && customDateRange) {
    timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}} >= '${customDateRange.startDate}' AND {${PROJECT_FIELDS.SALES_DATE}} <= '${customDateRange.endDate}'`;
  } else if (timeRange !== 'lifetime') {
    const now = new Date();
    const currentYear = now.getFullYear();
    const currentMonth = now.getMonth();

    let startDateStr: string;
    let endDateStr: string = now.toISOString().split('T')[0];

    switch (timeRange) {
      case 'ytd':
        startDateStr = `${currentYear}-01-01`;
        break;
      case 'month':
        startDateStr = new Date(currentYear, currentMonth, 1).toISOString().split('T')[0];
        break;
      case 'week':
        const weekAgo = new Date(now);
        weekAgo.setDate(weekAgo.getDate() - 7);
        startDateStr = weekAgo.toISOString().split('T')[0];
        break;
      case 'last_30':
        const thirtyDaysAgo = new Date(now);
        thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30);
        startDateStr = thirtyDaysAgo.toISOString().split('T')[0];
        break;
      case 'last_90':
        const ninetyDaysAgo = new Date(now);
        ninetyDaysAgo.setDate(ninetyDaysAgo.getDate() - 90);
        startDateStr = ninetyDaysAgo.toISOString().split('T')[0];
        break;
      case 'last_12_months':
        const twelveMonthsAgo = new Date(now);
        twelveMonthsAgo.setMonth(twelveMonthsAgo.getMonth() - 12);
        startDateStr = twelveMonthsAgo.toISOString().split('T')[0];
        break;
      default:
        startDateStr = `${currentYear}-01-01`;
    }

    timeFilter = `AND {${PROJECT_FIELDS.SALES_DATE}} >= '${startDateStr}' AND {${PROJECT_FIELDS.SALES_DATE}} <= '${endDateStr}'`;
  }

  // Build rep filter based on role
  const repField = repRole === 'closer' ? PROJECT_FIELDS.CLOSER_NAME : PROJECT_FIELDS.SETTER_NAME;
  const repEmailField = repRole === 'closer' ? PROJECT_FIELDS.CLOSER_EMAIL : PROJECT_FIELDS.SETTER_EMAIL;

  // Query projects
  const query = `{${repEmailField}} = '${repEmail}' ${timeFilter}`;

  const response = await qbClient.getRecords({
    tableId: process.env.QUICKBASE_TABLE_ID!,
    query,
    select: [
      PROJECT_FIELDS.RECORD_ID,
      PROJECT_FIELDS.PROJECT_ID,
      PROJECT_FIELDS.CUSTOMER_NAME,
      PROJECT_FIELDS.STATUS,
      PROJECT_FIELDS.SYSTEM_SIZE,
      PROJECT_FIELDS.GROSS_PPW,
      PROJECT_FIELDS.NET_PPW,
      PROJECT_FIELDS.COMMISSIONABLE_PPW,
      PROJECT_FIELDS.SALES_DATE,
      PROJECT_FIELDS.INSTALL_COMPLETED_DATE,
      PROJECT_FIELDS.SALES_OFFICE_NAME,
      PROJECT_FIELDS.SALES_DATE,
      PROJECT_FIELDS.PTO_APPROVED,
    ],
  });

  // Transform to project summary format
  return response.data.map((record: any) => {
    const salesDate = record[PROJECT_FIELDS.SALES_DATE]?.value || null;
    const installDate = record[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value || null;
    const ptoDate = record[PROJECT_FIELDS.PTO_APPROVED]?.value || null;

    // Calculate cycle time if both dates are available
    let cycleTime: number | null = null;
    if (salesDate && ptoDate) {
      const start = new Date(salesDate);
      const end = new Date(ptoDate);
      cycleTime = Math.floor((end.getTime() - start.getTime()) / (1000 * 60 * 60 * 24));
    }

    return {
      recordId: record[PROJECT_FIELDS.RECORD_ID]?.value || 0,
      projectId: record[PROJECT_FIELDS.PROJECT_ID]?.value || '',
      customerName: record[PROJECT_FIELDS.CUSTOMER_NAME]?.value || '',
      status: record[PROJECT_FIELDS.STATUS]?.value || '',
      systemSize: record[PROJECT_FIELDS.SYSTEM_SIZE]?.value || 0,
      grossPpw: record[PROJECT_FIELDS.GROSS_PPW]?.value || 0,
      netPpw: record[PROJECT_FIELDS.NET_PPW]?.value || 0,
      commissionablePpw: record[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value || 0,
      salesDate,
      installDate,
      cycleTime,
      officeName: record[PROJECT_FIELDS.SALES_OFFICE_NAME]?.value || null,
    };
  });
}

/**
 * Calculate monthly performance trends from projects
 */
function calculateMonthlyTrends(
  projects: RepDetailMetrics['projects'],
  timeRange: string
): RepDetailMetrics['monthlyTrends'] {
  const monthlyData = new Map<string, {
    projects: number;
    installs: number;
    cancellations: number;
    holds: number;
    totalSystemSize: number;
    totalNetPpw: number;
  }>();

  projects.forEach(project => {
    if (!project.salesDate) return;

    const month = project.salesDate.substring(0, 7); // YYYY-MM format

    const existing = monthlyData.get(month) || {
      projects: 0,
      installs: 0,
      cancellations: 0,
      holds: 0,
      totalSystemSize: 0,
      totalNetPpw: 0,
    };

    existing.projects += 1;
    existing.totalSystemSize += project.systemSize;
    existing.totalNetPpw += project.netPpw;

    if (project.status.toLowerCase().includes('install') || project.installDate) {
      existing.installs += 1;
    }
    if (project.status.toLowerCase().includes('cancel')) {
      existing.cancellations += 1;
    }
    if (project.status.toLowerCase().includes('hold')) {
      existing.holds += 1;
    }

    monthlyData.set(month, existing);
  });

  // Convert to array and calculate averages
  return Array.from(monthlyData.entries())
    .map(([month, data]) => ({
      month,
      projects: data.projects,
      installs: data.installs,
      cancellations: data.cancellations,
      holds: data.holds,
      avgSystemSize: data.projects > 0 ? data.totalSystemSize / data.projects : 0,
      avgNetPpw: data.projects > 0 ? data.totalNetPpw / data.projects : 0,
    }))
    .sort((a, b) => a.month.localeCompare(b.month));
}
