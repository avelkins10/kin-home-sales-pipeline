import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { getQualityMetricsForUsers } from '@/lib/repcard/qualityMetrics';
import { sql } from '@/lib/db/client';

export const runtime = 'nodejs';

// Cache implementation
const qualityCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 3600000; // 60 minutes (quality metrics change less frequently)
const MAX_CACHE_ENTRIES = 100;

function cleanCache() {
  const now = Date.now();
  for (const [key, value] of Array.from(qualityCache.entries())) {
    if (now - value.timestamp > CACHE_TTL) {
      qualityCache.delete(key);
    }
  }

  if (qualityCache.size > MAX_CACHE_ENTRIES) {
    const entries = Array.from(qualityCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    const toDelete = entries.slice(0, qualityCache.size - MAX_CACHE_ENTRIES);
    toDelete.forEach(([key]) => qualityCache.delete(key));
  }
}

// Calculate date range from timeRange parameter
function calculateDateRange(timeRange: string, startDate?: string, endDate?: string) {
  const now = new Date();
  let calculatedStartDate: string;
  let calculatedEndDate: string;

  switch (timeRange) {
    case 'week':
      const weekStart = new Date(now);
      weekStart.setDate(now.getDate() - now.getDay());
      calculatedStartDate = weekStart.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'month':
      calculatedStartDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'last_month':
      const lastMonthStart = new Date(now.getFullYear(), now.getMonth() - 1, 1);
      const lastMonthEnd = new Date(now.getFullYear(), now.getMonth(), 0);
      calculatedStartDate = lastMonthStart.toISOString().split('T')[0];
      calculatedEndDate = lastMonthEnd.toISOString().split('T')[0];
      break;
    case 'ytd':
      calculatedStartDate = new Date(now.getFullYear(), 0, 1).toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'last_30':
      const last30 = new Date(now);
      last30.setDate(now.getDate() - 30);
      calculatedStartDate = last30.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'last_90':
      const last90 = new Date(now);
      last90.setDate(now.getDate() - 90);
      calculatedStartDate = last90.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'last_12_months':
      const last12 = new Date(now);
      last12.setMonth(now.getMonth() - 12);
      calculatedStartDate = last12.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'lifetime':
      // Return a very old date for lifetime
      calculatedStartDate = '2000-01-01';
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'custom':
      if (!startDate || !endDate) {
        throw new Error('startDate and endDate are required for custom time range');
      }
      calculatedStartDate = startDate;
      calculatedEndDate = endDate;
      break;
    default:
      // Default to last 30 days
      const last30Default = new Date(now);
      last30Default.setDate(now.getDate() - 30);
      calculatedStartDate = last30Default.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
  }

  return { startDate: calculatedStartDate, endDate: calculatedEndDate };
}

export async function GET(request: NextRequest) {
  const start = Date.now();

  try {
    // Authentication
    const session = await getServerSession(authOptions);
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    // Extract parameters
    const { searchParams } = new URL(request.url);
    const timeRange = searchParams.get('timeRange') || 'last_30';
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    const officeIds = searchParams.get('officeIds')?.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id));

    // Calculate date range
    let calculatedStartDate: string;
    let calculatedEndDate: string;
    try {
      const dateRange = calculateDateRange(timeRange, startDate, endDate);
      calculatedStartDate = dateRange.startDate;
      calculatedEndDate = dateRange.endDate;
    } catch (error) {
      return NextResponse.json(
        { error: error instanceof Error ? error.message : 'Invalid date range' },
        { status: 400 }
      );
    }

    // Build cache key
    const cacheKey = `${timeRange}:${calculatedStartDate}:${calculatedEndDate}:${officeIds?.join(',') || 'all'}`;

    // Check cache
    cleanCache();
    const cached = qualityCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      return NextResponse.json({
        ...cached.data,
        metadata: { ...cached.data.metadata, cached: true }
      });
    }

    // Fetch users filtered by office if specified
    let users: any[];
    if (officeIds && officeIds.length > 0) {
      const result = await sql.query(
        `SELECT DISTINCT u.id, u.repcard_user_id
         FROM users u
         JOIN offices o ON o.name = ANY(u.sales_office)
         WHERE u.repcard_user_id IS NOT NULL
           AND o.quickbase_office_id = ANY($1::int[])`,
        [officeIds]
      );
      users = result.rows;
    } else {
      users = await sql`
        SELECT id, repcard_user_id
        FROM users
        WHERE repcard_user_id IS NOT NULL
      ` as unknown as any[];
    }

    if (users.length === 0) {
      return NextResponse.json({
        showRate: 0,
        sitRate: 0,
        closeRate: 0,
        followUpRate: 0,
        metadata: {
          timeRange,
          startDate: calculatedStartDate,
          endDate: calculatedEndDate,
          officeIds: officeIds ? officeIds.map(String) : undefined,
          totalUsers: 0,
          cached: false,
          calculatedAt: new Date().toISOString()
        }
      });
    }

    // Get RepCard user IDs
    const repcardUserIds = users
      .map(u => u.repcard_user_id)
      .filter(Boolean);

    if (repcardUserIds.length === 0) {
      return NextResponse.json({
        showRate: 0,
        sitRate: 0,
        closeRate: 0,
        followUpRate: 0,
        metadata: {
          timeRange,
          startDate: calculatedStartDate,
          endDate: calculatedEndDate,
          officeIds: officeIds ? officeIds.map(String) : undefined,
          totalUsers: 0,
          cached: false,
          calculatedAt: new Date().toISOString()
        }
      });
    }

    // Fetch quality metrics for all users (aggregated)
    const qualityMetrics = await getQualityMetricsForUsers({
      repcardUserIds,
      startDate: calculatedStartDate,
      endDate: calculatedEndDate,
      useCache: true
    });

    // Extract metrics
    const showRate = qualityMetrics.appointmentShowRate.percentageShowRate || 0;
    const sitRate = qualityMetrics.appointmentSitRate.percentageSitRate || 0;

    // Close rate calculation (closed appointments / total appointments)
    const totalAppointments = qualityMetrics.appointmentShowRate.totalAppointments || 1;
    const closeRate = qualityMetrics.followUpConsistency.totalFollowUpAppointments > 0
      ? (qualityMetrics.followUpConsistency.totalFollowUpAppointments / totalAppointments) * 100
      : 0;

    // Follow-up rate
    const followUpRate = qualityMetrics.followUpConsistency.percentageWithFollowUp || 0;

    const response = {
      showRate,
      sitRate,
      closeRate,
      followUpRate,
      metadata: {
        timeRange,
        startDate: calculatedStartDate,
        endDate: calculatedEndDate,
        officeIds: officeIds ? officeIds.map(String) : undefined,
        totalUsers: users.length,
        totalAppointments: qualityMetrics.appointmentShowRate.totalAppointments,
        cached: false,
        calculatedAt: new Date().toISOString()
      }
    };

    // Cache result
    qualityCache.set(cacheKey, {
      data: response,
      timestamp: Date.now()
    });

    const duration = Date.now() - start;
    console.log(`[Quality Aggregate API] Success in ${duration}ms`);

    return NextResponse.json(response);

  } catch (error) {
    const duration = Date.now() - start;
    console.error(`[Quality Aggregate API] Error after ${duration}ms:`, error);
    return NextResponse.json(
      { error: 'Internal server error', message: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
