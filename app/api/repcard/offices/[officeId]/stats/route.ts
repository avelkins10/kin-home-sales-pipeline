import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getQualityMetricsForOffice } from '@/lib/repcard/qualityMetrics';
import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';
import type { OfficeStatsResponse, GracefulDegradationResponse } from '@/lib/repcard/types';

export const runtime = 'nodejs';

// Cache implementation
// NOTE: In-memory cache won't persist across serverless invocations
// For production, consider using Redis (Upstash) for cache persistence
const officeStatsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 1800000; // 30 minutes in milliseconds
const MAX_CACHE_ENTRIES = 50; // Fewer offices than users

// Calculate date range based on timeRange
function calculateDateRange(timeRange: string, startDate?: string, endDate?: string) {
  const now = new Date();
  let calculatedStartDate: string;
  let calculatedEndDate: string;
  
  switch (timeRange) {
    case 'today':
      calculatedStartDate = now.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
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
    case 'quarter':
      const quarterStart = new Date(now.getFullYear(), Math.floor(now.getMonth() / 3) * 3, 1);
      calculatedStartDate = quarterStart.toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
      break;
    case 'ytd':
      calculatedStartDate = new Date(now.getFullYear(), 0, 1).toISOString().split('T')[0];
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
      calculatedStartDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
  }
  
  return { startDate: calculatedStartDate, endDate: calculatedEndDate };
}

// Clean up expired cache entries
function cleanCache() {
  const now = Date.now();
  for (const [key, value] of officeStatsCache.entries()) {
    if (now - value.timestamp > CACHE_TTL) {
      officeStatsCache.delete(key);
    }
  }
  
  // LRU eviction if cache is too large
  if (officeStatsCache.size > MAX_CACHE_ENTRIES) {
    const entries = Array.from(officeStatsCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    const toDelete = entries.slice(0, officeStatsCache.size - MAX_CACHE_ENTRIES);
    toDelete.forEach(([key]) => officeStatsCache.delete(key));
  }
}

export async function GET(
  request: NextRequest,
  { params }: { params: { officeId: string } }
) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;
  
  try {
    logApiRequest('GET', path, { endpoint: 'repcard-office-stats', requestId });
    
    // Authentication - only managers can view office stats
    const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;
    
    // Decode officeId (handle URL encoding)
    const officeName = decodeURIComponent(params.officeId);
    
    // Authorization check - office leaders can only view their own office
    if (auth.session.user.role === 'office_leader') {
      // Ensure salesOffice is an array and handle missing data gracefully
      const userOffices = Array.isArray(auth.session.user.salesOffice) 
        ? auth.session.user.salesOffice 
        : [];
      
      if (userOffices.length === 0) {
        // If no office data available, skip restriction for office_leader role
        // This allows office leaders to access stats when office data is missing
        console.warn(`Office leader ${auth.session.user.id} has no salesOffice data, allowing access`);
      } else if (!userOffices.includes(officeName)) {
        const duration = Date.now() - start;
        logApiResponse('GET', path, duration, { status: 403, cached: false, requestId });
        return NextResponse.json(
          { error: 'Unauthorized: You can only view your own office stats' },
          { status: 403 }
        );
      }
    }
    
    // Extract and validate parameters
    const { searchParams } = new URL(request.url);
    const timeRange = searchParams.get('timeRange');
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');
    
    // Calculate date range based on timeRange
    let startDate: string;
    let endDate: string;
    
    if (timeRange) {
      try {
        const dateRange = calculateDateRange(timeRange, startDateParam, endDateParam);
        startDate = dateRange.startDate;
        endDate = dateRange.endDate;
      } catch (error) {
        const duration = Date.now() - start;
        logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
        return NextResponse.json(
          { error: error instanceof Error ? error.message : 'Invalid date range' },
          { status: 400 }
        );
      }
    } else {
      startDate = startDateParam || new Date(new Date().getFullYear(), new Date().getMonth(), 1).toISOString().split('T')[0];
      endDate = endDateParam || new Date().toISOString().split('T')[0];
    }
    
    // Validate date format
    const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
    if (!dateRegex.test(startDate) || !dateRegex.test(endDate)) {
      return NextResponse.json(
        { error: 'Invalid date format. Use YYYY-MM-DD' },
        { status: 400 }
      );
    }
    
    // Validate date range
    if (new Date(startDate) > new Date(endDate)) {
      return NextResponse.json(
        { error: 'startDate must be less than or equal to endDate' },
        { status: 400 }
      );
    }
    
    // Build cache key
    const cacheKey = `${officeName}:${startDate}:${endDate}`;
    
    // Check cache
    cleanCache();
    const cached = officeStatsCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      // Log duration even on cached responses for consistency
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: true, requestId });
      return NextResponse.json({
        ...cached.data,
        metadata: {
          ...cached.data.metadata,
          cached: true
        }
      });
    }
    
    // Office validation
    const officeCountResult = await sql`
      SELECT COUNT(*) as count
      FROM users
      WHERE office = ${officeName}
    `;
    const officeCount = Array.from(officeCountResult);

    if (officeCount[0].count === 0) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 404, cached: false, requestId });
      return NextResponse.json(
        { error: 'Office not found or has no users' },
        { status: 404 }
      );
    }
    
    // Fetch office users with RepCard IDs
    const officeUsersResult = await sql`
      SELECT id, name, email, repcard_user_id, role
      FROM users
      WHERE office = ${officeName} AND repcard_user_id IS NOT NULL
    `;
    const officeUsers = Array.from(officeUsersResult);

    if (officeUsers.length === 0) {
      const response: GracefulDegradationResponse = {
        hasRepcardData: false,
        message: 'No users in this office are linked to RepCard',
        officeName
      };
      
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
      return NextResponse.json(response);
    }
    
    // Fetch aggregate quality metrics
    let qualityMetrics;
    try {
      qualityMetrics = await getQualityMetricsForOffice(officeName, startDate, endDate);
    } catch (error) {
      logError('repcard-office-stats', error as Error, { requestId, context: 'quality metrics fetch for office' });
      // Continue with partial data
      qualityMetrics = {
        appointmentSpeed: { percentage: 0, averageHours: 0 },
        attachmentRate: { percentage: 0 },
        rescheduleRate: { average: 0 },
        followUpConsistency: { percentage: 0 },
        compositeScore: 0,
        topPerformers: []
      };
    }
    
    // Fetch office-level volume stats in parallel
    const repcardUserIds = officeUsers.map(u => u.repcard_user_id);
    const userEmails = officeUsers.map(u => u.email);
    
    const [repcardCustomers, repcardAppointments, quickbaseProjects] = await Promise.all([
      // Fetch customers with pagination
      (async () => {
        const allCustomers = [];
        let page = 1;
        let hasMore = true;
        
        while (hasMore) {
          try {
            const response = await repcardClient.getCustomers({
              page,
              perPage: 100
            });
            allCustomers.push(...response.result.data);
            hasMore = response.result.currentPage < response.result.lastPage;
            page++;
          } catch (error) {
            logError('repcard-office-stats', error as Error, { requestId, context: 'RepCard customers fetch' });
            return [];
          }
        }
        return allCustomers;
      })(),
      // Fetch appointments with pagination
      (async () => {
        const allAppointments = [];
        let page = 1;
        let hasMore = true;
        
        while (hasMore) {
          try {
            const response = await repcardClient.getAppointments({
              setterIds: repcardUserIds.join(','),
              fromDate: startDate,
              toDate: endDate,
              page,
              perPage: 100
            });
            allAppointments.push(...response.result.data);
            hasMore = response.result.currentPage < response.result.lastPage;
            page++;
          } catch (error) {
            logError('repcard-office-stats', error as Error, { requestId, context: 'RepCard appointments fetch' });
            return [];
          }
        }
        return allAppointments;
      })(),
      (async () => {
        try {
          const result = await sql`
            SELECT closer_email, system_cost, date_closed
            FROM projects
            WHERE closer_email = ANY(${userEmails})
              AND date_closed >= ${startDate}
              AND date_closed <= ${endDate}
          `;
          return Array.from(result);
        } catch (error) {
          logError('repcard-office-stats', error as Error, { requestId, context: 'QuickBase projects fetch' });
          return [];
        }
      })()
    ]);
    
    // Calculate office metrics
    const totalDoorsKnocked = repcardCustomers?.length || 0;
    const totalAppointmentsSet = repcardAppointments?.length || 0;
    const totalSalesClosed = quickbaseProjects?.length || 0;
    const totalRevenue = quickbaseProjects?.reduce((sum, p) => sum + Number(p.system_cost || 0), 0) || 0;
    
    // Calculate averages per rep
    const teamSize = officeUsers.length;
    const averagePerRep = {
      doorsKnocked: teamSize > 0 ? totalDoorsKnocked / teamSize : 0,
      appointmentsSet: teamSize > 0 ? totalAppointmentsSet / teamSize : 0,
      salesClosed: teamSize > 0 ? totalSalesClosed / teamSize : 0,
      revenue: teamSize > 0 ? totalRevenue / teamSize : 0
    };
    
    // Build response
    const response: OfficeStatsResponse = {
      officeName,
      teamSize,
      volumeStats: {
        totalDoorsKnocked,
        totalAppointmentsSet,
        totalSalesClosed,
        totalRevenue
      },
      qualityStats: {
        appointmentSpeed: {
          percentage: qualityMetrics.appointmentSpeed.percentage,
          averageHours: qualityMetrics.appointmentSpeed.averageHours
        },
        attachmentRate: {
          percentage: qualityMetrics.attachmentRate.percentage
        },
        rescheduleRate: {
          average: qualityMetrics.rescheduleRate.average
        },
        followUpConsistency: {
          percentage: qualityMetrics.followUpConsistency.percentage
        },
        compositeScore: qualityMetrics.compositeScore
      },
      averagePerRep,
      topPerformers: qualityMetrics.topPerformers || [],
      metadata: {
        startDate,
        endDate,
        timeRange,
        cached: false,
        calculatedAt: new Date().toISOString()
      }
    };
    
    // Cache result
    officeStatsCache.set(cacheKey, {
      data: response,
      timestamp: Date.now()
    });
    
    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(response);
    
  } catch (error) {
    const duration = Date.now() - start;
    logError('repcard-office-stats', error as Error, { requestId, context: 'repcard-office-stats' });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      { 
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
        stack: process.env.NODE_ENV === 'development' ? (error instanceof Error ? error.stack : undefined) : undefined
      },
      { status: 500 }
    );
  }
}
