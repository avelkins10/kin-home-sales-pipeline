import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getQualityMetricsForUsers, calculateCompositeQualityScore } from '@/lib/repcard/qualityMetrics';
import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';
import type { LeaderboardMetric, LeaderboardRole, LeaderboardEntry, LeaderboardResponse } from '@/lib/repcard/types';

export const runtime = 'nodejs';

// Cache implementation
// NOTE: In-memory cache won't persist across serverless invocations
// For production, consider using Redis (Upstash) for cache persistence
const leaderboardCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 1800000; // 30 minutes in milliseconds (reduced API calls to avoid rate limiting)
const MAX_CACHE_ENTRIES = 100;

// Type definitions are now imported from lib/repcard/types

// Clean up expired cache entries
function cleanCache() {
  const now = Date.now();
  for (const [key, value] of Array.from(leaderboardCache.entries())) {
    if (now - value.timestamp > CACHE_TTL) {
      leaderboardCache.delete(key);
    }
  }
  
  // LRU eviction if cache is too large
  if (leaderboardCache.size > MAX_CACHE_ENTRIES) {
    const entries = Array.from(leaderboardCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    const toDelete = entries.slice(0, leaderboardCache.size - MAX_CACHE_ENTRIES);
    toDelete.forEach(([key]) => leaderboardCache.delete(key));
  }
}

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
    default:
      calculatedStartDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
      calculatedEndDate = now.toISOString().split('T')[0];
  }
  
  return { startDate: calculatedStartDate, endDate: calculatedEndDate };
}

export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;
  
  try {
    logApiRequest('GET', path, { endpoint: 'repcard-leaderboard', requestId });
    
    // Authentication - accessible by all authenticated users
    const session = await getServerSession(authOptions);
    if (!session) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 401, cached: false, requestId });
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }
    
    // Extract and validate parameters
    const { searchParams } = new URL(request.url);
    const role = (searchParams.get('role') || 'all') as LeaderboardRole;
    const metric = (searchParams.get('metric') || 'quality_score') as LeaderboardMetric;
    const timeRange = searchParams.get('timeRange') || 'month';
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    const officeIds = searchParams.get('officeIds')?.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id));
    const limit = Math.min(parseInt(searchParams.get('limit') || '50'), 100);
    const page = Math.max(parseInt(searchParams.get('page') || '1'), 1);
    
    // Validate parameters
    const validRoles: LeaderboardRole[] = ['setter', 'closer', 'all'];
    const validMetrics: LeaderboardMetric[] = ['doors_knocked', 'appointments_set', 'sales_closed', 'revenue', 'quality_score', 'appointment_speed', 'attachment_rate'];
    const validTimeRanges = ['today', 'week', 'month', 'quarter', 'ytd', 'custom', 'last_30', 'last_90', 'last_12_months'];
    
    if (!validRoles.includes(role)) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
      return NextResponse.json(
        { error: `Invalid role. Must be one of: ${validRoles.join(', ')}` },
        { status: 400 }
      );
    }
    
    if (!validMetrics.includes(metric)) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
      return NextResponse.json(
        { error: `Invalid metric. Must be one of: ${validMetrics.join(', ')}` },
        { status: 400 }
      );
    }
    
    if (!validTimeRanges.includes(timeRange)) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
      return NextResponse.json(
        { error: `Invalid timeRange. Must be one of: ${validTimeRanges.join(', ')}` },
        { status: 400 }
      );
    }
    
    // Calculate date range
    let calculatedStartDate: string;
    let calculatedEndDate: string;
    try {
      const dateRange = calculateDateRange(timeRange, startDate, endDate);
      calculatedStartDate = dateRange.startDate;
      calculatedEndDate = dateRange.endDate;
    } catch (error) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
      return NextResponse.json(
        { error: error instanceof Error ? error.message : 'Invalid date range' },
        { status: 400 }
      );
    }
    
    // Build cache key
    const cacheKey = `${role}:${metric}:${timeRange}:${calculatedStartDate}:${calculatedEndDate}:${officeIds ? officeIds.map(String).join(',') : 'all'}:${limit}:${page}`;
    
    // Check cache
    cleanCache();
    const cached = leaderboardCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
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
    
    // Fetch users
    let users: any[];

    if (officeIds && officeIds.length > 0) {
      // Use sql.query for office filtering with proper array parameters
      if (role !== 'all') {
        const result = await sql.query(
          `SELECT DISTINCT u.id, u.name, u.email, u.repcard_user_id, u.sales_office[1] as office, u.role
           FROM users u
           JOIN offices o ON o.name = ANY(u.sales_office)
           WHERE u.repcard_user_id IS NOT NULL
             AND u.role = $1
             AND o.quickbase_office_id = ANY($2::int[])`,
          [role, officeIds]
        );
        users = result.rows;
      } else {
        const result = await sql.query(
          `SELECT DISTINCT u.id, u.name, u.email, u.repcard_user_id, u.sales_office[1] as office, u.role
           FROM users u
           JOIN offices o ON o.name = ANY(u.sales_office)
           WHERE u.repcard_user_id IS NOT NULL
             AND o.quickbase_office_id = ANY($1::int[])`,
          [officeIds]
        );
        users = result.rows;
      }
    } else {
      // No office filter - use sql template
      if (role !== 'all') {
        const result = await sql`
          SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
          FROM users
          WHERE repcard_user_id IS NOT NULL AND role = ${role}
        `;
        users = Array.from(result);
      } else {
        const result = await sql`
          SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
          FROM users
          WHERE repcard_user_id IS NOT NULL
        `;
        users = Array.from(result);
      }
    }
    
    if (users.length === 0) {
      const response: LeaderboardResponse = {
        leaderboard: [],
        metadata: {
          role,
          metric,
          timeRange,
          startDate: calculatedStartDate,
          endDate: calculatedEndDate,
          officeIds: officeIds ? officeIds.map(String) : undefined,
          totalEntries: 0,
          page,
          limit,
          totalPages: 0,
          cached: false,
          calculatedAt: new Date().toISOString()
        }
      };
      
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
      return NextResponse.json(response);
    }
    
    // Fetch metric data based on selected metric
    let leaderboardEntries: LeaderboardEntry[] = [];
    
    if (metric === 'quality_score' || metric === 'appointment_speed' || metric === 'attachment_rate') {
      // Use quality metrics service - call per user to get individual metrics
      const userMetricsPromises = users.map(async (user) => {
        try {
          const userMetrics = await getQualityMetricsForUsers({
            repcardUserIds: [user.repcard_user_id],
            startDate: calculatedStartDate,
            endDate: calculatedEndDate,
            useCache: true
          });
          
          let metricValue = 0;
          if (metric === 'quality_score') {
            metricValue = calculateCompositeQualityScore(userMetrics);
          } else if (metric === 'appointment_speed') {
            metricValue = userMetrics.appointmentSpeed.percentageWithin24Hours;
          } else if (metric === 'attachment_rate') {
            metricValue = userMetrics.attachmentRate.percentageWithAttachments;
          }
          
          return {
            rank: 0, // Will be set after sorting
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue,
            metricType: metric
          };
        } catch (error) {
          // Return zero metrics for failed users
          return {
            rank: 0,
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue: 0,
            metricType: metric
          };
        }
      });
      
      leaderboardEntries = await Promise.all(userMetricsPromises);
    } else {
      // Fetch data from RepCard or QuickBase
      const repcardUserIds = (users as any[]).map((u: any) => u.repcard_user_id);
      
      if (metric === 'doors_knocked') {
        // Query database for customers (much faster than API, no rate limits)
        const customerCountsRaw = await sql`
          SELECT
            setter_user_id,
            COUNT(*) as count
          FROM repcard_customers
          WHERE setter_user_id = ANY(${repcardUserIds.map(String)}::text[])
            AND created_at >= ${calculatedStartDate}::timestamp
            AND created_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day')
          GROUP BY setter_user_id
        `;
        const customerCounts = Array.from(customerCountsRaw);

        // Create lookup map for fast access
        const countsMap = new Map(
          customerCounts.map((row: any) => [row.setter_user_id, parseInt(row.count)])
        );

        // Count customers per user (doors knocked = customers created by user)
        leaderboardEntries = (users as any[]).map((user: any) => {
          const count = countsMap.get(user.repcard_user_id?.toString()) || 0;
          return {
            rank: 0,
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue: count,
            metricType: metric
          };
        });
      } else if (metric === 'appointments_set') {
        // Query database for appointments (much faster than API, no rate limits)
        const appointmentCountsRaw = await sql`
          SELECT
            setter_user_id,
            COUNT(*) as count
          FROM repcard_appointments
          WHERE setter_user_id = ANY(${repcardUserIds.map(String)}::text[])
            AND appointment_date >= ${calculatedStartDate}::date
            AND appointment_date <= ${calculatedEndDate}::date
          GROUP BY setter_user_id
        `;
        const appointmentCounts = Array.from(appointmentCountsRaw);

        // Create lookup map for fast access
        const countsMap = new Map(
          appointmentCounts.map((row: any) => [row.setter_user_id, parseInt(row.count)])
        );

        leaderboardEntries = (users as any[]).map((user: any) => {
          const count = countsMap.get(user.repcard_user_id?.toString()) || 0;
          return {
            rank: 0,
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue: count,
            metricType: metric
          };
        });
      } else if (metric === 'sales_closed' || metric === 'revenue') {
        // Fetch customer status logs from RepCard to find sales
        // NOTE: We'll need to filter for "sold" or "closed" status types
        // For now, fetch all customers and their status logs
        let allStatusLogs: any[] = [];
        let page = 1;
        let hasMore = true;

        while (hasMore) {
          try {
            const response = await repcardClient.getCustomerStatusLogs({
              userIds: repcardUserIds.join(','),
              fromDate: calculatedStartDate,
              toDate: calculatedEndDate,
              page,
              perPage: 100
            });

            allStatusLogs.push(...response.result.data);
            hasMore = response.result.currentPage < (response.result.totalPages || 1);
            page++;
          } catch (error) {
            console.error(`Failed to fetch status logs page ${page}:`, error);
            hasMore = false;
          }
        }

        // Filter for "sold" statuses (customize based on actual status names in RepCard)
        // Common status names: "Sold", "Closed", "Won", "Installed", etc.
        const soldStatuses = allStatusLogs.filter((log: any) => {
          const statusName = log.statusTo?.statusName?.toLowerCase() || '';
          return statusName.includes('sold') ||
                 statusName.includes('closed') ||
                 statusName.includes('won') ||
                 statusName.includes('install');
        });

        // For revenue, we need to fetch the customers to get systemCost
        const soldCustomerIds = [...new Set(soldStatuses.map((log: any) => log.customerId))];
        const customers = await Promise.all(
          soldCustomerIds.slice(0, 50).map(async (customerId: number) => { // Limit to avoid too many requests
            try {
              const customer = await repcardClient.getCustomerById(customerId);
              return customer.result;
            } catch (error) {
              console.error(`Failed to fetch customer ${customerId}:`, error);
              return null;
            }
          })
        );

        leaderboardEntries = (users as any[]).map((user: any) => {
          // Find sales for this user
          const userSales = soldStatuses.filter((log: any) =>
            log.userId?.toString() === user.repcard_user_id?.toString()
          );

          let metricValue = 0;
          if (metric === 'sales_closed') {
            metricValue = userSales.length;
          } else {
            // Revenue: sum up systemCost from customers
            const userCustomerIds = userSales.map((log: any) => log.customerId);
            metricValue = customers
              .filter((c: any) => c && userCustomerIds.includes(c.id))
              .reduce((sum: number, c: any) =>
                sum + (c.customFields?.systemCost || 0), 0
              );
          }

          return {
            rank: 0,
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue,
            metricType: metric
          };
        });
      }
    }
    
    // Sort by metric value (descending) and assign ranks
    leaderboardEntries.sort((a, b) => b.metricValue - a.metricValue);
    leaderboardEntries.forEach((entry, index) => {
      entry.rank = index + 1;
    });
    
    // Apply pagination
    const totalEntries = leaderboardEntries.length;
    const totalPages = Math.ceil(totalEntries / limit);
    const offset = (page - 1) * limit;
    const paginatedEntries = leaderboardEntries.slice(offset, offset + limit);
    
    // Build response
    const response: LeaderboardResponse = {
      leaderboard: paginatedEntries,
      metadata: {
        role,
        metric,
        timeRange,
        startDate: calculatedStartDate,
        endDate: calculatedEndDate,
        officeIds: officeIds ? officeIds.map(String) : undefined,
        totalEntries,
        page,
        limit,
        totalPages,
        cached: false,
        calculatedAt: new Date().toISOString()
      }
    };
    
    // Cache result
    leaderboardCache.set(cacheKey, {
      data: response,
      timestamp: Date.now()
    });
    
    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(response);
    
  } catch (error) {
    const duration = Date.now() - start;
    logError('repcard-leaderboard', error as Error, { requestId, context: 'repcard-leaderboard' });
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
