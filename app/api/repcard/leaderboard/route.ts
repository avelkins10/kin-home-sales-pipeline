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

    // Query users table (master table) - users should have repcard_user_id linked from sync
    if (officeIds && officeIds.length > 0) {
      // Use sql.query for office filtering with proper array parameters
      // Fix: Use LEFT JOIN to handle NULL sales_office, but only include users where office matches
      if (role !== 'all') {
        const result = await sql.query(
          `SELECT DISTINCT u.id, u.name, u.email, u.repcard_user_id, u.sales_office[1] as office, u.role
           FROM users u
           LEFT JOIN offices o ON o.name = ANY(u.sales_office)
           WHERE u.repcard_user_id IS NOT NULL
             AND u.role = $1
             AND o.quickbase_office_id = ANY($2::int[])
           LIMIT 1000`,
          [role, officeIds]
        );
        users = result.rows;
      } else {
        const result = await sql.query(
          `SELECT DISTINCT u.id, u.name, u.email, u.repcard_user_id, u.sales_office[1] as office, u.role
           FROM users u
           LEFT JOIN offices o ON o.name = ANY(u.sales_office)
           WHERE u.repcard_user_id IS NOT NULL
             AND o.quickbase_office_id = ANY($1::int[])
           LIMIT 1000`,
          [officeIds]
        );
        users = result.rows;
      }
      
      // If office filtering returns 0 users, fall back to all users (office filter might be too restrictive)
      // BUT also log WHY it's filtering out users
      if (users.length === 0) {
        console.log(`[RepCard Leaderboard] ⚠️ Office filter returned 0 users for officeIds: ${officeIds}`);
        console.log(`[RepCard Leaderboard] Falling back to all users to ensure data is shown`);
        
        // Debug: Check how many users match office filter vs total
        const totalUsers = await sql`
          SELECT COUNT(*) as count FROM users WHERE repcard_user_id IS NOT NULL
        `;
        const total = Array.isArray(totalUsers) ? totalUsers[0]?.count : totalUsers.rows?.[0]?.count || 0;
        console.log(`[RepCard Leaderboard] Total users with repcard_user_id: ${total}`);
        
        if (role !== 'all') {
          const result = await sql`
            SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
            FROM users
            WHERE repcard_user_id IS NOT NULL AND role = ${role}
            LIMIT 1000
          `;
          users = Array.from(result);
        } else {
          const result = await sql`
            SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
            FROM users
            WHERE repcard_user_id IS NOT NULL
            LIMIT 1000
          `;
          users = Array.from(result);
        }
        console.log(`[RepCard Leaderboard] ✅ Fallback returned ${users.length} users`);
      }
    } else {
      // No office filter - use sql template
      if (role !== 'all') {
        const result = await sql`
          SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
          FROM users
          WHERE repcard_user_id IS NOT NULL AND role = ${role}
          LIMIT 1000
        `;
        users = Array.from(result);
      } else {
        const result = await sql`
          SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
          FROM users
          WHERE repcard_user_id IS NOT NULL
          LIMIT 1000
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
        // Fix: Query customers first, then join to users to filter by role
        // This ensures we don't miss customers created by closers who have setter_user_id
        const customerCountsRaw = await sql`
          SELECT
            u.id as user_id,
            u.name as user_name,
            u.email as user_email,
            u.repcard_user_id::text as repcard_user_id,
            u.sales_office[1] as office,
            u.role,
            COUNT(c.repcard_customer_id) as count
          FROM repcard_customers c
          INNER JOIN users u ON u.repcard_user_id::text = c.setter_user_id::text
          ${officeIds && officeIds.length > 0 ? sql`
            LEFT JOIN offices o ON o.name = ANY(u.sales_office)
          ` : sql``}
          WHERE u.repcard_user_id IS NOT NULL
            AND c.created_at >= ${calculatedStartDate}::timestamp
            AND c.created_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day')
            ${role !== 'all' ? sql`AND u.role = ${role}` : sql``}
            ${officeIds && officeIds.length > 0 ? sql`AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR u.sales_office IS NULL)` : sql``}
          GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
        `;
        const customerCounts = Array.from(customerCountsRaw);

        // Map directly to leaderboard entries
        leaderboardEntries = customerCounts.map((row: any) => ({
          rank: 0,
          userId: row.user_id,
          userName: row.user_name,
          userEmail: row.user_email,
          office: row.office,
          role: row.role,
          metricValue: parseInt(row.count),
          metricType: metric
        }));
        
        // If no results and role filter is applied, also check users that might have been filtered out
        if (leaderboardEntries.length === 0 && role !== 'all') {
          // Fallback: get all users and create entries with 0 counts
          const allUsers = await sql`
            SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
            FROM users
            WHERE repcard_user_id IS NOT NULL AND role = ${role}
            LIMIT 1000
          `;
          leaderboardEntries = Array.from(allUsers).map((user: any) => ({
            rank: 0,
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue: 0,
            metricType: metric
          }));
        }
      } else if (metric === 'appointments_set') {
        // Query database for appointments (much faster than API, no rate limits)
        // Fix: Query appointments first, then join to users to filter by role
        const appointmentCountsRaw = await sql`
          SELECT
            u.id as user_id,
            u.name as user_name,
            u.email as user_email,
            u.repcard_user_id::text as repcard_user_id,
            u.sales_office[1] as office,
            u.role,
            COUNT(a.repcard_appointment_id) as count
          FROM repcard_appointments a
          INNER JOIN users u ON u.repcard_user_id::text = a.setter_user_id::text
          ${officeIds && officeIds.length > 0 ? sql`
            LEFT JOIN offices o ON o.name = ANY(u.sales_office)
          ` : sql``}
          WHERE u.repcard_user_id IS NOT NULL
            AND (
              (a.scheduled_at IS NOT NULL AND a.scheduled_at >= ${calculatedStartDate}::timestamp AND a.scheduled_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day'))
              OR
              (a.scheduled_at IS NULL AND a.created_at >= ${calculatedStartDate}::timestamp AND a.created_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day'))
            )
            ${role !== 'all' ? sql`AND u.role = ${role}` : sql``}
            ${officeIds && officeIds.length > 0 ? sql`AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR u.sales_office IS NULL)` : sql``}
          GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
        `;
        const appointmentCounts = Array.from(appointmentCountsRaw);

        // Map directly to leaderboard entries
        leaderboardEntries = appointmentCounts.map((row: any) => ({
          rank: 0,
          userId: row.user_id,
          userName: row.user_name,
          userEmail: row.user_email,
          office: row.office,
          role: row.role,
          metricValue: parseInt(row.count),
          metricType: metric
        }));
        
        // CRITICAL FIX: If INNER JOIN returns 0 results, fallback to show ALL users
        if (leaderboardEntries.length === 0) {
          console.log(`[RepCard Leaderboard] ⚠️ INNER JOIN returned 0 entries for appointments_set`);
          console.log(`[RepCard Leaderboard] Falling back to LEFT JOIN to show all users`);
          
          const fallbackQuery = await sql`
            SELECT
              u.id as user_id,
              u.name as user_name,
              u.email as user_email,
              u.repcard_user_id::text as repcard_user_id,
              u.sales_office[1] as office,
              u.role,
              COUNT(a.repcard_appointment_id) as count
            FROM users u
            LEFT JOIN repcard_appointments a ON u.repcard_user_id::text = a.setter_user_id::text
              AND (
                (a.scheduled_at IS NOT NULL AND a.scheduled_at >= ${calculatedStartDate}::timestamp AND a.scheduled_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day'))
                OR
                (a.scheduled_at IS NULL AND a.created_at >= ${calculatedStartDate}::timestamp AND a.created_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day'))
              )
            ${officeIds && officeIds.length > 0 ? sql`
              LEFT JOIN offices o ON o.name = ANY(u.sales_office)
            ` : sql``}
            WHERE u.repcard_user_id IS NOT NULL
              ${role !== 'all' ? sql`AND u.role = ${role}` : sql``}
              ${officeIds && officeIds.length > 0 ? sql`AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR u.sales_office IS NULL)` : sql``}
            GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
            ORDER BY count DESC
            LIMIT 1000
          `;
          const fallbackResults = Array.from(fallbackQuery);
          leaderboardEntries = fallbackResults.map((row: any) => ({
            rank: 0,
            userId: row.user_id,
            userName: row.user_name,
            userEmail: row.user_email,
            office: row.office,
            role: row.role,
            metricValue: parseInt(row.count) || 0,
            metricType: metric
          }));
          console.log(`[RepCard Leaderboard] ✅ Fallback returned ${leaderboardEntries.length} users`);
        }
        
        // If no results and role filter is applied, also check users that might have been filtered out
        if (leaderboardEntries.length === 0 && role !== 'all') {
          // Fallback: get all users and create entries with 0 counts
          const allUsers = await sql`
            SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
            FROM users
            WHERE repcard_user_id IS NOT NULL AND role = ${role}
            LIMIT 1000
          `;
          leaderboardEntries = Array.from(allUsers).map((user: any) => ({
            rank: 0,
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue: 0,
            metricType: metric
          }));
        }
      } else if (metric === 'sales_closed' || metric === 'revenue') {
        // Query synced status logs from database (much faster than API calls)
        // Filter for "sold" statuses - check both old_status and new_status transitions
        // Fix: Cast changed_by_user_id to TEXT for comparison
        const statusLogsRaw = await sql`
          SELECT
            sl.changed_by_user_id::text as changed_by_user_id,
            sl.repcard_customer_id,
            sl.new_status,
            sl.old_status,
            sl.changed_at,
            c.raw_data->'customFields'->>'systemCost' as system_cost
          FROM repcard_status_logs sl
          LEFT JOIN repcard_customers c ON c.repcard_customer_id = sl.repcard_customer_id
          WHERE sl.changed_by_user_id::text = ANY(${repcardUserIds.map(String)}::text[])
            AND sl.changed_at >= ${calculatedStartDate}::timestamp
            AND sl.changed_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day')
            AND (
              LOWER(sl.new_status) LIKE '%sold%' OR
              LOWER(sl.new_status) LIKE '%closed%' OR
              LOWER(sl.new_status) LIKE '%won%' OR
              LOWER(sl.new_status) LIKE '%install%'
            )
        `;
        const statusLogs = Array.from(statusLogsRaw);

        // Group sales by user
        const salesByUser = new Map<string, { count: number; revenue: number }>();
        
        for (const log of statusLogs) {
          const userId = log.changed_by_user_id?.toString();
          if (!userId) continue;
          
          if (!salesByUser.has(userId)) {
            salesByUser.set(userId, { count: 0, revenue: 0 });
          }
          
          const userSales = salesByUser.get(userId)!;
          userSales.count++;
          
          // Add revenue if available
          if (log.system_cost) {
            const cost = parseFloat(log.system_cost) || 0;
            userSales.revenue += cost;
          }
        }

        leaderboardEntries = (users as any[]).map((user: any) => {
          const userSales = salesByUser.get(user.repcard_user_id?.toString()) || { count: 0, revenue: 0 };
          
          return {
            rank: 0,
            userId: user.id,
            userName: user.name,
            userEmail: user.email,
            office: user.office,
            role: user.role,
            metricValue: metric === 'sales_closed' ? userSales.count : userSales.revenue,
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
    
    // Debug: Log final results
    console.log(`[RepCard Leaderboard] Final: ${leaderboardEntries.length} entries, top 3:`, 
      leaderboardEntries.slice(0, 3).map(e => `${e.userName}: ${e.metricValue}`).join(', '));
    
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
