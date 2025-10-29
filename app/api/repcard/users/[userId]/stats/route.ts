import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getQualityMetricsForUser } from '@/lib/repcard/qualityMetrics';
import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';
import type { UserStatsResponse, GracefulDegradationResponse } from '@/lib/repcard/types';

export const runtime = 'nodejs';

// Cache implementation
// NOTE: In-memory cache won't persist across serverless invocations
// For production, consider using Redis (Upstash) for cache persistence
const userStatsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 900000; // 15 minutes in milliseconds
const MAX_CACHE_ENTRIES = 100;

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
  for (const [key, value] of userStatsCache.entries()) {
    if (now - value.timestamp > CACHE_TTL) {
      userStatsCache.delete(key);
    }
  }
  
  // LRU eviction if cache is too large
  if (userStatsCache.size > MAX_CACHE_ENTRIES) {
    const entries = Array.from(userStatsCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    const toDelete = entries.slice(0, userStatsCache.size - MAX_CACHE_ENTRIES);
    toDelete.forEach(([key]) => userStatsCache.delete(key));
  }
}

export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;
  
  try {
    logApiRequest('GET', path, { endpoint: 'repcard-user-stats', requestId });
    
    // Authentication
    const auth = await requireRole(['closer', 'setter', 'office_leader', 'regional', 'super_admin']);
    if (!auth.authorized) return auth.response;
    
    // Authorization check - users can only view their own stats unless they're managers
    if (['closer', 'setter'].includes(auth.session.user.role) && params.userId !== auth.session.user.id) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 403, cached: false, requestId });
      return NextResponse.json(
        { error: 'Unauthorized: You can only view your own stats' },
        { status: 403 }
      );
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
    const cacheKey = `${params.userId}:${startDate}:${endDate}`;
    
    // Check cache
    cleanCache();
    const cached = userStatsCache.get(cacheKey);
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
    
    // User lookup
    const userResultRaw = await sql`
      SELECT id, name, email, repcard_user_id, sales_office[1] as office, role
      FROM users
      WHERE id = ${params.userId}
    `;
    const userResult = Array.from(userResultRaw);

    if (userResult.length === 0) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 404, cached: false, requestId });
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      );
    }
    
    const user = userResult[0];
    
    // Handle missing RepCard ID
    if (!user.repcard_user_id) {
      const response: GracefulDegradationResponse = {
        hasRepcardData: false,
        message: 'User not linked to RepCard',
        userId: user.id,
        userName: user.name,
        userEmail: user.email,
        officeName: user.office
      };
      
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
      return NextResponse.json(response);
    }
    
    // Fetch RepCard data from DATABASE (not API) for better performance
    // Use the synced data instead of calling RepCard API
    let repcardCustomers: any[] = [];
    let repcardAppointments: any[] = [];
    
    try {
      // Fetch customers from database
      const customersResult = await sql`
        SELECT 
          repcard_customer_id,
          name,
          email,
          phone,
          address,
          created_at,
          updated_at,
          raw_data
        FROM repcard_customers
        WHERE setter_user_id::text = ${String(user.repcard_user_id)}
          AND created_at >= ${startDate}::timestamp
          AND created_at <= (${endDate}::timestamp + INTERVAL '1 day')
        ORDER BY created_at DESC
        LIMIT 1000
      `;
      repcardCustomers = Array.from(customersResult);

      // Fetch appointments from database
      const appointmentsResult = await sql`
        SELECT 
          repcard_appointment_id,
          repcard_customer_id,
          scheduled_at,
          completed_at,
          disposition,
          status_category,
          created_at,
          updated_at,
          raw_data
        FROM repcard_appointments
        WHERE setter_user_id::text = ${String(user.repcard_user_id)}
          AND scheduled_at >= ${startDate}::timestamp
          AND scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')
        ORDER BY scheduled_at DESC
        LIMIT 1000
      `;
      repcardAppointments = Array.from(appointmentsResult);

      console.log(`[RepCard Stats] Fetched ${repcardCustomers.length} customers and ${repcardAppointments.length} appointments from database for user ${user.id}`);
    } catch (dbError) {
      logError('repcard-user-stats', dbError as Error, { requestId, context: 'Database fetch failed, falling back to API', userId: user.id });
      
      // Fallback to API if database query fails
      try {
        [repcardCustomers, repcardAppointments] = await Promise.all([
          // Fetch customers with pagination
          (async () => {
            const allCustomers = [];
            let page = 1;
            let hasMore = true;
            
            while (hasMore && page <= 10) { // Limit to 10 pages for timeout protection
              try {
                const response = await repcardClient.getCustomers({ 
                  userId: typeof user.repcard_user_id === 'string' ? parseInt(user.repcard_user_id) : user.repcard_user_id, 
                  page,
                  perPage: 100
                });
                allCustomers.push(...response.result.data);
                hasMore = response.result.currentPage < response.result.lastPage;
                page++;
              } catch (error) {
                logError('repcard-user-stats', error as Error, { requestId, context: 'RepCard customers fetch', userId: user.id });
                break;
              }
            }
            return allCustomers;
          })(),
          // Fetch appointments with pagination
          (async () => {
            const allAppointments = [];
            let page = 1;
            let hasMore = true;
            
            while (hasMore && page <= 10) { // Limit to 10 pages for timeout protection
              try {
                const response = await repcardClient.getAppointments({ 
                  setterIds: String(user.repcard_user_id),
                  fromDate: startDate, 
                  toDate: endDate,
                  page,
                  perPage: 100
                });
                allAppointments.push(...response.result.data);
                hasMore = response.result.currentPage < response.result.lastPage;
                page++;
              } catch (error) {
                logError('repcard-user-stats', error as Error, { requestId, context: 'RepCard appointments fetch', userId: user.id });
                break;
              }
            }
            return allAppointments;
          })()
        ]);
      } catch (apiError) {
        logError('repcard-user-stats', apiError as Error, { requestId, context: 'API fallback also failed', userId: user.id });
        repcardCustomers = [];
        repcardAppointments = [];
      }
    }
    
    // Fetch quality metrics (uses database queries internally)
    let qualityMetrics;
    try {
      qualityMetrics = await getQualityMetricsForUser(user.id, startDate, endDate);
    } catch (error) {
      logError('repcard-user-stats', error as Error, { requestId, context: 'Quality metrics fetch', userId: user.id });
      qualityMetrics = {
        appointmentSpeed: { percentageWithin24Hours: 0, averageHoursToSchedule: 0, totalAppointments: 0, appointmentsWithin24Hours: 0 },
        attachmentRate: { percentageWithAttachments: 0, totalAttachments: 0, totalCustomers: 0, customersWithAttachments: 0 },
        rescheduleRate: { averageReschedulesPerCustomer: 0, totalReschedules: 0, totalCustomers: 0, customersWithReschedules: 0 },
        followUpConsistency: { percentageWithFollowUps: 0, totalFollowUpAppointments: 0, customersRequiringFollowUps: 0, customersWithFollowUps: 0 },
        period: { startDate, endDate },
        calculatedAt: new Date().toISOString()
      };
    }
    
    // Fetch QuickBase data
    let quickbaseSales = { sales_count: 0, total_revenue: 0 };
    let quickbaseAppointments = { appointments_count: 0 };
    
    try {
      if (user.role === 'closer') {
        const salesResultRaw = await sql`
          SELECT COUNT(*)::int as sales_count, COALESCE(SUM(system_cost), 0)::numeric as total_revenue
          FROM projects
          WHERE closer_email = ${user.email}
            AND date_closed >= ${startDate}
            AND date_closed <= ${endDate}
        `;
        const salesResult = Array.from(salesResultRaw);
        quickbaseSales = {
          sales_count: Number(salesResult[0].sales_count),
          total_revenue: Number(salesResult[0].total_revenue)
        };
      }

      if (user.role === 'setter') {
        const appointmentsResultRaw = await sql`
          SELECT COUNT(*)::int as appointments_count
          FROM projects
          WHERE setter_email = ${user.email}
            AND date_created >= ${startDate}
            AND date_created <= ${endDate}
        `;
        const appointmentsResult = Array.from(appointmentsResultRaw);
        quickbaseAppointments = {
          appointments_count: Number(appointmentsResult[0].appointments_count)
        };
      }
    } catch (error) {
      logError('repcard-user-stats', error as Error, { requestId, context: 'QuickBase data fetch' });
      // Continue with RepCard data only
    }
    
    // Calculate volume stats
    const doorsKnocked = repcardCustomers?.length || 0;
    const appointmentsSet = repcardAppointments?.length || 0;
    const salesClosed = Number(quickbaseSales.sales_count) || 0;
    const revenueGenerated = Number(quickbaseSales.total_revenue) || 0;
    
    // Calculate efficiency stats
    const doorsPerAppointment = appointmentsSet > 0 ? doorsKnocked / appointmentsSet : 0;
    const appointmentsPerSale = salesClosed > 0 ? appointmentsSet / salesClosed : 0;
    const averageDealSize = salesClosed > 0 ? revenueGenerated / salesClosed : 0;
    
    // Calculate average time to close (simplified - would need more complex logic for real implementation)
    const averageTimeToClose = 0; // Placeholder - would need to calculate from appointment to close dates
    
    // Build response
    const response: UserStatsResponse = {
      user: {
        id: user.id,
        name: user.name,
        email: user.email,
        office: user.office,
        role: user.role,
        repcardUserId: user.repcard_user_id
      },
      volumeStats: {
        doorsKnocked,
        appointmentsSet,
        salesClosed,
        revenueGenerated
      },
      qualityStats: {
        appointmentSpeed: {
          percentage: qualityMetrics.appointmentSpeed.percentageWithin24Hours,
          averageHours: qualityMetrics.appointmentSpeed.averageHoursToSchedule
        },
        attachmentRate: {
          percentage: qualityMetrics.attachmentRate.percentageWithAttachments,
          totalAttachments: qualityMetrics.attachmentRate.totalAttachments
        },
        rescheduleRate: {
          average: qualityMetrics.rescheduleRate.averageReschedulesPerCustomer,
          totalReschedules: qualityMetrics.rescheduleRate.totalReschedules
        },
        followUpConsistency: {
          percentage: qualityMetrics.followUpConsistency.percentageWithFollowUps,
          totalFollowUps: qualityMetrics.followUpConsistency.totalFollowUpAppointments
        }
      },
      efficiencyStats: {
        doorsPerAppointment,
        appointmentsPerSale,
        averageDealSize,
        averageTimeToClose
      },
      metadata: {
        startDate,
        endDate,
        timeRange,
        cached: false,
        calculatedAt: new Date().toISOString()
      }
    };
    
    // Cache result
    userStatsCache.set(cacheKey, {
      data: response,
      timestamp: Date.now()
    });
    
    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(response);
    
  } catch (error) {
    const duration = Date.now() - start;
    logError('repcard-user-stats', error as Error, { requestId, context: 'repcard-user-stats' });
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
