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
const CACHE_TTL = 300000; // 5 minutes in milliseconds (aligned with 5-minute sync interval)
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
      // CRITICAL FIX: Fetch based on role - setters get appointments they set, closers get appointments they closed
      let appointmentsQuery;
      if (user.role === 'closer') {
        // Closers: Get appointments where they are the closer
        appointmentsQuery = sql`
          SELECT 
            repcard_appointment_id,
            repcard_customer_id,
            setter_user_id,
            closer_user_id,
            scheduled_at,
            completed_at,
            disposition,
            status_category,
            is_reschedule,
            reschedule_count,
            original_appointment_id,
            created_at,
            updated_at,
            raw_data
          FROM repcard_appointments
          WHERE closer_user_id::text = ${String(user.repcard_user_id)}
            AND scheduled_at >= ${startDate}::timestamp
            AND scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')
          ORDER BY scheduled_at DESC
          LIMIT 1000
        `;
      } else {
        // Setters: Get appointments where they are the setter
        appointmentsQuery = sql`
          SELECT 
            repcard_appointment_id,
            repcard_customer_id,
            setter_user_id,
            closer_user_id,
            scheduled_at,
            completed_at,
            disposition,
            status_category,
            is_reschedule,
            reschedule_count,
            original_appointment_id,
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
      }
      const appointmentsResult = await appointmentsQuery;
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
    
    // Fetch quality metrics from DATABASE (not API) - CRITICAL FIX
    let qualityMetrics;
    try {
      // Calculate appointment speed (within 48 hours) from database
      let appointmentSpeedQuery;
      if (user.role === 'closer') {
        appointmentSpeedQuery = sql`
          SELECT 
            COUNT(*)::int as total_appointments,
            COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
            AVG(CASE 
              WHEN scheduled_at IS NOT NULL AND c.created_at IS NOT NULL 
              THEN EXTRACT(EPOCH FROM (scheduled_at - c.created_at)) / 3600.0 
              ELSE NULL 
            END) as avg_hours
          FROM repcard_appointments a
          LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
          WHERE a.closer_user_id::text = ${String(user.repcard_user_id)}
            AND a.scheduled_at >= ${startDate}::timestamp
            AND a.scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')
        `;
      } else {
        appointmentSpeedQuery = sql`
          SELECT 
            COUNT(*)::int as total_appointments,
            COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
            AVG(CASE 
              WHEN scheduled_at IS NOT NULL AND c.created_at IS NOT NULL 
              THEN EXTRACT(EPOCH FROM (scheduled_at - c.created_at)) / 3600.0 
              ELSE NULL 
            END) as avg_hours
          FROM repcard_appointments a
          LEFT JOIN repcard_customers c ON c.repcard_customer_id = a.repcard_customer_id
          WHERE a.setter_user_id::text = ${String(user.repcard_user_id)}
            AND a.scheduled_at >= ${startDate}::timestamp
            AND a.scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')
        `;
      }
      
      const speedResult = Array.from(await appointmentSpeedQuery);
      const speedData = speedResult[0] || { total_appointments: 0, within_48h: 0, avg_hours: 0 };
      const totalAppointments = Number(speedData.total_appointments) || 0;
      const within48h = Number(speedData.within_48h) || 0;
      const avgHours = Number(speedData.avg_hours) || 0;
      const appointmentSpeedPercentage = totalAppointments > 0 ? (within48h / totalAppointments) * 100 : 0;

      // Calculate attachment rate (power bill) from database
      let attachmentQuery;
      if (user.role === 'closer') {
        // For closers, count appointments with power bill
        attachmentQuery = sql`
          SELECT 
            COUNT(DISTINCT a.repcard_customer_id)::int as total_customers,
            COUNT(DISTINCT CASE WHEN a.has_power_bill = TRUE THEN a.repcard_customer_id END)::int as with_attachments,
            COUNT(*) FILTER (WHERE a.has_power_bill = TRUE)::int as total_attachments
          FROM repcard_appointments a
          WHERE a.closer_user_id::text = ${String(user.repcard_user_id)}
            AND a.scheduled_at >= ${startDate}::timestamp
            AND a.scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')
        `;
      } else {
        // For setters, count customers with attachments
        attachmentQuery = sql`
          SELECT 
            COUNT(DISTINCT c.repcard_customer_id)::int as total_customers,
            COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END)::int as with_attachments,
            COUNT(att.id)::int as total_attachments
          FROM repcard_customers c
          LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
          WHERE c.setter_user_id::text = ${String(user.repcard_user_id)}
            AND c.created_at >= ${startDate}::timestamp
            AND c.created_at <= (${endDate}::timestamp + INTERVAL '1 day')
        `;
      }
      
      const attachmentResult = Array.from(await attachmentQuery);
      const attachmentData = attachmentResult[0] || { total_customers: 0, with_attachments: 0, total_attachments: 0 };
      const totalCustomers = Number(attachmentData.total_customers) || 0;
      const withAttachments = Number(attachmentData.with_attachments) || 0;
      const totalAttachments = Number(attachmentData.total_attachments) || 0;
      const attachmentPercentage = totalCustomers > 0 ? (withAttachments / totalCustomers) * 100 : 0;

      // Calculate reschedule rate from database
      let rescheduleQuery;
      if (user.role === 'closer') {
        rescheduleQuery = sql`
          SELECT 
            COUNT(DISTINCT a.repcard_customer_id)::int as total_customers,
            COUNT(*) FILTER (WHERE a.is_reschedule = TRUE)::int as total_reschedules,
            COUNT(DISTINCT CASE WHEN a.is_reschedule = TRUE THEN a.repcard_customer_id END)::int as customers_with_reschedules
          FROM repcard_appointments a
          WHERE a.closer_user_id::text = ${String(user.repcard_user_id)}
            AND a.scheduled_at >= ${startDate}::timestamp
            AND a.scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')
        `;
      } else {
        rescheduleQuery = sql`
          SELECT 
            COUNT(DISTINCT a.repcard_customer_id)::int as total_customers,
            COUNT(*) FILTER (WHERE a.is_reschedule = TRUE)::int as total_reschedules,
            COUNT(DISTINCT CASE WHEN a.is_reschedule = TRUE THEN a.repcard_customer_id END)::int as customers_with_reschedules
          FROM repcard_appointments a
          WHERE a.setter_user_id::text = ${String(user.repcard_user_id)}
            AND a.scheduled_at >= ${startDate}::timestamp
            AND a.scheduled_at <= (${endDate}::timestamp + INTERVAL '1 day')
        `;
      }
      
      const rescheduleResult = Array.from(await rescheduleQuery);
      const rescheduleData = rescheduleResult[0] || { total_customers: 0, total_reschedules: 0, customers_with_reschedules: 0 };
      const totalRescheduleCustomers = Number(rescheduleData.total_customers) || 0;
      const totalReschedules = Number(rescheduleData.total_reschedules) || 0;
      const customersWithReschedules = Number(rescheduleData.customers_with_reschedules) || 0;
      const avgReschedules = totalRescheduleCustomers > 0 ? totalReschedules / totalRescheduleCustomers : 0;

      // Follow-up consistency (simplified - would need more complex logic)
      const followUpConsistency = {
        customersRequiringFollowUps: 0,
        customersWithFollowUps: 0,
        percentageWithFollowUps: 0,
        totalFollowUpAppointments: 0
      };

      qualityMetrics = {
        appointmentSpeed: {
          totalAppointments,
          appointmentsWithin24Hours: within48h, // Note: This is actually 48h, but keeping name for compatibility
          percentageWithin24Hours: appointmentSpeedPercentage,
          averageHoursToSchedule: avgHours
        },
        attachmentRate: {
          totalCustomers,
          customersWithAttachments: withAttachments,
          percentageWithAttachments: attachmentPercentage,
          totalAttachments
        },
        rescheduleRate: {
          totalCustomers: totalRescheduleCustomers,
          totalReschedules,
          averageReschedulesPerCustomer: avgReschedules,
          customersWithReschedules
        },
        followUpConsistency,
        period: { startDate, endDate },
        calculatedAt: new Date().toISOString()
      };

      console.log(`[RepCard Stats] Quality metrics calculated from database:`, {
        appointmentSpeed: `${appointmentSpeedPercentage.toFixed(1)}% (${within48h}/${totalAppointments})`,
        attachmentRate: `${attachmentPercentage.toFixed(1)}% (${withAttachments}/${totalCustomers})`,
        rescheduleRate: avgReschedules.toFixed(2)
      });
    } catch (error) {
      logError('repcard-user-stats', error as Error, { requestId, context: 'Quality metrics fetch from database', userId: user.id });
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
    
    // Calculate volume stats - CRITICAL: Role-based attribution
    let doorsKnocked = 0;
    let appointmentsSet = 0;
    let appointmentsSat = 0; // For closers: appointments they sat
    let salesClosed = 0;
    let revenueGenerated = 0;
    let rescheduleCount = 0;
    let appointmentsWithPowerBill = 0;
    let appointmentsWithin48h = 0;

    if (user.role === 'setter') {
      // Setter metrics: doors knocked, appointments set, quality metrics
      doorsKnocked = repcardCustomers?.length || 0;
      appointmentsSet = repcardAppointments?.length || 0;
      
      // Calculate setter-specific quality metrics
      appointmentsWithPowerBill = repcardAppointments.filter((apt: any) => {
        // Check if customer has attachments (power bill)
        const customer = repcardCustomers.find((c: any) => c.repcard_customer_id === apt.repcard_customer_id);
        return customer && customer.raw_data?.attachments && customer.raw_data.attachments.length > 0;
      }).length;
      
      // Calculate appointments within 48 hours
      appointmentsWithin48h = repcardAppointments.filter((apt: any) => {
        const customer = repcardCustomers.find((c: any) => c.repcard_customer_id === apt.repcard_customer_id);
        if (!customer || !apt.scheduled_at || !customer.created_at) return false;
        const hoursDiff = (new Date(apt.scheduled_at).getTime() - new Date(customer.created_at).getTime()) / (1000 * 60 * 60);
        return hoursDiff <= 48;
      }).length;
      
      // Reschedule count for setter (appointments they set that were rescheduled)
      rescheduleCount = repcardAppointments.filter((apt: any) => apt.is_reschedule === true).length;
      
      salesClosed = Number(quickbaseSales.sales_count) || 0;
      revenueGenerated = Number(quickbaseSales.total_revenue) || 0;
    } else if (user.role === 'closer') {
      // Closer metrics: appointments sat, sales closed, revenue, outcomes
      appointmentsSat = repcardAppointments?.length || 0; // Appointments where they are the closer
      
      // Sales closed from appointments (disposition contains 'closed')
      const closedAppointments = repcardAppointments.filter((apt: any) => 
        apt.disposition && apt.disposition.toLowerCase().includes('closed')
      );
      salesClosed = closedAppointments.length;
      
      // Revenue from closed appointments
      revenueGenerated = closedAppointments.reduce((sum: number, apt: any) => {
        const customer = repcardCustomers.find((c: any) => c.repcard_customer_id === apt.repcard_customer_id);
        if (customer && customer.raw_data?.customFields?.systemCost) {
          return sum + (parseFloat(customer.raw_data.customFields.systemCost) || 0);
        }
        return sum;
      }, 0);
      
      // Add QuickBase sales if available
      salesClosed += Number(quickbaseSales.sales_count) || 0;
      revenueGenerated += Number(quickbaseSales.total_revenue) || 0;
      
      // Reschedule count for closer (appointments they ran that were reschedules)
      rescheduleCount = repcardAppointments.filter((apt: any) => apt.is_reschedule === true).length;
    } else {
      // Both roles or unknown - show all metrics
      doorsKnocked = repcardCustomers?.length || 0;
      appointmentsSet = repcardAppointments.filter((apt: any) => 
        apt.setter_user_id && String(apt.setter_user_id) === String(user.repcard_user_id)
      ).length;
      appointmentsSat = repcardAppointments.filter((apt: any) => 
        apt.closer_user_id && String(apt.closer_user_id) === String(user.repcard_user_id)
      ).length;
      salesClosed = Number(quickbaseSales.sales_count) || 0;
      revenueGenerated = Number(quickbaseSales.total_revenue) || 0;
      rescheduleCount = repcardAppointments.filter((apt: any) => apt.is_reschedule === true).length;
    }
    
    // Calculate efficiency stats
    const doorsPerAppointment = appointmentsSet > 0 ? doorsKnocked / appointmentsSet : 0;
    const appointmentsPerSale = salesClosed > 0 ? (appointmentsSat || appointmentsSet) / salesClosed : 0;
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
        appointmentsSat: appointmentsSat || 0, // For closers: appointments they sat
        salesClosed,
        revenueGenerated,
        rescheduleCount, // Total reschedules
        appointmentsWithPowerBill, // Setter metric: appointments with PB attached
        appointmentsWithin48h // Setter metric: appointments set within 48h
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
