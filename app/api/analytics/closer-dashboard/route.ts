import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { sql } from '@/lib/db/client';
import { qbClient } from '@/lib/quickbase/client';
import { PROJECT_FIELDS, QB_TABLE_PROJECTS } from '@/lib/constants/fieldIds';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

export const runtime = 'nodejs';

/**
 * Calculate date range from TimeRange and CustomDateRange
 */
function calculateDateRange(
  timeRange: TimeRange,
  customDateRange?: CustomDateRange
): { startDate: string; endDate: string } {
  const now = new Date();
  let startDate: string;
  let endDate: string = now.toISOString().split('T')[0];
  
  switch (timeRange) {
    case 'ytd':
      startDate = new Date(now.getFullYear(), 0, 1).toISOString().split('T')[0];
      break;
    case 'last_30':
      const last30 = new Date(now);
      last30.setDate(now.getDate() - 30);
      startDate = last30.toISOString().split('T')[0];
      break;
    case 'last_90':
      const last90 = new Date(now);
      last90.setDate(now.getDate() - 90);
      startDate = last90.toISOString().split('T')[0];
      break;
    case 'last_12_months':
      const last12 = new Date(now);
      last12.setMonth(now.getMonth() - 12);
      startDate = last12.toISOString().split('T')[0];
      break;
    case 'custom':
      if (!customDateRange) {
        throw new Error('Custom date range required for custom time range');
      }
      startDate = customDateRange.startDate;
      endDate = customDateRange.endDate;
      break;
    default:
      startDate = new Date(now.getFullYear(), 0, 1).toISOString().split('T')[0];
  }
  
  return { startDate, endDate };
}

interface CloserDashboardMetrics {
  closerId: string;
  closerName: string;
  closerEmail: string;
  office: string;
  // Appointments
  appointmentsAssigned: number;
  appointmentsSat: number;
  appointmentsNoSit: number;
  sitRate: number;
  // Disposition breakdown
  closed: number;
  followUps: number;
  otherSits: number;
  noShow: number;
  cancelled: number;
  rescheduled: number;
  noSitReasons: Record<string, number>;
  // QuickBase project metrics
  projectsLinked: number;
  avgPPW: number;
  avgSystemSize: number;
  totalRevenue: number;
}

export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;
  
  try {
    logApiRequest('GET', path, { endpoint: 'closer-dashboard', requestId });
    
    // Authentication
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
    const timeRange = (searchParams.get('timeRange') || 'month') as TimeRange;
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    const officeIds = searchParams.get('officeIds')?.split(',').filter(Boolean).map(Number);
    
    // Validate timeRange
    const validTimeRanges = ['today', 'week', 'month', 'quarter', 'ytd', 'custom', 'last_30', 'last_90', 'last_12_months'];
    if (!validTimeRanges.includes(timeRange)) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
      return NextResponse.json(
        { error: `Invalid timeRange. Must be one of: ${validTimeRanges.join(', ')}` },
        { status: 400 }
      );
    }
    
    // Build custom date range if provided
    let customDateRange: CustomDateRange | undefined;
    if (timeRange === 'custom' && startDate && endDate) {
      customDateRange = { startDate, endDate };
    }
    
    const { startDate: calculatedStartDate, endDate: calculatedEndDate } = calculateDateRange(timeRange, customDateRange);
    
    // Get all closers with RepCard IDs
    let closersQuery = sql`
      SELECT DISTINCT 
        u.id,
        u.name,
        u.email,
        u.sales_office[1] as office,
        u.repcard_user_id::text as repcard_user_id
      FROM users u
      WHERE u.role = 'closer'
      AND u.repcard_user_id IS NOT NULL
    `;
    
    let closers: any[];
    if (officeIds && officeIds.length > 0) {
      closers = await sql.query(
        `SELECT DISTINCT 
          u.id,
          u.name,
          u.email,
          u.sales_office[1] as office,
          u.repcard_user_id::text as repcard_user_id
         FROM users u
         JOIN offices o ON o.name = ANY(u.sales_office)
         WHERE u.role = 'closer'
         AND u.repcard_user_id IS NOT NULL
         AND o.quickbase_office_id = ANY($1::int[])`,
        [officeIds]
      ) as any;
      closers = closers.rows || closers;
    } else {
      closers = await closersQuery as any;
      closers = closers.rows || closers;
    }
    
    if (closers.length === 0) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
      return NextResponse.json([]);
    }
    
    const repcardUserIds = closers
      .map((c: any) => c.repcard_user_id)
      .filter(Boolean);
    
    // Get RepCard appointments for these closers
    // Use status_category which is already calculated and normalized
    const appointmentsResult = await sql`
      SELECT 
        a.repcard_appointment_id,
        a.closer_user_id::text as closer_user_id,
        a.disposition,
        a.status_category,
        a.scheduled_at,
        a.completed_at,
        a.notes,
        c.name as customer_name,
        c.email as customer_email,
        c.phone as customer_phone,
        ru.first_name || ' ' || ru.last_name as closer_name,
        ru.email as closer_email
      FROM repcard_appointments a
      LEFT JOIN repcard_customers c ON a.repcard_customer_id::text = c.repcard_customer_id::text
      LEFT JOIN repcard_users ru ON a.closer_user_id::text = ru.repcard_user_id::text
      WHERE a.closer_user_id::text = ANY(${repcardUserIds}::text[])
      AND a.scheduled_at::date >= ${calculatedStartDate}::date
      AND a.scheduled_at::date <= ${calculatedEndDate}::date
      ORDER BY a.scheduled_at DESC
    `;
    
    const appointments = appointmentsResult.rows || appointmentsResult;
    
    // Get QuickBase projects for these closers
    const closerIdsForQB = repcardUserIds.map(id => id.toString()).join(',');
    const projectsResponse = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.CLOSER_ID,
        PROJECT_FIELDS.CLOSER_NAME,
        PROJECT_FIELDS.CLOSER_EMAIL,
        PROJECT_FIELDS.SALES_DATE,
        PROJECT_FIELDS.GROSS_PPW,
        PROJECT_FIELDS.NET_PPW,
        PROJECT_FIELDS.COMMISSIONABLE_PPW,
        PROJECT_FIELDS.SYSTEM_SIZE_KW,
        PROJECT_FIELDS.SYSTEM_PRICE,
        PROJECT_FIELDS.CUSTOMER_EMAIL,
        PROJECT_FIELDS.CUSTOMER_PHONE,
        PROJECT_FIELDS.SALES_OFFICE,
      ],
      where: `{${PROJECT_FIELDS.CLOSER_ID}.IN.${closerIdsForQB}} AND {${PROJECT_FIELDS.SALES_DATE}.AF.'${calculatedStartDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${calculatedEndDate}'}`,
      options: { top: 10000 },
    });
    
    const projects = projectsResponse.data || [];
    
    // Build metrics for each closer
    const metricsMap = new Map<string, CloserDashboardMetrics>();
    
    // Initialize metrics for all closers
    for (const closer of closers) {
      metricsMap.set(closer.repcard_user_id, {
        closerId: closer.repcard_user_id,
        closerName: closer.name,
        closerEmail: closer.email,
        office: closer.office || 'Unknown',
        appointmentsAssigned: 0,
        appointmentsSat: 0,
        appointmentsNoSit: 0,
        sitRate: 0,
        closed: 0,
        followUps: 0,
        otherSits: 0,
        noShow: 0,
        cancelled: 0,
        rescheduled: 0,
        noSitReasons: {},
        projectsLinked: 0,
        avgPPW: 0,
        avgSystemSize: 0,
        totalRevenue: 0,
      });
    }
    
    // Process appointments - use status_category which is already normalized
    for (const appt of appointments) {
      const closerId = appt.closer_user_id;
      if (!closerId || !metricsMap.has(closerId)) continue;
      
      const metrics = metricsMap.get(closerId)!;
      metrics.appointmentsAssigned++;
      
      // Use status_category which is already calculated and normalized in the database
      const statusCategory = appt.status_category || 'pending';
      
      // Categorize based on status_category
      switch (statusCategory) {
        case 'sat_closed':
          metrics.closed++;
          metrics.appointmentsSat++;
          break;
        case 'sat_no_close':
          // These are sits that didn't close - could be follow-ups or other sits
          // Check notes/disposition for more detail
          const disposition = (appt.disposition || '').toLowerCase();
          if (disposition.includes('follow') || disposition.includes('follow-up')) {
            metrics.followUps++;
          } else {
            metrics.otherSits++;
          }
          metrics.appointmentsSat++;
          break;
        case 'no_show':
          metrics.noShow++;
          metrics.appointmentsNoSit++;
          // Extract reason from notes if available
          if (appt.notes) {
            const reason = extractNoSitReason(appt.notes);
            if (reason) {
              metrics.noSitReasons[reason] = (metrics.noSitReasons[reason] || 0) + 1;
            }
          }
          break;
        case 'cancelled':
          metrics.cancelled++;
          metrics.appointmentsNoSit++;
          if (appt.notes) {
            const reason = extractNoSitReason(appt.notes);
            if (reason) {
              metrics.noSitReasons[reason] = (metrics.noSitReasons[reason] || 0) + 1;
            }
          }
          break;
        case 'rescheduled':
          metrics.rescheduled++;
          metrics.appointmentsNoSit++;
          break;
        case 'completed':
          // Completed but no specific disposition - count as sit
          metrics.otherSits++;
          metrics.appointmentsSat++;
          break;
        case 'scheduled':
        case 'pending':
        default:
          // Not yet completed - don't count as sit or no-sit yet
          // These are future appointments
          break;
      }
    }
    
    // Process QuickBase projects
    // IMPORTANT: Not all appointments have QuickBase projects!
    // Only appointments that resulted in sales (sat_closed) typically have projects
    const projectsByCloser = new Map<string, any[]>();
    for (const project of projects) {
      const closerId = project[PROJECT_FIELDS.CLOSER_ID]?.value;
      if (!closerId) continue;
      
      const closerIdStr = closerId.toString();
      if (!projectsByCloser.has(closerIdStr)) {
        projectsByCloser.set(closerIdStr, []);
      }
      projectsByCloser.get(closerIdStr)!.push(project);
    }
    
    // Calculate project metrics - only for closers who have projects
    for (const [closerId, closerProjects] of projectsByCloser.entries()) {
      if (!metricsMap.has(closerId)) continue;
      
      const metrics = metricsMap.get(closerId)!;
      metrics.projectsLinked = closerProjects.length;
      
      // Calculate averages
      const ppwValues: number[] = [];
      const systemSizes: number[] = [];
      let totalRevenue = 0;
      
      for (const project of closerProjects) {
        // Use commissionable PPW if available, fallback to net PPW, then gross PPW
        const ppw = project[PROJECT_FIELDS.COMMISSIONABLE_PPW]?.value || 
                   project[PROJECT_FIELDS.NET_PPW]?.value || 
                   project[PROJECT_FIELDS.GROSS_PPW]?.value;
        if (ppw && typeof ppw === 'number' && ppw > 0) {
          ppwValues.push(ppw);
        }
        
        const systemSize = project[PROJECT_FIELDS.SYSTEM_SIZE_KW]?.value;
        if (systemSize && typeof systemSize === 'number' && systemSize > 0) {
          systemSizes.push(systemSize);
        }
        
        const systemPrice = project[PROJECT_FIELDS.SYSTEM_PRICE]?.value;
        if (systemPrice && typeof systemPrice === 'number' && systemPrice > 0) {
          totalRevenue += systemPrice;
        }
      }
      
      metrics.avgPPW = ppwValues.length > 0 
        ? ppwValues.reduce((a, b) => a + b, 0) / ppwValues.length 
        : 0;
      metrics.avgSystemSize = systemSizes.length > 0
        ? systemSizes.reduce((a, b) => a + b, 0) / systemSizes.length
        : 0;
      metrics.totalRevenue = totalRevenue;
    }
    
    // Note: Closers with appointments but no projects will have:
    // - projectsLinked: 0
    // - avgPPW: 0
    // - avgSystemSize: 0
    // - totalRevenue: 0
    // This is correct - not all appointments result in QuickBase projects!
    
    // Calculate sit rate
    for (const metrics of metricsMap.values()) {
      metrics.sitRate = metrics.appointmentsAssigned > 0
        ? (metrics.appointmentsSat / metrics.appointmentsAssigned) * 100
        : 0;
    }
    
    const result = Array.from(metricsMap.values());
    
    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(result);
    
  } catch (error) {
    const duration = Date.now() - start;
    logError('closer-dashboard', error as Error, { requestId, context: 'closer-dashboard' });
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

/**
 * Extract no-sit reason from appointment notes
 */
function extractNoSitReason(notes: string): string | null {
  if (!notes) return null;
  
  const lowerNotes = notes.toLowerCase();
  
  // Common no-sit reasons
  const reasons = [
    'not home',
    'not interested',
    'already has solar',
    'renting',
    'moving',
    'no answer',
    'wrong address',
    'not available',
    'busy',
    'not ready',
    'price',
    'financing',
    'roof',
    'hoa',
  ];
  
  for (const reason of reasons) {
    if (lowerNotes.includes(reason)) {
      return reason.charAt(0).toUpperCase() + reason.slice(1);
    }
  }
  
  return 'Other';
}

