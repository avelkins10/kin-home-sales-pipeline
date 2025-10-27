import { sql } from '@/lib/db/client';
import { getQualityMetricsForUsers } from '@/lib/repcard/qualityMetrics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { repcardClient } from '@/lib/repcard/client';

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

/**
 * Get appointments sat count for each closer from QuickBase projects
 */
export async function getAppointmentsSatByCloser(
  timeRange: TimeRange,
  customDateRange?: CustomDateRange,
  officeIds?: number[]
): Promise<Array<{
  userId: string;
  userName: string;
  userEmail: string;
  office: string;
  appointmentsSat: number;
}>> {
  // Calculate date range
  const { startDate, endDate } = calculateDateRange(timeRange, customDateRange);
  
  // Fetch all closers
  let closersQuery = sql`
    SELECT id, name, email, sales_office[1] as office, repcard_user_id
    FROM users
    WHERE role = 'closer'
    AND repcard_user_id IS NOT NULL
  `;

  let closers: any[];

  if (officeIds && officeIds.length > 0) {
    // Use sql.query with proper array parameter
    const result = await sql.query(
      `SELECT DISTINCT u.id, u.name, u.email, u.sales_office[1] as office, u.repcard_user_id
       FROM users u
       JOIN offices o ON o.name = ANY(u.sales_office)
       WHERE u.role = 'closer'
       AND u.repcard_user_id IS NOT NULL
       AND o.quickbase_office_id = ANY($1::int[])`,
      [officeIds]
    );
    closers = result.rows;
  } else {
    closers = await closersQuery as unknown as any[];
  }
  
  // Get RepCard user IDs for all closers
  const repcardUserIds = closers
    .filter((c: any) => c.repcard_user_id)
    .map((c: any) => c.repcard_user_id);

  if (repcardUserIds.length === 0) {
    // No closers with RepCard IDs, return zeros
    return closers.map((closer: any) => ({
      userId: closer.id,
      userName: closer.name,
      userEmail: closer.email,
      office: closer.office,
      appointmentsSat: 0
    }));
  }

  // Fetch all appointments from RepCard for these closers
  let allAppointments: any[] = [];
  let page = 1;
  let hasMore = true;

  while (hasMore) {
    try {
      const response = await repcardClient.getAppointments({
        closerIds: repcardUserIds.join(','),
        fromDate: startDate,
        toDate: endDate,
        page,
        perPage: 100
      });

      allAppointments.push(...response.result.data);
      hasMore = response.result.currentPage < (response.result.totalPages || 1);
      page++;
    } catch (error) {
      console.error(`Failed to fetch appointments page ${page}:`, error);
      hasMore = false;
    }
  }

  // Count appointments per closer
  const results = closers.map((closer: any) => {
    const closerAppointments = allAppointments.filter(
      (apt: any) => apt.closerId?.toString() === closer.repcard_user_id?.toString()
    );

    return {
      userId: closer.id,
      userName: closer.name,
      userEmail: closer.email,
      office: closer.office,
      appointmentsSat: closerAppointments.length
    };
  });

  return results;
}

/**
 * Get follow-up counts for each closer from quality metrics
 */
export async function getFollowUpsByCloser(
  timeRange: TimeRange,
  customDateRange?: CustomDateRange,
  officeIds?: number[]
): Promise<Array<{
  userId: string;
  followUps: number;
}>> {
  // Calculate date range
  const { startDate, endDate } = calculateDateRange(timeRange, customDateRange);

  // Fetch all closers
  let closersQuery = sql`
    SELECT id, repcard_user_id
    FROM users
    WHERE role = 'closer'
    AND repcard_user_id IS NOT NULL
  `;

  let closers: any[];

  if (officeIds && officeIds.length > 0) {
    // Use sql.query with proper array parameter
    const result = await sql.query(
      `SELECT DISTINCT u.id, u.repcard_user_id
       FROM users u
       JOIN offices o ON o.name = ANY(u.sales_office)
       WHERE u.role = 'closer'
       AND u.repcard_user_id IS NOT NULL
       AND o.quickbase_office_id = ANY($1::int[])`,
      [officeIds]
    );
    closers = result.rows;
  } else {
    closers = await closersQuery as unknown as any[];
  }
  
  // Fetch quality metrics for each closer
  const results = await Promise.all(
    closers.map(async (closer) => {
      try {
        // Use quality metrics service
        const qualityMetrics = await getQualityMetricsForUsers({
          repcardUserIds: [closer.repcard_user_id],
          startDate,
          endDate,
          useCache: true
        });
        
        return {
          userId: closer.id,
          followUps: qualityMetrics.followUpConsistency.totalFollowUpAppointments
        };
      } catch (error) {
        console.error(`Failed to fetch follow-ups for closer ${closer.id}:`, error);
        return {
          userId: closer.id,
          followUps: 0
        };
      }
    })
  );
  
  return results;
}
