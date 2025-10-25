import { sql } from '@/lib/db/client';
import { getQualityMetricsForUsers } from '@/lib/repcard/qualityMetrics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

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
    SELECT id, name, email, office 
    FROM users 
    WHERE role = 'closer' 
    AND repcard_user_id IS NOT NULL
  `;
  
  if (officeIds && officeIds.length > 0) {
    closersQuery = sql`
      SELECT id, name, email, office 
      FROM users 
      WHERE role = 'closer' 
      AND repcard_user_id IS NOT NULL
      AND office = ANY(${officeIds})
    `;
  }
  
  const closers = await closersQuery as unknown as any[];
  
  // For each closer, count projects (appointments sat)
  const results = await Promise.all(
    closers.map(async (closer) => {
      const projects = await sql`
        SELECT COUNT(*) as count
        FROM projects
        WHERE closer_email = ${closer.email}
        AND date_created >= ${startDate}
        AND date_created <= ${endDate}
      ` as unknown as any[];
      
      return {
        userId: closer.id,
        userName: closer.name,
        userEmail: closer.email,
        office: closer.office,
        appointmentsSat: parseInt(projects[0]?.count || '0')
      };
    })
  );
  
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
  
  if (officeIds && officeIds.length > 0) {
    closersQuery = sql`
      SELECT id, repcard_user_id 
      FROM users 
      WHERE role = 'closer' 
      AND repcard_user_id IS NOT NULL
      AND office = ANY(${officeIds})
    `;
  }
  
  const closers = await closersQuery as unknown as any[];
  
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
