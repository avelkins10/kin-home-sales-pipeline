import { repcardClient } from './client';
import type {
  RepCardCustomer,
  RepCardAppointment,
  RepCardCustomerStatusLog,
  RepCardCustomerAttachment,
} from './types';
import { differenceInMilliseconds, parseISO } from 'date-fns';
import { sql } from '@/lib/db/client';

/**
 * Parameters for quality metrics calculation
 */
export interface QualityMetricsParams {
  /** Array of dashboard user IDs (not RepCard IDs) */
  userIds?: string[];
  /** Array of RepCard user IDs (direct) */
  repcardUserIds?: string[];
  /** Array of office names */
  officeIds?: string[];
  /** Start date in YYYY-MM-DD format */
  startDate: string;
  /** End date in YYYY-MM-DD format */
  endDate: string;
  /** Whether to use cache (default true) */
  useCache?: boolean;
}

/**
 * Appointment speed metric data
 */
export interface AppointmentSpeedMetric {
  totalAppointments: number;
  appointmentsWithin24Hours: number;
  percentageWithin24Hours: number;
  averageHoursToSchedule: number;
}

/**
 * Attachment rate metric data
 */
export interface AttachmentRateMetric {
  totalCustomers: number;
  customersWithAttachments: number;
  percentageWithAttachments: number;
  totalAttachments: number;
}

/**
 * Reschedule rate metric data
 */
export interface RescheduleRateMetric {
  totalCustomers: number;
  totalReschedules: number;
  averageReschedulesPerCustomer: number;
  customersWithReschedules: number;
}

/**
 * Follow-up consistency metric data
 */
export interface FollowUpConsistencyMetric {
  customersRequiringFollowUps: number;
  customersWithFollowUps: number;
  percentageWithFollowUps: number;
  totalFollowUpAppointments: number;
}

/**
 * Complete quality metrics data
 */
export interface QualityMetrics {
  appointmentSpeed: AppointmentSpeedMetric;
  attachmentRate: AttachmentRateMetric;
  rescheduleRate: RescheduleRateMetric;
  followUpConsistency: FollowUpConsistencyMetric;
  period: { startDate: string; endDate: string };
  calculatedAt: string;
}

/**
 * Quality metrics for a single user
 */
export interface UserQualityMetrics extends QualityMetrics {
  userId: string;
  repcardUserId: string;
  userName: string;
  office?: string;
}

/**
 * Quality metrics for an office
 */
export interface OfficeQualityMetrics extends QualityMetrics {
  officeName: string;
  userCount: number;
  topPerformers: Array<{ userId: string; userName: string; qualityScore: number }>;
}

// Cache implementation
const qualityMetricsCache = new Map<string, { data: any; timestamp: number }>();
const CACHE_TTL = 1800000; // 30 minutes in milliseconds
const MAX_CACHE_ENTRIES = 100;

interface CacheStats {
  hits: number;
  misses: number;
  evictions: number;
  expiredRemovals: number;
  currentSize: number;
}

const cacheStats: CacheStats = {
  hits: 0,
  misses: 0,
  evictions: 0,
  expiredRemovals: 0,
  currentSize: 0,
};

/**
 * Get cache statistics
 */
export function getCacheStats(): CacheStats {
  return {
    ...cacheStats,
    currentSize: qualityMetricsCache.size,
  };
}

/**
 * Get cached metrics if available and not expired
 */
function getCachedMetrics(cacheKey: string): any | undefined {
  const cached = qualityMetricsCache.get(cacheKey);
  if (!cached) {
    cacheStats.misses++;
    return undefined;
  }

  const now = Date.now();
  if (now - cached.timestamp > CACHE_TTL) {
    qualityMetricsCache.delete(cacheKey);
    cacheStats.expiredRemovals++;
    cacheStats.misses++;
    return undefined;
  }

  // Refresh timestamp for LRU behavior
  cached.timestamp = now;
  cacheStats.hits++;
  return cached.data;
}

/**
 * Set cached metrics with TTL
 */
function setCachedMetrics(cacheKey: string, data: any): void {
  cacheStats.misses++;
  
  // Remove expired entries
  const now = Date.now();
  for (const [key, value] of qualityMetricsCache.entries()) {
    if (now - value.timestamp > CACHE_TTL) {
      qualityMetricsCache.delete(key);
      cacheStats.expiredRemovals++;
    }
  }

  // Evict oldest entries if over limit
  if (qualityMetricsCache.size >= MAX_CACHE_ENTRIES) {
    const entries = Array.from(qualityMetricsCache.entries());
    entries.sort((a, b) => a[1].timestamp - b[1].timestamp);
    
    const toEvict = entries.slice(0, qualityMetricsCache.size - MAX_CACHE_ENTRIES + 1);
    for (const [key] of toEvict) {
      qualityMetricsCache.delete(key);
      cacheStats.evictions++;
    }
  }

  qualityMetricsCache.set(cacheKey, { data, timestamp: now });
}

/**
 * Build cache key for metrics
 */
function buildCacheKey(scope: 'user' | 'office', id: string, startDate: string, endDate: string): string {
  return `quality-metrics:${scope}:${id}:${startDate}:${endDate}`;
}

/**
 * Clear all cached quality metrics
 */
export function clearQualityMetricsCache(): void {
  const clearedCount = qualityMetricsCache.size;
  qualityMetricsCache.clear();
  cacheStats.hits = 0;
  cacheStats.misses = 0;
  cacheStats.evictions = 0;
  cacheStats.expiredRemovals = 0;
  cacheStats.currentSize = 0;
  console.log(`Cleared ${clearedCount} quality metrics cache entries`);
}

/**
 * Fetch all pages from a paginated RepCard endpoint
 */
async function fetchAllPages<T>(
  fetchFunction: (page: number) => Promise<{ result: { data: T[]; currentPage: number; totalPages?: number; lastPage?: number } }>
): Promise<T[]> {
  const allData: T[] = [];
  let currentPage = 1;
  let totalPages = 1;

  do {
    const response = await fetchFunction(currentPage);
    allData.push(...response.result.data);
    currentPage = response.result.currentPage + 1;
    totalPages = response.result.totalPages || response.result.lastPage || 1;
  } while (currentPage <= totalPages);

  return allData;
}

/**
 * Resolve RepCard user IDs from various parameter types
 */
async function resolveRepcardUserIds(params: QualityMetricsParams): Promise<string[]> {
  if (params.repcardUserIds && params.repcardUserIds.length > 0) {
    return params.repcardUserIds.filter(id => id != null);
  }

  let repcardUserIds: string[] = [];

  if (params.userIds && params.userIds.length > 0) {
    const users = await sql`
      SELECT repcard_user_id 
      FROM users 
      WHERE id = ANY(${params.userIds}) 
      AND repcard_user_id IS NOT NULL
    `;
    repcardUserIds = (users as any[]).map((user: any) => user.repcard_user_id).filter(id => id != null);
  }

  if (params.officeIds && params.officeIds.length > 0) {
    const users = await sql`
      SELECT repcard_user_id 
      FROM users 
      WHERE office = ANY(${params.officeIds}) 
      AND repcard_user_id IS NOT NULL
    `;
    repcardUserIds = [...repcardUserIds, ...(users as any[]).map((user: any) => user.repcard_user_id).filter(id => id != null)];
  }

  if (repcardUserIds.length === 0) {
    throw new Error('No valid RepCard user IDs found for the specified parameters');
  }

  return repcardUserIds;
}

/**
 * Calculate appointment speed metrics
 * Measures time from customer creation to appointment scheduling
 */
export function calculateAppointmentSpeed(customers: RepCardCustomer[], appointments: RepCardAppointment[]): AppointmentSpeedMetric {
  // Create map of customer ID to createdAt timestamp
  const customerMap = new Map<number, Date>();
  customers.forEach(customer => {
    if (customer.createdAt) {
      customerMap.set(customer.id, parseISO(customer.createdAt));
    }
  });

  // Filter appointments that have matching customers
  const validAppointments = appointments.filter(appointment => 
    appointment.contact?.id && customerMap.has(appointment.contact.id)
  );

  if (validAppointments.length === 0) {
    return {
      totalAppointments: 0,
      appointmentsWithin24Hours: 0,
      percentageWithin24Hours: 0,
      averageHoursToSchedule: 0,
    };
  }

  let appointmentsWithin24Hours = 0;
  let totalHours = 0;

  validAppointments.forEach(appointment => {
    const customerCreatedAt = customerMap.get(appointment.contact!.id);
    const appointmentCreatedAt = parseISO(appointment.createdAt);
    
    const diffMs = differenceInMilliseconds(appointmentCreatedAt, customerCreatedAt!);
    const diffHours = Math.max(0, diffMs / (1000 * 60 * 60)); // Guard against negative diffs
    
    totalHours += diffHours;
    
    if (diffHours < 24) {
      appointmentsWithin24Hours++;
    }
  });

  const percentageWithin24Hours = (appointmentsWithin24Hours / validAppointments.length) * 100;
  const averageHoursToSchedule = totalHours / validAppointments.length;

  return {
    totalAppointments: validAppointments.length,
    appointmentsWithin24Hours,
    percentageWithin24Hours,
    averageHoursToSchedule,
  };
}

/**
 * Calculate attachment rate metrics
 */
export function calculateAttachmentRate(customers: RepCardCustomer[], attachments: RepCardCustomerAttachment[]): AttachmentRateMetric {
  const customersWithAttachments = new Set<number>();
  attachments.forEach(attachment => {
    if (attachment.customerId) {
      customersWithAttachments.add(attachment.customerId);
    }
  });

  const totalCustomers = customers.length;
  const customersWithAttachmentsCount = customersWithAttachments.size;
  const percentageWithAttachments = totalCustomers > 0 ? (customersWithAttachmentsCount / totalCustomers) * 100 : 0;

  return {
    totalCustomers,
    customersWithAttachments: customersWithAttachmentsCount,
    percentageWithAttachments,
    totalAttachments: attachments.length,
  };
}

/**
 * Calculate reschedule rate metrics
 */
export function calculateRescheduleRate(statusLogs: RepCardCustomerStatusLog[]): RescheduleRateMetric {
  // Group status logs by customer ID
  const customerStatusMap = new Map<number, RepCardCustomerStatusLog[]>();
  statusLogs.forEach(log => {
    if (log.customerId) {
      if (!customerStatusMap.has(log.customerId)) {
        customerStatusMap.set(log.customerId, []);
      }
      customerStatusMap.get(log.customerId)!.push(log);
    }
  });

  let totalReschedules = 0;
  let customersWithReschedules = 0;

  customerStatusMap.forEach((logs, customerId) => {
    // Sort by createdAt to ensure chronological order
    const sortedLogs = logs.sort((a, b) => 
      new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime()
    );

    // Count actual reschedules - look for specific reschedule patterns
    const reschedules = sortedLogs.filter(log => {
      // Check if this is a reschedule event
      return log.statusFrom !== null && 
             (log.statusTo?.statusName?.toLowerCase().includes('rescheduled') ||
              log.statusFrom?.statusName?.toLowerCase().includes('scheduled') && 
              log.statusTo?.statusName?.toLowerCase().includes('scheduled'));
    }).length;
    
    if (reschedules > 0) {
      totalReschedules += reschedules;
      customersWithReschedules++;
    }
  });

  const totalCustomers = customerStatusMap.size;
  const averageReschedulesPerCustomer = totalCustomers > 0 ? totalReschedules / totalCustomers : 0;

  return {
    totalCustomers,
    totalReschedules,
    averageReschedulesPerCustomer,
    customersWithReschedules,
  };
}

/**
 * Calculate follow-up consistency metrics
 */
export function calculateFollowUpConsistency(appointments: RepCardAppointment[]): FollowUpConsistencyMetric {
  // Group appointments by customer ID
  const customerAppointmentsMap = new Map<number, RepCardAppointment[]>();
  appointments.forEach(appointment => {
    if (appointment.contact?.id) {
      if (!customerAppointmentsMap.has(appointment.contact.id)) {
        customerAppointmentsMap.set(appointment.contact.id, []);
      }
      customerAppointmentsMap.get(appointment.contact.id)!.push(appointment);
    }
  });

  let customersRequiringFollowUps = 0;
  let customersWithFollowUps = 0;
  let totalFollowUpAppointments = 0;

  customerAppointmentsMap.forEach((appointments, customerId) => {
    // Sort appointments by date to determine first vs follow-up
    const sortedAppointments = appointments.sort((a, b) => 
      new Date(a.createdAt).getTime() - new Date(b.createdAt).getTime()
    );
    
    if (sortedAppointments.length > 1) {
      customersRequiringFollowUps++;
      
      // Check if first appointment was completed/sat
      const firstAppointment = sortedAppointments[0];
      const isFirstAppointmentCompleted = firstAppointment.status?.title?.toLowerCase().includes('completed') ||
                                        firstAppointment.status?.title?.toLowerCase().includes('sat');
      
      if (isFirstAppointmentCompleted) {
        customersWithFollowUps++;
        totalFollowUpAppointments += sortedAppointments.length - 1;
      }
    }
  });

  const percentageWithFollowUps = customersRequiringFollowUps > 0 
    ? (customersWithFollowUps / customersRequiringFollowUps) * 100 
    : 0;

  return {
    customersRequiringFollowUps,
    customersWithFollowUps,
    percentageWithFollowUps,
    totalFollowUpAppointments,
  };
}

/**
 * Get quality metrics for one or more users
 */
export async function getQualityMetricsForUsers(params: QualityMetricsParams): Promise<QualityMetrics> {
  try {
    const repcardUserIds = await resolveRepcardUserIds(params);
    
    // Build cache key with stable hash for multi-user requests
    const cacheKey = buildCacheKey(
      'user',
      repcardUserIds.length === 1 ? repcardUserIds[0] : repcardUserIds.sort().join(','),
      params.startDate,
      params.endDate
    );

    // Check cache
    if (params.useCache !== false) {
      const cached = getCachedMetrics(cacheKey);
      if (cached) {
        return cached;
      }
    }

    console.log(`Fetching quality metrics for ${repcardUserIds.length} RepCard users`);

    // Fetch data from RepCard API with pagination
    const [customers, appointments, statusLogs, attachments] = await Promise.all([
      fetchAllPages((page) => repcardClient.getCustomers({
        page,
        perPage: 100,
        // Note: RepCard API doesn't support date filtering for customers
        // We'll filter by createdAt after fetching
      })),
      fetchAllPages((page) => repcardClient.getAppointments({
        page,
        perPage: 100,
        fromDate: params.startDate,
        toDate: params.endDate,
        setterIds: repcardUserIds.join(','),
      })),
      fetchAllPages((page) => repcardClient.getCustomerStatusLogs({
        page,
        perPage: 100,
        fromDate: params.startDate,
        toDate: params.endDate,
        userIds: repcardUserIds.join(','),
      })),
      fetchAllPages((page) => repcardClient.getCustomerAttachments({
        page,
        perPage: 100,
        fromDate: params.startDate,
        toDate: params.endDate,
        userIds: repcardUserIds.join(','),
      })),
    ]);

    console.log(`Fetched ${customers.length} customers, ${appointments.length} appointments, ${statusLogs.length} status logs, ${attachments.length} attachments`);

    // Filter customers by date range since RepCard API doesn't support date filtering
    const startDateObj = new Date(params.startDate);
    const endDateObj = new Date(params.endDate);
    endDateObj.setHours(23, 59, 59, 999); // Include the entire end date
    
    const filteredCustomers = (customers as RepCardCustomer[]).filter(customer => {
      const customerDate = new Date(customer.createdAt);
      return customerDate >= startDateObj && customerDate <= endDateObj;
    });

    console.log(`Filtered to ${filteredCustomers.length} customers within date range`);

    // Calculate metrics
    const appointmentSpeed = calculateAppointmentSpeed(filteredCustomers, appointments as RepCardAppointment[]);
    const attachmentRate = calculateAttachmentRate(filteredCustomers, attachments as RepCardCustomerAttachment[]);
    const rescheduleRate = calculateRescheduleRate(statusLogs as RepCardCustomerStatusLog[]);
    const followUpConsistency = calculateFollowUpConsistency(appointments as RepCardAppointment[]);

    // Build result
    const metrics: QualityMetrics = {
      appointmentSpeed,
      attachmentRate,
      rescheduleRate,
      followUpConsistency,
      period: {
        startDate: params.startDate,
        endDate: params.endDate,
      },
      calculatedAt: new Date().toISOString(),
    };

    // Cache result
    setCachedMetrics(cacheKey, metrics);

    return metrics;
  } catch (error) {
    console.error('Error calculating quality metrics:', error);
    throw new Error(`Failed to calculate quality metrics: ${error instanceof Error ? error.message : 'Unknown error'}`);
  }
}

/**
 * Get quality metrics for a single user
 */
export async function getQualityMetricsForUser(
  userId: string,
  startDate: string,
  endDate: string,
  useCache = true
): Promise<UserQualityMetrics> {
  try {
    // Query database for user
    const users = await sql`
      SELECT id, name, repcard_user_id, office 
      FROM users 
      WHERE id = ${userId}
    `;

    if ((users as any[]).length === 0) {
      throw new Error(`User with ID ${userId} not found`);
    }

    const user = (users as any[])[0];
    if (!user.repcard_user_id) {
      throw new Error(`User ${userId} has no RepCard user ID`);
    }

    // Check user-specific cache first
    const cacheKey = buildCacheKey('user', userId, startDate, endDate);
    if (useCache) {
      const cached = getCachedMetrics(cacheKey);
      if (cached) {
        return cached;
      }
    }

    // Get quality metrics
    const metrics = await getQualityMetricsForUsers({
      repcardUserIds: [user.repcard_user_id],
      startDate,
      endDate,
      useCache,
    });

    // Build user metrics
    const userMetrics: UserQualityMetrics = {
      ...metrics,
      userId: user.id,
      repcardUserId: user.repcard_user_id,
      userName: user.name,
      office: user.office,
    };

    // Cache user-specific result
    setCachedMetrics(cacheKey, userMetrics);

    return userMetrics;
  } catch (error) {
    console.error('Error getting user quality metrics:', error);
    throw new Error(`Failed to get user quality metrics: ${error instanceof Error ? error.message : 'Unknown error'}`);
  }
}

/**
 * Get quality metrics for an office
 */
export async function getQualityMetricsForOffice(
  officeName: string,
  startDate: string,
  endDate: string,
  useCache = true
): Promise<OfficeQualityMetrics> {
  try {
    // Query database for office users
    const users = await sql`
      SELECT id, name, repcard_user_id 
      FROM users 
      WHERE office = ${officeName} 
      AND repcard_user_id IS NOT NULL
    `;

    if ((users as any[]).length === 0) {
      throw new Error(`No users found for office: ${officeName}`);
    }

    const repcardUserIds = (users as any[]).map((user: any) => user.repcard_user_id);

    // Get aggregate metrics
    const metrics = await getQualityMetricsForUsers({
      repcardUserIds,
      startDate,
      endDate,
      useCache: false, // Don't use cache for aggregate calculation
    });

    // Calculate individual metrics for top performers
    const userMetrics = await Promise.all(
      (users as any[]).map(async (user: any) => {
        const userMetric = await getQualityMetricsForUsers({
          repcardUserIds: [user.repcard_user_id],
          startDate,
          endDate,
          useCache: false,
        });
        
        const qualityScore = calculateCompositeQualityScore(userMetric);
        
        return {
          userId: user.id,
          userName: user.name,
          qualityScore,
        };
      })
    );

    // Sort by quality score and take top 5
    const topPerformers = userMetrics
      .sort((a, b) => b.qualityScore - a.qualityScore)
      .slice(0, 5);

    // Build office metrics
    const officeMetrics: OfficeQualityMetrics = {
      ...metrics,
      officeName,
      userCount: (users as any[]).length,
      topPerformers,
    };

    // Cache result
    const cacheKey = buildCacheKey('office', officeName, startDate, endDate);
    if (useCache) {
      setCachedMetrics(cacheKey, officeMetrics);
    }

    return officeMetrics;
  } catch (error) {
    console.error('Error getting office quality metrics:', error);
    throw new Error(`Failed to get office quality metrics: ${error instanceof Error ? error.message : 'Unknown error'}`);
  }
}

/**
 * Get quality metrics grouped by time periods
 */
export async function getQualityMetricsByTimeRange(
  params: QualityMetricsParams & { groupBy: 'day' | 'week' | 'month' }
): Promise<Array<{ metrics: QualityMetrics; label: string }>> {
  try {
    const startDate = parseISO(params.startDate);
    const endDate = parseISO(params.endDate);
    
    // Generate time periods based on groupBy
    const periods: Array<{ start: Date; end: Date; label: string }> = [];
    
    if (params.groupBy === 'day') {
      for (let d = new Date(startDate); d <= endDate; d.setDate(d.getDate() + 1)) {
        const dayStart = new Date(d);
        const dayEnd = new Date(d);
        dayEnd.setHours(23, 59, 59, 999);
        
        periods.push({
          start: dayStart,
          end: dayEnd,
          label: d.toISOString().split('T')[0],
        });
      }
    } else if (params.groupBy === 'week') {
      // Implementation for weekly grouping
      // This is a simplified version - you might want to use a proper date library
      for (let d = new Date(startDate); d <= endDate; d.setDate(d.getDate() + 7)) {
        const weekStart = new Date(d);
        const weekEnd = new Date(d);
        weekEnd.setDate(weekEnd.getDate() + 6);
        weekEnd.setHours(23, 59, 59, 999);
        
        periods.push({
          start: weekStart,
          end: weekEnd,
          label: `Week of ${weekStart.toISOString().split('T')[0]}`,
        });
      }
    } else if (params.groupBy === 'month') {
      // Implementation for monthly grouping
      for (let d = new Date(startDate); d <= endDate; d.setMonth(d.getMonth() + 1)) {
        const monthStart = new Date(d.getFullYear(), d.getMonth(), 1);
        const monthEnd = new Date(d.getFullYear(), d.getMonth() + 1, 0);
        monthEnd.setHours(23, 59, 59, 999);
        
        periods.push({
          start: monthStart,
          end: monthEnd,
          label: `${monthStart.getFullYear()}-${String(monthStart.getMonth() + 1).padStart(2, '0')}`,
        });
      }
    }

    // Calculate metrics for each period
    const results = await Promise.all(
      periods.map(async (period) => {
        const periodMetrics = await getQualityMetricsForUsers({
          repcardUserIds: params.repcardUserIds,
          startDate: period.start.toISOString().split('T')[0],
          endDate: period.end.toISOString().split('T')[0],
          useCache: false,
        });
        
        return {
          metrics: periodMetrics,
          label: period.label,
        };
      })
    );

    return results;
  } catch (error) {
    console.error('Error getting quality metrics by time range:', error);
    throw new Error(`Failed to get quality metrics by time range: ${error instanceof Error ? error.message : 'Unknown error'}`);
  }
}

/**
 * Calculate composite quality score
 */
export function calculateCompositeQualityScore(metrics: QualityMetrics): number {
  const appointmentSpeed = metrics.appointmentSpeed.percentageWithin24Hours;
  const attachmentRate = metrics.attachmentRate.percentageWithAttachments;
  const rescheduleRate = Math.max(0, 100 - (metrics.rescheduleRate.averageReschedulesPerCustomer * 10));
  const followUpConsistency = metrics.followUpConsistency.percentageWithFollowUps;

  const compositeScore = (
    appointmentSpeed * 0.3 +
    attachmentRate * 0.2 +
    rescheduleRate * 0.25 +
    followUpConsistency * 0.25
  );

  return Math.max(0, Math.min(100, compositeScore));
}

/**
 * Format metrics for display
 */
export function formatMetricsForDisplay(metrics: QualityMetrics): Record<string, string> {
  return {
    appointmentSpeed: `${metrics.appointmentSpeed.percentageWithin24Hours.toFixed(1)}%`,
    attachmentRate: `${metrics.attachmentRate.percentageWithAttachments.toFixed(1)}%`,
    rescheduleRate: `${metrics.rescheduleRate.averageReschedulesPerCustomer.toFixed(1)}`,
    followUpConsistency: `${metrics.followUpConsistency.percentageWithFollowUps.toFixed(1)}%`,
    compositeScore: `${calculateCompositeQualityScore(metrics).toFixed(1)}`,
  };
}
