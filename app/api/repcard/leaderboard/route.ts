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
    const configId = searchParams.get('configId'); // Optional: use configuration
    let role = (searchParams.get('role') || 'all') as LeaderboardRole;
    let metric = (searchParams.get('metric') || 'quality_score') as LeaderboardMetric;
    let timeRange = searchParams.get('timeRange') || 'month';
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    let officeIds = searchParams.get('officeIds')?.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id));
    const limit = Math.min(parseInt(searchParams.get('limit') || '50'), 100);
    const page = Math.max(parseInt(searchParams.get('page') || '1'), 1);
    
    // If configId is provided, load configuration
    let config: any = null;
    if (configId) {
      try {
        const configResult = await sql`
          SELECT * FROM repcard_leaderboard_config
          WHERE id = ${configId} AND enabled = true
        `;
        const configData = Array.isArray(configResult) ? configResult[0] : configResult.rows?.[0];
        if (configData) {
          config = configData;
          // Override parameters with config values if not explicitly provided
          if (!searchParams.get('role')) role = (config.roles?.[0] || 'all') as LeaderboardRole;
          if (!searchParams.get('metric')) metric = config.rank_by_metric as LeaderboardMetric;
          if (!searchParams.get('timeRange')) timeRange = config.date_range_default || 'month';
          if (config.office_ids && config.office_ids.length > 0 && !officeIds) {
            officeIds = config.office_ids;
          }
        }
      } catch (error) {
        console.error('[RepCard Leaderboard] Error loading config:', error);
        // Continue without config if load fails
      }
    }
    
    // Validate parameters - dynamically fetch valid metrics from database
    const validRoles: LeaderboardRole[] = ['setter', 'closer', 'all'];
    const validTimeRanges = ['today', 'week', 'month', 'quarter', 'ytd', 'custom', 'last_30', 'last_90', 'last_12_months'];
    
    // Get valid metrics from database (enabled and leaderboard_supported)
    let validMetrics: string[] = [];
    try {
      const metricsResult = await sql`
        SELECT metric_key FROM repcard_metric_definitions
        WHERE enabled = true AND leaderboard_supported = true
      `;
      validMetrics = Array.isArray(metricsResult) 
        ? metricsResult.map((m: any) => m.metric_key)
        : metricsResult.rows?.map((m: any) => m.metric_key) || [];
      
      // Log for debugging
      console.log(`[RepCard Leaderboard] Found ${validMetrics.length} valid metrics from database:`, validMetrics);
      
      // If no metrics found, use fallback
      if (validMetrics.length === 0) {
        console.warn('[RepCard Leaderboard] No metrics found in database, using fallback list');
        validMetrics = ['doors_knocked', 'appointments_set', 'sales_closed', 'revenue', 'quality_score', 'appointment_speed', 'attachment_rate'];
      }
    } catch (error) {
      console.error('[RepCard Leaderboard] Error fetching metrics:', error);
      // Fallback to hardcoded list if database query fails
      validMetrics = ['doors_knocked', 'appointments_set', 'sales_closed', 'revenue', 'quality_score', 'appointment_speed', 'attachment_rate'];
      console.log('[RepCard Leaderboard] Using fallback metrics:', validMetrics);
    }
    
    // If config is provided, validate metric is in enabled_metrics
    if (config && config.enabled_metrics && config.enabled_metrics.length > 0) {
      if (!config.enabled_metrics.includes(metric)) {
        const duration = Date.now() - start;
        logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
        return NextResponse.json(
          { error: `Metric ${metric} is not enabled in this leaderboard configuration. Enabled metrics: ${config.enabled_metrics.join(', ')}` },
          { status: 400 }
        );
      }
    }
    
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
      console.error(`[RepCard Leaderboard] Invalid metric requested: ${metric}`);
      console.error(`[RepCard Leaderboard] Valid metrics: ${validMetrics.join(', ')}`);
      return NextResponse.json(
        { 
          error: `Invalid metric. Must be one of: ${validMetrics.join(', ')}`,
          requestedMetric: metric,
          validMetrics: validMetrics
        },
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
      
      // Log for debugging
      console.log(`[RepCard Leaderboard] Date range: ${calculatedStartDate} to ${calculatedEndDate} (timeRange: ${timeRange})`);
      
      // DEBUG: Check if ANY data exists in the date range
      try {
        const dataCheck = await sql`
          SELECT 
            (SELECT COUNT(*) FROM repcard_customers WHERE created_at::date >= ${calculatedStartDate}::date AND created_at::date <= ${calculatedEndDate}::date) as customers,
            (SELECT COUNT(*) FROM repcard_appointments WHERE (
              (scheduled_at IS NOT NULL AND scheduled_at::date >= ${calculatedStartDate}::date AND scheduled_at::date <= ${calculatedEndDate}::date)
              OR
              (scheduled_at IS NULL AND created_at::date >= ${calculatedStartDate}::date AND created_at::date <= ${calculatedEndDate}::date)
            )) as appointments,
            (SELECT COUNT(*) FROM repcard_appointments a
             INNER JOIN users u ON u.repcard_user_id = a.setter_user_id
             WHERE (
               (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
               OR
               (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
             )) as matched_appointments,
            (SELECT MIN(created_at) FROM repcard_customers) as earliest_customer,
            (SELECT MAX(created_at) FROM repcard_customers) as latest_customer
        `;
        const check = Array.isArray(dataCheck) ? dataCheck[0] : dataCheck.rows?.[0];
        console.log(`[RepCard Leaderboard] DEBUG - Data in date range:`, {
          customers: check?.customers || 0,
          appointments_total: check?.appointments || 0,
          appointments_matched: check?.matched_appointments || 0,
          earliest_customer: check?.earliest_customer,
          latest_customer: check?.latest_customer,
          query_range: `${calculatedStartDate} to ${calculatedEndDate}`
        });
      } catch (debugError) {
        console.error('[RepCard Leaderboard] DEBUG query failed:', debugError);
      }
    } catch (error) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
      return NextResponse.json(
        { error: error instanceof Error ? error.message : 'Invalid date range' },
        { status: 400 }
      );
    }
    
    // Build cache key (include configId if present)
    const cacheKey = `${configId || 'manual'}:${role}:${metric}:${timeRange}:${calculatedStartDate}:${calculatedEndDate}:${officeIds ? officeIds.map(String).join(',') : 'all'}:${limit}:${page}`;
    
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
    
    // NEW APPROACH: Query RepCard users directly from repcard_users table
    // This shows ALL RepCard data regardless of user linking status
    // User linking is optional - it just adds app-specific names/roles
    let repcardUsers: any[] = [];
    let officeFilterFailed = false;

    // Build query for RepCard users - use repcard_users as primary source
    // Optionally join users table for app-specific info (names, roles) if linked
    if (officeIds && officeIds.length > 0) {
      // Filter by office_id from repcard_users or repcard_offices
      const officeQuery = await sql`
        SELECT DISTINCT 
          ru.repcard_user_id, 
          ru.first_name, 
          ru.last_name, 
          ru.email, 
          ru.office_id, 
          ru.office_name, 
          ru.role as repcard_role, 
          ru.status,
          u.id as app_user_id, 
          u.name as app_user_name, 
          u.role as app_role, 
          u.sales_office[1] as app_office
        FROM repcard_users ru
        LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
        LEFT JOIN repcard_offices ro ON ro.repcard_office_id = ru.office_id
        LEFT JOIN offices o ON o.name = ru.office_name OR o.name = ANY(u.sales_office)
        WHERE ru.status = 1
          AND (ro.repcard_office_id = ANY(${officeIds}::int[]) 
               OR o.quickbase_office_id = ANY(${officeIds}::int[]))
        ${role !== 'all' ? sql`AND (ru.role = ${role} OR u.role = ${role})` : sql``}
        LIMIT 1000
      `;
      repcardUsers = Array.from(officeQuery);
      
      if (repcardUsers.length === 0) {
        console.log(`[RepCard Leaderboard] ⚠️ Office filter returned 0 RepCard users, falling back to all`);
        officeFilterFailed = true;
        officeIds = undefined;
      }
    }
    
    // If no office filter or office filter failed, get all active RepCard users
    if (repcardUsers.length === 0) {
      const allUsersQuery = await sql`
        SELECT DISTINCT 
          ru.repcard_user_id, 
          ru.first_name, 
          ru.last_name, 
          ru.email,
          ru.office_id, 
          ru.office_name, 
          ru.role as repcard_role, 
          ru.status,
          u.id as app_user_id, 
          u.name as app_user_name, 
          u.role as app_role, 
          u.sales_office[1] as app_office
        FROM repcard_users ru
        LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
        WHERE ru.status = 1
        ${role !== 'all' ? sql`AND (ru.role = ${role} OR u.role = ${role})` : sql``}
        LIMIT 1000
      `;
      repcardUsers = Array.from(allUsersQuery);
    }
    
    // Extract RepCard user IDs for metric queries
    const repcardUserIds = repcardUsers.map((ru: any) => ru.repcard_user_id).filter(Boolean);
    
    if (repcardUserIds.length === 0) {
      console.log(`[RepCard Leaderboard] ⚠️ No RepCard users found`);
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
          calculatedAt: new Date().toISOString(),
          warning: 'No RepCard users found. Ensure RepCard data has been synced.'
        }
      };
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
      return NextResponse.json(response);
    }
    
    console.log(`[RepCard Leaderboard] Found ${repcardUserIds.length} RepCard users for leaderboard`);
    
    // Create user map for easy lookup (prefer app user info, fallback to RepCard info)
    const userMap = new Map();
    repcardUsers.forEach((ru: any) => {
      userMap.set(ru.repcard_user_id, {
        userId: ru.app_user_id || `repcard_${ru.repcard_user_id}`, // Use app user ID if linked, otherwise create temp ID
        userName: ru.app_user_name || `${ru.first_name || ''} ${ru.last_name || ''}`.trim() || ru.email || `RepCard User ${ru.repcard_user_id}`,
        userEmail: ru.email,
        repcardUserId: ru.repcard_user_id,
        office: ru.app_office || ru.office_name,
        role: ru.app_role || ru.repcard_role
      });
    });
    
    // For backward compatibility, create users array
    const users = Array.from(userMap.values());
    
    // Fetch metric data based on selected metric
    let leaderboardEntries: LeaderboardEntry[] = [];
    
    // Track if office filter failed (so we can skip office filtering in metric queries)
    const shouldSkipOfficeFilter = officeIds === undefined && officeFilterFailed;
    
    if (metric === 'quality_score' || metric === 'appointment_speed' || metric === 'attachment_rate') {
      // Calculate quality metrics from database instead of API to avoid rate limiting
      // Ensure repcardUserIds is an array of integers (migration 018 normalized to INTEGER)
      const repcardUserIds = users.map((u: any) => {
        const id = u.repcard_user_id;
        return typeof id === 'number' ? id : parseInt(String(id), 10);
      }).filter((id: any) => !isNaN(id) && id > 0);
      
      if (metric === 'appointment_speed') {
        // Calculate appointment speed: % of appointments scheduled within 24 hours of customer creation
        // Query RepCard tables directly - don't require user linking
        const speedQuery = await sql`
          SELECT
            ru.repcard_user_id,
            ru.first_name,
            ru.last_name,
            ru.email,
            ru.office_name,
            ru.role as repcard_role,
            u.id as app_user_id,
            u.name as app_user_name,
            u.role as app_role,
            u.sales_office[1] as app_office,
            COUNT(DISTINCT a.repcard_appointment_id) as total_appointments,
            COUNT(DISTINCT CASE 
              WHEN c.created_at IS NOT NULL 
                AND a.scheduled_at IS NOT NULL
                AND EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600 < 24
              THEN a.repcard_appointment_id
            END) as appointments_within_24h
          FROM repcard_users ru
          LEFT JOIN repcard_appointments a ON ru.repcard_user_id = a.setter_user_id
            AND (
              (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
              OR
              (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
            )
          LEFT JOIN repcard_customers c ON a.repcard_customer_id = c.repcard_customer_id
          LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
          WHERE ru.repcard_user_id = ANY(${repcardUserIds}::int[])
            AND ru.status = 1
          GROUP BY ru.repcard_user_id, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role, u.id, u.name, u.role, u.sales_office
        `;
        
        const speedResults = Array.from(speedQuery);
        leaderboardEntries = speedResults.map((row: any) => {
          const total = parseInt(row.total_appointments) || 0;
          const within24h = parseInt(row.appointments_within_24h) || 0;
          const percentage = total > 0 ? (within24h / total) * 100 : 0;
          
          // Use app user info if linked, otherwise use RepCard info
          const userInfo = userMap.get(row.repcard_user_id) || {
            userId: `repcard_${row.repcard_user_id}`,
            userName: `${row.first_name || ''} ${row.last_name || ''}`.trim() || row.email || `RepCard User ${row.repcard_user_id}`,
            userEmail: row.email,
            office: row.app_office || row.office_name,
            role: row.app_role || row.repcard_role
          };
          
          return {
            rank: 0,
            userId: userInfo.userId,
            userName: userInfo.userName,
            userEmail: userInfo.userEmail,
            office: userInfo.office,
            role: userInfo.role,
            metricValue: percentage,
            metricType: metric
          };
        });
      } else if (metric === 'attachment_rate') {
        // Calculate attachment rate: % of customers with attachments
        // Query RepCard tables directly - don't require user linking
        const attachmentQuery = await sql`
          SELECT
            ru.repcard_user_id,
            ru.first_name,
            ru.last_name,
            ru.email,
            ru.office_name,
            ru.role as repcard_role,
            u.id as app_user_id,
            u.name as app_user_name,
            u.role as app_role,
            u.sales_office[1] as app_office,
            COUNT(DISTINCT c.repcard_customer_id) as total_customers,
            COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END) as customers_with_attachments
          FROM repcard_users ru
          LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
            AND c.created_at::date >= ${calculatedStartDate}::date
            AND c.created_at::date <= ${calculatedEndDate}::date
          LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
          LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
          WHERE ru.repcard_user_id = ANY(${repcardUserIds}::int[])
            AND ru.status = 1
          GROUP BY ru.repcard_user_id, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role, u.id, u.name, u.role, u.sales_office
        `;
        
        const attachmentResults = Array.from(attachmentQuery);
        leaderboardEntries = attachmentResults.map((row: any) => {
          const total = parseInt(row.total_customers) || 0;
          const withAttachments = parseInt(row.customers_with_attachments) || 0;
          const percentage = total > 0 ? (withAttachments / total) * 100 : 0;
          
          // Use app user info if linked, otherwise use RepCard info
          const userInfo = userMap.get(row.repcard_user_id) || {
            userId: `repcard_${row.repcard_user_id}`,
            userName: `${row.first_name || ''} ${row.last_name || ''}`.trim() || row.email || `RepCard User ${row.repcard_user_id}`,
            userEmail: row.email,
            office: row.app_office || row.office_name,
            role: row.app_role || row.repcard_role
          };
          
          return {
            rank: 0,
            userId: userInfo.userId,
            userName: userInfo.userName,
            userEmail: userInfo.userEmail,
            office: userInfo.office,
            role: userInfo.role,
            metricValue: percentage,
            metricType: metric
          };
        });
      } else if (metric === 'quality_score') {
        // Calculate composite quality score from appointment speed and attachment rate
        // Query RepCard tables directly - don't require user linking
        const [speedQuery, attachmentQuery] = await Promise.all([
          sql`
            SELECT
              ru.repcard_user_id,
              COUNT(DISTINCT a.repcard_appointment_id) as total_appointments,
              COUNT(DISTINCT CASE 
                WHEN c.created_at IS NOT NULL 
                  AND a.scheduled_at IS NOT NULL
                  AND EXTRACT(EPOCH FROM (a.scheduled_at - c.created_at)) / 3600 < 24
                THEN a.repcard_appointment_id
              END) as appointments_within_24h
            FROM repcard_users ru
            LEFT JOIN repcard_appointments a ON ru.repcard_user_id = a.setter_user_id
              AND (
                (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                OR
                (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
              )
            LEFT JOIN repcard_customers c ON a.repcard_customer_id = c.repcard_customer_id
            WHERE ru.repcard_user_id = ANY(${repcardUserIds}::int[])
              AND ru.status = 1
            GROUP BY ru.repcard_user_id
          `,
          sql`
            SELECT
              ru.repcard_user_id,
              COUNT(DISTINCT c.repcard_customer_id) as total_customers,
              COUNT(DISTINCT CASE WHEN att.id IS NOT NULL THEN c.repcard_customer_id END) as customers_with_attachments
            FROM repcard_users ru
            LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
              AND c.created_at::date >= ${calculatedStartDate}::date
              AND c.created_at::date <= ${calculatedEndDate}::date
            LEFT JOIN repcard_customer_attachments att ON c.repcard_customer_id = att.repcard_customer_id::text
            WHERE ru.repcard_user_id = ANY(${repcardUserIds}::int[])
              AND ru.status = 1
            GROUP BY ru.repcard_user_id
          `
        ]);
        
        const speedResults = Array.from(speedQuery);
        const attachmentResults = Array.from(attachmentQuery);
        
        // Create maps for fast lookup (keyed by repcard_user_id)
        const speedMap = new Map();
        speedResults.forEach((row: any) => {
          const total = parseInt(row.total_appointments) || 0;
          const within24h = parseInt(row.appointments_within_24h) || 0;
          speedMap.set(row.repcard_user_id, total > 0 ? (within24h / total) * 100 : 0);
        });
        
        const attachmentMap = new Map();
        attachmentResults.forEach((row: any) => {
          const total = parseInt(row.total_customers) || 0;
          const withAttachments = parseInt(row.customers_with_attachments) || 0;
          attachmentMap.set(row.repcard_user_id, total > 0 ? (withAttachments / total) * 100 : 0);
        });
        
        // Calculate composite score for each RepCard user
        leaderboardEntries = repcardUsers.map((ru: any) => {
          const appointmentSpeed = speedMap.get(ru.repcard_user_id) || 0;
          const attachmentRate = attachmentMap.get(ru.repcard_user_id) || 0;
          
          // Simplified composite score: 60% appointment speed, 40% attachment rate
          const compositeScore = (appointmentSpeed * 0.6) + (attachmentRate * 0.4);
          
          // Use app user info if linked, otherwise use RepCard info
          const userInfo = userMap.get(ru.repcard_user_id) || {
            userId: `repcard_${ru.repcard_user_id}`,
            userName: `${ru.first_name || ''} ${ru.last_name || ''}`.trim() || ru.email || `RepCard User ${ru.repcard_user_id}`,
            userEmail: ru.email,
            office: ru.office_name,
            role: ru.repcard_role
          };
          
          return {
            rank: 0,
            userId: userInfo.userId,
            userName: userInfo.userName,
            userEmail: userInfo.userEmail,
            office: userInfo.office,
            role: userInfo.role,
            metricValue: Math.max(0, Math.min(100, compositeScore)),
            metricType: metric
          };
        });
      }
    } else {
      // Fetch data from RepCard or QuickBase
      // repcardUserIds already extracted above from repcardUsers
      
      if (metric === 'doors_knocked') {
        // Query ALL RepCard users (setters/closers) from repcard_users table
        // LEFT JOIN to users table to get app user info if linked
        // LEFT JOIN to repcard_customers to get door knock counts
        // This shows ALL RepCard users regardless of linking status
        let customerCountsRaw;
        
        if (officeIds && officeIds.length > 0 && !shouldSkipOfficeFilter) {
          // With office filter
          if (role !== 'all') {
            customerCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(c.repcard_customer_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
                AND c.created_at::date >= ${calculatedStartDate}::date
                AND c.created_at::date <= ${calculatedEndDate}::date
              LEFT JOIN offices o ON o.name = COALESCE(u.sales_office[1], ru.office_name)
              WHERE ru.status = 1
                AND (COALESCE(u.role, ru.role) = ${role} OR (u.role IS NULL AND ru.role = ${role}))
                AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR (u.sales_office IS NULL AND ru.office_name IS NULL))
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          } else {
            customerCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(c.repcard_customer_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
                AND c.created_at::date >= ${calculatedStartDate}::date
                AND c.created_at::date <= ${calculatedEndDate}::date
              LEFT JOIN offices o ON o.name = COALESCE(u.sales_office[1], ru.office_name)
              WHERE ru.status = 1
                AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR (u.sales_office IS NULL AND ru.office_name IS NULL))
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          }
        } else {
          // No office filter
          if (role !== 'all') {
            customerCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(c.repcard_customer_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
                AND c.created_at::date >= ${calculatedStartDate}::date
                AND c.created_at::date <= ${calculatedEndDate}::date
              WHERE ru.status = 1
                AND (COALESCE(u.role, ru.role) = ${role} OR (u.role IS NULL AND ru.role = ${role}))
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          } else {
            customerCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(c.repcard_customer_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_customers c ON ru.repcard_user_id = c.setter_user_id
                AND c.created_at::date >= ${calculatedStartDate}::date
                AND c.created_at::date <= ${calculatedEndDate}::date
              WHERE ru.status = 1
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          }
        }
        
        const customerCounts = Array.from(customerCountsRaw);
        
        console.log(`[RepCard Leaderboard] doors_knocked query returned ${customerCounts.length} RepCard users with data`);

        // Map directly to leaderboard entries
        leaderboardEntries = customerCounts.map((row: any) => ({
          rank: 0,
          userId: row.user_id,
          userName: row.user_name || 'Unknown',
          userEmail: row.user_email,
          office: row.office,
          role: row.role,
          metricValue: parseInt(row.count),
          metricType: metric
        }));
        
        // CRITICAL FIX: If INNER JOIN returns 0 results, fallback to show ALL users
        if (leaderboardEntries.length === 0) {
          console.log(`[RepCard Leaderboard] ⚠️ INNER JOIN returned 0 entries for doors_knocked`);
          console.log(`[RepCard Leaderboard] Date range: ${calculatedStartDate} to ${calculatedEndDate}`);
          console.log(`[RepCard Leaderboard] Falling back to LEFT JOIN to show all users`);
          
          let fallbackQuery;
          
          if (officeIds && officeIds.length > 0) {
            if (role !== 'all') {
              fallbackQuery = await sql`
                SELECT
                  u.id as user_id,
                  u.name as user_name,
                  u.email as user_email,
                  u.repcard_user_id,
                  u.sales_office[1] as office,
                  u.role,
                  COUNT(c.repcard_customer_id) as count
                FROM users u
                LEFT JOIN repcard_customers c ON u.repcard_user_id = c.setter_user_id
                  AND c.created_at::date >= ${calculatedStartDate}::date
                  AND c.created_at::date <= ${calculatedEndDate}::date
                LEFT JOIN offices o ON o.name = ANY(u.sales_office)
                WHERE u.repcard_user_id IS NOT NULL
                  AND u.role = ${role}
                  AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR u.sales_office IS NULL)
                GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
                ORDER BY count DESC
                LIMIT 1000
              `;
            } else {
              fallbackQuery = await sql`
                SELECT
                  u.id as user_id,
                  u.name as user_name,
                  u.email as user_email,
                  u.repcard_user_id,
                  u.sales_office[1] as office,
                  u.role,
                  COUNT(c.repcard_customer_id) as count
                FROM users u
                LEFT JOIN repcard_customers c ON u.repcard_user_id = c.setter_user_id
                  AND c.created_at::date >= ${calculatedStartDate}::date
                  AND c.created_at::date <= ${calculatedEndDate}::date
                LEFT JOIN offices o ON o.name = ANY(u.sales_office)
                WHERE u.repcard_user_id IS NOT NULL
                  AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR u.sales_office IS NULL)
                GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
                ORDER BY count DESC
                LIMIT 1000
              `;
            }
          } else {
            if (role !== 'all') {
              fallbackQuery = await sql`
                SELECT
                  u.id as user_id,
                  u.name as user_name,
                  u.email as user_email,
                  u.repcard_user_id,
                  u.sales_office[1] as office,
                  u.role,
                  COUNT(c.repcard_customer_id) as count
                FROM users u
                LEFT JOIN repcard_customers c ON u.repcard_user_id = c.setter_user_id
                  AND c.created_at::date >= ${calculatedStartDate}::date
                  AND c.created_at::date <= ${calculatedEndDate}::date
                WHERE u.repcard_user_id IS NOT NULL
                  AND u.role = ${role}
                GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
                ORDER BY count DESC
                LIMIT 1000
              `;
            } else {
              fallbackQuery = await sql`
                SELECT
                  u.id as user_id,
                  u.name as user_name,
                  u.email as user_email,
                  u.repcard_user_id,
                  u.sales_office[1] as office,
                  u.role,
                  COUNT(c.repcard_customer_id) as count
                FROM users u
                LEFT JOIN repcard_customers c ON u.repcard_user_id = c.setter_user_id
                  AND c.created_at::date >= ${calculatedStartDate}::date
                  AND c.created_at::date <= ${calculatedEndDate}::date
                WHERE u.repcard_user_id IS NOT NULL
                GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
                ORDER BY count DESC
                LIMIT 1000
              `;
            }
          }
          
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
      } else if (metric === 'appointments_set') {
        // Query ALL RepCard users (setters/closers) from repcard_users table
        // LEFT JOIN to users table to get app user info if linked
        // LEFT JOIN to repcard_appointments to get appointment counts
        // This shows ALL RepCard users regardless of linking status
        let appointmentCountsRaw;
        
        if (officeIds && officeIds.length > 0 && !shouldSkipOfficeFilter) {
          // With office filter
          if (role !== 'all') {
            appointmentCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(a.repcard_appointment_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_appointments a ON ru.repcard_user_id = a.setter_user_id
                AND (
                  (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                  OR
                  (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
                )
              LEFT JOIN offices o ON o.name = COALESCE(u.sales_office[1], ru.office_name)
              WHERE ru.status = 1
                AND (COALESCE(u.role, ru.role) = ${role} OR (u.role IS NULL AND ru.role = ${role}))
                AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR (u.sales_office IS NULL AND ru.office_name IS NULL))
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          } else {
            appointmentCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(a.repcard_appointment_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_appointments a ON ru.repcard_user_id = a.setter_user_id
                AND (
                  (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                  OR
                  (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
                )
              LEFT JOIN offices o ON o.name = COALESCE(u.sales_office[1], ru.office_name)
              WHERE ru.status = 1
                AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR (u.sales_office IS NULL AND ru.office_name IS NULL))
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          }
        } else {
          // No office filter
          if (role !== 'all') {
            appointmentCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(a.repcard_appointment_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_appointments a ON ru.repcard_user_id = a.setter_user_id
                AND (
                  (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                  OR
                  (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
                )
              WHERE ru.status = 1
                AND (COALESCE(u.role, ru.role) = ${role} OR (u.role IS NULL AND ru.role = ${role}))
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          } else {
            appointmentCountsRaw = await sql`
              SELECT
                COALESCE(u.id, ru.repcard_user_id::text) as user_id,
                COALESCE(u.name, TRIM(ru.first_name || ' ' || ru.last_name)) as user_name,
                COALESCE(u.email, ru.email) as user_email,
                ru.repcard_user_id,
                COALESCE(u.sales_office[1], ru.office_name) as office,
                COALESCE(u.role, ru.role) as role,
                COUNT(a.repcard_appointment_id) as count
              FROM repcard_users ru
              LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
              LEFT JOIN repcard_appointments a ON ru.repcard_user_id = a.setter_user_id
                AND (
                  (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                  OR
                  (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
                )
              WHERE ru.status = 1
              GROUP BY ru.repcard_user_id, u.id, u.name, u.email, u.sales_office, u.role, ru.first_name, ru.last_name, ru.email, ru.office_name, ru.role
            `;
          }
        }
        
        const appointmentCounts = Array.from(appointmentCountsRaw);
        
        console.log(`[RepCard Leaderboard] appointments_set query returned ${appointmentCounts.length} RepCard users with data`);
        if (appointmentCounts.length > 0) {
          console.log(`[RepCard Leaderboard] Sample appointment data:`, appointmentCounts.slice(0, 3).map((r: any) => ({
            user: r.user_name,
            count: r.count,
            repcard_user_id: r.repcard_user_id
          })));
        }

        // Map directly to leaderboard entries
        leaderboardEntries = appointmentCounts.map((row: any) => ({
          rank: 0,
          userId: row.user_id,
          userName: row.user_name || 'Unknown',
          userEmail: row.user_email,
          office: row.office,
          role: row.role,
          metricValue: parseInt(row.count),
          metricType: metric
        }));
        
        // CRITICAL FIX: If INNER JOIN returns 0 results, fallback to show ALL users
        if (leaderboardEntries.length === 0) {
          console.log(`[RepCard Leaderboard] ⚠️ INNER JOIN returned 0 entries for appointments_set`);
          console.log(`[RepCard Leaderboard] Date range: ${calculatedStartDate} to ${calculatedEndDate}`);
          console.log(`[RepCard Leaderboard] Role filter: ${role}, Office IDs: ${officeIds?.join(',') || 'none'}`);
          console.log(`[RepCard Leaderboard] shouldSkipOfficeFilter: ${shouldSkipOfficeFilter}, officeFilterFailed: ${officeFilterFailed}`);
          console.log(`[RepCard Leaderboard] Falling back to LEFT JOIN to show all users`);
          
          let fallbackQuery;
          
          // Skip office filter if it already failed (shouldSkipOfficeFilter) OR if officeIds was cleared
          if (officeIds && officeIds.length > 0 && !shouldSkipOfficeFilter) {
            console.log(`[RepCard Leaderboard] Fallback: Using office filter`);
            if (role !== 'all') {
              fallbackQuery = await sql`
                SELECT
                  u.id as user_id,
                  u.name as user_name,
                  u.email as user_email,
                  u.repcard_user_id,
                  u.sales_office[1] as office,
                  u.role,
                  COUNT(a.repcard_appointment_id) as count
                FROM users u
                LEFT JOIN repcard_appointments a ON u.repcard_user_id = a.setter_user_id
                  AND (
                    (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                    OR
                    (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
                  )
                LEFT JOIN offices o ON o.name = ANY(u.sales_office)
                WHERE u.repcard_user_id IS NOT NULL
                  AND u.role = ${role}
                  AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR u.sales_office IS NULL)
                GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
                ORDER BY count DESC
                LIMIT 1000
              `;
            } else {
              fallbackQuery = await sql`
                SELECT
                  u.id as user_id,
                  u.name as user_name,
                  u.email as user_email,
                  u.repcard_user_id,
                  u.sales_office[1] as office,
                  u.role,
                  COUNT(a.repcard_appointment_id) as count
                FROM users u
                LEFT JOIN repcard_appointments a ON u.repcard_user_id = a.setter_user_id
                  AND (
                    (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                    OR
                    (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
                  )
                LEFT JOIN offices o ON o.name = ANY(u.sales_office)
                WHERE u.repcard_user_id IS NOT NULL
                  AND (o.quickbase_office_id = ANY(${officeIds}::int[]) OR u.sales_office IS NULL)
                GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
                ORDER BY count DESC
                LIMIT 1000
              `;
            }
            
            // Process fallbackQuery immediately after setting it
            if (fallbackQuery) {
              const fallbackResults = Array.from(fallbackQuery);
              console.log(`[RepCard Leaderboard] Fallback query returned ${fallbackResults.length} users`);
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
            }
          } else {
            console.log(`[RepCard Leaderboard] Fallback: Skipping office filter (officeIds cleared or shouldSkipOfficeFilter=true)`);
            
            // Use the users we already fetched instead of querying again
            // This ensures we're working with the same user set
            const userIds = users.map((u: any) => u.id).filter(Boolean);
            
            if (userIds.length === 0) {
              console.log(`[RepCard Leaderboard] ⚠️ No users available for fallback query`);
              // Create entries from users array with 0 counts
              leaderboardEntries = users.map((user: any) => ({
                rank: 0,
                userId: user.id,
                userName: user.name,
                userEmail: user.email,
                office: user.office,
                role: user.role,
                metricValue: 0,
                metricType: metric
              }));
            } else {
              // Query appointments for the users we already have
              // Use repcard_user_ids instead of user ids for better matching
              const repcardUserIds = users.map((u: any) => u.repcard_user_id).filter(Boolean);
              
              const appointmentCountsRaw = await sql`
                SELECT
                  u.id as user_id,
                  u.name as user_name,
                  u.email as user_email,
                  u.repcard_user_id,
                  u.sales_office[1] as office,
                  u.role,
                  COUNT(a.repcard_appointment_id) as count
                FROM users u
                LEFT JOIN repcard_appointments a ON u.repcard_user_id = a.setter_user_id
                  AND (
                    (a.scheduled_at IS NOT NULL AND a.scheduled_at::date >= ${calculatedStartDate}::date AND a.scheduled_at::date <= ${calculatedEndDate}::date)
                    OR
                    (a.scheduled_at IS NULL AND a.created_at::date >= ${calculatedStartDate}::date AND a.created_at::date <= ${calculatedEndDate}::date)
                  )
                WHERE u.repcard_user_id = ANY(${repcardUserIds}::int[])
                GROUP BY u.id, u.name, u.email, u.repcard_user_id, u.sales_office, u.role
                ORDER BY count DESC
              `;
              
              const fallbackResults = Array.from(appointmentCountsRaw);
              console.log(`[RepCard Leaderboard] Fallback query returned ${fallbackResults.length} users`);
              
              // If fallback still returns 0, create entries from users array
              if (fallbackResults.length === 0) {
                console.log(`[RepCard Leaderboard] ⚠️ Fallback query returned 0 results, creating entries from users array`);
                leaderboardEntries = users.map((user: any) => ({
                  rank: 0,
                  userId: user.id,
                  userName: user.name,
                  userEmail: user.email,
                  office: user.office,
                  role: user.role,
                  metricValue: 0,
                  metricType: metric
                }));
              } else {
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
              }
            }
            
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
        const statusLogsRaw = await sql`
          SELECT
            sl.changed_by_user_id,
            sl.repcard_customer_id,
            sl.new_status,
            sl.old_status,
            sl.changed_at,
            c.raw_data->'customFields'->>'systemCost' as system_cost
          FROM repcard_status_logs sl
          LEFT JOIN repcard_customers c ON c.repcard_customer_id = sl.repcard_customer_id
          WHERE sl.changed_by_user_id = ANY(${repcardUserIds}::int[])
            AND sl.changed_at::date >= ${calculatedStartDate}::date
            AND sl.changed_at::date <= ${calculatedEndDate}::date
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
          const userId = log.changed_by_user_id;
          if (!userId) continue;
          
          const userIdStr = String(userId);
          if (!salesByUser.has(userIdStr)) {
            salesByUser.set(userIdStr, { count: 0, revenue: 0 });
          }
          
          const userSales = salesByUser.get(userIdStr)!;
          userSales.count++;
          
          // Add revenue if available
          if (log.system_cost) {
            const cost = parseFloat(log.system_cost) || 0;
            userSales.revenue += cost;
          }
        }

        leaderboardEntries = (users as any[]).map((user: any) => {
          const userSales = salesByUser.get(String(user.repcard_user_id)) || { count: 0, revenue: 0 };
          
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
    } // End of else block for RepCard/QuickBase data fetching (closes else from line 594)
    } // Close the if-else chain starting at line 424
    
    // CRITICAL FIX: If we have users but no leaderboard entries, create entries with 0 counts
    // This ensures the frontend shows users even when they have no data
    if (leaderboardEntries.length === 0 && users.length > 0) {
      console.log(`[RepCard Leaderboard] ⚠️ No data found for ${users.length} users, creating entries with 0 counts`);
      leaderboardEntries = users.map((user: any) => ({
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
        calculatedAt: new Date().toISOString(),
        configId: configId || undefined,
        configName: config?.name || undefined
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
