export const runtime = 'nodejs'

// app/api/analytics/hold-analysis/route.ts
import { NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { parseHoldReason } from '@/lib/utils/reason-parser';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { buildTimezoneAwareDateFilters, calculateDaysDifference, parseQuickbaseDateInTimezone } from '@/lib/utils/timezone-helpers';
import type { HoldAnalysis, HoldReasonCategory } from '@/lib/types/analytics';

// Quickbase table ID for projects
const QB_TABLE_PROJECTS = process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na';

// Simple in-memory cache for hold analysis with 60-second TTL
const holdAnalysisCache = new Map<string, { data: any; timestamp: number }>();

/**
 * GET /api/analytics/hold-analysis
 * 
 * Fetch hold reason analysis for analytics dashboard.
 * Only accessible by office_leader, regional, and super_admin roles.
 * 
 * Query Parameters:
 * - timeRange: 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' (default: 'ytd')
 * - officeIds: comma-separated list of office IDs for filtering (optional)
 * - startDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 * - endDate: YYYY-MM-DD format for custom time range (required if timeRange='custom')
 * 
 * Response:
 * {
 *   analysis: HoldAnalysis[],
 *   metadata: {
 *     timeRange: string,
 *     totalHolds: number,
 *     resolvedHolds: number,
 *     activeHolds: number,
 *     officeCount: number,
 *     cached: boolean
 *   }
 * }
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/analytics/hold-analysis', undefined, reqId);

  // Restrict access to managers only
  const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
  if (!auth.authorized) return auth.response;

  try {
    const { searchParams } = new URL(req.url);
    if (searchParams.has('userId') || searchParams.has('role')) {
      return NextResponse.json({ error: 'Do not supply userId/role in query params' }, { status: 400 });
    }

    // Extract time range parameter with validation
    const timeRange = searchParams.get('timeRange') as 'lifetime' | 'ytd' | 'month' | 'week' | 'custom' | null;
    const validTimeRanges = ['lifetime', 'ytd', 'month', 'week', 'custom', 'last_30', 'last_90', 'last_12_months'];
    if (timeRange && !validTimeRanges.includes(timeRange)) {
      return NextResponse.json({ error: 'Invalid timeRange parameter. Must be one of: lifetime, ytd, month, week, custom' }, { status: 400 });
    }
    const effectiveTimeRange = timeRange || 'ytd'; // Default to YTD for analytics

    // Extract office IDs parameter
    const officeIdsParam = searchParams.get('officeIds');
    let officeIds: number[] | undefined;
    if (officeIdsParam) {
      try {
        officeIds = officeIdsParam.split(',').map(id => parseInt(id.trim(), 10)).filter(id => !isNaN(id));
        if (officeIds.length === 0) {
          return NextResponse.json({ error: 'Invalid officeIds parameter. Must be comma-separated numbers' }, { status: 400 });
        }
      } catch (error) {
        return NextResponse.json({ error: 'Invalid officeIds parameter. Must be comma-separated numbers' }, { status: 400 });
      }
    }

    // Extract custom date range if provided
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');

    // Validate custom date range
    if (effectiveTimeRange === 'custom') {
      if (!startDate || !endDate) {
        return NextResponse.json({ error: 'startDate and endDate are required for custom time range' }, { status: 400 });
      }

      // Validate date format (YYYY-MM-DD)
      const dateRegex = /^\d{4}-\d{2}-\d{2}$/;
      if (!dateRegex.test(startDate) || !dateRegex.test(endDate)) {
        return NextResponse.json({ error: 'Invalid date format. Use YYYY-MM-DD' }, { status: 400 });
      }

      // Validate date range is logical
      const start = new Date(startDate);
      const end = new Date(endDate);
      if (start > end) {
        return NextResponse.json({ error: 'startDate must be before or equal to endDate' }, { status: 400 });
      }
    }

    const { id, role, salesOffice, timezone } = auth.session.user as any;
    const userId = id as string;
    const userTimezone = timezone || 'America/New_York';

    // Check cache first - include timeRange, officeIds, and custom dates in cache key
    const officeKey = officeIds ? officeIds.sort().join(',') : (salesOffice ? salesOffice.sort().join(',') : '');
    const customRangeKey = effectiveTimeRange === 'custom' ? `${startDate}:${endDate}` : '';
    const cacheKey = `${userId}:${role}:${officeKey}:${effectiveTimeRange}:${customRangeKey}`;
    const cached = holdAnalysisCache.get(cacheKey);
    const cacheTTL = 60 * 1000; // 60 seconds
    
    if (cached && Date.now() - cached.timestamp < cacheTTL) {
      logApiResponse('GET', '/api/analytics/hold-analysis', Date.now() - startedAt, { cached: true, timeRange: effectiveTimeRange, totalHolds: cached.data.analysis?.length || 0 }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Build timezone-aware date filters based on timeRange - filter by DATE_ON_HOLD within the period
    const dateFilters = buildTimezoneAwareDateFilters(
      effectiveTimeRange,
      userTimezone,
      PROJECT_FIELDS.DATE_ON_HOLD,
      startDate || undefined,
      endDate || undefined
    );

    // Build office filter based on role
    let officeFilter: any = {};
    if (role === 'office_leader' && salesOffice && salesOffice.length > 0) {
      officeFilter = {
        field: PROJECT_FIELDS.OFFICE_RECORD_ID,
        operator: 'in',
        value: salesOffice
      };
    } else if (role === 'regional' && officeIds && officeIds.length > 0) {
      officeFilter = {
        field: PROJECT_FIELDS.OFFICE_RECORD_ID,
        operator: 'in',
        value: officeIds
      };
    }

    // Query projects on hold from Quickbase
    const { qbClient } = await import('@/lib/quickbase/client');
    
    // Build where clause for Quickbase query
    const whereConditions = [];
    
    // Add date filters
    if (dateFilters.length > 0) {
      dateFilters.forEach(filter => {
        whereConditions.push(`{${filter.field}.${filter.operator.toUpperCase()}.${filter.value}}`);
      });
    }
    
    // Add office filter
    if (Object.keys(officeFilter).length > 0) {
      const officeValues = officeFilter.value.map((id: number) => id.toString()).join(',');
      whereConditions.push(`{${officeFilter.field}.IN.${officeValues}}`);
    }
    
    const whereClause = whereConditions.length > 0 ? whereConditions.join(' AND ') : '';
    
    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.CANCEL_REASON,
        PROJECT_FIELDS.AUDIT_LOG_PRE_CANCEL_STATUS,
        PROJECT_FIELDS.RECENT_NOTE_CATEGORY,
        PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED,
        PROJECT_FIELDS.DATE_ON_HOLD,
        PROJECT_FIELDS.ON_HOLD
      ],
      where: whereClause,
      options: {
        skip: 0,
        top: 5000 // Limit to prevent timeout
      }
    });
    const holdProjects = response.data || [];

    // Parse hold reasons and calculate resolution times with source tracking
    const reasonData = new Map<HoldReasonCategory, { count: number; totalResolutionDays: number; resolvedCount: number; sources: Map<string, number> }>();
    let resolvedHolds = 0;
    let activeHolds = 0;
    
    for (const project of holdProjects) {
      const parsedReason = parseHoldReason({
        auditLogPreCancelStatus: project[PROJECT_FIELDS.AUDIT_LOG_PRE_CANCEL_STATUS]?.value,
        cancelReason: project[PROJECT_FIELDS.CANCEL_REASON]?.value,
        recentNoteCategory: project[PROJECT_FIELDS.RECENT_NOTE_CATEGORY]?.value,
        intakeMissingItems: project[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value
      });

      const category = parsedReason.category as HoldReasonCategory;
      const currentData = reasonData.get(category) || { count: 0, totalResolutionDays: 0, resolvedCount: 0, sources: new Map() };

      // Track source distribution
      const source = parsedReason.source;
      currentData.sources.set(source, (currentData.sources.get(source) || 0) + 1);

      // Check if project is resolved using ON_HOLD field (false = resolved, true = still on hold)
      const onHold = project[PROJECT_FIELDS.ON_HOLD]?.value;
      const isResolved = !onHold; // false, null, or undefined means hold is resolved

      if (isResolved) {
        resolvedHolds++;
        // Note: Cannot calculate exact resolution time without HOLD_END_DATE field (placeholder field 9998)
        // avgResolutionDays will remain null for MVP
      } else {
        activeHolds++;
      }

      currentData.count++;
      reasonData.set(category, currentData);
    }

    // Calculate percentages and average resolution times
    const totalHolds = holdProjects.length;
    const analysis: HoldAnalysis[] = Array.from(reasonData.entries())
      .map(([category, data]) => {
        // Find the dominant source
        const dominantSource = Array.from(data.sources.entries())
          .sort((a, b) => b[1] - a[1])[0];
        
        return {
          category,
          count: data.count,
          percentage: totalHolds > 0 ? (data.count / totalHolds) * 100 : 0,
          avgResolutionDays: data.resolvedCount > 0 ? Math.round(data.totalResolutionDays / data.resolvedCount) : null,
          resolvedCount: data.resolvedCount,
          dominantSource: dominantSource ? dominantSource[0] : 'unknown'
        };
      })
      .sort((a, b) => b.count - a.count) // Sort by count descending
      .slice(0, 10); // Top 10 reasons

    // Calculate weighted average resolution time
    const totalResolvedCount = analysis.reduce((sum, item) => sum + (item.resolvedCount || 0), 0);
    const totalResolutionDays = analysis.reduce((sum, item) => sum + ((item.avgResolutionDays || 0) * (item.resolvedCount || 0)), 0);
    const overallAvgResolution = totalResolvedCount > 0 ? Math.round(totalResolutionDays / totalResolvedCount) : null;

    // Add unit-like assertions to confirm data integrity
    console.log(`[Hold Analysis] Data integrity check: resolvedHolds=${resolvedHolds}, activeHolds=${activeHolds}, totalHolds=${totalHolds}`);
    console.log(`[Hold Analysis] Resolution calculation: totalResolvedCount=${totalResolvedCount}, totalResolutionDays=${totalResolutionDays}, overallAvgResolution=${overallAvgResolution}`);
    
    // Assert that resolved + active = total
    if (resolvedHolds + activeHolds !== totalHolds) {
      console.warn(`[Hold Analysis] Data mismatch: resolvedHolds(${resolvedHolds}) + activeHolds(${activeHolds}) !== totalHolds(${totalHolds})`);
    }

    const responseData = {
      analysis,
      metadata: {
        timeRange: effectiveTimeRange,
        ...(effectiveTimeRange === 'custom' && { startDate, endDate }),
        totalHolds,
        resolvedHolds,
        activeHolds,
        overallAvgResolution,
        officeCount: officeIds ? officeIds.length : (salesOffice ? salesOffice.length : 0),
        cached: false,
      },
    };

    // Cache the result
    holdAnalysisCache.set(cacheKey, { data: responseData, timestamp: Date.now() });

    // Clean up old cache entries
    if (holdAnalysisCache.size > 50) {
      const now = Date.now();
      const entries = Array.from(holdAnalysisCache.entries());
      
      // Remove expired entries
      for (const [key, value] of entries) {
        if (now - value.timestamp > cacheTTL) {
          holdAnalysisCache.delete(key);
        }
      }
      
      // If still over 50 entries, evict oldest
      if (holdAnalysisCache.size > 50) {
        const remainingEntries = Array.from(holdAnalysisCache.entries());
        remainingEntries
          .sort((a, b) => a[1].timestamp - b[1].timestamp)
          .slice(0, holdAnalysisCache.size - 50)
          .forEach(([key]) => holdAnalysisCache.delete(key));
      }
    }

    logApiResponse('GET', '/api/analytics/hold-analysis', Date.now() - startedAt, { cached: false, timeRange: effectiveTimeRange, totalHolds }, reqId);
    return NextResponse.json(responseData, { status: 200 });
  } catch (error) {
    console.error('[/api/analytics/hold-analysis] ERROR:', error);
    logError('Failed to fetch hold analysis', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error),
      stack: error instanceof Error ? error.stack : undefined
    }, { status: 500 });
  }
}
