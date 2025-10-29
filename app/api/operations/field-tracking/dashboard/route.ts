// app/api/operations/field-tracking/dashboard/route.ts
export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getFieldTrackingData } from '@/lib/integrations/arrivy/service';
import { getArrivyEvents, getFieldTrackingTasks } from '@/lib/db/arrivy';
import { listArrivyEntities } from '@/lib/db/arrivy';
import type { FieldTrackingDashboardData, FieldTrackingTask } from '@/lib/types/operations';

// Simple in-memory cache for field tracking dashboard data
const fieldTrackingCache = new Map<string, { data: FieldTrackingDashboardData; timestamp: number }>();

// Cache TTL (30 seconds for real-time feel)
const CACHE_TTL = 30 * 1000;

/**
 * Helper function to filter and sort tasks with enhanced filtering
 */
async function getFieldTrackingDataWithFilters(
  coordinatorEmail: string | undefined,
  role: string,
  filters: {
    search?: string;
    status?: string;
    taskType?: string;
    crewMember?: string;
    dateRange?: { start: Date; end: Date };
    sortBy?: string;
    sortOrder?: string;
  }
) {
  // Fetch tasks with basic filters
  let tasks = await getFieldTrackingTasks({
    coordinatorEmail,
    search: filters.search,
    status: filters.status,
    taskType: filters.taskType,
    dateRange: filters.dateRange,
    limit: 500, // Fetch more for client-side filtering
  });

  // Filter by crew member if specified
  if (filters.crewMember) {
    const crewMemberId = parseInt(filters.crewMember);
    tasks = tasks.filter((task) => {
      if (!task.assigned_entity_ids) return false;
      return task.assigned_entity_ids.includes(crewMemberId);
    });
  }

  // Sort tasks
  if (filters.sortBy) {
    const sortOrder = filters.sortOrder === 'desc' ? -1 : 1;
    tasks.sort((a, b) => {
      let aVal: any;
      let bVal: any;

      switch (filters.sortBy) {
        case 'scheduled_start':
          aVal = a.scheduled_start ? new Date(a.scheduled_start).getTime() : 0;
          bVal = b.scheduled_start ? new Date(b.scheduled_start).getTime() : 0;
          break;
        case 'status':
          aVal = a.current_status || '';
          bVal = b.current_status || '';
          break;
        case 'customer_name':
          aVal = a.customer_name || '';
          bVal = b.customer_name || '';
          break;
        case 'task_type':
          aVal = a.task_type || '';
          bVal = b.task_type || '';
          break;
        default:
          return 0;
      }

      if (aVal < bVal) return -sortOrder;
      if (aVal > bVal) return sortOrder;
      return 0;
    });
  }

  // Calculate metrics
  const now = new Date();
  const todayStart = new Date(now.getFullYear(), now.getMonth(), now.getDate());

  const completedTasks = tasks.filter((t) => t.current_status === 'COMPLETE');
  let avgCompletionTime: number | undefined;

  if (completedTasks.length > 0) {
    const completionTimes = completedTasks
      .filter((t) => t.scheduled_start && t.latest_status_time)
      .map((t) => {
        const start = new Date(t.scheduled_start!).getTime();
        const complete = new Date(t.latest_status_time!).getTime();
        return (complete - start) / (1000 * 60);
      })
      .filter((time) => time > 0 && time < 24 * 60);

    if (completionTimes.length > 0) {
      avgCompletionTime = Math.round(
        completionTimes.reduce((sum, time) => sum + time, 0) / completionTimes.length
      );
    }
  }

  const metrics = {
    total: tasks.length,
    inProgress: tasks.filter((t) => ['ENROUTE', 'STARTED'].includes(t.current_status || '')).length,
    completedToday: tasks.filter(
      (t) =>
        t.current_status === 'COMPLETE' &&
        t.latest_status_time &&
        new Date(t.latest_status_time) >= todayStart
    ).length,
    delayed: tasks.filter(
      (t) =>
        t.scheduled_start &&
        new Date(t.scheduled_start) < now &&
        !['COMPLETE', 'CANCELLED'].includes(t.current_status || '')
    ).length,
    avgCompletionTime,
  };

  return { tasks, metrics };
}

export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/operations/field-tracking/dashboard', undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    // Check if user has operations role
    const { role, email, name } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

    if (!allowedRoles.includes(role)) {
      return NextResponse.json({
        error: 'Access denied. Operations role required.'
      }, { status: 403 });
    }

    // Parse filter parameters from URL
    const { searchParams } = new URL(req.url);
    const search = searchParams.get('search') || undefined;
    const status = searchParams.get('status') === 'all' ? undefined : searchParams.get('status') || undefined;
    const taskType = searchParams.get('taskType') === 'all' ? undefined : searchParams.get('taskType') || undefined;
    const crewMember = searchParams.get('crewMember') === 'all' ? undefined : searchParams.get('crewMember') || undefined;
    const dateFilter = searchParams.get('dateFilter') || 'today';
    const sortBy = searchParams.get('sortBy') || 'scheduled_start';
    const sortOrder = searchParams.get('sortOrder') || 'asc';

    // Convert date filter to date range (using UTC to match database timestamps)
    let dateRange: { start: Date; end: Date } | undefined;
    const now = new Date();

    // Get current date components in UTC
    const utcYear = now.getUTCFullYear();
    const utcMonth = now.getUTCMonth();
    const utcDate = now.getUTCDate();

    switch (dateFilter) {
      case 'today':
        // Create UTC date range for today (00:00:00 to 23:59:59.999 UTC)
        const todayStart = new Date(Date.UTC(utcYear, utcMonth, utcDate, 0, 0, 0, 0));
        const todayEnd = new Date(Date.UTC(utcYear, utcMonth, utcDate, 23, 59, 59, 999));
        dateRange = { start: todayStart, end: todayEnd };
        break;
      case 'tomorrow':
        // Create UTC date range for tomorrow
        const tomorrowStart = new Date(Date.UTC(utcYear, utcMonth, utcDate + 1, 0, 0, 0, 0));
        const tomorrowEnd = new Date(Date.UTC(utcYear, utcMonth, utcDate + 1, 23, 59, 59, 999));
        dateRange = { start: tomorrowStart, end: tomorrowEnd };
        break;
      case 'this_week':
        // Start of week (Sunday) in UTC
        const todayUTC = new Date(Date.UTC(utcYear, utcMonth, utcDate, 0, 0, 0, 0));
        const dayOfWeek = todayUTC.getUTCDay();
        const weekStart = new Date(Date.UTC(utcYear, utcMonth, utcDate - dayOfWeek, 0, 0, 0, 0));
        const weekEnd = new Date(Date.UTC(utcYear, utcMonth, utcDate + (6 - dayOfWeek), 23, 59, 59, 999));
        dateRange = { start: weekStart, end: weekEnd };
        break;
      case 'overdue':
        // Tasks scheduled before today
        const beforeToday = new Date(Date.UTC(utcYear, utcMonth, utcDate, 0, 0, 0, 0));
        dateRange = { start: new Date('2020-01-01'), end: beforeToday };
        break;
      case 'all':
      default:
        dateRange = undefined;
        break;
    }

    // Log filter parameters for debugging (helps diagnose timezone issues)
    if (process.env.NODE_ENV !== 'production') {
      console.log('[Field Tracking Dashboard] Filter params:', {
        dateFilter,
        dateRange: dateRange ? {
          start: dateRange.start.toISOString(),
          end: dateRange.end.toISOString(),
        } : null,
        search,
        status,
        taskType,
        crewMember,
      });
    }

    // Create cache key based on filters
    const cacheKey = `field-tracking:${email}:${JSON.stringify({
      search,
      status,
      taskType,
      crewMember,
      dateFilter,
      sortBy,
      sortOrder,
    })}`;

    // Check cache first (only use cache if no filters applied for real-time feel)
    const hasFilters = search || status || taskType || crewMember || dateFilter !== 'today';
    if (!hasFilters) {
      const cached = fieldTrackingCache.get(cacheKey);
      if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
        logApiResponse('GET', '/api/operations/field-tracking/dashboard', Date.now() - startedAt, { cached: true }, reqId);
        return NextResponse.json(cached.data, { status: 200 });
      }
    }

    // Fetch field tracking data with filters
    const coordinatorEmail = role === 'operations_coordinator' ? email : undefined;
    const { tasks, metrics } = await getFieldTrackingDataWithFilters(
      coordinatorEmail,
      role,
      {
        search,
        status,
        taskType,
        crewMember,
        dateRange,
        sortBy,
        sortOrder,
      }
    );

    // Fetch entities (crew members)
    const entities = await listArrivyEntities();

    // Fetch recent events for activity feed
    const events = await getArrivyEvents({
      limit: 50,
    });

    // Map events to include timestamp field
    const mappedEvents = events.map(event => ({
      ...event,
      timestamp: event.event_time,
    }));

    // Calculate task counts by status and type for FilterBar
    const taskCounts = {
      total: tasks.length,
      byStatus: tasks.reduce((acc, task) => {
        const status = task.current_status || 'unknown';
        acc[status] = (acc[status] || 0) + 1;
        acc['all'] = (acc['all'] || 0) + 1;
        return acc;
      }, {} as Record<string, number>),
      byType: tasks.reduce((acc, task) => {
        const type = task.task_type || 'other';
        acc[type] = (acc[type] || 0) + 1;
        acc['all'] = (acc['all'] || 0) + 1;
        return acc;
      }, {} as Record<string, number>),
    };

    // Combine into dashboard data
    const dashboardData: any = {
      tasks,
      entities,
      events: mappedEvents,
      metrics: {
        total_tasks: metrics.total,
        in_progress: metrics.inProgress,
        completed_today: metrics.completedToday,
        delayed: metrics.delayed,
        crews_active: entities.filter(e => e.extra_fields?.status === 'active').length,
        avg_completion_time: metrics.avgCompletionTime,
      },
      taskCounts, // Add task counts for FilterBar
    };

    // Cache the result
    fieldTrackingCache.set(cacheKey, { 
      data: dashboardData, 
      timestamp: Date.now(),
    });

    // Clean up old cache entries
    if (fieldTrackingCache.size > 50) {
      const now = Date.now();
      const entries = Array.from(fieldTrackingCache.entries());
      
      for (const [key, value] of entries) {
        if (now - value.timestamp > CACHE_TTL) {
          fieldTrackingCache.delete(key);
        }
      }
    }

    logApiResponse('GET', '/api/operations/field-tracking/dashboard', Date.now() - startedAt, { 
      tasks: tasks.length,
      entities: entities.length,
      events: events.length,
    }, reqId);

    return NextResponse.json(dashboardData, { status: 200 });

  } catch (error) {
    logError('Failed to fetch field tracking dashboard', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to load field tracking dashboard' 
    }, { status: 500 });
  }
}

