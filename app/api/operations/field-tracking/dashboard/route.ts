// app/api/operations/field-tracking/dashboard/route.ts
export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getFieldTrackingData } from '@/lib/integrations/arrivy/service';
import { getArrivyEvents } from '@/lib/db/arrivy';
import { listArrivyEntities } from '@/lib/db/arrivy';
import type { FieldTrackingDashboardData } from '@/lib/types/operations';

// Simple in-memory cache for field tracking dashboard data
const fieldTrackingCache = new Map<string, { data: FieldTrackingDashboardData; timestamp: number }>();

// Cache TTL (30 seconds for real-time feel)
const CACHE_TTL = 30 * 1000;

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

    const cacheKey = `field-tracking:${email}`;

    // Check cache first
    const cached = fieldTrackingCache.get(cacheKey);
    if (cached && Date.now() - cached.timestamp < CACHE_TTL) {
      logApiResponse('GET', '/api/operations/field-tracking/dashboard', Date.now() - startedAt, { cached: true }, reqId);
      return NextResponse.json(cached.data, { status: 200 });
    }

    // Fetch field tracking data
    const coordinatorEmail = role === 'operations_coordinator' ? email : undefined;
    const { tasks, metrics } = await getFieldTrackingData(coordinatorEmail, role);

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

    // Combine into dashboard data
    const dashboardData: FieldTrackingDashboardData = {
      tasks,
      entities,
      events: mappedEvents,
      metrics: {
        total_tasks: metrics.total,
        in_progress: metrics.inProgress,
        completed_today: metrics.completedToday,
        delayed: metrics.delayed,
        crews_active: entities.filter(e => e.extra_fields?.status === 'active').length,
      },
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

