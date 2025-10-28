// app/api/operations/field-tracking/tasks/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { syncProjectToArrivy } from '@/lib/integrations/arrivy/service';
import { getFieldTrackingTasks } from '@/lib/db/arrivy';

/**
 * GET - List field tracking tasks with filters
 */
export async function GET(req: NextRequest) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role, email } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Operations role required.' 
      }, { status: 403 });
    }

    // Parse query parameters
    const searchParams = req.nextUrl.searchParams;
    const taskType = searchParams.get('task_type') || undefined;
    const status = searchParams.get('status') || undefined;
    const search = searchParams.get('search') || undefined;
    const limit = parseInt(searchParams.get('limit') || '50', 10);
    const offset = parseInt(searchParams.get('offset') || '0', 10);

    // Parse date range
    const dateRange = searchParams.get('date_range');
    let dateRangeObj: { start: Date; end: Date } | undefined;
    
    if (dateRange) {
      const now = new Date();
      const startOfDay = new Date(now.getFullYear(), now.getMonth(), now.getDate());
      
      switch (dateRange) {
        case 'today':
          dateRangeObj = {
            start: startOfDay,
            end: new Date(startOfDay.getTime() + 24 * 60 * 60 * 1000),
          };
          break;
        case 'this_week':
          const startOfWeek = new Date(startOfDay);
          startOfWeek.setDate(startOfDay.getDate() - startOfDay.getDay());
          dateRangeObj = {
            start: startOfWeek,
            end: new Date(startOfWeek.getTime() + 7 * 24 * 60 * 60 * 1000),
          };
          break;
        case 'this_month':
          const startOfMonth = new Date(now.getFullYear(), now.getMonth(), 1);
          const endOfMonth = new Date(now.getFullYear(), now.getMonth() + 1, 0);
          dateRangeObj = {
            start: startOfMonth,
            end: endOfMonth,
          };
          break;
      }
    }

    // Fetch tasks
    const coordinatorEmail = role === 'operations_coordinator' ? email : undefined;
    const tasks = await getFieldTrackingTasks({
      coordinatorEmail,
      taskType: taskType as any,
      status,
      dateRange: dateRangeObj,
      search,
      limit,
      offset,
    });

    logApiResponse('GET', '/api/operations/field-tracking/tasks', Date.now() - startedAt, { 
      count: tasks.length,
    }, reqId);

    return NextResponse.json({ tasks }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch field tracking tasks', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to load field tracking tasks' 
    }, { status: 500 });
  }
}

/**
 * POST - Create/sync a task from QuickBase project
 */
export async function POST(req: NextRequest) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Operations role required.' 
      }, { status: 403 });
    }

    // Parse request body
    const body = await req.json();
    const { projectId, recordId, projectData } = body;

    if (!projectId || !recordId || !projectData) {
      return NextResponse.json({ 
        error: 'Missing required fields: projectId, recordId, projectData' 
      }, { status: 400 });
    }

    // Sync project to Arrivy
    const result = await syncProjectToArrivy(projectId, recordId, projectData);

    logApiResponse('POST', '/api/operations/field-tracking/tasks', Date.now() - startedAt, { 
      projectId,
      isNewTask: result.isNewTask,
    }, reqId);

    return NextResponse.json({ 
      success: true,
      task: result.task,
      trackerUrl: result.trackerUrl,
      isNewTask: result.isNewTask,
    }, { status: 201 });

  } catch (error) {
    logError('Failed to create/sync field tracking task', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to create/sync task',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, { status: 500 });
  }
}

