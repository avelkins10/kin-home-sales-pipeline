export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getPCTasksForProject, createPCTask } from '@/lib/quickbase/queries';
import type { PCTaskAssignmentPayload } from '@/lib/types/operations';

/**
 * GET /api/operations/tasks
 * Get PC tasks for a specific project
 */
export async function GET(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('GET', '/api/operations/tasks', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    
    // Check user role is operations_coordinator, operations_manager, closer, setter, office_leader, regional, or super_admin
    const allowedRoles = [
      'operations_coordinator',
      'operations_manager', 
      'closer',
      'setter',
      'office_leader',
      'regional',
      'super_admin'
    ];

    if (!allowedRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden', message: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(req.url);
    const projectId = searchParams.get('projectId');
    const recordId = searchParams.get('recordId');
    const taskId = searchParams.get('taskId');

    if (!projectId && !recordId) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'projectId or recordId is required' },
        { status: 400 }
      );
    }

    // Prefer recordId when present and validate it is a number
    const numericRecordId = recordId ? parseInt(recordId) : parseInt(projectId!);
    if (isNaN(numericRecordId)) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid recordId or projectId' },
        { status: 400 }
      );
    }

    // Extract user email from session
    const userEmail = auth.session.user.email;
    if (!userEmail) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'User email not found' },
        { status: 400 }
      );
    }

    // Get PC tasks for the project
    const tasks = await getPCTasksForProject(projectId || recordId!, numericRecordId, reqId);

    // Filter tasks based on user role
    let filteredTasks = tasks;
    
    if (['closer', 'setter'].includes(userRole)) {
      // Reps see only tasks assigned to them
      filteredTasks = tasks.filter(task => task.assignedTo === userEmail);
    } else if (['operations_coordinator', 'operations_manager'].includes(userRole)) {
      // PCs see all tasks they assigned
      filteredTasks = tasks.filter(task => task.assignedBy === userEmail);
    }
    // Managers and super_admin see all tasks (no filtering)

    logApiResponse('GET', '/api/operations/tasks', Date.now() - startedAt, {
      tasks: filteredTasks.length,
      total: tasks.length
    }, reqId);

    return NextResponse.json({
      tasks: filteredTasks,
      count: filteredTasks.length
    }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch PC tasks', error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: 'Failed to fetch tasks' },
      { status: 500 }
    );
  }
}

/**
 * POST /api/operations/tasks
 * Create a new PC task
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('POST', '/api/operations/tasks', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    
    // Check user role is operations_coordinator, operations_manager, office_leader, regional, or super_admin (only PCs can assign tasks)
    const allowedRoles = [
      'operations_coordinator',
      'operations_manager',
      'office_leader',
      'regional',
      'super_admin'
    ];

    if (!allowedRoles.includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden', message: 'Only PCs can assign tasks' },
        { status: 403 }
      );
    }

    // Parse request body
    const body = await req.json();
    const payload: PCTaskAssignmentPayload = {
      projectId: body.projectId,
      recordId: body.recordId,
      taskType: body.taskType,
      assignedTo: body.assignedTo,
      name: body.name,
      description: body.description,
      dueDate: body.dueDate,
      priority: body.priority
    };

    // Validate input
    if (!payload.projectId || !payload.recordId || !payload.taskType || !payload.assignedTo || !payload.name || !payload.description) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Missing required fields: projectId, recordId, taskType, assignedTo, name, description' },
        { status: 400 }
      );
    }

    // Validate recordId is a valid number
    if (!Number.isFinite(payload.recordId) || payload.recordId <= 0) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid recordId - must be a positive number' },
        { status: 400 }
      );
    }

    // Validate taskType is valid PCTaskType
    const validTaskTypes = [
      'callback_customer',
      'collect_documents', 
      'clarify_pricing',
      'handle_objection',
      'schedule_site_visit',
      'resolve_hoa_issue'
    ];

    if (!validTaskTypes.includes(payload.taskType)) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid taskType' },
        { status: 400 }
      );
    }

    // Validate assignedTo is valid email
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(payload.assignedTo)) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid assignedTo email' },
        { status: 400 }
      );
    }

    // Validate dueDate is future date if provided
    if (payload.dueDate) {
      const dueDate = new Date(payload.dueDate);
      const now = new Date();
      if (dueDate <= now) {
        return NextResponse.json(
          { error: 'Bad Request', message: 'Due date must be in the future' },
          { status: 400 }
        );
      }
    }

    // Extract PC email from session
    const pcEmail = auth.session.user.email;
    if (!pcEmail) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'User email not found' },
        { status: 400 }
      );
    }

    // Create the task
    const taskId = await createPCTask(payload, pcEmail, reqId);

    logApiResponse('POST', '/api/operations/tasks', Date.now() - startedAt, {
      taskId,
      success: true
    }, reqId);

    return NextResponse.json({
      success: true,
      taskId,
      message: 'Task created successfully'
    }, { status: 201 });

  } catch (error) {
    logError('Failed to create PC task', error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: 'Failed to create task' },
      { status: 500 }
    );
  }
}
