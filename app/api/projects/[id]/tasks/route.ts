import { NextResponse } from 'next/server';
import { requireAuth, requireProjectAccessById } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getProjectTasks } from '@/lib/quickbase/queries';

export const runtime = 'nodejs';

export async function GET(req: Request, { params }: { params: { id: string } }) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);
  
  // Log request
  logApiRequest('GET', '/api/projects/{id}/tasks', undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    // Parse and validate project ID
    const numericId = parseInt(params.id, 10);
    if (isNaN(numericId)) {
      return NextResponse.json({ error: 'Invalid project ID' }, { status: 400 });
    }

    // Authorization - verify user can access this project
    const access = await requireProjectAccessById(numericId);
    if (!access.authorized) {
      return access.response;
    }

    // Query tasks for the project
    const tasks = await getProjectTasks(numericId);

    // Log response
    const duration = Date.now() - startedAt;
    logApiResponse('GET', '/api/projects/{id}/tasks', duration, { count: tasks.length }, reqId);

    return NextResponse.json(tasks, { status: 200 });

  } catch (error) {
    logError('Failed to fetch tasks', error as Error, { projectId: params.id });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
