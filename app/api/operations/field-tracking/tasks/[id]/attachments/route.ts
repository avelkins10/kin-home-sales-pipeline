// app/api/operations/field-tracking/tasks/[id]/attachments/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiResponse, logError } from '@/lib/logging/logger';
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { getArrivyTaskByProjectId, getArrivyTaskByArrivyId } from '@/lib/db/arrivy';
import type { TaskAttachment } from '@/lib/types/operations';

/**
 * GET - Get attachments for a specific task
 */
export async function GET(
  req: NextRequest,
  { params }: { params: { id: string } }
) {
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

    if (!arrivyClient) {
      return NextResponse.json({ 
        error: 'Arrivy not configured' 
      }, { status: 503 });
    }

    const taskId = params.id;

    // Try to fetch task (check if it's a project ID or Arrivy task ID)
    let task = await getArrivyTaskByProjectId(taskId);
    
    if (!task) {
      // Try as Arrivy task ID
      const arrivyTaskId = parseInt(taskId, 10);
      if (!isNaN(arrivyTaskId)) {
        task = await getArrivyTaskByArrivyId(arrivyTaskId);
      }
    }

    if (!task) {
      return NextResponse.json({ 
        error: 'Task not found' 
      }, { status: 404 });
    }

    // Fetch attachments from Arrivy API
    const arrivyStatuses = await arrivyClient.getTaskStatuses(task.arrivy_task_id);
    const attachments: TaskAttachment[] = arrivyStatuses.flatMap(status => 
      (status.files || []).map(file => ({
        file_id: file.file_id,
        file_path: file.file_path,
        filename: file.filename,
        status_id: status.id,
        uploaded_by: status.reporter_name || null,
        uploaded_at: new Date(status.time),
      }))
    );

    logApiResponse('GET', `/api/operations/field-tracking/tasks/${taskId}/attachments`, Date.now() - startedAt, { 
      found: true,
      attachmentsCount: attachments.length,
    }, reqId);

    return NextResponse.json({ 
      attachments,
      task_id: task.arrivy_task_id,
      count: attachments.length,
    }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch task attachments', error as Error, { reqId, taskId: params.id });
    return NextResponse.json({ 
      error: 'Failed to load attachments',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, { status: 500 });
  }
}

