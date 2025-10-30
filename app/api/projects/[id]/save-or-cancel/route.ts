export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { requireProjectAccessById } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { updateProject, createNoteForProject } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
// getUserEmail is defined inline in tasks route, we'll import from there or use sql directly
import { sql } from '@/lib/db/client';

/**
 * POST /api/projects/[id]/save-or-cancel
 * Handle saving a customer (move from Pending Cancel to Active) or cancelling project
 * 
 * Body:
 * - action: 'save' | 'cancel'
 * - notes: string (required for save action)
 */
export async function POST(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).substring(7);

  logApiRequest('POST', `/api/projects/${params.id}/save-or-cancel`, undefined, reqId);

  try {
    // Authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userId = (auth.session.user as any).id as string;
    const userRole = auth.session.user.role;

    // Only closers, setters, team leads, and office leaders can save/cancel projects
    // (Matches who can see pending cancel tasks in the tasks API)
    if (!['closer', 'setter', 'team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(userRole)) {
      return NextResponse.json(
        { error: 'Forbidden', message: 'Only sales reps and team leaders can save or cancel projects' },
        { status: 403 }
      );
    }

    // Parse and validate project ID
    const numericId = parseInt(params.id, 10);
    if (isNaN(numericId)) {
      return NextResponse.json({ error: 'Invalid project ID' }, { status: 400 });
    }

    // Get user email first (needed for access check and notes)
    const emailResult = await sql.query(
      'SELECT email FROM users WHERE id = $1 AND email IS NOT NULL',
      [userId]
    );
    const userEmail = emailResult.rows.length > 0 ? emailResult.rows[0].email : null;
    if (!userEmail) {
      logError('User email not found in database', new Error('Missing email'), {
        userId,
        userRole,
        projectId: numericId,
        reqId
      });
      return NextResponse.json(
        { error: 'Bad Request', message: 'User email not found' },
        { status: 400 }
      );
    }

    // Verify user can access this project
    const access = await requireProjectAccessById(numericId);
    if (!access.authorized) {
      // Log access denial for debugging
      const { getProjectById } = await import('@/lib/quickbase/queries');
      const project = await getProjectById(numericId).catch(() => null);
      
      logError('Access denied to project', new Error('Forbidden'), {
        userId,
        userRole,
        userEmail: userEmail?.toLowerCase(),
        projectId: numericId,
        projectCloserEmail: project?.[PROJECT_FIELDS.CLOSER_EMAIL]?.value?.toLowerCase() || null,
        projectSetterEmail: project?.[PROJECT_FIELDS.SETTER_EMAIL]?.value?.toLowerCase() || null,
        reqId
      });
      
      return NextResponse.json(
        { 
          error: 'Forbidden', 
          message: 'You do not have access to this project. Please ensure you are the closer or setter assigned to this project, or have team/office leadership access.' 
        },
        { status: 403 }
      );
    }

    // Parse request body
    let body: { action?: string; notes?: string };
    try {
      body = await req.json();
    } catch (error) {
      logError('Failed to parse request body', error as Error, { reqId });
      return NextResponse.json(
        { error: 'Bad Request', message: 'Invalid JSON in request body' },
        { status: 400 }
      );
    }

    const { action, notes } = body;

    if (!action || !['save', 'cancel'].includes(action)) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'action must be "save" or "cancel"' },
        { status: 400 }
      );
    }

    if (action === 'save' && !notes?.trim()) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'notes are required when saving a customer' },
        { status: 400 }
      );
    }

    // Get current project status to verify it's Pending Cancel
    const { getProjectById } = await import('@/lib/quickbase/queries');
    const project = await getProjectById(numericId);
    
    if (!project) {
      return NextResponse.json({ error: 'Project not found' }, { status: 404 });
    }

    const currentStatus = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const isPendingCancel = typeof currentStatus === 'string' && 
      currentStatus.toLowerCase().includes('pending cancel');

    if (!isPendingCancel) {
      return NextResponse.json(
        { error: 'Bad Request', message: 'Project status must be "Pending Cancel" to use this action' },
        { status: 400 }
      );
    }

    // Update project status
    let newStatus: string;
    let noteContent: string;

    if (action === 'save') {
      // Move back to Active
      newStatus = 'Active';
      noteContent = `Customer Saved by ${auth.session.user.name || userEmail}\n\nSave Notes: ${notes}`;
    } else {
      // Cancel the project
      newStatus = 'Cancelled';
      noteContent = `Project Cancelled by ${auth.session.user.name || userEmail}${notes ? `\n\nNotes: ${notes}` : ''}`;
    }

    // Update project status in QuickBase
    logInfo('[SAVE_OR_CANCEL] Updating project in QuickBase', {
      projectId: numericId,
      previousStatus: currentStatus,
      newStatus,
      action,
      reqId
    });
    
    try {
      await updateProject(numericId, {
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: newStatus },
        [PROJECT_FIELDS.DATE_MODIFIED]: { value: new Date().toISOString() }
      });
      
      logInfo('[SAVE_OR_CANCEL] Project updated successfully in QuickBase', {
        projectId: numericId,
        newStatus,
        reqId
      });
    } catch (error) {
      logError('[SAVE_OR_CANCEL] Failed to update project in QuickBase', error as Error, {
        projectId: numericId,
        previousStatus: currentStatus,
        newStatus,
        action,
        reqId
      });
      return NextResponse.json({
        error: 'Internal Server Error',
        message: 'Failed to update project status in QuickBase',
        details: (error as Error).message
      }, { status: 500 });
    }

    // Create note in Install Communications for project coordinators (same format as sales rep notes)
    try {
      await createNoteForProject(
        numericId,
        noteContent,
        userEmail
      );
      logInfo('[SAVE_OR_CANCEL] Note created successfully in Install Communications', {
        projectId: numericId,
        action,
        reqId
      });
    } catch (error) {
      // Log error but don't fail the request - status update succeeded
      logError('[SAVE_OR_CANCEL] Failed to create note in Install Communications', error as Error, {
        projectId: numericId,
        action,
        noteContent,
        userEmail,
        reqId
      });
      // Continue execution - the status update was successful, note creation failure is non-critical
    }

    logInfo('[SAVE_OR_CANCEL] Project status updated', {
      projectId: numericId,
      action,
      previousStatus: currentStatus,
      newStatus,
      userEmail,
      reqId
    });

    const duration = Date.now() - startedAt;
    logApiResponse('POST', `/api/projects/${params.id}/save-or-cancel`, duration, {
      action,
      newStatus
    }, reqId);

    return NextResponse.json({
      success: true,
      message: action === 'save' 
        ? 'Customer saved successfully. Project moved back to Active.'
        : 'Project cancelled successfully.',
      projectId: numericId,
      newStatus
    }, { status: 200 });

  } catch (error) {
    logError('Failed to save or cancel project', error as Error, { 
      projectId: params.id,
      reqId 
    });

    return NextResponse.json({
      error: 'Internal Server Error',
      details: (error as Error).message
    }, { status: 500 });
  }
}

