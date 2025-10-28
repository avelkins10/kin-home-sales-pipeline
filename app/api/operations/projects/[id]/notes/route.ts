export const runtime = 'nodejs'

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getAllNotesForProject, createNoteForProject } from '@/lib/quickbase/queries';

/**
 * GET /api/operations/projects/[id]/notes
 * Fetch ALL notes for a project (for Operations view - no rep-visible filter)
 */
export async function GET(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', `/api/operations/projects/${params.id}/notes`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  // Check for operations roles
  const session = auth.session;
  const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

  if (!session?.user?.role || !allowedRoles.includes(session.user.role)) {
    return NextResponse.json(
      { error: 'Insufficient permissions. Operations access required.' },
      { status: 403 }
    );
  }

  try {
    const notes = await getAllNotesForProject(params.id);

    const duration = Date.now() - startedAt;
    logApiResponse('GET', `/api/operations/projects/${params.id}/notes`, duration, {
      count: notes.length
    }, reqId);

    return NextResponse.json(notes, { status: 200 });
  } catch (error) {
    logError('Failed to fetch all notes', error as Error, { projectId: params.id });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}

/**
 * POST /api/operations/projects/[id]/notes
 * Create a new note for a project (auto-tagged as Sales/Rep Visible)
 */
export async function POST(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', `/api/operations/projects/${params.id}/notes`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  // Check for operations roles
  const session = auth.session;
  const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

  if (!session?.user?.role || !allowedRoles.includes(session.user.role)) {
    return NextResponse.json(
      { error: 'Insufficient permissions. Operations access required.' },
      { status: 403 }
    );
  }

  try {
    const body = await req.json();
    const { content } = body;

    // Validate content
    if (!content || typeof content !== 'string' || content.trim().length === 0) {
      return NextResponse.json({ error: 'Note content is required' }, { status: 400 });
    }

    if (content.length > 5000) {
      return NextResponse.json({ error: 'Note content too long (max 5000 characters)' }, { status: 400 });
    }

    const { user } = auth.session as any;
    const result = await createNoteForProject(params.id, content.trim(), user.email || user.name);

    const duration = Date.now() - startedAt;
    logApiResponse('POST', `/api/operations/projects/${params.id}/notes`, duration, {
      noteCreated: true
    }, reqId);

    return NextResponse.json({ success: true, data: result }, { status: 201 });
  } catch (error) {
    logError('Failed to create note', error as Error, { projectId: params.id });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
