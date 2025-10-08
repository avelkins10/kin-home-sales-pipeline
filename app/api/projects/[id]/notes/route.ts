export const runtime = 'nodejs'

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getNotesForProject, createNoteForProject } from '@/lib/quickbase/queries';

/**
 * GET /api/projects/[id]/notes
 * Fetch rep-visible notes for a project
 */
export async function GET(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', `/api/projects/${params.id}/notes`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const notes = await getNotesForProject(params.id);

    const duration = Date.now() - startedAt;
    logApiResponse('GET', `/api/projects/${params.id}/notes`, duration, {
      count: notes.length
    }, reqId);

    return NextResponse.json(notes, { status: 200 });
  } catch (error) {
    logError('Failed to fetch notes', error as Error, { projectId: params.id });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}

/**
 * POST /api/projects/[id]/notes
 * Create a new note for a project (auto-tagged as Sales/Rep Visible)
 */
export async function POST(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', `/api/projects/${params.id}/notes`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
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
    logApiResponse('POST', `/api/projects/${params.id}/notes`, duration, {
      noteCreated: true
    }, reqId);

    return NextResponse.json({ success: true, data: result }, { status: 201 });
  } catch (error) {
    logError('Failed to create note', error as Error, { projectId: params.id });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
