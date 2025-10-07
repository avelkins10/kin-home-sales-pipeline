'use server'

// app/api/projects/[id]/route.ts
import { NextResponse } from 'next/server';
import { requireAuth, requireProjectAccessById } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export async function GET(req: Request, { params }: { params: { id: string } }) {
  const startedAt = Date.now();
  const id = params.id;
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', `/api/projects/${id}`, undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  const numericId = parseInt(id, 10);
  if (Number.isNaN(numericId)) {
    return NextResponse.json({ error: 'Invalid project ID' }, { status: 400 });
  }

  const access = await requireProjectAccessById(numericId);
  if (!access.authorized) return access.response;

  try {
    const { getProjectById } = await import('@/lib/quickbase/queries');
    const project = await getProjectById(numericId);
    if (!project) {
      return NextResponse.json({ error: 'Not Found' }, { status: 404 });
    }
    logApiResponse('GET', `/api/projects/${id}`, Date.now() - startedAt, {}, reqId);
    return NextResponse.json(project, { status: 200 });
  } catch (error) {
    logError('Failed to fetch project by id', error as Error, { id: numericId });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}


