export const runtime = 'nodejs'
export const dynamic = 'force-dynamic';

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getAddersForProject } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export async function GET(
  req: Request,
  { params }: { params: { id: string } }
) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', `/api/projects/${params.id}/adders`, undefined, reqId);

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  try {
    const projectId = params.id;
    const adders = await getAddersForProject(projectId);

    logApiResponse('GET', `/api/projects/${params.id}/adders`, Date.now() - startedAt, {
      count: adders.length
    }, reqId);

    return NextResponse.json(adders, { status: 200 });
  } catch (error) {
    logError('Failed to fetch adders', error as Error, { projectId: params.id });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
