export const runtime = 'nodejs'

import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';

// This endpoint is only available in development
export async function GET(req: Request) {
  // Only allow in development
  if (process.env.NODE_ENV !== 'development') {
    return NextResponse.json({ error: 'Not available in production' }, { status: 404 });
  }

  // Auth check
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  // Only allow super_admin access
  const { role } = auth.session.user as any;
  if (role !== 'super_admin') {
    return NextResponse.json({ error: 'Unauthorized' }, { status: 403 });
  }

  try {
    // Import cache stats from the cache utility
    const { getCacheStats } = await import('@/lib/cache/projectsCache');
    const cacheStats = getCacheStats();

    return NextResponse.json(cacheStats, { status: 200 });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to get cache stats' }, { status: 500 });
  }
}
