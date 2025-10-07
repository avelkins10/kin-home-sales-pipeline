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
    // Import the cache and stats from the projects route
    // Note: This is a bit of a hack since we can't directly import the cache
    // In a real implementation, you'd want to move the cache to a shared module
    const response = await fetch(`${req.url.replace('/_debug/cache/projects', '/projects')}`, {
      headers: {
        'x-debug-cache-stats': 'true'
      }
    });

    // For now, return basic cache configuration
    const cacheConfig = {
      ttl: parseInt(process.env.PROJECTS_CACHE_TTL_MS || '60000'),
      maxEntries: parseInt(process.env.PROJECTS_CACHE_MAX || '100'),
      environment: process.env.NODE_ENV,
      note: 'Cache stats are logged in the main API response. Check server logs for detailed metrics.'
    };

    return NextResponse.json(cacheConfig, { status: 200 });
  } catch (error) {
    return NextResponse.json({ error: 'Failed to get cache stats' }, { status: 500 });
  }
}
