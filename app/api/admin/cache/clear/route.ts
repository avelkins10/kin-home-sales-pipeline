import { NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { clearProjectsCache } from '@/lib/cache/projectsCache';

export async function POST() {
  // Auth check - admin only
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  // Only admin and super_admin can clear cache
  const userRole = auth.session.user?.role;
  if (userRole !== 'admin' && userRole !== 'super_admin') {
    return NextResponse.json({ error: 'Unauthorized - admin access required' }, { status: 403 });
  }

  try {
    const result = clearProjectsCache();
    return NextResponse.json({
      success: true,
      message: `Cache cleared successfully (${result.cleared} entries)`,
      cleared: result.cleared
    }, { status: 200 });
  } catch (error) {
    console.error('[Cache Clear] Error:', error);
    return NextResponse.json({ error: 'Failed to clear cache' }, { status: 500 });
  }
}
