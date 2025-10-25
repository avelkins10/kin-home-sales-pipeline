/**
 * API Route: GET /api/admin/sync-users/history
 * Admin endpoint for fetching sync run history and statistics
 */

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getSyncRunHistory, getSyncRunStats } from '@/lib/sync/syncRunLogger';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  try {
    // Auth check - admin access required
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    // Only super_admin, regional, or office_leader can view sync history
    const userRole = auth.session.user?.role;
    if (!['super_admin', 'regional', 'office_leader'].includes(userRole)) {
      return NextResponse.json({ 
        error: 'Unauthorized - admin access required' 
      }, { status: 403 });
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const limit = Math.min(parseInt(searchParams.get('limit') || '50'), 200); // Max 200 records

    const userId = auth.session.user?.id;
    const userEmail = auth.session.user?.email;

    logApiRequest('GET', '/api/admin/sync-users/history', {
      source: 'admin-dashboard',
      userId,
      userEmail,
      limit,
      timestamp: new Date().toISOString()
    });

    // Fetch sync run history and statistics
    const [history, stats] = await Promise.all([
      getSyncRunHistory(limit),
      getSyncRunStats()
    ]);

    const response = {
      history,
      stats,
      timestamp: new Date().toISOString()
    };

    logApiResponse('GET', '/api/admin/sync-users/history', 200, {
      userId,
      userEmail,
      historyCount: history.length,
      totalRuns: stats.total_runs,
      successRate: stats.success_rate,
      avgExecutionTime: stats.avg_execution_time_ms
    });

    return NextResponse.json(response, {
      headers: {
        'Cache-Control': 'private, max-age=60' // 1 minute cache
      }
    });

  } catch (error) {
    logError('Admin sync history fetch failed', error, {
      url: request.url,
      method: 'GET'
    });

    return NextResponse.json(
      { 
        error: 'Internal server error',
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}
