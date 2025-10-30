// app/api/analytics/basic-hold-stats/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { getBasicHoldStatistics } from '@/lib/analytics/basicHoldStats';
import { logAIRequest, logAIError } from '@/lib/analytics/aiMetrics';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

export async function GET(req: NextRequest) {
  try {
    const session = await getServerSession(authOptions);
    if (!session?.user?.quickbaseUserId) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const { searchParams } = new URL(req.url);
    const userId = searchParams.get('userId') || session.user.quickbaseUserId;
    const role = searchParams.get('role') || session.user.role || 'closer';
    const timeRange = searchParams.get('timeRange') || 'ytd';
    const customDateRange = searchParams.get('customDateRange');

    logAIRequest('getBasicHoldStats', userId, { role, timeRange, customDateRange });

    let dateRange;
    if (customDateRange) {
      try {
        const parsed = JSON.parse(customDateRange);
        dateRange = { startDate: parsed.startDate, endDate: parsed.endDate };
      } catch (error) {
        return NextResponse.json({ error: 'Invalid customDateRange format' }, { status: 400 });
      }
    }

    const stats = await getBasicHoldStatistics(userId, role, timeRange, dateRange);

    return NextResponse.json(stats);
  } catch (error: any) {
    console.error('[API] /api/analytics/basic-hold-stats error:', error);
    logAIError('getBasicHoldStats', 'unknown', error);
    return NextResponse.json({ 
      error: error.message || 'Internal server error' 
    }, { status: 500 });
  }
}
