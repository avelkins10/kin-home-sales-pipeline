import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getAppointmentsSatByCloser } from '@/lib/analytics/closerMetrics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `req-${Date.now()}`;
  
  try {
    logApiRequest('GET', path, { endpoint: 'closer-appointments-sat', requestId });
    
    // Authentication
    const session = await getServerSession(authOptions);
    if (!session) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 401, cached: false, requestId });
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }
    
    // Extract and validate parameters
    const { searchParams } = new URL(request.url);
    const timeRange = (searchParams.get('timeRange') || 'month') as TimeRange;
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    const officeIds = searchParams.get('officeIds')?.split(',').filter(Boolean).map(Number);
    
    // Validate timeRange
    const validTimeRanges = ['today', 'week', 'month', 'quarter', 'ytd', 'custom', 'last_30', 'last_90', 'last_12_months'];
    if (!validTimeRanges.includes(timeRange)) {
      const duration = Date.now() - start;
      logApiResponse('GET', path, duration, { status: 400, cached: false, requestId });
      return NextResponse.json(
        { error: `Invalid timeRange. Must be one of: ${validTimeRanges.join(', ')}` },
        { status: 400 }
      );
    }
    
    // Build custom date range if provided
    let customDateRange: CustomDateRange | undefined;
    if (timeRange === 'custom' && startDate && endDate) {
      customDateRange = { startDate, endDate };
    }
    
    // Fetch appointments sat data
    const data = await getAppointmentsSatByCloser(timeRange, customDateRange, officeIds);
    
    const duration = Date.now() - start;
    logApiResponse('GET', path, duration, { status: 200, cached: false, requestId });
    return NextResponse.json(data);
    
  } catch (error) {
    const duration = Date.now() - start;
    logError('closer-appointments-sat', error as Error, { requestId, context: 'closer-appointments-sat' });
    logApiResponse('GET', path, duration, { status: 500, cached: false, requestId });
    return NextResponse.json(
      { 
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
        stack: process.env.NODE_ENV === 'development' ? (error instanceof Error ? error.stack : undefined) : undefined
      },
      { status: 500 }
    );
  }
}
