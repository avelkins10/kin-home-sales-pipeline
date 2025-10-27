import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getPCInboundRepQueue } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCInboundQueueData } from '@/lib/types/operations';
import { getInboundCache, setInboundCache } from '@/lib/cache/communications';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  const reqId = `inbound-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('GET /api/operations/communications/inbound', {}, reqId);

    // Verify authentication and role
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    const pcEmail = session.user.email;
    const pcName = session.user.name || session.user.email;

    // Check cache first
    const cached = getInboundCache(pcEmail);
    
    if (cached) {
      logApiResponse('GET /api/operations/communications/inbound', { cached: true }, reqId);
      return NextResponse.json({
        ...cached.data,
        cached: true
      });
    }

    // Fetch fresh data
    const role = session.user.role;
    const requests = await getPCInboundRepQueue(pcEmail, pcName, role, reqId);
    
    // Calculate counts
    const criticalCount = requests.filter(r => r.urgency === 'critical').length;
    const highCount = requests.filter(r => r.urgency === 'high').length;
    
    const data: PCInboundQueueData = {
      requests,
      count: requests.length,
      criticalCount,
      highCount
    };

    // Cache the result
    setInboundCache(pcEmail, data);

    logApiResponse('GET /api/operations/communications/inbound', { 
      count: data.count, 
      criticalCount: data.criticalCount,
      highCount: data.highCount,
      cached: false 
    }, reqId);

    return NextResponse.json({
      ...data,
      cached: false
    });

  } catch (error) {
    logError('GET /api/operations/communications/inbound', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to fetch inbound queue data' },
      { status: 500 }
    );
  }
}
