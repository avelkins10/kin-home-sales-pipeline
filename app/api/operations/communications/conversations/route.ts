import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getPCConversationHistory } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCCommunicationsData, PCConversationFilters } from '@/lib/types/operations';
import { getConversationsCache, setConversationsCache } from '@/lib/cache/communications';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  const reqId = `conversations-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('GET /api/operations/communications/conversations', {}, reqId);

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

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const tab = searchParams.get('tab') as 'recent' | 'needs_response' | 'scheduled' || 'recent';
    const projectStage = searchParams.get('projectStage') || 'all';
    const communicationType = searchParams.get('communicationType') || 'all';
    const dateRange = searchParams.get('dateRange') as '7days' | '30days' | '90days' | 'all' || '7days';
    const search = searchParams.get('search') || '';

    const filters: PCConversationFilters = {
      tab,
      projectStage,
      communicationType: communicationType as 'all' | 'sms' | 'call' | 'email' | 'note',
      dateRange,
      search
    };

    // Check cache first
    const cached = getConversationsCache(pcEmail, filters);
    
    if (cached) {
      logApiResponse('GET /api/operations/communications/conversations', { cached: true }, reqId);
      return NextResponse.json({
        ...cached.data,
        cached: true
      });
    }

    // Fetch fresh data
    const role = session.user.role;
    const conversations = await getPCConversationHistory(pcEmail, pcName, role, filters, reqId);
    
    // Calculate needs response count
    const needsResponseCount = conversations.filter(c => c.needsResponse).length;
    
    const data: PCCommunicationsData = {
      conversations,
      count: conversations.length,
      needsResponseCount
    };

    // Cache the result
    setConversationsCache(pcEmail, filters, data);

    logApiResponse('GET /api/operations/communications/conversations', { 
      count: data.count, 
      needsResponseCount: data.needsResponseCount,
      cached: false 
    }, reqId);

    return NextResponse.json({
      ...data,
      cached: false
    });

  } catch (error) {
    logError('GET /api/operations/communications/conversations', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to fetch conversation data' },
      { status: 500 }
    );
  }
}
