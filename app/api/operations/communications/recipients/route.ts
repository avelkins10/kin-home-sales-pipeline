import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getPCBulkMessagingRecipients } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCBulkMessagingRecipient } from '@/lib/types/operations';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  const reqId = `recipients-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('GET /api/operations/communications/recipients', {}, reqId);

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
    const projectStage = searchParams.get('projectStage') || 'all';
    const lender = searchParams.get('lender') || 'all';
    const salesRep = searchParams.get('salesRep') || 'all';

    const filters = {
      projectStage: projectStage === 'all' ? undefined : projectStage,
      lender: lender === 'all' ? undefined : lender,
      salesRep: salesRep === 'all' ? undefined : salesRep
    };

    // Fetch recipients
    const role = session.user.role;
    const recipients: PCBulkMessagingRecipient[] = await getPCBulkMessagingRecipients(
      pcEmail,
      pcName,
      role,
      filters,
      reqId
    );

    logApiResponse('GET /api/operations/communications/recipients', { 
      count: recipients.length,
      filters
    }, reqId);

    return NextResponse.json({
      recipients,
      count: recipients.length
    });

  } catch (error) {
    logError('GET /api/operations/communications/recipients', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to fetch recipients' },
      { status: 500 }
    );
  }
}
