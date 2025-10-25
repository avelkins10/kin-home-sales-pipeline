import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { updateSalesAidRequest } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { clearInboundQueue } from '@/lib/cache/communications';

export const runtime = 'nodejs';

export async function PATCH(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  const reqId = `inbound-update-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('PATCH /api/operations/communications/inbound/[id]', { id: params.id }, reqId);

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

    const recordId = parseInt(params.id);
    if (isNaN(recordId)) {
      return NextResponse.json(
        { error: 'Invalid record ID' },
        { status: 400 }
      );
    }

    // Parse request body
    const body = await request.json();
    const { action, data } = body;

    if (!action || !['respond', 'escalate', 'resolve'].includes(action)) {
      return NextResponse.json(
        { error: 'Invalid action. Must be respond, escalate, or resolve' },
        { status: 400 }
      );
    }

    // Map action to updates
    let updates: any = {};
    
    switch (action) {
      case 'respond':
        updates.status = 'Working With Rep';
        break;
      case 'escalate':
        updates.escalateToSalesAid = true;
        updates.escalatedDateTime = new Date().toISOString();
        if (data?.assignedRep) {
          updates.assignedRep = data.assignedRep;
        }
        break;
      case 'resolve':
        updates.status = 'Resolved by Rep';
        updates.completedDate = new Date().toISOString();
        break;
    }

    // Update the sales aid request
    const success = await updateSalesAidRequest(recordId, updates, reqId);
    
    if (!success) {
      return NextResponse.json(
        { error: 'Failed to update sales aid request' },
        { status: 500 }
      );
    }

    // Clear inbound queue cache after successful update
    clearInboundQueue(session.user.email);

    logApiResponse('PATCH /api/operations/communications/inbound/[id]', { 
      success: true, 
      action,
      recordId 
    }, reqId);

    return NextResponse.json({
      success: true,
      message: `Sales aid request ${action}ed successfully`
    });

  } catch (error) {
    logError('PATCH /api/operations/communications/inbound/[id]', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to update sales aid request' },
      { status: 500 }
    );
  }
}
