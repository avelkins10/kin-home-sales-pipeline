import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { 
  updateEscalationStatus,
  extendEscalationGracePeriod,
  assignEscalationToRep,
  getEscalationHistory
} from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { sendSms } from '@/lib/integrations/twilio/sms';
import { PCEscalationAction } from '@/lib/types/operations';
import { invalidateForUser } from '../cache';

export const runtime = 'nodejs';

/**
 * PATCH /api/operations/escalations/[id]
 * Update escalation (assign, extend grace, resolve, notify customer)
 */
export async function PATCH(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('PATCH /api/operations/escalations/[id]', {
      reqId,
      escalationId: params.id
    });

    // Require authentication and check role
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

    if (!session?.user?.role || !allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    const escalationId = parseInt(params.id);
    if (isNaN(escalationId)) {
      return NextResponse.json(
        { error: 'Invalid escalation ID' },
        { status: 400 }
      );
    }

    const userEmail = session.user.email;
    if (!userEmail) {
      return NextResponse.json(
        { error: 'User email required' },
        { status: 400 }
      );
    }

    const body = await request.json();
    const { action, data } = body;

    if (!action) {
      return NextResponse.json(
        { error: 'Action required' },
        { status: 400 }
      );
    }

    let success = false;
    let message = '';

    switch (action as PCEscalationAction) {
      case 'assign':
        if (!data?.repEmail) {
          return NextResponse.json(
            { error: 'Rep email required for assignment' },
            { status: 400 }
          );
        }
        
        success = await assignEscalationToRep(escalationId, data.repEmail, userEmail, reqId);
        message = 'Escalation assigned successfully';
        break;

      case 'extend_grace':
        if (!data?.newDeadline || !data?.reason) {
          return NextResponse.json(
            { error: 'New deadline and reason required for grace extension' },
            { status: 400 }
          );
        }
        
        // Validate new deadline is in the future
        const newDeadline = new Date(data.newDeadline);
        if (newDeadline <= new Date()) {
          return NextResponse.json(
            { error: 'New deadline must be in the future' },
            { status: 400 }
          );
        }
        
        success = await extendEscalationGracePeriod(
          escalationId, 
          data.newDeadline, 
          data.reason, 
          userEmail, 
          reqId
        );
        message = 'Grace period extended successfully';
        break;

      case 'resolve':
        if (!data?.note) {
          return NextResponse.json(
            { error: 'Resolution note required' },
            { status: 400 }
          );
        }
        
        success = await updateEscalationStatus(
          escalationId, 
          'Resolved by Rep', 
          userEmail, 
          data.note, 
          reqId
        );
        message = 'Escalation resolved successfully';
        break;

      case 'notify_customer':
        if (!data?.customerPhone || !data?.message) {
          return NextResponse.json(
            { error: 'Customer phone and message required for notification' },
            { status: 400 }
          );
        }
        
        try {
          await sendSms(data.customerPhone, data.message);
          success = true;
          message = 'Customer notification sent successfully';
        } catch (smsError) {
          logError('Failed to send SMS notification', smsError, reqId);
          return NextResponse.json(
            { error: 'Failed to send customer notification' },
            { status: 500 }
          );
        }
        break;

      default:
        return NextResponse.json(
          { error: 'Invalid action' },
          { status: 400 }
        );
    }

    if (!success) {
      return NextResponse.json(
        { error: 'Failed to update escalation' },
        { status: 500 }
      );
    }

    // Invalidate escalations cache for the user
    if (success) {
      invalidateForUser(userEmail);
    }

    logApiResponse('PATCH /api/operations/escalations/[id]', {
      reqId,
      action,
      success
    });

    return NextResponse.json({
      success: true,
      message
    });

  } catch (error) {
    logError('PATCH /api/operations/escalations/[id]', error, reqId);
    return NextResponse.json(
      { error: 'Failed to update escalation' },
      { status: 500 }
    );
  }
}

/**
 * GET /api/operations/escalations/[id]
 * Get escalation history
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('GET /api/operations/escalations/[id]', {
      reqId,
      escalationId: params.id
    });

    // Require authentication and check role
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;

    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];

    if (!session?.user?.role || !allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    const escalationId = parseInt(params.id);
    if (isNaN(escalationId)) {
      return NextResponse.json(
        { error: 'Invalid escalation ID' },
        { status: 400 }
      );
    }

    // Get escalation history
    const history = await getEscalationHistory(escalationId, reqId);

    logApiResponse('GET /api/operations/escalations/[id]', {
      reqId,
      count: history.length
    });

    return NextResponse.json({
      history,
      count: history.length
    });

  } catch (error) {
    logError('GET /api/operations/escalations/[id]', error, reqId);
    return NextResponse.json(
      { error: 'Failed to fetch escalation history' },
      { status: 500 }
    );
  }
}
