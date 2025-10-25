import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { bulkUpdateOutreachRecords } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCOutreachBulkAction, PCOutreachBulkActionPayload } from '@/lib/types/operations';

export const runtime = 'nodejs';

// Rate limiting cache for bulk actions
const bulkActionCache = new Map<string, number>();
const RATE_LIMIT_WINDOW = 10 * 1000; // 10 seconds
const RATE_LIMIT_CLEANUP_INTERVAL = 60 * 1000; // 1 minute

// Clean up rate limit cache periodically
setInterval(() => {
  const now = Date.now();
  for (const [email, timestamp] of bulkActionCache.entries()) {
    if (now - timestamp > RATE_LIMIT_WINDOW) {
      bulkActionCache.delete(email);
    }
  }
}, RATE_LIMIT_CLEANUP_INTERVAL);

/**
 * POST /api/operations/outreach/bulk-action
 * Perform bulk actions on outreach records
 */
export async function POST(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('POST /api/operations/outreach/bulk-action', {
      reqId,
      url: request.url
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

    const pcEmail = session.user.email;

    // Parse request body
    const body: PCOutreachBulkActionPayload = await request.json();
    const { action, recordIds, data = {} } = body;

    // Validate input
    if (!action || !recordIds || !Array.isArray(recordIds) || recordIds.length === 0) {
      return NextResponse.json(
        { error: 'Invalid request: action and recordIds are required' },
        { status: 400 }
      );
    }

    // Validate action type
    const validActions: PCOutreachBulkAction[] = ['mark_contacted', 'assign_to_rep', 'bulk_sms', 'schedule_checkin'];
    if (!validActions.includes(action)) {
      return NextResponse.json(
        { error: 'Invalid action type' },
        { status: 400 }
      );
    }

    // Validate record IDs are numbers
    if (!recordIds.every(id => typeof id === 'number' && id > 0)) {
      return NextResponse.json(
        { error: 'All record IDs must be positive numbers' },
        { status: 400 }
      );
    }

    // Check rate limiting (unless super admin)
    if (session.user.role !== 'super_admin') {
      const lastActionTime = bulkActionCache.get(pcEmail);
      const now = Date.now();
      
      if (lastActionTime && now - lastActionTime < RATE_LIMIT_WINDOW) {
        return NextResponse.json(
          { error: 'Rate limit exceeded. Please wait before performing another bulk action.' },
          { status: 429 }
        );
      }
      
      bulkActionCache.set(pcEmail, now);
    }

    // Special handling for bulk_sms (Twilio integration placeholder)
    if (action === 'bulk_sms') {
      logApiResponse('POST /api/operations/outreach/bulk-action', {
        reqId,
        action: 'bulk_sms',
        message: 'SMS functionality will be available after Twilio integration'
      });

      return NextResponse.json({
        success: false,
        result: {
          success: false,
          processed: 0,
          failed: recordIds.length,
          errors: [{
            recordId: 0,
            error: 'SMS functionality will be available after Twilio integration'
          }],
          message: 'SMS functionality will be available after Twilio integration'
        },
        message: 'SMS functionality will be available after Twilio integration'
      });
    }

    // Perform bulk update
    const result = await bulkUpdateOutreachRecords(action, recordIds, data, reqId);

    // Invalidate outreach cache for this PC
    // Note: In a real implementation, you'd want to invalidate specific cache entries
    // For now, we'll let the cache expire naturally

    logApiResponse('POST /api/operations/outreach/bulk-action', {
      reqId,
      action,
      recordCount: recordIds.length,
      result
    });

    return NextResponse.json({
      success: result.success,
      result,
      message: result.message
    });

  } catch (error) {
    logError('POST /api/operations/outreach/bulk-action failed', error, { reqId });
    
    return NextResponse.json(
      { error: 'Failed to perform bulk action' },
      { status: 500 }
    );
  }
}
