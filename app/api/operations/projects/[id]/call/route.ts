export const runtime = 'nodejs'

// app/api/operations/projects/[id]/call/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { initiateCall } from '@/lib/integrations/twilio/calls';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { TwilioCallResult } from '@/lib/types/operations';
import { clearConversations } from '@/lib/cache/communications';

// Rate limiting cache
const callCache = new Map<string, number>();
const RATE_LIMIT_WINDOW = 10 * 1000; // 10 seconds

// Clean up rate limit cache
function cleanupRateLimitCache() {
  const now = Date.now();
  for (const [key, timestamp] of callCache.entries()) {
    if (now - timestamp > 60 * 1000) { // 1 minute
      callCache.delete(key);
    }
  }
}

export async function POST(request: NextRequest, { params }: { params: { id: string } }) {
  const projectId = params.id;
  const reqId = `call-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('POST', `/api/operations/projects/${projectId}/call`, {}, reqId);

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

    // Parse request body
    const body = await request.json();
    const { customerPhone, customerName } = body;

    // Validate input
    if (!customerPhone || !customerName) {
      return NextResponse.json(
        { error: 'Customer phone and customer name are required' },
        { status: 400 }
      );
    }

    // Validate phone number format (basic E.164 check)
    if (!customerPhone.startsWith('+') || customerPhone.length < 10) {
      return NextResponse.json(
        { error: 'Invalid phone number format. Must be E.164 format (e.g., +1234567890)' },
        { status: 400 }
      );
    }

    // Check rate limiting (unless super admin)
    if (session.user.role !== 'super_admin') {
      const now = Date.now();
      const lastCallTime = callCache.get(pcEmail);
      
      if (lastCallTime && (now - lastCallTime) < RATE_LIMIT_WINDOW) {
        return NextResponse.json(
          { error: 'Rate limit exceeded. Please wait before making another call.' },
          { status: 429 }
        );
      }
      
      callCache.set(pcEmail, now);
    }

    // Clean up rate limit cache
    cleanupRateLimitCache();

    // Initiate call
    const callResult: TwilioCallResult = await initiateCall(
      { to: customerPhone },
      { 
        projectId, 
        recordId: parseInt(projectId), 
        coordinatorEmail: pcEmail, 
        logToQuickbase: true 
      }
    );

    if (!callResult.success) {
      return NextResponse.json(
        { error: callResult.error || 'Failed to initiate call' },
        { status: 500 }
      );
    }

    // Clear conversations cache after successful call initiation
    clearConversations(pcEmail);

    logApiResponse('POST', `/api/operations/projects/${projectId}/call`, { 
      callSid: callResult.callSid,
      status: callResult.status,
      to: callResult.to
    }, reqId);

    return NextResponse.json({
      success: true,
      result: callResult,
      message: 'Call initiated successfully'
    });

  } catch (error) {
    logError('POST /api/operations/projects/[id]/call', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to initiate call' },
      { status: 500 }
    );
  }
}
