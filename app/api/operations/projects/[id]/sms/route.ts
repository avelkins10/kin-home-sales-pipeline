export const runtime = 'nodejs'

// app/api/operations/projects/[id]/sms/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { sendSms } from '@/lib/integrations/twilio/sms';
import { getSmsTemplate } from '@/lib/integrations/twilio/templates';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { TwilioSmsResult, SmsTemplateType } from '@/lib/types/operations';
import { clearConversations } from '@/lib/cache/communications';

// Rate limiting cache
const smsCache = new Map<string, number>();
const RATE_LIMIT_WINDOW = 30 * 1000; // 30 seconds

// Clean up rate limit cache
function cleanupRateLimitCache() {
  const now = Date.now();
  for (const [key, timestamp] of smsCache.entries()) {
    if (now - timestamp > 60 * 1000) { // 1 minute
      smsCache.delete(key);
    }
  }
}

export async function POST(request: NextRequest, { params }: { params: { id: string } }) {
  const projectId = params.id;
  const reqId = `sms-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('POST', `/api/operations/projects/${projectId}/sms`, {}, reqId);

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
    const { customerPhone, message, templateType } = body;

    // Validate input
    if (!customerPhone || !message) {
      return NextResponse.json(
        { error: 'Customer phone and message are required' },
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

    // Validate message length
    if (message.length > 1600) {
      return NextResponse.json(
        { error: 'Message too long (max 1600 characters)' },
        { status: 400 }
      );
    }

    // Check rate limiting (unless super admin)
    if (session.user.role !== 'super_admin') {
      const now = Date.now();
      const lastSmsTime = smsCache.get(pcEmail);
      
      if (lastSmsTime && (now - lastSmsTime) < RATE_LIMIT_WINDOW) {
        return NextResponse.json(
          { error: 'Rate limit exceeded. Please wait before sending another SMS.' },
          { status: 429 }
        );
      }
      
      smsCache.set(pcEmail, now);
    }

    // Clean up rate limit cache
    cleanupRateLimitCache();

    // Generate message from template if provided
    let finalMessage = message;
    if (templateType) {
      try {
        const template = getSmsTemplate(templateType as SmsTemplateType, {
          customerName: 'Customer', // Will be populated from project data
          projectId,
          coordinatorName: pcName
        });
        finalMessage = template;
      } catch (error) {
        // If template fails, use original message
        console.warn('Template generation failed, using original message:', error);
      }
    }

    // Send SMS
    const smsResult: TwilioSmsResult = await sendSms(
      { to: customerPhone, body: finalMessage },
      { 
        projectId, 
        recordId: parseInt(projectId), 
        coordinatorEmail: pcEmail, 
        logToQuickbase: true 
      }
    );

    if (!smsResult.success) {
      return NextResponse.json(
        { error: smsResult.error || 'Failed to send SMS' },
        { status: 500 }
      );
    }

    // Clear conversations cache after successful SMS send
    clearConversations(pcEmail);

    logApiResponse('POST', `/api/operations/projects/${projectId}/sms`, { 
      messageSid: smsResult.messageSid,
      status: smsResult.status,
      to: smsResult.to
    }, reqId);

    return NextResponse.json({
      success: true,
      result: smsResult,
      message: 'SMS sent successfully'
    });

  } catch (error) {
    logError('POST /api/operations/projects/[id]/sms', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to send SMS' },
      { status: 500 }
    );
  }
}
