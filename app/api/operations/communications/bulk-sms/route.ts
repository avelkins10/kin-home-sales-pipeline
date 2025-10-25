import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { sendSms } from '@/lib/integrations/twilio/sms';
import { getSmsTemplate } from '@/lib/integrations/twilio/templates';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCBulkMessagingPayload, PCBulkMessagingResult } from '@/lib/types/operations';
import { clearConversations } from '@/lib/cache/communications';
import { SmsTemplateType } from '@/lib/integrations/twilio/templates';

export const runtime = 'nodejs';

// Rate limiting cache
const bulkSmsCache = new Map<string, number>();
const RATE_LIMIT_WINDOW = 60 * 1000; // 60 seconds
const MAX_RECIPIENTS = 50;

// Clean up rate limit cache
function cleanupRateLimitCache() {
  const now = Date.now();
  for (const [key, timestamp] of bulkSmsCache.entries()) {
    if (now - timestamp > 5 * 60 * 1000) { // 5 minutes
      bulkSmsCache.delete(key);
    }
  }
}

export async function POST(request: NextRequest) {
  const reqId = `bulk-sms-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('POST /api/operations/communications/bulk-sms', {}, reqId);

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
    const body: PCBulkMessagingPayload = await request.json();
    const { templateType, recipients, variables, scheduledTime } = body;

    // Validate input
    if (!templateType || !recipients || recipients.length === 0) {
      return NextResponse.json(
        { error: 'Template type and recipients are required' },
        { status: 400 }
      );
    }

    if (recipients.length > MAX_RECIPIENTS) {
      return NextResponse.json(
        { error: `Maximum ${MAX_RECIPIENTS} recipients allowed per batch` },
        { status: 400 }
      );
    }

    // Check rate limiting (unless super admin)
    if (session.user.role !== 'super_admin') {
      const now = Date.now();
      const lastSendTime = bulkSmsCache.get(pcEmail);
      
      if (lastSendTime && (now - lastSendTime) < RATE_LIMIT_WINDOW) {
        return NextResponse.json(
          { error: 'Rate limit exceeded. Please wait before sending another bulk SMS.' },
          { status: 429 }
        );
      }
      
      bulkSmsCache.set(pcEmail, now);
    }

    // Clean up rate limit cache
    cleanupRateLimitCache();

    // Process each recipient
    const results: PCBulkMessagingResult = {
      success: true,
      sent: 0,
      failed: 0,
      errors: [],
      message: ''
    };

    for (const recipient of recipients) {
      try {
        // Merge recipient-specific variables with provided variables
        const mergedVariables = {
          ...variables,
          customerName: recipient.customerName,
          projectId: recipient.projectId,
          coordinatorName: pcName,
          coordinatorPhone: process.env.TWILIO_PHONE_NUMBER || ''
        };

        // Generate message from template
        const message = getSmsTemplate(templateType as SmsTemplateType, mergedVariables);
        
        // Send SMS
        const smsResult = await sendSms(
          { 
            to: recipient.customerPhone, 
            body: message 
          },
          { 
            projectId: recipient.projectId, 
            recordId: recipient.recordId, 
            coordinatorEmail: pcEmail, 
            logToQuickbase: true 
          }
        );

        if (smsResult.success) {
          results.sent++;
        } else {
          results.failed++;
          results.errors.push({
            projectId: recipient.projectId,
            error: smsResult.error || 'Unknown error'
          });
        }

      } catch (error) {
        results.failed++;
        results.errors.push({
          projectId: recipient.projectId,
          error: error instanceof Error ? error.message : 'Unknown error'
        });
      }
    }

    // Set overall success
    results.success = results.failed === 0;
    results.message = `Sent ${results.sent} messages successfully${results.failed > 0 ? `, ${results.failed} failed` : ''}`;

    // Clear conversations cache after successful bulk SMS
    if (results.sent > 0) {
      clearConversations(pcEmail);
    }

    logApiResponse('POST /api/operations/communications/bulk-sms', { 
      sent: results.sent,
      failed: results.failed,
      total: recipients.length
    }, reqId);

    return NextResponse.json({
      success: results.success,
      result: results,
      message: results.message
    });

  } catch (error) {
    logError('POST /api/operations/communications/bulk-sms', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to send bulk SMS' },
      { status: 500 }
    );
  }
}
