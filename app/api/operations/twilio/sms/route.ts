import { NextResponse } from 'next/server';
import twilio from 'twilio';
import { logApiRequest, logApiResponse, logTwilioWebhook, logError } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { OUTREACH_RECORD_FIELDS, QB_TABLE_OUTREACH_RECORDS } from '@/lib/constants/fieldIds';
import { createNotification } from '@/lib/db/notifications';
import { TwilioWebhookPayload } from '@/lib/types/operations';

export const runtime = 'nodejs';

export async function POST(request: Request) {
  const reqId = crypto.randomUUID();
  const start = Date.now();
  
  try {
    // Log request
    logApiRequest('POST', '/api/operations/twilio/sms', undefined, reqId);

    // Get Twilio signature for validation
    const signature = request.headers.get('X-Twilio-Signature');
    if (!signature) {
      logError('Missing Twilio signature', new Error('No signature header'), {
        service: 'twilio_webhook',
        reqId,
      });
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    // Get raw request body for signature validation
    const body = await request.text();
    
    // Build full URL from request
    const fullUrl = request.url;
    
    // Parse body into key/value object for signature validation
    const paramsObject = Object.fromEntries(new URLSearchParams(body));
    
    // Validate Twilio signature
    const isValid = twilio.validateRequest(
      process.env.TWILIO_AUTH_TOKEN!,
      signature,
      fullUrl,
      paramsObject
    );

    if (!isValid) {
      logError('Invalid Twilio signature', new Error('Signature validation failed'), {
        service: 'twilio_webhook',
        reqId,
      });
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    // Parse webhook payload
    const formData = new URLSearchParams(body);
    const payload: TwilioWebhookPayload = {
      MessageSid: formData.get('MessageSid') || undefined,
      MessageStatus: formData.get('MessageStatus') || undefined,
      From: formData.get('From') || '',
      To: formData.get('To') || '',
      Body: formData.get('Body') || undefined,
      ErrorCode: formData.get('ErrorCode') || undefined,
      ErrorMessage: formData.get('ErrorMessage') || undefined,
    };

    // Extract metadata from URL query string
    const { searchParams } = new URL(request.url);
    const projectId = searchParams.get('projectId') || undefined;
    const recordId = searchParams.get('recordId') ? parseInt(searchParams.get('recordId')!) : undefined;
    const coordinatorEmail = searchParams.get('coordinatorEmail') || undefined;

    // Log webhook event
    logTwilioWebhook('sms', payload.MessageStatus || 'unknown', {
      MessageSid: payload.MessageSid,
      status: payload.MessageStatus || 'unknown',
    });

    // Update QuickBase if recordId provided
    if (recordId && payload.MessageSid) {
      try {
        const updateData: Record<string, any> = {};

        if (payload.MessageStatus === 'delivered' || payload.MessageStatus === 'sent') {
          updateData[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = 'Complete';
          updateData[OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE] = new Date().toISOString();
        } else if (payload.MessageStatus === 'failed' || payload.MessageStatus === 'undelivered') {
          const attemptNote = `SMS delivery failed: ${payload.ErrorMessage || 'Unknown error'}`;
          updateData[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] = attemptNote;
        }

        if (Object.keys(updateData).length > 0) {
          await qbClient.updateRecord({
            to: QB_TABLE_OUTREACH_RECORDS,
            data: [{
              3: { value: recordId },
              ...Object.fromEntries(
                Object.entries(updateData).map(([key, value]) => [key, { value }])
              )
            }]
          });
        }
      } catch (error) {
        logError('Failed to update QuickBase record', error, {
          service: 'twilio_webhook',
          recordId,
          messageSid: payload.MessageSid,
          reqId,
        });
      }
    }

    // Create notification for failures
    if ((payload.MessageStatus === 'failed' || payload.MessageStatus === 'undelivered') && coordinatorEmail) {
      try {
        await createNotification({
          userId: coordinatorEmail, // Assuming coordinatorEmail is the user identifier
          type: 'system_alert',
          priority: 'normal',
          title: 'SMS Delivery Failed',
          message: `SMS to ${payload.To} failed: ${payload.ErrorMessage || 'Unknown error'}`,
          metadata: {
            messageSid: payload.MessageSid,
            errorCode: payload.ErrorCode,
            projectId,
            recordId,
          },
        });
      } catch (error) {
        logError('Failed to create notification', error, {
          service: 'twilio_webhook',
          coordinatorEmail,
          messageSid: payload.MessageSid,
          reqId,
        });
      }
    }

    // Log response
    const duration = Date.now() - start;
    logApiResponse('POST', '/api/operations/twilio/sms', duration, undefined, reqId);

    // Return success response
    return NextResponse.json({ 
      success: true, 
      status: payload.MessageStatus 
    });

  } catch (error) {
    logError('SMS webhook handler error', error, {
      service: 'twilio_webhook',
      reqId,
    });

    return NextResponse.json(
      { error: 'Internal server error' }, 
      { status: 500 }
    );
  }
}
