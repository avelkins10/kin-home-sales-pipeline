import { NextResponse } from 'next/server';
import twilio from 'twilio';
import { logApiRequest, logApiResponse, logTwilioWebhook, logError } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { OUTREACH_RECORD_FIELDS, QB_TABLE_OUTREACH_RECORDS, INSTALL_COMMUNICATION_FIELDS, QB_TABLE_INSTALL_COMMUNICATIONS } from '@/lib/constants/fieldIds';
import { createNotification } from '@/lib/db/notifications';
import { TwilioWebhookPayload } from '@/lib/types/operations';

export const runtime = 'nodejs';

export async function POST(request: Request) {
  const reqId = crypto.randomUUID();
  const start = Date.now();
  
  try {
    // Log request
    logApiRequest('POST', '/api/operations/twilio/voice/status', undefined, reqId);

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
    const callSid = formData.get('CallSid');
    const callStatus = formData.get('CallStatus');
    const callDuration = formData.get('CallDuration');
    const from = formData.get('From');
    const to = formData.get('To');
    const errorCode = formData.get('ErrorCode');
    const errorMessage = formData.get('ErrorMessage');

    // Extract metadata from URL query string
    const { searchParams } = new URL(request.url);
    const projectId = searchParams.get('projectId') || undefined;
    const recordId = searchParams.get('recordId') ? parseInt(searchParams.get('recordId')!) : undefined;
    const coordinatorEmail = searchParams.get('coordinatorEmail') || undefined;

    // Log webhook event
    logTwilioWebhook('voice', callStatus || 'unknown', {
      CallSid: callSid,
      status: callStatus || 'unknown',
    });

    // Update QuickBase based on call status
    if (recordId && callSid) {
      try {
        // First, query the current record to get existing attempt count and note
        const currentRecord = await qbClient.queryRecords({
          from: QB_TABLE_OUTREACH_RECORDS,
          select: [OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS, OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE],
          where: `{3.EX.'${recordId}'}`
        });

        const currentAttempts = currentRecord.data?.[0]?.data?.[OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS]?.value || 0;
        const currentNote = currentRecord.data?.[0]?.data?.[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE]?.value || '';

        const updateData: Record<string, any> = {};

        if (callStatus === 'completed') {
          updateData[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = 'Complete';
          updateData[OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE] = new Date().toISOString();
          
          const entry = `Call completed (Duration: ${callDuration}s, SID: ${callSid})`;
          const timestamped = `[${new Date().toISOString()}] ${entry}`;
          const combined = currentNote ? `${currentNote}\n${timestamped}` : timestamped;
          updateData[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] = combined;
          
        } else if (callStatus === 'no-answer' || callStatus === 'busy') {
          updateData[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = 'No Answer Left Message';
          
          const entry = `Call attempt - ${callStatus} (SID: ${callSid})`;
          const timestamped = `[${new Date().toISOString()}] ${entry}`;
          const combined = currentNote ? `${currentNote}\n${timestamped}` : timestamped;
          updateData[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] = combined;
          
        } else if (callStatus === 'failed') {
          const entry = `Call failed: ${errorMessage || 'Unknown error'} (SID: ${callSid})`;
          const timestamped = `[${new Date().toISOString()}] ${entry}`;
          const combined = currentNote ? `${currentNote}\n${timestamped}` : timestamped;
          updateData[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] = combined;
        }

        // Increment attempt count
        updateData[OUTREACH_RECORD_FIELDS.NUM_ATTEMPTS] = currentAttempts + 1;

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
          callSid,
          reqId,
        });
      }
    }

    // Create Install Communication record for completed calls
    if (callStatus === 'completed' && callDuration && parseInt(callDuration) > 0 && projectId && coordinatorEmail) {
      try {
        const communicationData = {
          [INSTALL_COMMUNICATION_FIELDS.DATE]: new Date().toISOString(),
          [INSTALL_COMMUNICATION_FIELDS.NOTE_BY]: coordinatorEmail,
          [INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]: projectId,
          [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: `Phone call completed (Duration: ${callDuration}s, SID: ${callSid})`,
        };

        await qbClient.updateRecord({
          to: QB_TABLE_INSTALL_COMMUNICATIONS,
          data: [{
            ...Object.fromEntries(
              Object.entries(communicationData).map(([key, value]) => [key, { value }])
            )
          }]
        });
      } catch (error) {
        logError('Failed to create Install Communication record', error, {
          service: 'twilio_webhook',
          projectId,
          callSid,
          reqId,
        });
      }
    }

    // Create notification for failed calls
    if (callStatus === 'failed' && coordinatorEmail) {
      try {
        await createNotification({
          userId: coordinatorEmail, // Assuming coordinatorEmail is the user identifier
          type: 'system_alert',
          priority: 'normal',
          title: 'Call Failed',
          message: `Call to ${to} failed: ${errorMessage || 'Unknown error'}`,
          metadata: {
            callSid,
            errorCode,
            projectId,
            recordId,
          },
        });
      } catch (error) {
        logError('Failed to create notification', error, {
          service: 'twilio_webhook',
          coordinatorEmail,
          callSid,
          reqId,
        });
      }
    }

    // Log response
    const duration = Date.now() - start;
    logApiResponse('POST', '/api/operations/twilio/voice/status', duration, undefined, reqId);

    // Return success response
    return NextResponse.json({ 
      success: true, 
      status: callStatus 
    });

  } catch (error) {
    logError('Voice status webhook handler error', error, {
      service: 'twilio_webhook',
      reqId,
    });

    return NextResponse.json(
      { error: 'Internal server error' }, 
      { status: 500 }
    );
  }
}
