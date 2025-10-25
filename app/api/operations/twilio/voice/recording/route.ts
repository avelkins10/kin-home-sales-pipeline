import { NextResponse } from 'next/server';
import twilio from 'twilio';
import { logApiRequest, logApiResponse, logTwilioWebhook, logError } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { INSTALL_COMMUNICATION_FIELDS, QB_TABLE_INSTALL_COMMUNICATIONS } from '@/lib/constants/fieldIds';

export const runtime = 'nodejs';

export async function POST(request: Request) {
  const reqId = crypto.randomUUID();
  const start = Date.now();
  
  try {
    // Log request
    logApiRequest('POST', '/api/operations/twilio/voice/recording', undefined, reqId);

    // Get Twilio signature for validation
    const signature = request.headers.get('X-Twilio-Signature');
    if (!signature) {
      logError('Missing Twilio signature', new Error('No signature header'), {
        service: 'twilio_webhook',
        reqId,
      });
      return new Response('Unauthorized', { status: 401 });
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
      return new Response('Unauthorized', { status: 401 });
    }

    // Parse webhook payload
    const formData = new URLSearchParams(body);
    const recordingSid = formData.get('RecordingSid');
    const recordingStatus = formData.get('RecordingStatus');
    const recordingUrl = formData.get('RecordingUrl');
    const callSid = formData.get('CallSid');

    // Extract metadata from URL query string
    const { searchParams } = new URL(request.url);
    const projectId = searchParams.get('projectId') || undefined;
    const recordId = searchParams.get('recordId') ? parseInt(searchParams.get('recordId')!) : undefined;
    const coordinatorEmail = searchParams.get('coordinatorEmail') || undefined;

    // Log webhook event
    logTwilioWebhook('voice', 'recording:' + recordingStatus, { 
      CallSid: callSid, 
      status: recordingStatus 
    });

    // Update QuickBase with recording information if projectId and coordinatorEmail are available
    if (projectId && coordinatorEmail && recordingSid && recordingUrl) {
      try {
        const communicationData = {
          [INSTALL_COMMUNICATION_FIELDS.DATE]: new Date().toISOString(),
          [INSTALL_COMMUNICATION_FIELDS.NOTE_BY]: coordinatorEmail,
          [INSTALL_COMMUNICATION_FIELDS.RELATED_PROJECT]: projectId,
          [INSTALL_COMMUNICATION_FIELDS.COMMUNICATION_NOTE]: `Call recording completed (Status: ${recordingStatus}, Recording SID: ${recordingSid}, URL: ${recordingUrl})`,
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
        logError('Failed to create Install Communication record for recording', error, {
          service: 'twilio_webhook',
          projectId,
          recordingSid,
          reqId,
        });
      }
    }

    // Log response
    const duration = Date.now() - start;
    logApiResponse('POST', '/api/operations/twilio/voice/recording', duration, undefined, reqId);

    // Return success response
    return NextResponse.json({ success: true });

  } catch (error) {
    logError('Voice recording webhook handler error', error, {
      service: 'twilio_webhook',
      reqId,
    });

    return NextResponse.json(
      { error: 'Internal server error' }, 
      { status: 500 }
    );
  }
}