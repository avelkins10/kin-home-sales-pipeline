import { NextResponse } from 'next/server';
import twilio from 'twilio';
import { logApiRequest, logApiResponse, logTwilioWebhook, logError } from '@/lib/logging/logger';
import { twilioConfig } from '@/lib/integrations/twilio/client';

export const runtime = 'nodejs';

export async function POST(request: Request) {
  const reqId = crypto.randomUUID();
  const start = Date.now();
  
  try {
    // Log request
    logApiRequest('POST', '/api/operations/twilio/voice', undefined, reqId);

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
    const callSid = formData.get('CallSid');
    const from = formData.get('From');
    const to = formData.get('To');
    const callStatus = formData.get('CallStatus');
    const direction = formData.get('Direction');
    
    // Extract metadata from URL query string
    const { searchParams } = new URL(request.url);
    const projectId = searchParams.get('projectId') || undefined;
    const coordinatorName = searchParams.get('coordinatorName') || 'Project Coordinator';

    // Log webhook event
    logTwilioWebhook('voice', callStatus || 'unknown', {
      CallSid: callSid,
      status: callStatus || 'unknown',
    });

    // Generate TwiML response
    const twiml = new twilio.twiml.VoiceResponse();

    if (direction === 'inbound') {
      // Customer calling in
      twiml.say('Thank you for calling Kin Home. Please hold while we connect you to your project coordinator.');
      
      // Get coordinator phone from query params only (no fallback to Twilio DID)
      const coordinatorPhone = searchParams.get('coordinatorPhone');
      
      if (coordinatorPhone) {
        twiml.dial(coordinatorPhone);
      } else {
        // No coordinator phone - play message and terminate
        // Note: A datastore lookup can be added later to resolve coordinator phone from project metadata
        twiml.say('We apologize, but we are unable to connect you to a coordinator at this time. Please try calling back during business hours or contact us through our website.');
        twiml.pause({ length: 2 });
        twiml.say('Thank you for calling Kin Home. Goodbye.');
      }
      
    } else if (direction?.startsWith('outbound') || direction === 'outbound-api') {
      // PC calling customer
      twiml.say(`Hello, this is ${coordinatorName} from Kin Home calling about your solar project. Please stay on the line.`);
      
      // Record the call if enabled
      if (twilioConfig?.recordCalls) {
        twiml.record({
          action: `${process.env.TWILIO_WEBHOOK_URL}/api/operations/twilio/voice/recording`,
          method: 'POST',
          maxLength: 300, // 5 minutes max
        });
      }
      
      // Add a brief pause before hanging up
      twiml.pause({ length: 2 });
      twiml.say('Thank you for your time. Have a great day!');
    } else {
      // Fallback for unknown direction
      twiml.say('We\'re sorry, an error occurred. Please try again later.');
    }

    // Log response
    const duration = Date.now() - start;
    logApiResponse('POST', '/api/operations/twilio/voice', duration, undefined, reqId);

    // Return TwiML response
    return new Response(twiml.toString(), {
      headers: {
        'Content-Type': 'text/xml',
      },
    });

  } catch (error) {
    logError('Voice webhook handler error', error, {
      service: 'twilio_webhook',
      reqId,
    });

    // Return fallback TwiML (never return 500 for TwiML endpoints)
    const twiml = new twilio.twiml.VoiceResponse();
    twiml.say('We\'re sorry, an error occurred. Please try again later.');
    
    return new Response(twiml.toString(), {
      headers: {
        'Content-Type': 'text/xml',
      },
    });
  }
}
