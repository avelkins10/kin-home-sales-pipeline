// app/api/webhooks/arrivy/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { processWebhookEvent } from '@/lib/integrations/arrivy/service';
import type { ArrivyWebhookPayload } from '@/lib/integrations/arrivy/types';
import { logArrivyWebhook, logError, logWarn } from '@/lib/logging/logger';
import crypto from 'crypto';

export const runtime = 'nodejs';

/**
 * Arrivy Webhook Endpoint
 * Receives and processes real-time events from Arrivy
 */
export async function POST(request: NextRequest) {
  const requestId = crypto.randomUUID();
  
  try {
    // Read raw body for signature verification
    const rawBody = await request.text();

    // TEMPORARILY DISABLED: Signature verification
    // Arrivy's "Custom Webhook Authentication Keys" doesn't send x-arrivy-signature header
    // TODO: Investigate proper authentication method with Arrivy

    // const webhookSecret = process.env.ARRIVY_WEBHOOK_SECRET;
    // if (webhookSecret) {
    //   const signature = request.headers.get('x-arrivy-signature');
    //
    //   if (!signature) {
    //     logWarn('[Arrivy Webhook] Missing signature header', { requestId });
    //     return NextResponse.json(
    //       { error: 'Missing webhook signature' },
    //       { status: 401 }
    //     );
    //   }
    //
    //   // Compute HMAC-SHA256 over raw body
    //   const expectedSignature = crypto
    //     .createHmac('sha256', webhookSecret)
    //     .update(rawBody)
    //     .digest('hex');
    //
    //   // Support both formats: "hex" or "sha256=hex"
    //   const providedSignature = signature.startsWith('sha256=')
    //     ? signature.substring(7)
    //     : signature;
    //
    //   // Constant-time comparison to prevent timing attacks
    //   const isValid = crypto.timingSafeEqual(
    //     Buffer.from(expectedSignature, 'hex'),
    //     Buffer.from(providedSignature, 'hex')
    //   );
    //
    //   if (!isValid) {
    //     logWarn('[Arrivy Webhook] Invalid signature', { requestId });
    //     return NextResponse.json(
    //       { error: 'Invalid webhook signature' },
    //       { status: 401 }
    //     );
    //   }
    // }

    // Parse webhook payload after signature verification
    let payload: ArrivyWebhookPayload;
    try {
      payload = JSON.parse(rawBody);
    } catch (parseError) {
      logError('[Arrivy Webhook] Invalid JSON payload', parseError as Error, { requestId });
      return NextResponse.json(
        { error: 'Invalid JSON payload' },
        { status: 400 }
      );
    }

    // Validate required fields
    if (!payload.EVENT_ID || !payload.EVENT_TYPE || !payload.OBJECT_ID || !payload.EVENT_TIME) {
      logWarn('[Arrivy Webhook] Invalid payload - missing required fields', {
        requestId,
        hasEventId: !!payload.EVENT_ID,
        hasEventType: !!payload.EVENT_TYPE,
        hasObjectId: !!payload.OBJECT_ID,
        hasEventTime: !!payload.EVENT_TIME,
      });

      return NextResponse.json(
        { error: 'Invalid webhook payload - missing required fields' },
        { status: 400 }
      );
    }

    // Log webhook event
    logArrivyWebhook(
      payload.EVENT_TYPE,
      payload.EVENT_SUB_TYPE || null,
      {
        EVENT_ID: payload.EVENT_ID,
        OBJECT_ID: payload.OBJECT_ID,
        REPORTER_ID: payload.REPORTER_ID,
        REPORTER_NAME: payload.REPORTER_NAME,
        payload,
      }
    );

    // Process webhook event
    const result = await processWebhookEvent(payload);

    // Return 200 for successful processing or duplicates
    if (result.duplicate) {
      return NextResponse.json({
        success: true,
        requestId,
        eventId: result.eventId,
        duplicate: true,
        message: 'Duplicate event, already processed',
      }, { status: 200 });
    }

    return NextResponse.json({
      success: true,
      requestId,
      eventId: result.eventId,
      processed: true,
      notificationCreated: result.notificationCreated || false,
    }, { status: 200 });

  } catch (error) {
    logError('[Arrivy Webhook] Failed to process webhook', error as Error, {
      requestId,
    });

    // Return 400 for bad request/payload errors
    if (error instanceof SyntaxError) {
      return NextResponse.json(
        { error: 'Invalid JSON payload' },
        { status: 400 }
      );
    }

    // Return 500 for processing failures to trigger Arrivy retries
    return NextResponse.json({
      success: false,
      requestId,
      error: 'Internal server error',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, { status: 500 });
  }
}

/**
 * GET handler for health check
 */
export async function GET() {
  return NextResponse.json({
    status: 'ok',
    service: 'arrivy-webhook',
    timestamp: new Date().toISOString(),
  });
}

