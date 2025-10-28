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
    // Parse webhook payload
    const payload: ArrivyWebhookPayload = await request.json();

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

    // Optional: Verify webhook signature if configured
    const webhookSecret = process.env.ARRIVY_WEBHOOK_SECRET;
    if (webhookSecret) {
      const signature = request.headers.get('x-arrivy-signature');
      if (signature) {
        // Verify signature (if Arrivy provides this feature)
        // const isValid = verifySignature(payload, signature, webhookSecret);
        // if (!isValid) {
        //   return NextResponse.json({ error: 'Invalid signature' }, { status: 401 });
        // }
      }
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

    // Return 200 OK within 30 seconds (per Arrivy requirements)
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

    // Still return 200 to prevent retry storm if it's a processing error
    // Only return 500 for truly unrecoverable errors
    if (error instanceof SyntaxError) {
      return NextResponse.json(
        { error: 'Invalid JSON payload' },
        { status: 400 }
      );
    }

    return NextResponse.json({
      success: false,
      requestId,
      error: 'Internal server error',
    }, { status: 200 }); // Return 200 to acknowledge receipt
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

