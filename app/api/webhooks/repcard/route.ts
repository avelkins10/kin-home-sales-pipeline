import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { processAppointmentWebhook, processCustomerWebhook } from '@/lib/repcard/webhook-processor';

export const runtime = 'nodejs';

/**
 * RepCard Webhook Endpoint
 * Receives real-time events from RepCard when appointments or customers are created/updated
 * 
 * This provides near-instant updates instead of waiting for the 5-minute cron sync
 */
export async function POST(request: NextRequest) {
  const requestId = crypto.randomUUID();
  const start = Date.now();
  
  try {
    logApiRequest('POST', '/api/webhooks/repcard', { requestId });

    // Read raw body for signature verification (if RepCard supports it)
    const rawBody = await request.text();

    // Authentication: RepCard may send API key in headers, or we can rely on webhook URL being secret
    // For now, make it optional (similar to Arrivy webhook) until we confirm how RepCard authenticates
    const apiKey = request.headers.get('x-api-key') || request.headers.get('authorization');
    const expectedApiKey = process.env.REPCARD_API_KEY;
    
    // Optional authentication - only verify if both API key and expected key are present
    // This allows webhook to work even if RepCard doesn't send API key in headers
    if (expectedApiKey && apiKey) {
      // Both are present - verify they match
      const apiKeyValue = apiKey.startsWith('Bearer ') ? apiKey.substring(7) : apiKey;
      if (apiKeyValue !== expectedApiKey && !apiKeyValue.includes(expectedApiKey)) {
        logError('[RepCard Webhook] Invalid API key', new Error('Unauthorized'), { requestId });
        return NextResponse.json(
          { error: 'Unauthorized' },
          { status: 401 }
        );
      }
      logInfo('[RepCard Webhook] API key verified', { requestId });
    } else {
      // No API key provided - log but allow (webhook URL acts as secret)
      logInfo('[RepCard Webhook] No API key in headers, proceeding (webhook URL is secret)', { requestId });
    }

    // Parse webhook payload
    let payload: any;
    try {
      payload = JSON.parse(rawBody);
    } catch (parseError) {
      logError('[RepCard Webhook] Invalid JSON payload', parseError as Error, { requestId });
      return NextResponse.json(
        { error: 'Invalid JSON payload' },
        { status: 400 }
      );
    }

    // Log the full payload for debugging
    logInfo('[RepCard Webhook] Received payload', {
      requestId,
      payloadKeys: Object.keys(payload),
      payload: JSON.stringify(payload).substring(0, 500) // First 500 chars for debugging
    });

    // Validate payload structure
    // RepCard sends: trigger_event (e.g., "Appointment Set", "New Contact", "Door knocked")
    // But it might also be in the URL or headers, so be flexible
    const triggerEvent = payload.trigger_event || payload.event_type || payload.type || payload.event || payload.triggerEvent;
    
    // If no trigger event in payload, try to infer from the request or allow processing anyway
    // RepCard might send the event type differently
    if (!triggerEvent) {
      logInfo('[RepCard Webhook] No trigger event in payload, attempting to process anyway', {
        requestId,
        payload,
        hasAppointmentId: !!payload.appointment_id,
        hasContactId: !!payload.contact_id,
        hasCustomerId: !!payload.customer_id
      });
      // Don't fail - try to process based on what data is present
    }

    // Map RepCard trigger events to our object types
    // Be flexible - RepCard might send data in different formats
    const eventType = triggerEvent || 'unknown';
    let objectType: string | undefined;
    let objectId: string | number | undefined;

    // First, try to determine object type from payload structure (most reliable)
    // RepCard webhook format: { id: 628195, contact: { id: 46662815 }, user: { id: 139887 }, closer: { id: 174886 } }
    if (payload.id && (payload.contact || payload.closer || payload.user || payload.appt_start_time)) {
      // Has appointment-like structure (id + contact/closer/user/time)
      objectType = 'appointment';
      objectId = payload.id;
    } else if (payload.appointment_id || payload.appointmentId || payload.appointment?.id) {
      objectType = 'appointment';
      objectId = payload.appointment_id || payload.appointmentId || payload.appointment?.id;
    } else if (payload.contact_id || payload.customer_id || payload.contactId || payload.customerId || payload.contact?.id) {
      objectType = 'customer';
      objectId = payload.contact_id || payload.customer_id || payload.contactId || payload.customerId || payload.contact?.id;
    } else if (payload.id && !payload.contact && !payload.closer) {
      // Just an ID without appointment structure - likely a customer
      objectType = 'customer';
      objectId = payload.id;
    } else if (triggerEvent) {
      // If we have trigger event, use it to determine type
      const triggerLower = triggerEvent.toLowerCase();
      if (triggerLower.includes('appointment')) {
        objectType = 'appointment';
        objectId = payload.appointment_id || payload.appointmentId || payload.id || payload.object_id;
      } else if (triggerLower.includes('contact') || triggerLower.includes('customer') || triggerLower.includes('door')) {
        objectType = 'customer';
        objectId = payload.contact_id || payload.customer_id || payload.contactId || payload.customerId || payload.id || payload.object_id;
      } else if (triggerLower.includes('user')) {
        objectType = 'user';
        objectId = payload.user_id || payload.userId || payload.id || payload.object_id;
      } else {
        // Unknown trigger - try to infer from payload
        objectType = payload.object_type || payload.resource || payload.entity;
        objectId = payload.object_id || payload.id || payload.resource_id;
      }
    } else {
      // No trigger event and can't infer from structure - try generic approach
      objectType = payload.object_type || payload.resource || payload.entity;
      objectId = payload.object_id || payload.id || payload.resource_id;
    }

    logInfo('[RepCard Webhook] Received event', {
      requestId,
      eventType,
      objectType,
      objectId,
      timestamp: payload.timestamp || payload.created_at || payload.updated_at || new Date().toISOString(),
      // Log key fields from actual RepCard payload format
      appointmentId: payload.id,
      contactId: payload.contact?.id,
      setterId: payload.user?.id,
      closerId: payload.closer?.id,
      scheduledTime: payload.appt_start_time || payload.appt_start_time_local
    });

    // Process webhook event based on object type (more reliable than trigger event)
    let result: any = { success: true, processed: false };

    // Process based on object type first (most reliable)
    // WEBHOOK-FIRST: Parse payload directly instead of calling API
    if (objectType === 'appointment') {
      result = await processAppointmentWebhook(payload, requestId);
    } else if (objectType === 'customer' || objectType === 'contact') {
      result = await processCustomerWebhook(payload, requestId);
    } else if (objectType === 'user') {
      // User events - sync users (optional, less critical)
      logInfo('[RepCard Webhook] User event received, skipping (users sync via cron)', {
        requestId,
        eventType,
        objectId
      });
      result = { processed: false, message: 'User events handled by cron sync' };
    } else {
      // Unknown object type - try to infer from payload structure
      logInfo('[RepCard Webhook] Unknown object type, attempting to infer from payload', {
        requestId,
        eventType,
        objectType,
        payload
      });
      
      if (payload.appointment_id || payload.appointmentId || payload.appointment?.id) {
        result = await processAppointmentWebhook(payload, requestId);
      } else if (payload.contact_id || payload.customer_id || payload.contactId || payload.customerId || payload.contact?.id) {
        result = await processCustomerWebhook(payload, requestId);
      } else {
        // Can't determine - but don't fail, just log and return success
        logInfo('[RepCard Webhook] Could not determine object type, accepting webhook anyway', { 
          requestId, 
          payload: JSON.stringify(payload).substring(0, 500)
        });
        result = { processed: false, message: 'Unknown object type, but webhook accepted' };
      }
    }

    const duration = Date.now() - start;
    logApiResponse('POST', '/api/webhooks/repcard', duration, { status: 200, requestId });

    return NextResponse.json({
      success: true,
      requestId,
      processed: result.processed,
      eventType,
      objectType,
      objectId,
      message: result.message || 'Webhook processed successfully'
    }, { status: 200 });

  } catch (error) {
    const duration = Date.now() - start;
    logError('[RepCard Webhook] Failed to process webhook', error as Error, { requestId });
    logApiResponse('POST', '/api/webhooks/repcard', duration, { status: 500, requestId });

    // Return 200 to prevent webhook retries for transient errors
    // Log the error but don't fail the webhook
    return NextResponse.json({
      success: false,
      requestId,
      error: 'Webhook processing failed',
      message: error instanceof Error ? error.message : 'Unknown error'
    }, { status: 200 }); // Return 200 so RepCard doesn't retry
  }
}

// NOTE: processAppointmentWebhook and processCustomerWebhook are now in lib/repcard/webhook-processor.ts
// They parse webhook payloads directly instead of calling the API

/**
 * GET endpoint for health check
 */
export async function GET(request: NextRequest) {
  return NextResponse.json({
    status: 'ok',
    service: 'repcard-webhook',
    timestamp: new Date().toISOString()
  });
}
