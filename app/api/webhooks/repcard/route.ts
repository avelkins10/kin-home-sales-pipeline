import { NextRequest, NextResponse } from 'next/server';
import { sql } from '@/lib/db/client';
import { logApiRequest, logApiResponse, logError, logInfo } from '@/lib/logging/logger';
import { syncAppointments, syncCustomers } from '@/lib/repcard/sync-service';

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

    // TODO: Add signature verification if RepCard provides webhook secrets
    // For now, we'll rely on IP whitelisting or API key in headers
    const apiKey = request.headers.get('x-api-key') || request.headers.get('authorization');
    const expectedApiKey = process.env.REPCARD_API_KEY;
    
    if (expectedApiKey && apiKey !== expectedApiKey && !apiKey?.includes(expectedApiKey)) {
      logError('[RepCard Webhook] Invalid API key', new Error('Unauthorized'), { requestId });
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
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

    // Validate payload structure
    // RepCard sends: trigger_event (e.g., "Appointment Set", "New Contact", "Door knocked")
    const triggerEvent = payload.trigger_event || payload.event_type || payload.type || payload.event;
    
    if (!triggerEvent) {
      logError('[RepCard Webhook] Missing trigger event', new Error('Invalid payload'), { requestId, payload });
      return NextResponse.json(
        { error: 'Missing trigger event' },
        { status: 400 }
      );
    }

    // Map RepCard trigger events to our object types
    const eventType = triggerEvent;
    let objectType: string | undefined;
    let objectId: string | number | undefined;

    // Determine object type from trigger event
    if (triggerEvent === 'Appointment Set' || triggerEvent === 'Appointment Update' || triggerEvent === 'Appointment Outcome') {
      objectType = 'appointment';
      objectId = payload.appointment_id || payload.appointmentId || payload.id || payload.object_id;
    } else if (triggerEvent === 'New Contact' || triggerEvent === 'Update Contact' || triggerEvent === 'Contact Removed' || triggerEvent === 'Door knocked') {
      objectType = 'customer';
      objectId = payload.contact_id || payload.customer_id || payload.contactId || payload.customerId || payload.id || payload.object_id;
    } else if (triggerEvent === 'New User' || triggerEvent === 'Update User' || triggerEvent === 'Remove User') {
      objectType = 'user';
      objectId = payload.user_id || payload.userId || payload.id || payload.object_id;
    } else if (triggerEvent === 'Status Changed' || triggerEvent === 'Contact Type Changed') {
      // These could be customer or appointment related - check payload
      if (payload.contact_id || payload.customer_id) {
        objectType = 'customer';
        objectId = payload.contact_id || payload.customer_id;
      } else if (payload.appointment_id) {
        objectType = 'appointment';
        objectId = payload.appointment_id;
      } else {
        objectType = 'customer'; // Default to customer for status changes
        objectId = payload.id || payload.object_id;
      }
    } else {
      // Fallback: try to infer from payload
      objectType = payload.object_type || payload.resource || payload.entity;
      objectId = payload.object_id || payload.id || payload.resource_id;
    }

    logInfo('[RepCard Webhook] Received event', {
      requestId,
      eventType,
      objectType,
      objectId,
      timestamp: payload.timestamp || payload.created_at || new Date().toISOString()
    });

    // Process webhook event based on trigger event
    let result: any = { success: true, processed: false };

    // Map RepCard trigger events to processing functions
    const triggerEventLower = eventType?.toLowerCase() || '';
    
    if (triggerEventLower.includes('appointment set') || 
        triggerEventLower.includes('appointment update') || 
        triggerEventLower.includes('appointment outcome') ||
        objectType === 'appointment') {
      // Appointment-related events
      result = await processAppointmentWebhook(payload, requestId);
    } else if (triggerEventLower.includes('new contact') || 
               triggerEventLower.includes('update contact') || 
               triggerEventLower.includes('contact removed') ||
               triggerEventLower.includes('door knocked') ||
               objectType === 'customer' || objectType === 'contact') {
      // Customer/Contact-related events
      result = await processCustomerWebhook(payload, requestId);
    } else if (triggerEventLower.includes('status changed') || 
               triggerEventLower.includes('contact type changed')) {
      // Status changes - could be customer or appointment
      // Check payload to determine
      if (payload.appointment_id || payload.appointmentId) {
        result = await processAppointmentWebhook(payload, requestId);
      } else {
        result = await processCustomerWebhook(payload, requestId);
      }
    } else if (triggerEventLower.includes('user')) {
      // User events - sync users (optional, less critical)
      logInfo('[RepCard Webhook] User event received, skipping (users sync via cron)', {
        requestId,
        eventType,
        objectId
      });
      result = { processed: false, message: 'User events handled by cron sync' };
    } else {
      // Unknown event - try to infer from payload
      logInfo('[RepCard Webhook] Unknown trigger event, attempting to infer from payload', {
        requestId,
        eventType,
        payload
      });
      
      if (payload.appointment_id || payload.appointmentId || payload.contact?.id) {
        result = await processAppointmentWebhook(payload, requestId);
      } else if (payload.contact_id || payload.customer_id || payload.customerId) {
        result = await processCustomerWebhook(payload, requestId);
      } else {
        logInfo('[RepCard Webhook] Could not determine object type, skipping', { requestId, payload });
        result = { processed: false, message: 'Unknown trigger event, skipped' };
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

/**
 * Process appointment webhook event
 */
async function processAppointmentWebhook(payload: any, requestId: string): Promise<{ processed: boolean; message: string }> {
  try {
    const appointmentId = payload.appointment_id || payload.id || payload.object_id;
    
    if (!appointmentId) {
      return { processed: false, message: 'Missing appointment ID' };
    }

    logInfo('[RepCard Webhook] Processing appointment webhook', {
      requestId,
      appointmentId,
      triggerEvent: payload.trigger_event || payload.event_type || payload.type
    });

    // Trigger incremental sync for this specific appointment
    // This will fetch the latest data from RepCard API
    const syncResult = await syncAppointments({
      incremental: true
    });

    // Also auto-link appointments to customers
    const linkResult = await sql`
      UPDATE repcard_appointments a
      SET 
        customer_id = c.id,
        updated_at = NOW()
      FROM repcard_customers c
      WHERE a.repcard_customer_id::text = c.repcard_customer_id::text
        AND a.customer_id IS NULL
        AND c.id IS NOT NULL
    `;
    const appointmentsLinked = Array.isArray(linkResult) ? 0 : (linkResult as any).rowCount || 0;

    logInfo('[RepCard Webhook] Appointment sync completed', {
      requestId,
      appointmentId,
      recordsFetched: syncResult.recordsFetched,
      recordsUpdated: syncResult.recordsUpdated,
      appointmentsLinked
    });

    return {
      processed: true,
      message: `Appointment ${appointmentId} synced successfully`
    };

  } catch (error) {
    logError('[RepCard Webhook] Failed to process appointment', error as Error, { requestId });
    throw error;
  }
}

/**
 * Process customer webhook event
 */
async function processCustomerWebhook(payload: any, requestId: string): Promise<{ processed: boolean; message: string }> {
  try {
    const customerId = payload.customer_id || payload.contact_id || payload.id || payload.object_id;
    
    if (!customerId) {
      return { processed: false, message: 'Missing customer ID' };
    }

    logInfo('[RepCard Webhook] Processing customer webhook', {
      requestId,
      customerId,
      triggerEvent: payload.trigger_event || payload.event_type || payload.type
    });

    // Trigger incremental sync for customers
    const syncResult = await syncCustomers({
      incremental: true
    });

    // After customer sync, link any appointments that were waiting for this customer
    const linkResult = await sql`
      UPDATE repcard_appointments a
      SET 
        customer_id = c.id,
        updated_at = NOW()
      FROM repcard_customers c
      WHERE a.repcard_customer_id::text = c.repcard_customer_id::text
        AND a.customer_id IS NULL
        AND c.id IS NOT NULL
        AND c.repcard_customer_id::text = ${customerId.toString()}::text
    `;
    const appointmentsLinked = Array.isArray(linkResult) ? 0 : (linkResult as any).rowCount || 0;

    logInfo('[RepCard Webhook] Customer sync completed', {
      requestId,
      customerId,
      recordsFetched: syncResult.recordsFetched,
      recordsUpdated: syncResult.recordsUpdated,
      appointmentsLinked
    });

    return {
      processed: true,
      message: `Customer ${customerId} synced successfully, linked ${appointmentsLinked} appointments`
    };

  } catch (error) {
    logError('[RepCard Webhook] Failed to process customer', error as Error, { requestId });
    throw error;
  }
}

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
