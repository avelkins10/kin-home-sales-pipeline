/**
 * RepCard Webhook Processor
 * 
 * WEBHOOK-FIRST ARCHITECTURE: Parses webhook payloads directly and inserts/updates database.
 * Only falls back to API calls if payload is missing critical data.
 * 
 * This is much more efficient than calling the API for every webhook event.
 */

import { sql } from '@/lib/db/client';
import { logError, logInfo } from '@/lib/logging/logger';
import { syncAppointments, syncCustomers } from './sync-service';
import { repcardClient } from './client';
import { syncCustomerAttachments, syncAppointmentAttachments } from './comprehensive-sync';

/**
 * Process appointment webhook payload directly
 * 
 * RepCard webhook payload format:
 * {
 *   id: 628195,
 *   contact: { id: 46662815, createdAt: "2026-01-22 22:57:44+00:00" },
 *   user: { id: 139887 },
 *   closer: { id: 174886 },
 *   appt_start_time: "2026-01-24 16:00:00",
 *   appt_end_time: "2026-01-24 17:00:00",
 *   appointment_status_title: "Scheduled",
 *   duration: 60,
 *   notes: "...",
 *   created_at: "2026-01-22 22:57:58+00:00",
 *   updated_at: "2026-01-22 22:57:58+00:00"
 * }
 */
export async function processAppointmentWebhook(
  payload: any,
  requestId: string
): Promise<{ processed: boolean; message: string; usedApi?: boolean }> {
  try {
    // Extract IDs from various possible formats
    const appointmentId = payload.id || payload.appointment_id || payload.appointmentId || payload.object_id;
    const repcardCustomerId = payload.contact?.id || payload.contact_id || payload.customer_id;
    const setterUserId = payload.user?.id || payload.user_id || payload.setter_id;
    const closerUserId = payload.closer?.id || payload.closer_id;

    if (!appointmentId) {
      logError('[RepCard Webhook Processor] Missing appointment ID in payload', new Error('Invalid payload'), { requestId, payload });
      return { processed: false, message: 'Missing appointment ID' };
    }

    logInfo('[RepCard Webhook Processor] Processing appointment from payload', {
      requestId,
      appointmentId,
      repcardCustomerId,
      setterUserId,
      closerUserId
    });

    // Parse scheduled time from various formats
    const scheduledAt = payload.appt_start_time || payload.appt_start_time_with_offset || payload.appt_start_time_local || payload.start_at || null;
    const completedAt = payload.appt_end_time || payload.appt_end_time_with_offset || payload.appt_end_time_local || payload.end_at || null;
    const duration = payload.duration || payload.durationTime || null;
    const notes = payload.notes || payload.appointment_notes || null;

    // Extract disposition from status
    const disposition = payload.appointment_status_title || payload.status?.title || payload.status_title || null;
    
    // Determine status category from disposition
    let statusCategory: string | null = null;
    if (disposition) {
      const dispLower = disposition.toLowerCase();
      if (dispLower.includes('cancel')) statusCategory = 'cancelled';
      else if (dispLower.includes('reschedule')) statusCategory = 'rescheduled';
      else if (dispLower.includes('no.show') || dispLower.includes('no_show') || dispLower.includes('no sit')) statusCategory = 'no_show';
      else if (dispLower.includes('sat.closed') || dispLower.includes('sat_closed') || dispLower.includes('closed')) statusCategory = 'sat_closed';
      else if (dispLower.includes('sat.no.close') || dispLower.includes('sat_no_close')) statusCategory = 'sat_no_close';
      else if (completedAt) statusCategory = 'completed';
      else if (scheduledAt) statusCategory = 'scheduled';
    }

    // Get customer_id from our database (if customer exists)
    // If customer doesn't exist, fetch it from RepCard API
    let customerId: string | null = null;
    if (repcardCustomerId) {
      const customerResult = await sql`
        SELECT id FROM repcard_customers
        WHERE repcard_customer_id = ${repcardCustomerId.toString()}::text
        LIMIT 1
      `;
      const customerRows = customerResult.rows || customerResult;
      if (customerRows.length > 0) {
        customerId = customerRows[0].id;
      } else {
        // Customer doesn't exist - fetch from RepCard API
        logInfo('[RepCard Webhook Processor] Customer not found, fetching from API', {
          requestId,
          repcardCustomerId
        });
        try {
          const customerResponse = await repcardClient.getCustomerById(parseInt(repcardCustomerId.toString(), 10));
          const customer = customerResponse.result;
          
          if (customer) {
            // Insert customer into database
            const customerSetterUserId = (customer as any).userId || customer.assignedUserId || (customer as any).setterUserId || setterUserId || null;
            
            const insertResult = await sql`
              INSERT INTO repcard_customers (
                repcard_customer_id,
                setter_user_id,
                office_id,
                name,
                email,
                phone,
                address,
                city,
                state,
                zip,
                status,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${customer.id.toString()}::text,
                ${customerSetterUserId},
                ${null}, -- office_id
                ${`${customer.firstName || ''} ${customer.lastName || ''}`.trim() || null},
                ${customer.email || null},
                ${customer.phone || null},
                ${customer.address || null},
                ${customer.city || null},
                ${customer.state || null},
                ${customer.zipCode || customer.zip || null},
                ${customer.statusId?.toString() || null},
                ${customer.createdAt ? new Date(customer.createdAt).toISOString() : null},
                ${customer.updatedAt ? new Date(customer.updatedAt).toISOString() : null},
                ${JSON.stringify(customer)}
              )
              ON CONFLICT (repcard_customer_id)
              DO UPDATE SET
                setter_user_id = COALESCE(EXCLUDED.setter_user_id, repcard_customers.setter_user_id),
                name = EXCLUDED.name,
                email = EXCLUDED.email,
                phone = EXCLUDED.phone,
                address = EXCLUDED.address,
                city = EXCLUDED.city,
                state = EXCLUDED.state,
                zip = EXCLUDED.zip,
                status = EXCLUDED.status,
                created_at = COALESCE(repcard_customers.created_at, EXCLUDED.created_at),
                updated_at = EXCLUDED.updated_at,
                raw_data = EXCLUDED.raw_data,
                synced_at = NOW()
              RETURNING id
            `;
            
            const insertedRows = insertResult.rows || insertResult;
            if (insertedRows.length > 0) {
              customerId = insertedRows[0].id;
              logInfo('[RepCard Webhook Processor] Customer synced from API', {
                requestId,
                repcardCustomerId,
                customerId
              });
            }
          }
        } catch (customerError) {
          logError('[RepCard Webhook Processor] Failed to fetch customer from API', customerError as Error, {
            requestId,
            repcardCustomerId
          });
          // Continue without customer - appointment will still be processed
        }
      }
    }

    // Get office_id from customer, setter, or closer (order of preference)
    let officeId: number | null = null;
    
    if (repcardCustomerId) {
      const customerOfficeResult = await sql`
        SELECT office_id FROM repcard_customers 
        WHERE repcard_customer_id = ${repcardCustomerId.toString()}::text
        LIMIT 1
      `;
      const customerOfficeRows = customerOfficeResult.rows || customerOfficeResult;
      if (customerOfficeRows.length > 0 && customerOfficeRows[0].office_id) {
        officeId = customerOfficeRows[0].office_id;
      }
    }
    
    if (!officeId && setterUserId) {
      const setterOfficeResult = await sql`
        SELECT office_id FROM repcard_users 
        WHERE repcard_user_id = ${setterUserId} 
        LIMIT 1
      `;
      const setterOfficeRows = setterOfficeResult.rows || setterOfficeResult;
      if (setterOfficeRows.length > 0 && setterOfficeRows[0].office_id) {
        officeId = setterOfficeRows[0].office_id;
      }
    }
    
    if (!officeId && closerUserId) {
      const closerOfficeResult = await sql`
        SELECT office_id FROM repcard_users 
        WHERE repcard_user_id = ${closerUserId} 
        LIMIT 1
      `;
      const closerOfficeRows = closerOfficeResult.rows || closerOfficeResult;
      if (closerOfficeRows.length > 0 && closerOfficeRows[0].office_id) {
        officeId = closerOfficeRows[0].office_id;
      }
    }

    // Determine reschedule status
    let isReschedule = false;
    let rescheduleCount = 0;
    let originalAppointmentId: string | null = null;

    if (repcardCustomerId) {
      const existingAppointmentsResult = await sql`
        SELECT
          id,
          repcard_appointment_id,
          scheduled_at,
          created_at,
          is_reschedule,
          original_appointment_id
        FROM repcard_appointments
        WHERE repcard_customer_id = ${repcardCustomerId.toString()}::text
          AND repcard_appointment_id != ${appointmentId.toString()}::text
        ORDER BY COALESCE(scheduled_at, created_at) ASC
      `;
      const existingAppointments = existingAppointmentsResult.rows || existingAppointmentsResult;

      if (existingAppointments.length > 0) {
        isReschedule = true;
        rescheduleCount = existingAppointments.length;
        const firstAppointment = existingAppointments[0];
        originalAppointmentId = firstAppointment.original_appointment_id || firstAppointment.id;
      }
    }

    // Parse timestamps
    const createdAt = payload.created_at || payload.createdAt || new Date().toISOString();
    const updatedAt = payload.updated_at || payload.updatedAt || new Date().toISOString();

    // Auto-create users in repcard_users if they don't exist (from payload data)
    // This ensures the joins work even if users haven't been synced yet
    if (setterUserId && payload.user) {
      try {
        const setterData = payload.user;
        await sql`
          INSERT INTO repcard_users (
            repcard_user_id,
            first_name,
            last_name,
            email,
            raw_data
          )
          VALUES (
            ${setterUserId}::int,
            ${setterData.firstName || setterData.first_name || null},
            ${setterData.lastName || setterData.last_name || null},
            ${setterData.email || null},
            ${JSON.stringify(setterData)}
          )
          ON CONFLICT (repcard_user_id) DO NOTHING
        `;
      } catch (userError) {
        // Non-fatal - continue even if user creation fails
        logInfo('[RepCard Webhook Processor] Could not auto-create setter user', {
          requestId,
          setterUserId,
          error: userError instanceof Error ? userError.message : String(userError)
        });
      }
    }

    if (closerUserId && payload.closer) {
      try {
        const closerData = payload.closer;
        await sql`
          INSERT INTO repcard_users (
            repcard_user_id,
            first_name,
            last_name,
            email,
            raw_data
          )
          VALUES (
            ${closerUserId}::int,
            ${closerData.firstName || closerData.first_name || null},
            ${closerData.lastName || closerData.last_name || null},
            ${closerData.email || null},
            ${JSON.stringify(closerData)}
          )
          ON CONFLICT (repcard_user_id) DO NOTHING
        `;
      } catch (userError) {
        // Non-fatal - continue even if user creation fails
        logInfo('[RepCard Webhook Processor] Could not auto-create closer user', {
          requestId,
          closerUserId,
          error: userError instanceof Error ? userError.message : String(userError)
        });
      }
    }

    // Insert/update appointment directly from payload
    // NOTE: is_within_48_hours and has_power_bill are calculated by database triggers
    const result = await sql`
      INSERT INTO repcard_appointments (
        repcard_appointment_id,
        customer_id,
        repcard_customer_id,
        setter_user_id,
        closer_user_id,
        office_id,
        disposition,
        status_category,
        scheduled_at,
        completed_at,
        duration,
        notes,
        is_reschedule,
        reschedule_count,
        original_appointment_id,
        appointment_link,
        remind_at,
        remind_text,
        appointment_location,
        latitude,
        longitude,
        contact_source,
        created_at,
        updated_at,
        raw_data
      )
      VALUES (
        ${appointmentId.toString()}::text,
        ${customerId},
        ${repcardCustomerId ? repcardCustomerId.toString() : null}::text,
        ${setterUserId || null},
        ${closerUserId || null},
        ${officeId},
        ${disposition},
        ${statusCategory},
        ${scheduledAt ? new Date(scheduledAt).toISOString() : null},
        ${completedAt ? new Date(completedAt).toISOString() : null},
        ${duration},
        ${notes},
        ${isReschedule},
        ${rescheduleCount},
        ${originalAppointmentId},
        ${payload.appointmentLink || payload.appointment_link || null},
        ${payload.remind_at ? new Date(payload.remind_at).toISOString() : null},
        ${payload.remind_text || null},
        ${payload.appointmentLocation || payload.appointment_location || null},
        ${payload.latitude ? parseFloat(payload.latitude.toString()) : null},
        ${payload.longitude ? parseFloat(payload.longitude.toString()) : null},
        ${payload.contact?.contactSource || payload.contactSource || null},
        ${new Date(createdAt).toISOString()},
        ${new Date(updatedAt).toISOString()},
        ${JSON.stringify(payload)}
      )
      ON CONFLICT (repcard_appointment_id)
      DO UPDATE SET
        customer_id = EXCLUDED.customer_id,
        setter_user_id = EXCLUDED.setter_user_id,
        closer_user_id = EXCLUDED.closer_user_id,
        office_id = COALESCE(EXCLUDED.office_id, repcard_appointments.office_id),
        disposition = EXCLUDED.disposition,
        status_category = EXCLUDED.status_category,
        scheduled_at = EXCLUDED.scheduled_at,
        completed_at = EXCLUDED.completed_at,
        duration = EXCLUDED.duration,
        notes = EXCLUDED.notes,
        is_reschedule = EXCLUDED.is_reschedule,
        reschedule_count = EXCLUDED.reschedule_count,
        original_appointment_id = EXCLUDED.original_appointment_id,
        appointment_link = EXCLUDED.appointment_link,
        remind_at = EXCLUDED.remind_at,
        remind_text = EXCLUDED.remind_text,
        appointment_location = EXCLUDED.appointment_location,
        latitude = EXCLUDED.latitude,
        longitude = EXCLUDED.longitude,
        contact_source = EXCLUDED.contact_source,
        updated_at = EXCLUDED.updated_at,
        raw_data = EXCLUDED.raw_data
      RETURNING (xmax = 0) AS inserted
    `;

    const row = result.rows?.[0] || result[0];
    const wasInserted = row?.inserted;

    // Auto-link appointment to customer if customer exists
    if (repcardCustomerId && !customerId) {
      const linkResult = await sql`
        UPDATE repcard_appointments a
        SET 
          customer_id = c.id,
          updated_at = NOW()
        FROM repcard_customers c
        WHERE a.repcard_customer_id::text = c.repcard_customer_id::text
          AND a.repcard_appointment_id = ${appointmentId.toString()}::text
          AND a.customer_id IS NULL
          AND c.id IS NOT NULL
      `;
    }

    logInfo('[RepCard Webhook Processor] Appointment processed from payload', {
      requestId,
      appointmentId,
      wasInserted,
      customerId,
      officeId
    });

    // Trigger attachment sync for this appointment (async, don't wait)
    if (repcardCustomerId) {
      setImmediate(async () => {
        try {
          // Sync customer attachments if customer exists
          await syncCustomerAttachments({ 
            incremental: true,
            fromDate: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0] // Last 7 days
          });
          logInfo('[RepCard Webhook Processor] Customer attachments synced after webhook', {
            requestId,
            repcardCustomerId
          });
        } catch (error) {
          logError('[RepCard Webhook Processor] Failed to sync customer attachments after webhook', error as Error, {
            requestId,
            repcardCustomerId
          });
        }
      });
    }

    // Sync appointment attachments (async, don't wait)
    setImmediate(async () => {
      try {
        await syncAppointmentAttachments({ 
          incremental: true,
          fromDate: new Date(Date.now() - 7 * 24 * 60 * 60 * 1000).toISOString().split('T')[0] // Last 7 days
        });
        logInfo('[RepCard Webhook Processor] Appointment attachments synced after webhook', {
          requestId,
          appointmentId
        });
      } catch (error) {
        logError('[RepCard Webhook Processor] Failed to sync appointment attachments after webhook', error as Error, {
          requestId,
          appointmentId
        });
      }
    });

    return {
      processed: true,
      message: `Appointment ${appointmentId} ${wasInserted ? 'inserted' : 'updated'} from webhook payload`,
      usedApi: false
    };

  } catch (error) {
    logError('[RepCard Webhook Processor] Failed to process appointment from payload', error as Error, { requestId });
    
    // Fallback to API if payload processing fails
    logInfo('[RepCard Webhook Processor] Falling back to API sync', { requestId });
    try {
      const syncResult = await syncAppointments({ incremental: true });
      return {
        processed: true,
        message: `Appointment processed via API fallback`,
        usedApi: true
      };
    } catch (apiError) {
      logError('[RepCard Webhook Processor] API fallback also failed', apiError as Error, { requestId });
      return {
        processed: false,
        message: `Error processing appointment: ${error instanceof Error ? error.message : 'Unknown error'}`
      };
    }
  }
}

/**
 * Process customer webhook payload directly
 * 
 * RepCard webhook payload format:
 * {
 *   id: 46662815,
 *   user: { id: 139887 },
 *   name: "John Doe",
 *   firstName: "John",
 *   lastName: "Doe",
 *   email: "john@example.com",
 *   phone: "+1234567890",
 *   address: "123 Main St",
 *   city: "Tampa",
 *   state: "FL",
 *   zip: "33601",
 *   createdAt: "2026-01-22 22:57:44+00:00",
 *   updatedAt: "2026-01-22 22:57:44+00:00"
 * }
 */
export async function processCustomerWebhook(
  payload: any,
  requestId: string
): Promise<{ processed: boolean; message: string; usedApi?: boolean }> {
  try {
    // Extract customer ID from various possible formats
    const repcardCustomerId = payload.id || payload.contact_id || payload.customer_id || payload.contactId || payload.customerId || payload.object_id;
    const setterUserId = payload.user?.id || payload.owner?.id || payload.user_id || payload.owner_id;

    if (!repcardCustomerId) {
      logError('[RepCard Webhook Processor] Missing customer ID in payload', new Error('Invalid payload'), { requestId, payload });
      return { processed: false, message: 'Missing customer ID' };
    }

    logInfo('[RepCard Webhook Processor] Processing customer from payload', {
      requestId,
      repcardCustomerId,
      setterUserId
    });

    // Parse customer data from payload
    const firstName = payload.firstName || payload.first_name || payload.contact?.firstName || '';
    const lastName = payload.lastName || payload.last_name || payload.contact?.lastName || '';
    const name = payload.name || payload.contact?.name || `${firstName} ${lastName}`.trim() || null;
    const email = payload.email || payload.contact?.email || null;
    const phone = payload.phone || payload.phoneNumber || payload.contact?.phone || payload.contact?.phoneNumber || null;
    const address = payload.address || payload.contact?.address || null;
    const city = payload.city || payload.contact?.city || null;
    const state = payload.state || payload.contact?.state || null;
    const zip = payload.zip || payload.zipCode || payload.contact?.zip || payload.contact?.zipCode || null;
    const statusId = payload.status || payload.statusId || payload.status_id || null;

    // Parse timestamps
    const createdAt = payload.created_at || payload.createdAt || payload.contact?.createdAt || payload.contact?.created_at || new Date().toISOString();
    const updatedAt = payload.updated_at || payload.updatedAt || payload.contact?.updatedAt || payload.contact?.updated_at || new Date().toISOString();

    // Insert/update customer directly from payload
    const result = await sql`
      INSERT INTO repcard_customers (
        repcard_customer_id,
        setter_user_id,
        office_id,
        name,
        email,
        phone,
        address,
        city,
        state,
        zip,
        status,
        contact_source,
        latitude,
        longitude,
        created_at,
        updated_at,
        raw_data
      )
      VALUES (
        ${repcardCustomerId.toString()}::text,
        ${setterUserId || null},
        ${null}, -- office_id not available in customer record
        ${name},
        ${email},
        ${phone},
        ${address},
        ${city},
        ${state},
        ${zip},
        ${statusId?.toString() || null},
        ${payload.contactSource || payload.contact?.contactSource || null},
        ${payload.latitude ? parseFloat(payload.latitude.toString()) : null},
        ${payload.longitude ? parseFloat(payload.longitude.toString()) : null},
        ${new Date(createdAt).toISOString()},
        ${new Date(updatedAt).toISOString()},
        ${JSON.stringify(payload)}
      )
      ON CONFLICT (repcard_customer_id)
      DO UPDATE SET
        setter_user_id = EXCLUDED.setter_user_id,
        name = EXCLUDED.name,
        email = EXCLUDED.email,
        phone = EXCLUDED.phone,
        address = EXCLUDED.address,
        city = EXCLUDED.city,
        state = EXCLUDED.state,
        zip = EXCLUDED.zip,
        status = EXCLUDED.status,
        contact_source = EXCLUDED.contact_source,
        latitude = EXCLUDED.latitude,
        longitude = EXCLUDED.longitude,
        created_at = COALESCE(repcard_customers.created_at, EXCLUDED.created_at),
        updated_at = EXCLUDED.updated_at,
        raw_data = EXCLUDED.raw_data,
        synced_at = NOW()
      RETURNING (xmax = 0) AS inserted
    `;

    const row = result.rows?.[0] || result[0];
    const wasInserted = row?.inserted;

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
        AND c.repcard_customer_id::text = ${repcardCustomerId.toString()}::text
    `;
    const appointmentsLinked = Array.isArray(linkResult) ? 0 : (linkResult as any).rowCount || 0;

    logInfo('[RepCard Webhook Processor] Customer processed from payload', {
      requestId,
      repcardCustomerId,
      wasInserted,
      appointmentsLinked
    });

    return {
      processed: true,
      message: `Customer ${repcardCustomerId} ${wasInserted ? 'inserted' : 'updated'} from webhook payload, linked ${appointmentsLinked} appointments`,
      usedApi: false
    };

  } catch (error) {
    logError('[RepCard Webhook Processor] Failed to process customer from payload', error as Error, { requestId });
    
    // Fallback to API if payload processing fails
    logInfo('[RepCard Webhook Processor] Falling back to API sync', { requestId });
    try {
      const syncResult = await syncCustomers({ incremental: true });
      return {
        processed: true,
        message: `Customer processed via API fallback`,
        usedApi: true
      };
    } catch (apiError) {
      logError('[RepCard Webhook Processor] API fallback also failed', apiError as Error, { requestId });
      return {
        processed: false,
        message: `Error processing customer: ${error instanceof Error ? error.message : 'Unknown error'}`
      };
    }
  }
}

/**
 * Process door knock webhook payload directly
 * 
 * RepCard webhook payload format for door knocks:
 * {
 *   trigger_event: "Door knocked",
 *   user: { id: 139887 },
 *   contact: { id: 46662815 },
 *   door_knocked_at: "2026-01-22 22:57:44+00:00",
 *   status: "Not Interested",
 *   contact_distance: 18.13,
 *   verified: true
 * }
 */
export async function processDoorKnockWebhook(
  payload: any,
  requestId: string
): Promise<{ processed: boolean; message: string; usedApi?: boolean }> {
  try {
    // Extract IDs from various possible formats
    const setterUserId = payload.user?.id || payload.user_id || payload.setter_id || payload.setterId;
    const repcardCustomerId = payload.contact?.id || payload.contact_id || payload.customer_id || payload.contactId || payload.customerId;
    const doorKnockedAt = payload.door_knocked_at || payload.doorKnockedAt || payload.door_knocked_at || payload.timestamp || payload.created_at || new Date().toISOString();
    const status = payload.status || payload.door_knock_status || null;
    const contactDistance = payload.contact_distance || payload.contactDistance || null;
    const verified = payload.verified || payload.is_verified || false;
    const latitude = payload.latitude || payload.contact?.latitude || null;
    const longitude = payload.longitude || payload.contact?.longitude || null;

    if (!setterUserId) {
      logError('[RepCard Webhook Processor] Missing setter user ID in door knock payload', new Error('Invalid payload'), { requestId, payload });
      return { processed: false, message: 'Missing setter user ID' };
    }

    logInfo('[RepCard Webhook Processor] Processing door knock from payload', {
      requestId,
      setterUserId,
      repcardCustomerId,
      doorKnockedAt
    });

    // Get office_id from setter
    let officeId: number | null = null;
    if (setterUserId) {
      const setterOfficeResult = await sql`
        SELECT office_id FROM repcard_users 
        WHERE repcard_user_id = ${setterUserId.toString()}::text 
        LIMIT 1
      `;
      const setterOfficeRows = setterOfficeResult.rows || setterOfficeResult;
      if (setterOfficeRows.length > 0) {
        officeId = setterOfficeRows[0].office_id;
      }
    }

    // Get customer_id from repcard_customer_id if available
    let customerId: string | null = null;
    if (repcardCustomerId) {
      const customerResult = await sql`
        SELECT id FROM repcard_customers
        WHERE repcard_customer_id = ${repcardCustomerId.toString()}::text
        LIMIT 1
      `;
      const customerRows = customerResult.rows || customerResult;
      if (customerRows.length > 0) {
        customerId = customerRows[0].id;
      }
    }

    // Generate unique door knock ID (use timestamp + setter + customer if available)
    const doorKnockId = `${setterUserId}_${repcardCustomerId || 'unknown'}_${new Date(doorKnockedAt).getTime()}`;

    // Insert door knock event
    const result = await sql`
      INSERT INTO repcard_door_knocks (
        repcard_door_knock_id,
        setter_user_id,
        repcard_customer_id,
        customer_id,
        office_id,
        door_knocked_at,
        status,
        contact_distance,
        latitude,
        longitude,
        verified,
        created_at,
        updated_at,
        raw_data
      )
      VALUES (
        ${doorKnockId},
        ${setterUserId.toString()}::text,
        ${repcardCustomerId ? repcardCustomerId.toString() : null}::text,
        ${customerId},
        ${officeId},
        ${new Date(doorKnockedAt).toISOString()},
        ${status},
        ${contactDistance ? parseFloat(contactDistance.toString()) : null},
        ${latitude ? parseFloat(latitude.toString()) : null},
        ${longitude ? parseFloat(longitude.toString()) : null},
        ${verified},
        ${new Date(doorKnockedAt).toISOString()},
        ${new Date().toISOString()},
        ${JSON.stringify(payload)}
      )
      ON CONFLICT (repcard_door_knock_id)
      DO UPDATE SET
        repcard_customer_id = EXCLUDED.repcard_customer_id,
        customer_id = EXCLUDED.customer_id,
        office_id = COALESCE(EXCLUDED.office_id, repcard_door_knocks.office_id),
        door_knocked_at = EXCLUDED.door_knocked_at,
        status = EXCLUDED.status,
        contact_distance = EXCLUDED.contact_distance,
        latitude = EXCLUDED.latitude,
        longitude = EXCLUDED.longitude,
        verified = EXCLUDED.verified,
        updated_at = EXCLUDED.updated_at,
        raw_data = EXCLUDED.raw_data,
        synced_at = NOW()
      RETURNING (xmax = 0) AS inserted
    `;

    const row = result.rows?.[0] || result[0];
    const wasInserted = row?.inserted;

    logInfo('[RepCard Webhook Processor] Door knock processed', {
      requestId,
      doorKnockId,
      setterUserId,
      wasInserted: !!wasInserted
    });

    return {
      processed: true,
      message: `Door knock ${doorKnockId} ${wasInserted ? 'inserted' : 'updated'} from webhook payload`,
      usedApi: false
    };

  } catch (error) {
    logError('[RepCard Webhook Processor] Failed to process door knock from payload', error as Error, { requestId });
    
    // Don't fail the webhook - log and return success
    return {
      processed: false,
      message: `Door knock processing failed: ${error instanceof Error ? error.message : 'Unknown error'}`,
      usedApi: false
    };
  }
}
