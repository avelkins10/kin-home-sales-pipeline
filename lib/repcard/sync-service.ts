/**
 * RepCard Sync Service
 *
 * Fetches data from RepCard API and stores it in local database for fast analytics.
 * Supports both full sync (initial load) and incremental sync (updates only).
 */

import { sql } from '@/lib/db/client';
import { repcardClient } from './client';
import type { RepCardCustomer, RepCardAppointment, RepCardCustomerStatusLog } from './types';
import { enrichUserFromRepCard } from '@/lib/users/enrich-user';
import { parseRepCardDateTime } from '@/lib/utils/repcard-date-parser';

export interface SyncResult {
  entityType: 'customers' | 'appointments' | 'status_logs';
  syncType: 'full' | 'incremental';
  recordsFetched: number;
  recordsInserted: number;
  recordsUpdated: number;
  recordsFailed: number;
  duration: number;
  error?: string;
}

/**
 * Get the last successful sync timestamp for an entity type
 */
async function getLastSyncTimestamp(entityType: string): Promise<Date | null> {
  try {
    const result = await sql`
      SELECT last_record_date
      FROM repcard_sync_log
      WHERE entity_type = ${entityType}
        AND status = 'completed'
        AND last_record_date IS NOT NULL
      ORDER BY completed_at DESC
      LIMIT 1
    `;

    const rows = result.rows || result;
    if (rows.length > 0 && rows[0].last_record_date) {
      return new Date(rows[0].last_record_date);
    }
    return null;
  } catch (error) {
    console.error(`[RepCard Sync] Error getting last sync timestamp for ${entityType}:`, error);
    return null;
  }
}

/**
 * Create a sync log entry
 */
async function createSyncLog(entityType: string, syncType: 'full' | 'incremental'): Promise<string> {
  const result = await sql`
    INSERT INTO repcard_sync_log (entity_type, sync_type, status, started_at)
    VALUES (${entityType}, ${syncType}, 'running', NOW())
    RETURNING id
  `;

  // @vercel/postgres returns rows in a rows array
  const row = result.rows?.[0] || result[0];
  if (!row || !row.id) {
    throw new Error('Failed to create sync log entry - no ID returned');
  }
  return row.id;
}

/**
 * Update sync log when completed
 */
async function completeSyncLog(
  syncLogId: string,
  status: 'completed' | 'failed',
  stats: {
    recordsFetched: number;
    recordsInserted: number;
    recordsUpdated: number;
    recordsFailed: number;
    lastRecordDate?: Date;
    error?: string;
  }
) {
  await sql`
    UPDATE repcard_sync_log
    SET
      status = ${status},
      completed_at = NOW(),
      records_fetched = ${stats.recordsFetched},
      records_inserted = ${stats.recordsInserted},
      records_updated = ${stats.recordsUpdated},
      records_failed = ${stats.recordsFailed},
      last_record_date = ${stats.lastRecordDate || null},
      error_message = ${stats.error || null}
    WHERE id = ${syncLogId}
  `;
}

/**
 * Sync customers from RepCard
 * @param startDate Optional start date for filtering (YYYY-MM-DD)
 * @param endDate Optional end date for filtering (YYYY-MM-DD)
 * @param incremental If true, only fetch records updated since last sync
 */
export async function syncCustomers(options: {
  startDate?: string;
  endDate?: string;
  incremental?: boolean;
} = {}): Promise<SyncResult> {
  const startTime = Date.now();
  const syncType = options.incremental ? 'incremental' : 'full';
  const syncLogId = await createSyncLog('customers', syncType);

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;
  let lastRecordDate: Date | undefined;

  try {
    console.log(`[RepCard Sync] Starting ${syncType} sync for customers...`);

    // For incremental sync, get the last sync timestamp
    // IMPORTANT: If incremental=false (full sync), don't use date filters to get ALL customers
    let startDate = options.startDate;
    if (options.incremental && !startDate) {
      const lastSync = await getLastSyncTimestamp('customers');
      if (lastSync) {
        startDate = lastSync.toISOString().split('T')[0];
        console.log(`[RepCard Sync] Incremental sync from ${startDate}`);
      }
    } else if (!options.incremental) {
      // Full sync - don't filter by date to get all customers
      startDate = undefined;
      console.log(`[RepCard Sync] Full sync - fetching all customers (no date filter)`);
    }

    // Fetch all customers with pagination
    let page = 1;
    let hasMore = true;
    // Increased limit for full sync to get all customers (not just recent ones)
    const MAX_PAGES = options.incremental ? 10 : 100; // Full sync: up to 10,000 customers
    const MAX_DURATION_MS = 240000; // 4 minutes (leave 1 min buffer before 5 min timeout)
    const syncStartTime = Date.now();

    while (hasMore && page <= MAX_PAGES) {
      // Check timeout - exit gracefully before hitting 5 min limit
      const elapsed = Date.now() - syncStartTime;
      if (elapsed > MAX_DURATION_MS) {
        console.log(`[RepCard Sync] ⏱️ Timeout protection: Stopping customers sync after ${(elapsed / 1000).toFixed(1)}s (page ${page})`);
        break;
      }
      try {
        const response = await repcardClient.getCustomers({
          page,
          perPage: 100,
          startDate,
          endDate: options.endDate
        });

        // Validate response structure
        if (!response || !response.result) {
          console.error(`[RepCard Sync] Invalid response structure on page ${page}:`, response);
          console.error(`[RepCard Sync] Full response:`, JSON.stringify(response, null, 2).substring(0, 1000));
          throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
        }

        const customers = Array.isArray(response.result.data) ? response.result.data : [];
        if (customers.length === 0 && page === 1) {
          console.log(`[RepCard Sync] ⚠️ No customers found on page 1`);
          console.log(`[RepCard Sync] Response structure:`, JSON.stringify(response.result, null, 2).substring(0, 1000));
          console.log(`[RepCard Sync] Total in response:`, response.result.total);
          console.log(`[RepCard Sync] Current page:`, response.result.currentPage);
          console.log(`[RepCard Sync] Last page:`, response.result.lastPage);
          console.log(`[RepCard Sync] Date filters:`, { startDate, endDate: options.endDate });
          
          // If total is 0, the API really has no customers (or date filter is too restrictive)
          if (response.result.total === 0) {
            console.log(`[RepCard Sync] ⚠️ API returned 0 total customers. This might indicate:`);
            console.log(`[RepCard Sync]   1. Date filter is excluding all customers`);
            console.log(`[RepCard Sync]   2. API key doesn't have access`);
            console.log(`[RepCard Sync]   3. No customers exist in RepCard`);
            // Don't throw - continue to see if we can get any data
          }
        }
        console.log(`[RepCard Sync] Page ${page}: Got ${customers.length} customers (total: ${response.result.total || 'unknown'})`);

        // Batch enrich users - collect all unique setter_user_ids first
        const setterUserIds = new Set<number>();
        for (const customer of customers) {
          const setterUserId = (customer as any).userId || customer.assignedUserId || (customer as any).setterUserId || (customer as any).setter_user_id;
          if (setterUserId) {
            setterUserIds.add(setterUserId);
          }
        }

        // Batch check which users already exist
        if (setterUserIds.size > 0) {
          try {
            const existingUsersResult = await sql`
              SELECT repcard_user_id FROM users 
              WHERE repcard_user_id = ANY(${Array.from(setterUserIds)}::int[])
            `;
            // Handle @vercel/postgres result format
            const existingUsers = Array.isArray(existingUsersResult) 
              ? existingUsersResult 
              : (existingUsersResult.rows || []);
            const existingIds = new Set(existingUsers.map((u: any) => u?.repcard_user_id).filter(Boolean));
            
            // Only enrich users that don't exist yet (batch API calls with delay)
            const idsToEnrich = Array.from(setterUserIds).filter(id => !existingIds.has(id.toString()));
            
            // Limit enrichment to avoid timeout (max 5 per page - reduced from 10)
            const idsToEnrichLimited = idsToEnrich.slice(0, 5);
            
            for (const userId of idsToEnrichLimited) {
              try {
                const repcardUserResponse = await repcardClient.getUserDetails(userId);
                const repcardUser = repcardUserResponse.result;
                
                if (repcardUser.email) {
                  await enrichUserFromRepCard(userId, {
                    email: repcardUser.email,
                    firstName: repcardUser.firstName,
                    lastName: repcardUser.lastName,
                    officeName: repcardUser.office,
                    teamName: repcardUser.team,
                    profileImage: repcardUser.image
                  });
                }
                
                // Small delay to avoid rate limits
                await new Promise(resolve => setTimeout(resolve, 100));
              } catch (repcardError) {
                console.warn(`[RepCard Sync] Could not enrich user ${userId}:`, repcardError);
              }
            }
          } catch (userCheckError) {
            console.error(`[RepCard Sync] Error checking existing users for customers:`, userCheckError);
            // Continue processing customers even if user check fails
          }
        }

        // Process each customer
        for (const customer of customers) {
          try {
            // Extract setter_user_id from customer data (RepCard user ID)
            // API provides: userId (setter who created the customer) and ownerId (same as userId)
            const setterUserId = (customer as any).userId || (customer as any).ownerId || customer.assignedUserId || (customer as any).setterUserId || (customer as any).setter_user_id;

            // Track latest updated_at for incremental sync
            const updatedAt = new Date(customer.updatedAt);
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Upsert customer into database
            // Use fullName if available, otherwise construct from firstName + lastName
            const customerName = (customer as any).fullName || 
                                 `${customer.firstName || ''} ${customer.lastName || ''}`.trim() || 
                                 null;
            
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
                ${customer.id.toString()}::text,
                ${setterUserId},
                ${null}, -- office_id not available in customer record
                ${customerName},
                ${customer.email || null},
                ${customer.phone || (customer as any).phoneNumber || null},
                ${customer.address || null},
                ${customer.city || null},
                ${customer.state || null},
                ${customer.zipCode || (customer as any).zip || null},
                ${customer.statusId?.toString() || null},
                ${(customer as any).contactSource || null},
                ${(customer as any).latitude ? parseFloat((customer as any).latitude.toString()) : null},
                ${(customer as any).longitude ? parseFloat((customer as any).longitude.toString()) : null},
                ${customer.createdAt},
                ${customer.updatedAt},
                ${JSON.stringify(customer)}
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
                -- CRITICAL: Update created_at if it's NULL (backfill missing data)
                created_at = COALESCE(repcard_customers.created_at, EXCLUDED.created_at),
                updated_at = EXCLUDED.updated_at,
                raw_data = EXCLUDED.raw_data,
                synced_at = NOW()
              RETURNING (xmax = 0) AS inserted
            `;

            const row = result.rows?.[0] || result[0];
            if (row?.inserted) {
              recordsInserted++;
            } else {
              recordsUpdated++;
            }
            recordsFetched++;

            // Extract door knocks from customer data and sync them
            // Customer payload has doorKnocks and verifiedDoorKnocks arrays
            const doorKnocks = (customer as any).doorKnocks || [];
            const verifiedDoorKnocks = (customer as any).verifiedDoorKnocks || [];
            
            // Process verified door knocks (preferred) or regular door knocks
            const knocksToProcess = verifiedDoorKnocks.length > 0 ? verifiedDoorKnocks : doorKnocks;
            
            for (const doorKnock of knocksToProcess) {
              try {
                // Extract door knock data
                const doorKnockedAt = doorKnock.door_knocked_at || doorKnock.doorKnockedAt || customer.createdAt;
                const status = doorKnock.status || null;
                const contactDistance = doorKnock.contact_distance || doorKnock.contactDistance || null;
                const verified = verifiedDoorKnocks.length > 0 && verifiedDoorKnocks.includes(doorKnock);
                
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
                
                // Get customer_id from database
                const customerIdResult = await sql`
                  SELECT id FROM repcard_customers
                  WHERE repcard_customer_id = ${customer.id.toString()}::text
                  LIMIT 1
                `;
                const customerIdRows = customerIdResult.rows || customerIdResult;
                const customerId = customerIdRows.length > 0 ? customerIdRows[0].id : null;
                
                // Generate unique door knock ID
                const doorKnockId = `${setterUserId}_${customer.id}_${new Date(doorKnockedAt).getTime()}`;
                
                // Insert door knock
                await sql`
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
                    ${setterUserId ? setterUserId.toString() : null}::text,
                    ${customer.id.toString()}::text,
                    ${customerId},
                    ${officeId},
                    ${new Date(doorKnockedAt).toISOString()},
                    ${status},
                    ${contactDistance ? parseFloat(contactDistance.toString()) : null},
                    ${(customer as any).latitude ? parseFloat((customer as any).latitude.toString()) : null},
                    ${(customer as any).longitude ? parseFloat((customer as any).longitude.toString()) : null},
                    ${verified},
                    ${new Date(doorKnockedAt).toISOString()},
                    ${new Date().toISOString()},
                    ${JSON.stringify(doorKnock)}
                  )
                  ON CONFLICT (repcard_door_knock_id)
                  DO UPDATE SET
                    repcard_customer_id = EXCLUDED.repcard_customer_id,
                    customer_id = EXCLUDED.customer_id,
                    office_id = COALESCE(EXCLUDED.office_id, repcard_door_knocks.office_id),
                    door_knocked_at = EXCLUDED.door_knocked_at,
                    status = EXCLUDED.status,
                    contact_distance = EXCLUDED.contact_distance,
                    verified = EXCLUDED.verified,
                    updated_at = EXCLUDED.updated_at,
                    raw_data = EXCLUDED.raw_data,
                    synced_at = NOW()
                `;
              } catch (doorKnockError) {
                // Log but don't fail customer sync if door knock fails
                console.warn(`[RepCard Sync] Failed to sync door knock for customer ${customer.id}:`, doorKnockError);
              }
            }

          } catch (error) {
            console.error(`[RepCard Sync] Failed to process customer ${customer.id}:`, error);
            recordsFailed++;
          }
        }

        hasMore = response.result.currentPage < response.result.lastPage;
        page++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to fetch customers page ${page}:`, error);
        throw error; // Re-throw to mark sync as failed
      }
    }

    console.log(`[RepCard Sync] Customers sync completed: ${recordsFetched} fetched, ${recordsInserted} inserted, ${recordsUpdated} updated`);

    await completeSyncLog(syncLogId, 'completed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      lastRecordDate
    });

    return {
      entityType: 'customers',
      syncType,
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime
    };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    console.error(`[RepCard Sync] Customers sync failed:`, error);

    await completeSyncLog(syncLogId, 'failed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      error: errorMessage
    });

    return {
      entityType: 'customers',
      syncType,
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime,
      error: errorMessage
    };
  }
}

/**
 * Sync appointments from RepCard and link to customers
 */
export async function syncAppointments(options: {
  fromDate?: string;
  toDate?: string;
  incremental?: boolean;
} = {}): Promise<SyncResult> {
  const startTime = Date.now();
  const syncType = options.incremental ? 'incremental' : 'full';
  const syncLogId = await createSyncLog('appointments', syncType);

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;
  let lastRecordDate: Date | undefined;

  try {
    console.log(`[RepCard Sync] Starting ${syncType} sync for appointments...`);

    // For incremental sync, calculate date range
    let fromDate = options.fromDate;
    if (options.incremental && !fromDate) {
      const lastSync = await getLastSyncTimestamp('appointments');
      if (lastSync) {
        fromDate = lastSync.toISOString().split('T')[0];
        console.log(`[RepCard Sync] Incremental sync from ${fromDate}`);
      }
    }

    // Fetch all appointments with pagination
    let page = 1;
    let hasMore = true;
    const MAX_PAGES = 10; // Reduced limit: ~1000 appointments per sync to avoid timeout
    const MAX_DURATION_MS = 240000; // 4 minutes (leave 1 min buffer before 5 min timeout)
    const syncStartTime = Date.now();

    while (hasMore && page <= MAX_PAGES) {
      // Check timeout - exit gracefully before hitting 5 min limit
      const elapsed = Date.now() - syncStartTime;
      if (elapsed > MAX_DURATION_MS) {
        console.log(`[RepCard Sync] ⏱️ Timeout protection: Stopping appointments sync after ${(elapsed / 1000).toFixed(1)}s (page ${page})`);
        break;
      }
      try {
        const response = await repcardClient.getAppointments({
          page,
          perPage: 100,
          fromDate,
          toDate: options.toDate
        });

        // Validate response structure
        if (!response || !response.result) {
          console.error(`[RepCard Sync] Invalid response structure on page ${page}:`, response);
          throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
        }

        const appointments = Array.isArray(response.result.data) ? response.result.data : [];
        if (appointments.length === 0 && page === 1) {
          console.log(`[RepCard Sync] No appointments found - API may have returned different structure`);
          console.log(`[RepCard Sync] Response structure:`, JSON.stringify(response.result, null, 2).substring(0, 500));
        }
        console.log(`[RepCard Sync] Page ${page}: Got ${appointments.length} appointments`);

        // Batch enrich users - collect all unique user IDs first
        const userIds = new Set<number>();
        for (const appointment of appointments) {
          if (appointment.userId) userIds.add(appointment.userId);
          if (appointment.closerId) userIds.add(appointment.closerId);
        }

        // Batch check which users already exist
        if (userIds.size > 0) {
          try {
            const existingUsersResult = await sql`
              SELECT repcard_user_id FROM users 
              WHERE repcard_user_id = ANY(${Array.from(userIds)}::int[])
            `;
            // Handle @vercel/postgres result format
            const existingUsers = Array.isArray(existingUsersResult) 
              ? existingUsersResult 
              : (existingUsersResult.rows || []);
            const existingIds = new Set(existingUsers.map((u: any) => u?.repcard_user_id).filter(Boolean));
            
            // Only enrich users that don't exist yet (limit to avoid timeout - max 5 per page)
            const idsToEnrich = Array.from(userIds).filter(id => !existingIds.has(id.toString())).slice(0, 5);
            
            for (const userId of idsToEnrich) {
              try {
                const repcardUserResponse = await repcardClient.getUserDetails(userId);
                const repcardUser = repcardUserResponse.result;
                
                if (repcardUser.email) {
                  await enrichUserFromRepCard(userId, {
                    email: repcardUser.email,
                    firstName: repcardUser.firstName,
                    lastName: repcardUser.lastName,
                    officeName: repcardUser.office,
                    teamName: repcardUser.team,
                    profileImage: repcardUser.image
                  });
                }
                
                // Small delay to avoid rate limits
                await new Promise(resolve => setTimeout(resolve, 100));
              } catch (repcardError) {
                console.warn(`[RepCard Sync] Could not enrich user ${userId}:`, repcardError);
              }
            }
          } catch (userCheckError) {
            console.error(`[RepCard Sync] Error checking existing users for appointments:`, userCheckError);
            // Continue processing appointments even if user check fails
          }
        }

        // Process each appointment
        for (const appointment of appointments) {
          try {
            // Validate appointment structure
            if (!appointment || !appointment.id) {
              console.warn(`[RepCard Sync] Skipping invalid appointment (missing id):`, appointment);
              recordsFailed++;
              continue;
            }

            if (!appointment.contact || !appointment.contact.id) {
              console.warn(`[RepCard Sync] Skipping appointment ${appointment.id} - missing contact information:`, {
                appointmentId: appointment.id,
                hasContact: !!appointment.contact,
                contactId: appointment.contact?.id
              });
              recordsFailed++;
              continue;
            }

            const updatedAt = new Date(appointment.updatedAt);
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Get customer_id from our database (batch this query if processing multiple appointments)
            // For now, keep individual query but optimize later if needed
            const customerResult = await sql`
              SELECT id FROM repcard_customers
              WHERE repcard_customer_id = ${appointment.contact.id}::text
              LIMIT 1
            `;

            const customerRows = customerResult.rows || customerResult;
            const customerId = customerRows.length > 0 ? customerRows[0].id : null;

            // Extract disposition from status (capture all possible status fields)
            const disposition = appointment.status?.title || appointment.status?.category?.title || null;
            
            // Determine status category from disposition
            let statusCategory: string | null = null;
            if (disposition) {
              const dispLower = disposition.toLowerCase();
              if (dispLower.includes('cancel')) statusCategory = 'cancelled';
              else if (dispLower.includes('reschedule')) statusCategory = 'rescheduled';
              else if (dispLower.includes('no.show') || dispLower.includes('no_show')) statusCategory = 'no_show';
              else if (dispLower.includes('sat.closed') || dispLower.includes('sat_closed') || dispLower.includes('closed')) statusCategory = 'sat_closed';
              else if (dispLower.includes('sat.no.close') || dispLower.includes('sat_no_close')) statusCategory = 'sat_no_close';
              else if (appointment.endAt) statusCategory = 'completed';
              else if (appointment.startAt) statusCategory = 'scheduled';
            }

            // Extract user IDs (RepCard user IDs)
            const setterUserId = appointment.userId; // setter (who created the customer/appointment)
            const closerUserId = appointment.closerId; // closer (who runs the appointment)

            // Get office_id from customer, setter, or closer (order of preference)
            // Handle optional userId and closerId safely
            let officeId: number | null = null;
            
            // Try customer first
            const customerOfficeResult = await sql`
              SELECT office_id FROM repcard_customers 
              WHERE repcard_customer_id = ${appointment.contact.id}::text
              LIMIT 1
            `;
            const customerOfficeRows = customerOfficeResult.rows || customerOfficeResult;
            if (customerOfficeRows.length > 0 && customerOfficeRows[0].office_id) {
              officeId = customerOfficeRows[0].office_id;
            }
            
            // Try setter if no customer office
            if (!officeId && appointment.userId) {
              const setterOfficeResult = await sql`
                SELECT office_id FROM repcard_users 
                WHERE repcard_user_id = ${appointment.userId?.toString()}::text 
                LIMIT 1
              `;
              const setterOfficeRows = setterOfficeResult.rows || setterOfficeResult;
              if (setterOfficeRows.length > 0 && setterOfficeRows[0].office_id) {
                officeId = setterOfficeRows[0].office_id;
              }
            }
            
            // Try closer if still no office
            if (!officeId && appointment.closerId) {
              const closerOfficeResult = await sql`
                SELECT office_id FROM repcard_users 
                WHERE repcard_user_id = ${appointment.closerId?.toString()}::text 
                LIMIT 1
              `;
              const closerOfficeRows = closerOfficeResult.rows || closerOfficeResult;
              if (closerOfficeRows.length > 0 && closerOfficeRows[0].office_id) {
                officeId = closerOfficeRows[0].office_id;
              }
            }

            // Calculate is_within_48_hours if we have customer data
            // NOTE: Changed to "within 2 calendar days" instead of strict 48 hours
            // Counts: same day (0), next day (1), or day after next (2)
            // This is more business-appropriate (e.g., Friday morning -> Sunday night = within 2 days)
            let isWithin48Hours = false;
            if (appointment.contact?.id && customerId && appointment.startAt) {
              // Use customerId if available, otherwise query by repcard_customer_id
              const customerCreatedResult = customerId 
                ? await sql`SELECT created_at FROM repcard_customers WHERE id = ${customerId} LIMIT 1`
                : await sql`SELECT created_at FROM repcard_customers WHERE repcard_customer_id = ${appointment.contact.id}::text LIMIT 1`;
              const customerCreatedRows = customerCreatedResult.rows || customerCreatedResult;
              if (customerCreatedRows.length > 0 && customerCreatedRows[0].created_at) {
                const customerCreated = new Date(customerCreatedRows[0].created_at);
                // Parse startAt with timezone if available
                const appointmentScheduledStr = appointment.startAt && (appointment as any).startAtTimezone
                  ? parseRepCardDateTime(appointment.startAt, (appointment as any).startAtTimezone)
                  : appointment.startAt;
                const appointmentScheduled = appointmentScheduledStr ? new Date(appointmentScheduledStr) : null;
                if (appointmentScheduled) {
                  // Convert both to Eastern Time and compare calendar days
                  // Using a helper to get Eastern date (handles DST automatically)
                  const toEasternDate = (date: Date): string => {
                    return new Intl.DateTimeFormat('en-US', {
                      timeZone: 'America/New_York',
                      year: 'numeric',
                      month: '2-digit',
                      day: '2-digit'
                    }).format(date);
                  };
                  
                  const customerDate = toEasternDate(customerCreated);
                  const scheduledDate = toEasternDate(appointmentScheduled);
                  
                  // Calculate difference in calendar days
                  const customerDateObj = new Date(customerDate + 'T00:00:00');
                  const scheduledDateObj = new Date(scheduledDate + 'T00:00:00');
                  const diffDays = Math.floor((scheduledDateObj.getTime() - customerDateObj.getTime()) / (1000 * 60 * 60 * 24));
                  
                  // Within 2 calendar days: same day (0), next day (1), or day after next (2)
                  isWithin48Hours = diffDays >= 0 && diffDays <= 2;
                }
              }
            }

            // Calculate has_power_bill: ANY attachment = power bill (simplified for now)
            let hasPowerBill = false;
            if (appointment.contact?.id) {
              // Check for customer attachments
              const customerAttachmentsResult = customerId
                ? await sql`SELECT COUNT(*)::int as count FROM repcard_customer_attachments WHERE customer_id = ${customerId} LIMIT 1`
                : await sql`SELECT COUNT(*)::int as count FROM repcard_customer_attachments WHERE repcard_customer_id = ${appointment.contact.id}::text LIMIT 1`;
              const customerAttRows = customerAttachmentsResult.rows || customerAttachmentsResult;
              const customerAttCount = customerAttRows.length > 0 ? (customerAttRows[0]?.count || 0) : 0;
              
              // Check for appointment attachments
              const appointmentAttachmentsResult = await sql`
                SELECT COUNT(*)::int as count 
                FROM repcard_appointment_attachments 
                WHERE repcard_appointment_id = ${appointment.id}::text 
                LIMIT 1
              `;
              const appointmentAttRows = appointmentAttachmentsResult.rows || appointmentAttachmentsResult;
              const appointmentAttCount = appointmentAttRows.length > 0 ? (appointmentAttRows[0]?.count || 0) : 0;
              
              hasPowerBill = customerAttCount > 0 || appointmentAttCount > 0;
            }

            // Determine reschedule status before inserting
            // Check if there are existing appointments for this customer
            let isReschedule = false;
            let rescheduleCount = 0;
            let originalAppointmentId: string | null = null;

            if (appointment.contact?.id) {
              const existingAppointmentsResult = await sql`
                SELECT
                  id,
                  repcard_appointment_id,
                  scheduled_at,
                  created_at,
                  is_reschedule,
                  original_appointment_id
                FROM repcard_appointments
                WHERE repcard_customer_id = ${appointment.contact.id}::text
                  AND repcard_appointment_id != ${appointment.id}::text
                ORDER BY COALESCE(scheduled_at, created_at) ASC
              `;
              const existingAppointments = existingAppointmentsResult.rows || existingAppointmentsResult;

              if (existingAppointments.length > 0) {
                // This is a reschedule
                isReschedule = true;
                rescheduleCount = existingAppointments.length;

                // Link to original appointment (first one for this customer)
                const firstAppointment = existingAppointments[0];
                originalAppointmentId = firstAppointment.original_appointment_id || firstAppointment.id;
              }
            }

            // Upsert appointment with ALL fields including reschedule tracking
            // Ensure all required fields are present
            if (!appointment.createdAt || !appointment.updatedAt) {
              console.warn(`[RepCard Sync] Appointment ${appointment.id} missing required timestamps, skipping`);
              recordsFailed++;
              continue;
            }

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
                appointment_location,
                latitude,
                longitude,
                contact_source,
                is_within_48_hours,
                has_power_bill,
                is_reschedule,
                reschedule_count,
                original_appointment_id,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${appointment.id.toString()}::text,
                ${customerId},
                ${appointment.contact.id.toString()}::text,
                ${appointment.userId ? appointment.userId.toString() : null}::text,
                ${appointment.closerId ? appointment.closerId.toString() : null}::text,
                ${officeId},
                ${disposition},
                ${statusCategory},
                ${appointment.startAt && (appointment as any).startAtTimezone 
                  ? parseRepCardDateTime(appointment.startAt, (appointment as any).startAtTimezone) 
                  : appointment.startAt || null},
                ${appointment.endAt || null},
                ${appointment.durationTime || null},
                ${appointment.notes || null},
                ${(appointment as any).appointmentLocation || null},
                ${(appointment as any).latitude ? parseFloat((appointment as any).latitude.toString()) : null},
                ${(appointment as any).longitude ? parseFloat((appointment as any).longitude.toString()) : null},
                ${appointment.contact?.contactSource || (appointment as any).contactSource || null},
                ${isWithin48Hours},
                ${hasPowerBill},
                ${isReschedule},
                ${rescheduleCount},
                ${originalAppointmentId},
                ${appointment.createdAt},
                ${appointment.updatedAt},
                ${JSON.stringify(appointment)}
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
                appointment_location = EXCLUDED.appointment_location,
                latitude = EXCLUDED.latitude,
                longitude = EXCLUDED.longitude,
                contact_source = EXCLUDED.contact_source,
                -- EVENT-DRIVEN: Metrics are recalculated by triggers when scheduled_at or repcard_customer_id changes
                -- Don't set is_within_48_hours or has_power_bill here - triggers handle it automatically
                -- Updating scheduled_at or repcard_customer_id will trigger recalculation
                is_reschedule = EXCLUDED.is_reschedule,
                reschedule_count = EXCLUDED.reschedule_count,
                original_appointment_id = EXCLUDED.original_appointment_id,
                updated_at = EXCLUDED.updated_at,
                raw_data = EXCLUDED.raw_data
              RETURNING (xmax = 0) AS inserted
            `;

            const row = result.rows?.[0] || result[0];
            if (row?.inserted) {
              recordsInserted++;
            } else {
              recordsUpdated++;
            }
            recordsFetched++;

          } catch (error) {
            const errorMsg = error instanceof Error ? error.message : String(error);
            const appointmentId = appointment?.id || 'unknown';
            console.error(`[RepCard Sync] Failed to process appointment ${appointmentId}:`, errorMsg);
            if (error instanceof Error && error.stack) {
              console.error(`[RepCard Sync] Stack trace:`, error.stack.substring(0, 500));
            }
            // Log first few errors in detail for debugging
            if (recordsFailed < 5) {
              console.error(`[RepCard Sync] Appointment data that failed:`, JSON.stringify({
                id: appointment?.id,
                contact: appointment?.contact,
                userId: appointment?.userId,
                closerId: appointment?.closerId,
                createdAt: appointment?.createdAt,
                updatedAt: appointment?.updatedAt
              }, null, 2));
            }
            recordsFailed++;
          }
        }

        hasMore = response.result.currentPage < response.result.totalPages;
        page++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to fetch appointments page ${page}:`, error);
        throw error;
      }
    }

    console.log(`[RepCard Sync] Appointments sync completed: ${recordsFetched} fetched, ${recordsInserted} inserted, ${recordsUpdated} updated`);

    await completeSyncLog(syncLogId, 'completed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      lastRecordDate
    });

    return {
      entityType: 'appointments',
      syncType,
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime
    };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    console.error(`[RepCard Sync] Appointments sync failed:`, error);

    await completeSyncLog(syncLogId, 'failed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      error: errorMessage
    });

    return {
      entityType: 'appointments',
      syncType,
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime,
      error: errorMessage
    };
  }
}

/**
 * Sync customer status logs from RepCard
 */
export async function syncStatusLogs(options: {
  fromDate?: string;
  toDate?: string;
  incremental?: boolean;
} = {}): Promise<SyncResult> {
  const startTime = Date.now();
  const syncType = options.incremental ? 'incremental' : 'full';
  const syncLogId = await createSyncLog('status_logs', syncType);

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;
  let lastRecordDate: Date | undefined;

  try {
    console.log(`[RepCard Sync] Starting ${syncType} sync for status logs...`);

    let fromDate = options.fromDate;
    if (options.incremental && !fromDate) {
      const lastSync = await getLastSyncTimestamp('status_logs');
      if (lastSync) {
        fromDate = lastSync.toISOString().split('T')[0];
        console.log(`[RepCard Sync] Incremental sync from ${fromDate}`);
      }
    }

    let page = 1;
    let hasMore = true;
    const MAX_PAGES = 200;

    while (hasMore && page <= MAX_PAGES) {
      try {
        const response = await repcardClient.getCustomerStatusLogs({
          page,
          perPage: 100,
          fromDate,
          toDate: options.toDate
        });

        // Validate response structure
        if (!response || !response.result) {
          console.error(`[RepCard Sync] Invalid response structure on page ${page}:`, response);
          throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
        }

        const statusLogs = Array.isArray(response.result.data) ? response.result.data : [];
        if (statusLogs.length === 0 && page === 1) {
          console.log(`[RepCard Sync] No status logs found - API may have returned different structure`);
          console.log(`[RepCard Sync] Response structure:`, JSON.stringify(response.result, null, 2).substring(0, 500));
        }
        console.log(`[RepCard Sync] Page ${page}: Got ${statusLogs.length} status logs`);

        for (const log of statusLogs) {
          try {
            const updatedAt = new Date(log.updatedAt);
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Get customer_id from our database
            const customerResult = await sql`
              SELECT id FROM repcard_customers
              WHERE repcard_customer_id = ${log.customerId.toString()}::text
              LIMIT 1
            `;

            const customerRows = customerResult.rows || customerResult;
            const customerId = customerRows.length > 0 ? customerRows[0].id : null;

            const result = await sql`
              INSERT INTO repcard_status_logs (
                repcard_log_id,
                customer_id,
                repcard_customer_id,
                old_status,
                new_status,
                changed_at,
                changed_by_user_id,
                raw_data
              )
              VALUES (
                ${log._id.toString()}::text,
                ${customerId},
                ${log.customerId.toString()}::text,
                ${log.statusFrom?.statusName || null},
                ${log.statusTo.statusName},
                ${log.createdAt},
                ${log.userId},
                ${JSON.stringify(log)}
              )
              ON CONFLICT (repcard_log_id)
              DO UPDATE SET
                customer_id = EXCLUDED.customer_id,
                old_status = EXCLUDED.old_status,
                new_status = EXCLUDED.new_status,
                changed_at = EXCLUDED.changed_at,
                changed_by_user_id = EXCLUDED.changed_by_user_id,
                raw_data = EXCLUDED.raw_data,
                synced_at = NOW()
              RETURNING (xmax = 0) AS inserted
            `;

            const row = result.rows?.[0] || result[0];
            if (row?.inserted) {
              recordsInserted++;
            } else {
              recordsUpdated++;
            }
            recordsFetched++;

          } catch (error) {
            console.error(`[RepCard Sync] Failed to process status log ${log._id}:`, error);
            recordsFailed++;
          }
        }

        hasMore = response.result.currentPage < response.result.totalPages;
        page++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to fetch status logs page ${page}:`, error);
        throw error;
      }
    }

    console.log(`[RepCard Sync] Status logs sync completed: ${recordsFetched} fetched, ${recordsInserted} inserted, ${recordsUpdated} updated`);

    await completeSyncLog(syncLogId, 'completed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      lastRecordDate
    });

    return {
      entityType: 'status_logs',
      syncType,
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime
    };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    console.error(`[RepCard Sync] Status logs sync failed:`, error);

    await completeSyncLog(syncLogId, 'failed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      error: errorMessage
    });

    return {
      entityType: 'status_logs',
      syncType,
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime,
      error: errorMessage
    };
  }
}

/**
 * Run a full sync of all RepCard data
 */
export async function runFullSync(options: {
  startDate?: string;
  endDate?: string;
} = {}): Promise<SyncResult[]> {
  console.log('[RepCard Sync] Starting full sync...');
  const results: SyncResult[] = [];

  // Sync customers first (required for foreign key relationships)
  const customersResult = await syncCustomers({
    startDate: options.startDate,
    endDate: options.endDate,
    incremental: false
  });
  results.push(customersResult);

  // Then sync appointments (references customers)
  const appointmentsResult = await syncAppointments({
    fromDate: options.startDate,
    toDate: options.endDate,
    incremental: false
  });
  results.push(appointmentsResult);

  // Finally sync status logs
  const statusLogsResult = await syncStatusLogs({
    fromDate: options.startDate,
    toDate: options.endDate,
    incremental: false
  });
  results.push(statusLogsResult);

  console.log('[RepCard Sync] Full sync completed:', results);
  return results;
}

/**
 * Run an incremental sync (only updates since last sync)
 */
export async function runIncrementalSync(): Promise<SyncResult[]> {
  console.log('[RepCard Sync] Starting incremental sync...');
  const results: SyncResult[] = [];

  // Sync in same order to maintain foreign key integrity
  const customersResult = await syncCustomers({ incremental: true });
  results.push(customersResult);

  const appointmentsResult = await syncAppointments({ incremental: true });
  results.push(appointmentsResult);

  const statusLogsResult = await syncStatusLogs({ incremental: true });
  results.push(statusLogsResult);

  console.log('[RepCard Sync] Incremental sync completed:', results);
  return results;
}
