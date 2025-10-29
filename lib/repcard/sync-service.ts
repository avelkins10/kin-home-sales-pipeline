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
    let startDate = options.startDate;
    if (options.incremental && !startDate) {
      const lastSync = await getLastSyncTimestamp('customers');
      if (lastSync) {
        startDate = lastSync.toISOString().split('T')[0];
        console.log(`[RepCard Sync] Incremental sync from ${startDate}`);
      }
    }

    // Fetch all customers with pagination
    let page = 1;
    let hasMore = true;
    const MAX_PAGES = 200; // Safety limit (20,000 customers max)

    while (hasMore && page <= MAX_PAGES) {
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
          throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
        }

        const customers = Array.isArray(response.result.data) ? response.result.data : [];
        if (customers.length === 0 && page === 1) {
          console.log(`[RepCard Sync] No customers found - API may have returned different structure`);
          console.log(`[RepCard Sync] Response structure:`, JSON.stringify(response.result, null, 2).substring(0, 500));
        }
        console.log(`[RepCard Sync] Page ${page}: Got ${customers.length} customers`);

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
          const existingUsers = await sql`
            SELECT repcard_user_id FROM users 
            WHERE repcard_user_id = ANY(${Array.from(setterUserIds).map(String)}::text[])
          `;
          const existingIds = new Set(existingUsers.map((u: any) => u.repcard_user_id));
          
          // Only enrich users that don't exist yet (batch API calls with delay)
          const idsToEnrich = Array.from(setterUserIds).filter(id => !existingIds.has(id.toString()));
          
          // Limit enrichment to avoid timeout (max 10 per page)
          const idsToEnrichLimited = idsToEnrich.slice(0, 10);
          
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
        }

        // Process each customer
        for (const customer of customers) {
          try {
            // Extract setter_user_id from customer data (RepCard user ID)
            const setterUserId = (customer as any).userId || customer.assignedUserId || (customer as any).setterUserId || (customer as any).setter_user_id;

            // Track latest updated_at for incremental sync
            const updatedAt = new Date(customer.updatedAt);
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Upsert customer into database
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
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${customer.id},
                ${setterUserId},
                ${null}, -- office_id not available in customer record
                ${`${customer.firstName} ${customer.lastName}`},
                ${customer.email || null},
                ${customer.phone || null},
                ${customer.address || null},
                ${customer.city || null},
                ${customer.state || null},
                ${customer.zipCode || null},
                ${customer.statusId?.toString() || null},
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
    const MAX_PAGES = 200;

    while (hasMore && page <= MAX_PAGES) {
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
          const existingUsers = await sql`
            SELECT repcard_user_id FROM users 
            WHERE repcard_user_id = ANY(${Array.from(userIds).map(String)}::text[])
          `;
          const existingIds = new Set(existingUsers.map((u: any) => u.repcard_user_id));
          
          // Only enrich users that don't exist yet (limit to avoid timeout)
          const idsToEnrich = Array.from(userIds).filter(id => !existingIds.has(id.toString())).slice(0, 10);
          
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

            // Get customer_id from our database
            const customerResult = await sql`
              SELECT id FROM repcard_customers
              WHERE repcard_customer_id = ${appointment.contact.id}
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
            // Use safe optional chaining for userId and closerId
            const officeResult = await sql`
              SELECT 
                COALESCE(
                  (SELECT office_id FROM repcard_customers WHERE repcard_customer_id = ${appointment.contact.id} LIMIT 1),
                  ${appointment.userId ? sql`(SELECT office_id FROM repcard_users WHERE repcard_user_id = ${appointment.userId} LIMIT 1)` : sql`NULL`},
                  ${appointment.closerId ? sql`(SELECT office_id FROM repcard_users WHERE repcard_user_id = ${appointment.closerId} LIMIT 1)` : sql`NULL`}
                ) as office_id
            `;
            const officeRows = officeResult.rows || officeResult;
            const officeId = officeRows.length > 0 ? officeRows[0].office_id : null;

            // Calculate is_within_48_hours if we have customer data
            let isWithin48Hours = false;
            if (appointment.contact?.id) {
              const customerCreatedResult = await sql`
                SELECT created_at FROM repcard_customers 
                WHERE repcard_customer_id = ${appointment.contact.id} 
                LIMIT 1
              `;
              const customerCreatedRows = customerCreatedResult.rows || customerCreatedResult;
              if (customerCreatedRows.length > 0 && customerCreatedRows[0].created_at) {
                const customerCreated = new Date(customerCreatedRows[0].created_at);
                const appointmentCreated = new Date(appointment.createdAt);
                const diffHours = (appointmentCreated.getTime() - customerCreated.getTime()) / (1000 * 60 * 60);
                isWithin48Hours = diffHours >= 0 && diffHours <= 48;
              }
            }

            // Upsert appointment with ALL fields
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
                is_within_48_hours,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${appointment.id},
                ${customerId},
                ${appointment.contact.id},
                ${appointment.userId || null},
                ${appointment.closerId || null},
                ${officeId},
                ${disposition},
                ${statusCategory},
                ${appointment.startAt || null},
                ${appointment.endAt || null},
                ${appointment.durationTime || null},
                ${appointment.notes || null},
                ${isWithin48Hours},
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
                is_within_48_hours = EXCLUDED.is_within_48_hours,
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
              WHERE repcard_customer_id = ${log.customerId}
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
                ${log._id},
                ${customerId},
                ${log.customerId},
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
