/**
 * RepCard Sync Service
 *
 * Fetches data from RepCard API and stores it in local database for fast analytics.
 * Supports both full sync (initial load) and incremental sync (updates only).
 */

import { sql } from '@/lib/db/client';
import { repcardClient } from './client';
import type { RepCardCustomer, RepCardAppointment, RepCardCustomerStatusLog } from './types';

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

    if (result.length > 0 && result[0].last_record_date) {
      return new Date(result[0].last_record_date);
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
  return result[0].id;
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

        const customers = response.result.data;
        console.log(`[RepCard Sync] Page ${page}: Got ${customers.length} customers`);

        // Process each customer
        for (const customer of customers) {
          try {
            // Extract setter_user_id from customer data
            // Based on RepCard API, the userId field might be in different places
            const setterUserId = (customer as any).userId || customer.assignedUserId;

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

            if (result[0]?.inserted) {
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

        const appointments = response.result.data;
        console.log(`[RepCard Sync] Page ${page}: Got ${appointments.length} appointments`);

        // Process each appointment
        for (const appointment of appointments) {
          try {
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

            const customerId = customerResult.length > 0 ? customerResult[0].id : null;

            // Extract disposition from status
            const disposition = appointment.status?.title || appointment.status?.category?.title || null;

            // Upsert appointment
            const result = await sql`
              INSERT INTO repcard_appointments (
                repcard_appointment_id,
                customer_id,
                repcard_customer_id,
                setter_user_id,
                closer_user_id,
                disposition,
                scheduled_at,
                completed_at,
                duration,
                notes,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${appointment.id},
                ${customerId},
                ${appointment.contact.id},
                ${appointment.userId},
                ${appointment.closerId},
                ${disposition},
                ${appointment.startAt},
                ${appointment.endAt},
                ${appointment.durationTime},
                ${appointment.notes || null},
                ${appointment.createdAt},
                ${appointment.updatedAt},
                ${JSON.stringify(appointment)}
              )
              ON CONFLICT (repcard_appointment_id)
              DO UPDATE SET
                customer_id = EXCLUDED.customer_id,
                setter_user_id = EXCLUDED.setter_user_id,
                closer_user_id = EXCLUDED.closer_user_id,
                disposition = EXCLUDED.disposition,
                scheduled_at = EXCLUDED.scheduled_at,
                completed_at = EXCLUDED.completed_at,
                duration = EXCLUDED.duration,
                notes = EXCLUDED.notes,
                updated_at = EXCLUDED.updated_at,
                raw_data = EXCLUDED.raw_data,
                synced_at = NOW()
              RETURNING (xmax = 0) AS inserted
            `;

            if (result[0]?.inserted) {
              recordsInserted++;
            } else {
              recordsUpdated++;
            }
            recordsFetched++;

          } catch (error) {
            console.error(`[RepCard Sync] Failed to process appointment ${appointment.id}:`, error);
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

        const statusLogs = response.result.data;
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

            const customerId = customerResult.length > 0 ? customerResult[0].id : null;

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

            if (result[0]?.inserted) {
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
