/**
 * Comprehensive RepCard Sync Service
 * 
 * Syncs ALL available RepCard data:
 * - Users/Reps
 * - Offices
 * - Customers (leads)
 * - Appointments
 * - Status Logs
 * - Customer Attachments
 * - Appointment Attachments
 * 
 * This is the master sync service that pulls everything from RepCard
 */

import { sql } from '@/lib/db/client';
import { repcardClient } from './client';
import type { 
  RepCardUserMinimal, 
  RepCardOffice, 
  RepCardCustomer,
  RepCardAppointment,
  RepCardCustomerStatusLog,
  RepCardCustomerAttachment,
  RepCardAppointmentAttachment
} from './types';
import { syncCustomers, syncAppointments, syncStatusLogs } from './sync-service';

export interface ComprehensiveSyncResult {
  users: SyncEntityResult;
  offices: SyncEntityResult;
  customers: SyncEntityResult;
  appointments: SyncEntityResult;
  statusLogs: SyncEntityResult;
  customerAttachments: SyncEntityResult;
  appointmentAttachments: SyncEntityResult;
  totalDuration: number;
  startedAt: string;
  completedAt: string;
}

export interface SyncEntityResult {
  entityType: string;
  recordsFetched: number;
  recordsInserted: number;
  recordsUpdated: number;
  recordsFailed: number;
  duration: number;
  error?: string;
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
  const row = result.rows?.[0] || result[0];
  if (!row || !row.id) {
    throw new Error('Failed to create sync log entry');
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
 * Sync RepCard Users
 */
export async function syncUsers(options: {
  incremental?: boolean;
} = {}): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncType = options.incremental ? 'incremental' : 'full';
  const syncLogId = await createSyncLog('users', syncType);

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;
  let lastRecordDate: Date | undefined;

  try {
    console.log(`[RepCard Sync] Starting ${syncType} sync for users...`);

    let page = 1;
    let hasMore = true;
    const MAX_PAGES = 200;

    while (hasMore && page <= MAX_PAGES) {
      try {
        const response = await repcardClient.getUsersMinimal({
          page,
          perPage: 100
        });

        // Validate response structure
        if (!response || !response.result) {
          console.error(`[RepCard Sync] Invalid response structure on page ${page}:`, response);
          throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
        }

        const users = Array.isArray(response.result.data) ? response.result.data : [];
        console.log(`[RepCard Sync] Page ${page}: Got ${users.length} users`);

        for (const user of users) {
          try {
            const updatedAt = new Date((user as any).updatedAt || new Date());
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Upsert user
            const result = await sql`
              INSERT INTO repcard_users (
                repcard_user_id,
                company_id,
                office_id,
                first_name,
                last_name,
                email,
                phone,
                username,
                role,
                status,
                office_name,
                team,
                job_title,
                profile_image,
                rating,
                bio,
                badge_id,
                qr_code,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${user.id},
                ${user.companyId},
                ${user.officeId || null},
                ${(user as any).firstName || null},
                ${(user as any).lastName || null},
                ${user.email || null},
                ${(user as any).phone || null},
                ${(user as any).username || null},
                ${(user as any).role || null},
                ${(user as any).status ?? 1},
                ${(user as any).office || null},
                ${(user as any).team || null},
                ${(user as any).jobTitle || null},
                ${(user as any).image || null},
                ${(user as any).rating || null},
                ${(user as any).bio || null},
                ${(user as any).badgeId || null},
                ${(user as any).qrCode || null},
                ${(user as any).createdAt || null},
                ${updatedAt},
                ${JSON.stringify(user)}
              )
              ON CONFLICT (repcard_user_id)
              DO UPDATE SET
                office_id = EXCLUDED.office_id,
                first_name = EXCLUDED.first_name,
                last_name = EXCLUDED.last_name,
                email = EXCLUDED.email,
                phone = EXCLUDED.phone,
                username = EXCLUDED.username,
                role = EXCLUDED.role,
                status = EXCLUDED.status,
                office_name = EXCLUDED.office_name,
                team = EXCLUDED.team,
                job_title = EXCLUDED.job_title,
                profile_image = EXCLUDED.profile_image,
                rating = EXCLUDED.rating,
                bio = EXCLUDED.bio,
                badge_id = EXCLUDED.badge_id,
                qr_code = EXCLUDED.qr_code,
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
            const errorStack = error instanceof Error ? error.stack : undefined;
            console.error(`[RepCard Sync] Failed to process user ${user.id} (${user.email || 'no email'}):`, errorMsg);
            if (errorStack) {
              console.error(`[RepCard Sync] Stack trace:`, errorStack);
            }
            // Log first few errors in detail
            if (recordsFailed < 3) {
              console.error(`[RepCard Sync] User data that failed:`, JSON.stringify(user, null, 2));
            }
            recordsFailed++;
          }
        }

        hasMore = response.result.currentPage < response.result.lastPage;
        page++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to fetch users page ${page}:`, error);
        throw error;
      }
    }

    await completeSyncLog(syncLogId, 'completed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      lastRecordDate
    });

    return {
      entityType: 'users',
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime
    };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    await completeSyncLog(syncLogId, 'failed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      error: errorMessage
    });

    return {
      entityType: 'users',
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
 * Sync RepCard Offices
 */
export async function syncOffices(): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncLogId = await createSyncLog('offices', 'full');

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;

  try {
    console.log('[RepCard Sync] Starting sync for offices...');

    const response = await repcardClient.getOffices();
    // Validate response structure
    if (!response || !response.result) {
      console.error(`[RepCard Sync] Invalid response structure for offices:`, response);
      throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
    }

    const offices = Array.isArray(response.result.data) ? response.result.data : 
                    Array.isArray(response.result) ? response.result : [];
    console.log(`[RepCard Sync] Got ${offices.length} offices`);

    for (const office of offices) {
      try {
        const result = await sql`
          INSERT INTO repcard_offices (
            repcard_office_id,
            company_id,
            name,
            address,
            city,
            state,
            zip_code,
            created_at,
            updated_at,
            raw_data
          )
          VALUES (
            ${office.id},
            ${office.companyId},
            ${office.name},
            ${office.address || null},
            ${office.city || null},
            ${office.state || null},
            ${office.zipCode || null},
            ${office.createdAt || null},
            ${office.updatedAt || null},
            ${JSON.stringify(office)}
          )
          ON CONFLICT (repcard_office_id)
          DO UPDATE SET
            name = EXCLUDED.name,
            address = EXCLUDED.address,
            city = EXCLUDED.city,
            state = EXCLUDED.state,
            zip_code = EXCLUDED.zip_code,
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
        console.error(`[RepCard Sync] Failed to process office ${office.id}:`, error);
        recordsFailed++;
      }
    }

    await completeSyncLog(syncLogId, 'completed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed
    });

    return {
      entityType: 'offices',
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime
    };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    await completeSyncLog(syncLogId, 'failed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      error: errorMessage
    });

    return {
      entityType: 'offices',
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
 * Sync Customer Attachments
 */
export async function syncCustomerAttachments(options: {
  fromDate?: string;
  toDate?: string;
  incremental?: boolean;
} = {}): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncType = options.incremental ? 'incremental' : 'full';
  const syncLogId = await createSyncLog('customer_attachments', syncType);

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;
  let lastRecordDate: Date | undefined;

  try {
    console.log(`[RepCard Sync] Starting ${syncType} sync for customer attachments...`);

    let page = 1;
    let hasMore = true;
    const MAX_PAGES = 200;

    while (hasMore && page <= MAX_PAGES) {
      try {
        const response = await repcardClient.getCustomerAttachments({
          page,
          perPage: 100,
          fromDate: options.fromDate,
          toDate: options.toDate
        });

        // Validate response structure
        if (!response || !response.result) {
          console.error(`[RepCard Sync] Invalid response structure on page ${page}:`, response);
          throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
        }

        const attachments = Array.isArray(response.result.data) ? response.result.data : [];
        console.log(`[RepCard Sync] Page ${page}: Got ${attachments.length} customer attachments`);

        for (const attachment of attachments) {
          try {
            const updatedAt = new Date(attachment.updatedAt || attachment.createdAt);
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Get customer_id from our database
            const customerResult = await sql`
              SELECT id FROM repcard_customers
              WHERE repcard_customer_id = ${attachment.customerId}
              LIMIT 1
            `;
            const customerRows = customerResult.rows || customerResult;
            const customerId = customerRows.length > 0 ? customerRows[0].id : null;

            const result = await sql`
              INSERT INTO repcard_customer_attachments (
                repcard_attachment_id,
                customer_id,
                repcard_customer_id,
                attachment_type,
                file_name,
                file_url,
                file_size,
                uploaded_by_user_id,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${attachment.id},
                ${customerId},
                ${attachment.customerId},
                ${(attachment as any).type || null},
                ${(attachment as any).fileName || null},
                ${attachment.attachmentUrl || null},
                ${(attachment as any).fileSize || null},
                ${attachment.userId || null},
                ${(attachment as any).createdAt || null},
                ${updatedAt},
                ${JSON.stringify(attachment)}
              )
              ON CONFLICT (repcard_attachment_id)
              DO UPDATE SET
                customer_id = EXCLUDED.customer_id,
                attachment_type = EXCLUDED.attachment_type,
                file_name = EXCLUDED.file_name,
                file_url = EXCLUDED.file_url,
                file_size = EXCLUDED.file_size,
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
            console.error(`[RepCard Sync] Failed to process customer attachment ${attachment.id}:`, error);
            recordsFailed++;
          }
        }

        hasMore = response.result.currentPage < response.result.totalPages;
        page++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to fetch customer attachments page ${page}:`, error);
        throw error;
      }
    }

    await completeSyncLog(syncLogId, 'completed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      lastRecordDate
    });

    return {
      entityType: 'customer_attachments',
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime
    };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    await completeSyncLog(syncLogId, 'failed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      error: errorMessage
    });

    return {
      entityType: 'customer_attachments',
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
 * Sync Appointment Attachments
 */
export async function syncAppointmentAttachments(options: {
  fromDate?: string;
  toDate?: string;
  incremental?: boolean;
} = {}): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncType = options.incremental ? 'incremental' : 'full';
  const syncLogId = await createSyncLog('appointment_attachments', syncType);

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;
  let lastRecordDate: Date | undefined;

  try {
    console.log(`[RepCard Sync] Starting ${syncType} sync for appointment attachments...`);

    let page = 1;
    let hasMore = true;
    const MAX_PAGES = 200;

    while (hasMore && page <= MAX_PAGES) {
      try {
        const response = await repcardClient.getAppointmentAttachments({
          page,
          perPage: 100,
          fromDate: options.fromDate,
          toDate: options.toDate
        });

        // Validate response structure
        if (!response || !response.result) {
          console.error(`[RepCard Sync] Invalid response structure on page ${page}:`, response);
          throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
        }

        const attachments = Array.isArray(response.result.data) ? response.result.data : [];
        console.log(`[RepCard Sync] Page ${page}: Got ${attachments.length} appointment attachments`);

        for (const attachment of attachments) {
          try {
            const updatedAt = new Date(attachment.updatedAt || attachment.createdAt);
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Get appointment_id and customer_id from our database
            const appointmentResult = await sql`
              SELECT id, customer_id, repcard_customer_id
              FROM repcard_appointments
              WHERE repcard_appointment_id = ${attachment.appointmentId}
              LIMIT 1
            `;
            const appointmentRows = appointmentResult.rows || appointmentResult;
            const appointmentId = appointmentRows.length > 0 ? appointmentRows[0].id : null;
            const customerId = appointmentRows.length > 0 ? appointmentRows[0].customer_id : null;
            const repcardCustomerId = appointmentRows.length > 0 ? appointmentRows[0].repcard_customer_id : null;

            const result = await sql`
              INSERT INTO repcard_appointment_attachments (
                repcard_attachment_id,
                appointment_id,
                repcard_appointment_id,
                customer_id,
                repcard_customer_id,
                attachment_type,
                file_name,
                file_url,
                file_size,
                uploaded_by_user_id,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${attachment.id},
                ${appointmentId},
                ${attachment.appointmentId},
                ${customerId},
                ${repcardCustomerId},
                ${attachment.type || null},
                ${(attachment as any).fileName || null},
                ${attachment.attachmentUrl || null},
                ${(attachment as any).fileSize || null},
                ${attachment.userId || null},
                ${(attachment as any).createdAt || null},
                ${updatedAt},
                ${JSON.stringify(attachment)}
              )
              ON CONFLICT (repcard_attachment_id)
              DO UPDATE SET
                appointment_id = EXCLUDED.appointment_id,
                customer_id = EXCLUDED.customer_id,
                attachment_type = EXCLUDED.attachment_type,
                file_name = EXCLUDED.file_name,
                file_url = EXCLUDED.file_url,
                file_size = EXCLUDED.file_size,
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
            console.error(`[RepCard Sync] Failed to process appointment attachment ${attachment.id}:`, error);
            recordsFailed++;
          }
        }

        hasMore = response.result.currentPage < response.result.totalPages;
        page++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to fetch appointment attachments page ${page}:`, error);
        throw error;
      }
    }

    await completeSyncLog(syncLogId, 'completed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      lastRecordDate
    });

    return {
      entityType: 'appointment_attachments',
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      duration: Date.now() - startTime
    };

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    await completeSyncLog(syncLogId, 'failed', {
      recordsFetched,
      recordsInserted,
      recordsUpdated,
      recordsFailed,
      error: errorMessage
    });

    return {
      entityType: 'appointment_attachments',
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
 * Link RepCard users to users table by matching email
 * Updates users.repcard_user_id for matching users
 */
export async function linkRepCardUsersToUsers(): Promise<void> {
  try {
    console.log('[RepCard Sync] Linking RepCard users to users table...');
    
    // First, check what we're working with
    const statsBefore = await sql`
      SELECT 
        (SELECT COUNT(*) FROM repcard_users WHERE email IS NOT NULL AND email != '') as repcard_count,
        (SELECT COUNT(*) FROM users WHERE email IS NOT NULL AND email != '') as users_count,
        (SELECT COUNT(*) FROM users WHERE repcard_user_id IS NOT NULL) as already_linked,
        (SELECT COUNT(*) FROM users u
         INNER JOIN repcard_users ru ON LOWER(u.email) = LOWER(ru.email)
         WHERE u.repcard_user_id IS NULL
           AND ru.email IS NOT NULL AND ru.email != '') as ready_to_link
    `;
    
    const stats = statsBefore[0];
    console.log(`[RepCard Sync] Stats: ${stats?.repcard_count || 0} RepCard users with email, ${stats?.users_count || 0} users with email, ${stats?.already_linked || 0} already linked, ${stats?.ready_to_link || 0} ready to link`);
    
    const result = await sql`
      UPDATE users u
      SET 
        repcard_user_id = ru.repcard_user_id::text,
        last_synced_from_repcard_at = NOW()
      FROM repcard_users ru
      WHERE LOWER(u.email) = LOWER(ru.email)
        AND u.repcard_user_id IS NULL
        AND ru.email IS NOT NULL
        AND ru.email != ''
    `;
    
    const linkedCount = result.count || 0;
    console.log(`[RepCard Sync] ✅ Linked ${linkedCount} users to RepCard accounts`);
    
    // Log successful links
    if (linkedCount > 0) {
      const linkedUsers = await sql`
        SELECT u.id, u.email, u.repcard_user_id
        FROM users u
        WHERE u.repcard_user_id IS NOT NULL
          AND u.last_synced_from_repcard_at > NOW() - INTERVAL '1 minute'
      `;
      
      for (const user of linkedUsers) {
        await sql`
          INSERT INTO user_sync_log (
            user_id,
            source_system,
            external_id,
            match_method,
            confidence,
            synced_at,
            notes
          ) VALUES (
            ${user.id},
            'repcard',
            ${user.repcard_user_id},
            'email',
            1.0,
            NOW(),
            'Auto-linked from comprehensive RepCard sync'
          )
          ON CONFLICT DO NOTHING
        `;
      }
    }
    
  } catch (error) {
    console.error('[RepCard Sync] ❌ Error linking users:', error);
    throw error;
  }
}

/**
 * Run comprehensive sync of ALL RepCard data
 * Order matters: users/offices first, then customers, then appointments/attachments
 */
export async function runComprehensiveSync(options: {
  startDate?: string;
  endDate?: string;
  incremental?: boolean;
  skipUsers?: boolean;
  skipOffices?: boolean;
  skipCustomers?: boolean;
  skipAppointments?: boolean;
  skipStatusLogs?: boolean;
  skipCustomerAttachments?: boolean;
  skipAppointmentAttachments?: boolean;
} = {}): Promise<ComprehensiveSyncResult> {
  const startedAt = new Date().toISOString();
  const overallStartTime = Date.now();

  console.log('[RepCard Comprehensive Sync] Starting comprehensive sync...');
  console.log(`Options:`, options);

  const results: ComprehensiveSyncResult = {
    users: { entityType: 'users', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    offices: { entityType: 'offices', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    customers: { entityType: 'customers', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    appointments: { entityType: 'appointments', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    statusLogs: { entityType: 'status_logs', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    customerAttachments: { entityType: 'customer_attachments', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    appointmentAttachments: { entityType: 'appointment_attachments', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    totalDuration: 0,
    startedAt,
    completedAt: ''
  };

  try {
    // Step 1: Sync Users (needed for attribution)
    if (!options.skipUsers) {
      console.log('[RepCard Comprehensive Sync] Step 1/7: Syncing users...');
      results.users = await syncUsers({ incremental: options.incremental });
    }

    // Step 2: Sync Offices (needed for office data)
    if (!options.skipOffices) {
      console.log('[RepCard Comprehensive Sync] Step 2/7: Syncing offices...');
      results.offices = await syncOffices();
    }

    // Step 3: Sync Customers (needed for foreign keys)
    if (!options.skipCustomers) {
      console.log('[RepCard Comprehensive Sync] Step 3/7: Syncing customers...');
      const customersResult = await syncCustomers({
        startDate: options.startDate,
        endDate: options.endDate,
        incremental: options.incremental
      });
      results.customers = {
        entityType: customersResult.entityType,
        recordsFetched: customersResult.recordsFetched,
        recordsInserted: customersResult.recordsInserted,
        recordsUpdated: customersResult.recordsUpdated,
        recordsFailed: customersResult.recordsFailed,
        duration: customersResult.duration,
        error: customersResult.error
      };
    }

    // Step 4: Sync Appointments (depends on customers)
    if (!options.skipAppointments) {
      console.log('[RepCard Comprehensive Sync] Step 4/7: Syncing appointments...');
      const appointmentsResult = await syncAppointments({
        fromDate: options.startDate,
        toDate: options.endDate,
        incremental: options.incremental
      });
      results.appointments = {
        entityType: appointmentsResult.entityType,
        recordsFetched: appointmentsResult.recordsFetched,
        recordsInserted: appointmentsResult.recordsInserted,
        recordsUpdated: appointmentsResult.recordsUpdated,
        recordsFailed: appointmentsResult.recordsFailed,
        duration: appointmentsResult.duration,
        error: appointmentsResult.error
      };
    }

    // Step 5: Sync Status Logs (depends on customers)
    if (!options.skipStatusLogs) {
      console.log('[RepCard Comprehensive Sync] Step 5/7: Syncing status logs...');
      const statusLogsResult = await syncStatusLogs({
        fromDate: options.startDate,
        toDate: options.endDate,
        incremental: options.incremental
      });
      results.statusLogs = {
        entityType: statusLogsResult.entityType,
        recordsFetched: statusLogsResult.recordsFetched,
        recordsInserted: statusLogsResult.recordsInserted,
        recordsUpdated: statusLogsResult.recordsUpdated,
        recordsFailed: statusLogsResult.recordsFailed,
        duration: statusLogsResult.duration,
        error: statusLogsResult.error
      };
    }

    // Step 6: Sync Customer Attachments (depends on customers)
    if (!options.skipCustomerAttachments) {
      console.log('[RepCard Comprehensive Sync] Step 6/7: Syncing customer attachments...');
      results.customerAttachments = await syncCustomerAttachments({
        fromDate: options.startDate,
        toDate: options.endDate,
        incremental: options.incremental
      });
    }

    // Step 7: Sync Appointment Attachments (depends on appointments)
    if (!options.skipAppointmentAttachments) {
      console.log('[RepCard Comprehensive Sync] Step 7/7: Syncing appointment attachments...');
      results.appointmentAttachments = await syncAppointmentAttachments({
        fromDate: options.startDate,
        toDate: options.endDate,
        incremental: options.incremental
      });
    }

    // Step 8: Link RepCard users to users table (match by email)
    if (!options.skipUsers) {
      console.log('[RepCard Comprehensive Sync] Step 8/8: Linking RepCard users to users table...');
      await linkRepCardUsersToUsers();
    }

    results.totalDuration = Date.now() - overallStartTime;
    results.completedAt = new Date().toISOString();

    console.log('[RepCard Comprehensive Sync] ✅ Comprehensive sync completed!');
    console.log(`Total duration: ${(results.totalDuration / 1000).toFixed(2)}s`);
    console.log('Summary:', {
      users: `${results.users.recordsFetched} fetched, ${results.users.recordsInserted} inserted, ${results.users.recordsUpdated} updated`,
      offices: `${results.offices.recordsFetched} fetched, ${results.offices.recordsInserted} inserted, ${results.offices.recordsUpdated} updated`,
      customers: `${results.customers.recordsFetched} fetched, ${results.customers.recordsInserted} inserted, ${results.customers.recordsUpdated} updated`,
      appointments: `${results.appointments.recordsFetched} fetched, ${results.appointments.recordsInserted} inserted, ${results.appointments.recordsUpdated} updated`,
      statusLogs: `${results.statusLogs.recordsFetched} fetched, ${results.statusLogs.recordsInserted} inserted, ${results.statusLogs.recordsUpdated} updated`,
      customerAttachments: `${results.customerAttachments.recordsFetched} fetched, ${results.customerAttachments.recordsInserted} inserted, ${results.customerAttachments.recordsUpdated} updated`,
      appointmentAttachments: `${results.appointmentAttachments.recordsFetched} fetched, ${results.appointmentAttachments.recordsInserted} inserted, ${results.appointmentAttachments.recordsUpdated} updated`
    });

    return results;

  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    console.error('[RepCard Comprehensive Sync] ❌ Comprehensive sync failed:', error);
    
    results.totalDuration = Date.now() - overallStartTime;
    results.completedAt = new Date().toISOString();
    
    return results;
  }
}

