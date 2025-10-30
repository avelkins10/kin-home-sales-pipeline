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
 *
 * PROTECTED FIELDS IN USERS TABLE (never overwritten):
 * - name: User's display name (managed by admins in the app)
 * - role: User's role (managed by admins in the app)
 * - sales_office: Office array for access control (managed by admins)
 * - password_hash: Security field
 *
 * RepCard data is synced to repcard_users table, then linked to users table
 * via repcard_user_id only. This ensures app-managed fields are never overwritten.
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
  RepCardAppointmentAttachment,
  RepCardCustomerNote,
  RepCardCustomerStatus,
  RepCardCalendar,
  RepCardCustomField,
  RepCardLeaderboard,
  RepCardTeam
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
  customerNotes: SyncEntityResult;
  customerStatuses: SyncEntityResult;
  calendars: SyncEntityResult;
  customFields: SyncEntityResult;
  leaderboards: SyncEntityResult;
  teams: SyncEntityResult;
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
 * Sync RepCard Customer Notes
 */
export async function syncCustomerNotes(options: {
  startDate?: string;
  endDate?: string;
  incremental?: boolean;
} = {}): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncType = options.incremental ? 'incremental' : 'full';
  const syncLogId = await createSyncLog('customer_notes', syncType);

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;
  let lastRecordDate: Date | undefined;

  try {
    console.log(`[RepCard Sync] Starting ${syncType} sync for customer notes...`);

    let page = 1;
    let hasMore = true;
    const MAX_PAGES = 100;

    while (hasMore && page <= MAX_PAGES) {
      try {
        const response = await repcardClient.getCustomerNotes({
          page,
          perPage: 100
        });

        if (!response || !response.result || !Array.isArray(response.result.data)) {
          console.error(`[RepCard Sync] Invalid response structure on page ${page}:`, response);
          break;
        }

        const notes = response.result.data;
        console.log(`[RepCard Sync] Page ${page}: Got ${notes.length} notes`);

        for (const note of notes) {
          try {
            const createdAt = new Date(note.createdAt);
            const updatedAt = new Date(note.updatedAt);
            if (!lastRecordDate || updatedAt > lastRecordDate) {
              lastRecordDate = updatedAt;
            }

            // Find customer_id from repcard_customer_id
            const customerResult = await sql`
              SELECT id FROM repcard_customers WHERE repcard_customer_id = ${note.customerId} LIMIT 1
            `;
            const customerId = customerResult.rows?.[0]?.id || customerResult[0]?.id || null;

            // Find user_id from repcard_user_id
            const userResult = await sql`
              SELECT id FROM users WHERE repcard_user_id::text = ${String(note.userId)} LIMIT 1
            `;
            const userId = userResult.rows?.[0]?.id || userResult[0]?.id || null;

            // Note: RepCard uses MongoDB ObjectId as string for note IDs
            const noteId = typeof note.id === 'string' ? note.id : note.id.toString();

            await sql`
              INSERT INTO repcard_customer_notes (
                repcard_note_id,
                customer_id,
                repcard_customer_id,
                user_id,
                repcard_user_id,
                note,
                created_at,
                updated_at,
                raw_data
              )
              VALUES (
                ${noteId},
                ${customerId},
                ${note.customerId},
                ${userId},
                ${note.userId},
                ${note.note},
                ${createdAt},
                ${updatedAt},
                ${JSON.stringify(note)}
              )
              ON CONFLICT (repcard_note_id)
              DO UPDATE SET
                note = EXCLUDED.note,
                updated_at = EXCLUDED.updated_at,
                raw_data = EXCLUDED.raw_data,
                synced_at = NOW()
              RETURNING (xmax = 0) AS inserted
            `;

            const result = await sql`
              SELECT (xmax = 0) AS inserted FROM repcard_customer_notes WHERE repcard_note_id = ${noteId}
            `;
            const row = result.rows?.[0] || result[0];
            if (row?.inserted) {
              recordsInserted++;
            } else {
              recordsUpdated++;
            }
            recordsFetched++;

          } catch (error) {
            console.error(`[RepCard Sync] Failed to process note ${note.id}:`, error);
            recordsFailed++;
          }
        }

        hasMore = response.result.currentPage < response.result.lastPage;
        page++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to fetch notes page ${page}:`, error);
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
      entityType: 'customer_notes',
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
      entityType: 'customer_notes',
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
 * Sync RepCard Customer Status Definitions
 */
export async function syncCustomerStatuses(): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncLogId = await createSyncLog('customer_statuses', 'full');

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;

  try {
    console.log('[RepCard Sync] Starting sync for customer statuses...');

    const response = await repcardClient.getCustomerStatuses();
    if (!response || !response.result || !Array.isArray(response.result)) {
      throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
    }

    const statuses = response.result;
    console.log(`[RepCard Sync] Got ${statuses.length} status definitions`);

    for (const status of statuses) {
      try {
        await sql`
          INSERT INTO repcard_customer_statuses (
            repcard_status_id,
            status_name,
            short_name,
            type,
            colour,
            icon_name,
            icon_url,
            status_order,
            is_global,
            is_base_status,
            company_id,
            user_id,
            raw_data
          )
          VALUES (
            ${status.id},
            ${status.statusName},
            ${status.shortName || null},
            ${status.type || null},
            ${status.colour || null},
            ${status.iconName || null},
            ${status.iconUrl || null},
            ${status.statusOrder || null},
            ${status.isGlobal === 1 || status.isGlobal === true},
            ${status.isBaseStatus === 1 || status.isBaseStatus === true},
            ${status.companyId || null},
            ${status.userId || null},
            ${JSON.stringify(status)}
          )
          ON CONFLICT (repcard_status_id)
          DO UPDATE SET
            status_name = EXCLUDED.status_name,
            short_name = EXCLUDED.short_name,
            colour = EXCLUDED.colour,
            icon_name = EXCLUDED.icon_name,
            icon_url = EXCLUDED.icon_url,
            status_order = EXCLUDED.status_order,
            raw_data = EXCLUDED.raw_data,
            synced_at = NOW()
          RETURNING (xmax = 0) AS inserted
        `;

        const result = await sql`
          SELECT (xmax = 0) AS inserted FROM repcard_customer_statuses WHERE repcard_status_id = ${status.id}
        `;
        const row = result.rows?.[0] || result[0];
        if (row?.inserted) {
          recordsInserted++;
        } else {
          recordsUpdated++;
        }
        recordsFetched++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to process status ${status.id}:`, error);
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
      entityType: 'customer_statuses',
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
      entityType: 'customer_statuses',
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
 * Sync RepCard Calendars
 */
export async function syncCalendars(): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncLogId = await createSyncLog('calendars', 'full');

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;

  try {
    console.log('[RepCard Sync] Starting sync for calendars...');

    const response = await repcardClient.getCalendars({ status: 'active' });
    if (!response || !response.result || !Array.isArray(response.result)) {
      throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
    }

    const calendars = response.result;
    console.log(`[RepCard Sync] Got ${calendars.length} calendars`);

    // Fetch details for each calendar to get setters/closers/dispatchers
    for (const calendar of calendars) {
      try {
        let setters: number[] = [];
        let closers: number[] = [];
        let dispatchers: number[] = [];

        try {
          const detailsResponse = await repcardClient.getCalendarDetails(calendar.id);
          if (detailsResponse?.result) {
            setters = detailsResponse.result.setters?.map(s => s.id) || [];
            closers = detailsResponse.result.closers?.map(c => c.id) || [];
            dispatchers = detailsResponse.result.dispatchers?.map(d => d.id) || [];
          }
        } catch (error) {
          console.warn(`[RepCard Sync] Could not fetch details for calendar ${calendar.id}:`, error);
        }

        await sql`
          INSERT INTO repcard_calendars (
            repcard_calendar_id,
            name,
            company_id,
            status,
            setters,
            closers,
            dispatchers,
            raw_data
          )
          VALUES (
            ${calendar.id},
            ${calendar.name},
            ${calendar.companyId},
            ${calendar.status || 'active'},
            ${setters},
            ${closers},
            ${dispatchers},
            ${JSON.stringify(calendar)}
          )
          ON CONFLICT (repcard_calendar_id)
          DO UPDATE SET
            name = EXCLUDED.name,
            status = EXCLUDED.status,
            setters = EXCLUDED.setters,
            closers = EXCLUDED.closers,
            dispatchers = EXCLUDED.dispatchers,
            raw_data = EXCLUDED.raw_data,
            synced_at = NOW()
          RETURNING (xmax = 0) AS inserted
        `;

        const result = await sql`
          SELECT (xmax = 0) AS inserted FROM repcard_calendars WHERE repcard_calendar_id = ${calendar.id}
        `;
        const row = result.rows?.[0] || result[0];
        if (row?.inserted) {
          recordsInserted++;
        } else {
          recordsUpdated++;
        }
        recordsFetched++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to process calendar ${calendar.id}:`, error);
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
      entityType: 'calendars',
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
      entityType: 'calendars',
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
 * Sync RepCard Custom Fields
 */
export async function syncCustomFields(): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncLogId = await createSyncLog('custom_fields', 'full');

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;

  try {
    console.log('[RepCard Sync] Starting sync for custom fields...');

    const entityTypes: Array<'lead' | 'customer' | 'recruit' | 'other'> = ['lead', 'customer', 'recruit', 'other'];
    const allFields: any[] = [];

    for (const entityType of entityTypes) {
      try {
        const response = await repcardClient.getCustomFields(entityType);
        if (response?.result && Array.isArray(response.result)) {
          allFields.push(...response.result.map((field: any) => ({ ...field, entityType })));
        }
      } catch (error) {
        console.warn(`[RepCard Sync] Could not fetch custom fields for ${entityType}:`, error);
      }
    }

    console.log(`[RepCard Sync] Got ${allFields.length} custom field definitions`);

    for (const field of allFields) {
      try {
        await sql`
          INSERT INTO repcard_custom_fields (
            repcard_field_id,
            entity_type,
            internal_name,
            display_name,
            field_type,
            data_type,
            option_values,
            company_id,
            raw_data
          )
          VALUES (
            ${field.id},
            ${field.entityType},
            ${field.internalName},
            ${field.displayName},
            ${field.type},
            ${field.dataType},
            ${field.optionValues ? JSON.stringify(field.optionValues) : null},
            ${field.companyId || null},
            ${JSON.stringify(field)}
          )
          ON CONFLICT (repcard_field_id)
          DO UPDATE SET
            internal_name = EXCLUDED.internal_name,
            display_name = EXCLUDED.display_name,
            field_type = EXCLUDED.field_type,
            data_type = EXCLUDED.data_type,
            option_values = EXCLUDED.option_values,
            raw_data = EXCLUDED.raw_data,
            synced_at = NOW()
          RETURNING (xmax = 0) AS inserted
        `;

        const result = await sql`
          SELECT (xmax = 0) AS inserted FROM repcard_custom_fields WHERE repcard_field_id = ${field.id}
        `;
        const row = result.rows?.[0] || result[0];
        if (row?.inserted) {
          recordsInserted++;
        } else {
          recordsUpdated++;
        }
        recordsFetched++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to process custom field ${field.id}:`, error);
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
      entityType: 'custom_fields',
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
      entityType: 'custom_fields',
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
 * Sync RepCard Leaderboard Snapshots
 */
export async function syncLeaderboards(options: {
  startDate?: string;
  endDate?: string;
} = {}): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncLogId = await createSyncLog('leaderboards', 'full');

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;

  try {
    console.log('[RepCard Sync] Starting sync for leaderboards...');

    const response = await repcardClient.getLeaderboards({
      fromDate: options.startDate,
      toDate: options.endDate
    });

    if (!response || !response.result || !Array.isArray(response.result)) {
      throw new Error(`Invalid response structure: ${JSON.stringify(response)}`);
    }

    const leaderboards = response.result;
    console.log(`[RepCard Sync] Got ${leaderboards.length} leaderboard configurations`);

    const snapshotDate = new Date().toISOString().split('T')[0]; // Today's date

    for (const leaderboard of leaderboards) {
      try {
        if (!leaderboard.stats || !leaderboard.stats.stats) {
          continue;
        }

        const stats = leaderboard.stats.stats;
        console.log(`[RepCard Sync] Processing ${leaderboard.leaderboard_name}: ${stats.length} entries`);

        for (const stat of stats) {
          if (!stat.item_id || stat.item_type !== 'user') {
            continue;
          }

          try {
            // Find user_id from repcard_user_id
            const userResult = await sql`
              SELECT id FROM users WHERE repcard_user_id::text = ${String(stat.item_id)} LIMIT 1
            `;
            const userId = userResult.rows?.[0]?.id || userResult[0]?.id || null;

            const officeId = stat.office?.[0]?.item_id || stat.office_id || null;
            const officeName = stat.office?.[0]?.office_name || null;
            const teamId = stat.team?.[0]?.item_id || stat.office_team_id || null;
            const teamName = stat.team?.[0]?.team_name || null;

            await sql`
              INSERT INTO repcard_leaderboard_snapshots (
                snapshot_date,
                leaderboard_name,
                leaderboard_id,
                company_id,
                user_id,
                repcard_user_id,
                customer_count,
                door_knocks,
                lead_count,
                appointment_count,
                avg_door_knocks_per_day,
                avg_distance_per_knocks,
                avg_rating,
                video_viewed,
                review_count,
                referral_count,
                engagement_count,
                card_sent_count,
                recruit_count,
                item_rank,
                office_id,
                office_name,
                team_id,
                team_name,
                raw_data
              )
              VALUES (
                ${snapshotDate}::date,
                ${leaderboard.leaderboard_name},
                ${leaderboard._id},
                ${leaderboard.company_id},
                ${userId},
                ${stat.item_id},
                ${stat.customer_count || 0},
                ${stat.door_knocks || 0},
                ${stat.lead_count || 0},
                ${stat.appointment_count || 0},
                ${stat.avg_door_knocks_per_day || 0},
                ${stat.avg_distance_per_knocks || 0},
                ${stat.avg_rating || 0},
                ${stat.video_viewed || 0},
                ${stat.review_count || 0},
                ${stat.referral_count || 0},
                ${stat.engagement_count || 0},
                ${stat.card_sent_count || 0},
                ${stat.recruit_count || 0},
                ${stat.item_rank || null},
                ${officeId},
                ${officeName},
                ${teamId},
                ${teamName},
                ${JSON.stringify(stat)}
              )
              ON CONFLICT (snapshot_date, leaderboard_name, repcard_user_id)
              DO UPDATE SET
                customer_count = EXCLUDED.customer_count,
                door_knocks = EXCLUDED.door_knocks,
                lead_count = EXCLUDED.lead_count,
                appointment_count = EXCLUDED.appointment_count,
                avg_door_knocks_per_day = EXCLUDED.avg_door_knocks_per_day,
                avg_distance_per_knocks = EXCLUDED.avg_distance_per_knocks,
                avg_rating = EXCLUDED.avg_rating,
                video_viewed = EXCLUDED.video_viewed,
                review_count = EXCLUDED.review_count,
                referral_count = EXCLUDED.referral_count,
                engagement_count = EXCLUDED.engagement_count,
                card_sent_count = EXCLUDED.card_sent_count,
                recruit_count = EXCLUDED.recruit_count,
                item_rank = EXCLUDED.item_rank,
                office_id = EXCLUDED.office_id,
                office_name = EXCLUDED.office_name,
                team_id = EXCLUDED.team_id,
                team_name = EXCLUDED.team_name,
                raw_data = EXCLUDED.raw_data,
                synced_at = NOW()
              RETURNING (xmax = 0) AS inserted
            `;

            const result = await sql`
              SELECT (xmax = 0) AS inserted FROM repcard_leaderboard_snapshots 
              WHERE snapshot_date = ${snapshotDate}::date 
                AND leaderboard_name = ${leaderboard.leaderboard_name}
                AND repcard_user_id = ${stat.item_id}
            `;
            const row = result.rows?.[0] || result[0];
            if (row?.inserted) {
              recordsInserted++;
            } else {
              recordsUpdated++;
            }
            recordsFetched++;

          } catch (error) {
            console.error(`[RepCard Sync] Failed to process leaderboard entry for user ${stat.item_id}:`, error);
            recordsFailed++;
          }
        }

      } catch (error) {
        console.error(`[RepCard Sync] Failed to process leaderboard ${leaderboard.leaderboard_name}:`, error);
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
      entityType: 'leaderboards',
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
      entityType: 'leaderboards',
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
 * Sync RepCard Teams (from offices)
 */
export async function syncTeams(): Promise<SyncEntityResult> {
  const startTime = Date.now();
  const syncLogId = await createSyncLog('teams', 'full');

  let recordsFetched = 0;
  let recordsInserted = 0;
  let recordsUpdated = 0;
  let recordsFailed = 0;

  try {
    console.log('[RepCard Sync] Starting sync for teams...');

    // Teams are nested in office data, so we need to fetch offices and extract teams
    const officesResponse = await repcardClient.getOffices();
    if (!officesResponse?.result) {
      throw new Error(`Invalid response structure: ${JSON.stringify(officesResponse)}`);
    }

    const offices = Array.isArray(officesResponse.result.data) ? officesResponse.result.data : 
                    Array.isArray(officesResponse.result) ? officesResponse.result : [];
    
    const allTeams: any[] = [];

    // Extract teams from office data (if teams are in the response)
    for (const office of offices) {
      if ((office as any).teams && Array.isArray((office as any).teams)) {
        for (const team of (office as any).teams) {
          allTeams.push({
            ...team,
            office_id: office.id,
            repcard_office_id: office.id
          });
        }
      }
    }

    console.log(`[RepCard Sync] Got ${allTeams.length} teams`);

    // Also sync teams from user data (users have team_id and team_name)
    const usersWithTeams = await sql`
      SELECT DISTINCT team_id, team_name, office_id
      FROM repcard_users
      WHERE team_id IS NOT NULL
    `;
    
    for (const userTeam of Array.from(usersWithTeams)) {
      if (userTeam.team_id && !allTeams.find(t => t.id === userTeam.team_id)) {
        allTeams.push({
          id: userTeam.team_id,
          team_name: userTeam.team_name,
          office_id: userTeam.office_id
        });
      }
    }

    for (const team of allTeams) {
      try {
        const teamId = team.id || team.item_id || team._id;
        if (!teamId) continue;

        await sql`
          INSERT INTO repcard_teams (
            repcard_team_id,
            team_name,
            office_id,
            repcard_office_id,
            team_logo,
            company_id,
            raw_data
          )
          VALUES (
            ${teamId},
            ${team.team_name || team.teamName || 'Unknown'},
            ${team.office_id || null},
            ${team.repcard_office_id || team.office_id || null},
            ${team.team_logo || team.teamLogo || null},
            ${team.company_id || team.companyId || null},
            ${JSON.stringify(team)}
          )
          ON CONFLICT (repcard_team_id)
          DO UPDATE SET
            team_name = EXCLUDED.team_name,
            office_id = EXCLUDED.office_id,
            repcard_office_id = EXCLUDED.repcard_office_id,
            team_logo = EXCLUDED.team_logo,
            raw_data = EXCLUDED.raw_data,
            synced_at = NOW()
          RETURNING (xmax = 0) AS inserted
        `;

        const result = await sql`
          SELECT (xmax = 0) AS inserted FROM repcard_teams WHERE repcard_team_id = ${teamId}
        `;
        const row = result.rows?.[0] || result[0];
        if (row?.inserted) {
          recordsInserted++;
        } else {
          recordsUpdated++;
        }
        recordsFetched++;

      } catch (error) {
        console.error(`[RepCard Sync] Failed to process team ${team.id}:`, error);
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
      entityType: 'teams',
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
      entityType: 'teams',
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

    // Link RepCard users to users table (SAFE - only updates repcard_user_id)
    //
    // PROTECTED FIELDS - DO NOT UPDATE:
    // ✗ name - User's display name (managed by admins)
    // ✗ role - User's role (managed by admins)
    // ✗ sales_office - Office array for access control (managed by admins)
    //
    // SAFE TO UPDATE:
    // ✓ repcard_user_id - External ID linking to RepCard
    // ✓ last_synced_from_repcard_at - Sync timestamp
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
  skipCustomerNotes?: boolean;
  skipCustomerStatuses?: boolean;
  skipCalendars?: boolean;
  skipCustomFields?: boolean;
  skipLeaderboards?: boolean;
  skipTeams?: boolean;
} = {}): Promise<ComprehensiveSyncResult> {
  const startedAt = new Date().toISOString();
  const overallStartTime = Date.now();
  const MAX_DURATION_MS = 240000; // 4 minutes (leave 1 min buffer before 5 min timeout)

  console.log('[RepCard Comprehensive Sync] Starting comprehensive sync...');
  console.log(`Options:`, options);
  
  // Helper to check if we should continue or exit early
  const checkTimeout = () => {
    const elapsed = Date.now() - overallStartTime;
    if (elapsed > MAX_DURATION_MS) {
      console.log(`[RepCard Comprehensive Sync] ⏱️ Timeout protection: Stopping sync after ${(elapsed / 1000).toFixed(1)}s`);
      return false;
    }
    return true;
  };

  const results: ComprehensiveSyncResult = {
    users: { entityType: 'users', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    offices: { entityType: 'offices', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    customers: { entityType: 'customers', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    appointments: { entityType: 'appointments', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    statusLogs: { entityType: 'status_logs', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    customerAttachments: { entityType: 'customer_attachments', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    appointmentAttachments: { entityType: 'appointment_attachments', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    customerNotes: { entityType: 'customer_notes', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    customerStatuses: { entityType: 'customer_statuses', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    calendars: { entityType: 'calendars', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    customFields: { entityType: 'custom_fields', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    leaderboards: { entityType: 'leaderboards', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    teams: { entityType: 'teams', recordsFetched: 0, recordsInserted: 0, recordsUpdated: 0, recordsFailed: 0, duration: 0 },
    totalDuration: 0,
    startedAt,
    completedAt: ''
  };

  try {
    // Step 1: Sync Users (needed for attribution)
    if (!options.skipUsers) {
      console.log('[RepCard Comprehensive Sync] Step 1/13: Syncing users...');
      results.users = await syncUsers({ incremental: options.incremental });
    }

    // Step 2: Sync Offices (needed for office data)
    if (!options.skipOffices) {
      console.log('[RepCard Comprehensive Sync] Step 2/13: Syncing offices...');
      results.offices = await syncOffices();
    }

    // Step 3: Sync Customers (needed for foreign keys)
    if (!options.skipCustomers && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 3/13: Syncing customers...');
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
    if (!options.skipAppointments && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 4/13: Syncing appointments...');
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
    if (!options.skipStatusLogs && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 5/13: Syncing status logs...');
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
    if (!options.skipCustomerAttachments && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 6/13: Syncing customer attachments...');
      results.customerAttachments = await syncCustomerAttachments({
        fromDate: options.startDate,
        toDate: options.endDate,
        incremental: options.incremental
      });
    }

    // Step 7: Sync Appointment Attachments (depends on appointments)
    if (!options.skipAppointmentAttachments && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 7/13: Syncing appointment attachments...');
      results.appointmentAttachments = await syncAppointmentAttachments({
        fromDate: options.startDate,
        toDate: options.endDate,
        incremental: options.incremental
      });
    }

    // Step 8: Sync Customer Notes (depends on customers)
    if (!options.skipCustomerNotes && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 8/13: Syncing customer notes...');
      results.customerNotes = await syncCustomerNotes({
        startDate: options.startDate,
        endDate: options.endDate,
        incremental: options.incremental
      });
    }

    // Step 9: Sync Customer Status Definitions (rarely changes, can skip if needed)
    if (!options.skipCustomerStatuses && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 9/13: Syncing customer statuses...');
      results.customerStatuses = await syncCustomerStatuses();
    }

    // Step 10: Sync Calendars (rarely changes)
    if (!options.skipCalendars && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 10/13: Syncing calendars...');
      results.calendars = await syncCalendars();
    }

    // Step 11: Sync Custom Fields (rarely changes)
    if (!options.skipCustomFields && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 11/13: Syncing custom fields...');
      results.customFields = await syncCustomFields();
    }

    // Step 12: Sync Leaderboard Snapshots (for historical tracking)
    if (!options.skipLeaderboards && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 12/13: Syncing leaderboard snapshots...');
      results.leaderboards = await syncLeaderboards({
        startDate: options.startDate,
        endDate: options.endDate
      });
    }

    // Step 13: Sync Teams (from offices)
    if (!options.skipTeams && checkTimeout()) {
      console.log('[RepCard Comprehensive Sync] Step 13/13: Syncing teams...');
      results.teams = await syncTeams();
    }

    // Step 14: Link RepCard users to users table (match by email)
    if (!options.skipUsers) {
      console.log('[RepCard Comprehensive Sync] Step 14/14: Linking RepCard users to users table...');
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
      appointmentAttachments: `${results.appointmentAttachments.recordsFetched} fetched, ${results.appointmentAttachments.recordsInserted} inserted, ${results.appointmentAttachments.recordsUpdated} updated`,
      customerNotes: `${results.customerNotes.recordsFetched} fetched, ${results.customerNotes.recordsInserted} inserted, ${results.customerNotes.recordsUpdated} updated`,
      customerStatuses: `${results.customerStatuses.recordsFetched} fetched, ${results.customerStatuses.recordsInserted} inserted, ${results.customerStatuses.recordsUpdated} updated`,
      calendars: `${results.calendars.recordsFetched} fetched, ${results.calendars.recordsInserted} inserted, ${results.calendars.recordsUpdated} updated`,
      customFields: `${results.customFields.recordsFetched} fetched, ${results.customFields.recordsInserted} inserted, ${results.customFields.recordsUpdated} updated`,
      leaderboards: `${results.leaderboards.recordsFetched} fetched, ${results.leaderboards.recordsInserted} inserted, ${results.leaderboards.recordsUpdated} updated`,
      teams: `${results.teams.recordsFetched} fetched, ${results.teams.recordsInserted} inserted, ${results.teams.recordsUpdated} updated`
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

