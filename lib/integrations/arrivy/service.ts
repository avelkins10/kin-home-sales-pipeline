// lib/integrations/arrivy/service.ts
import { arrivyClient } from './client';
import type {
  ArrivyTask,
  ArrivyTaskCreateParams,
  ArrivyTaskUpdateParams,
  ArrivyWebhookPayload,
} from './types';
import {
  upsertArrivyTask,
  upsertArrivyEntity,
  getArrivyTaskByProjectId,
  getArrivyTaskByArrivyId,
  insertArrivyEvent,
  insertArrivyTaskStatus,
  updateArrivyTaskStatus,
  getFieldTrackingTasks,
  getArrivyEntityByEmail,
  getCoordinatorEmailForTask,
  getCrewContactsForTask,
  type ArrivyTaskData,
  type ArrivyEntityData,
  type ArrivyEventData,
  type ArrivyTaskStatusData,
  type FieldTrackingTaskWithDetails,
} from '@/lib/db/arrivy';
import { logError, logInfo, logArrivyWebhook } from '@/lib/logging/logger';
import { createNotification, getNotificationsForUser } from '@/lib/db/notifications';
import { shouldSendEmailNotification } from '@/lib/db/pcNotificationPreferences';
import type { ArrivyFieldAlertMetadata } from '@/lib/types/notification';

/**
 * Arrivy Service Layer
 * Handles business logic for Arrivy integration
 */

// =============================================================================
// TASK SYNCHRONIZATION
// =============================================================================

/**
 * Sync a QuickBase project to Arrivy as a task
 * @param projectId - QuickBase project ID
 * @param recordId - QuickBase record ID
 * @param projectData - Project data from QuickBase
 * @returns Arrivy task with tracker URL
 */
export async function syncProjectToArrivy(
  projectId: string,
  recordId: number,
  projectData: {
    customerName?: string;
    customerPhone?: string;
    customerEmail?: string;
    customerAddress?: string;
    city?: string;
    state?: string;
    zipCode?: string;
    taskType: 'survey' | 'install' | 'inspection' | 'service';
    scheduledStart?: Date;
    scheduledEnd?: Date;
    coordinatorEmail?: string;
    details?: string;
    templateId?: string;
  }
): Promise<{
  task: ArrivyTask;
  trackerUrl: string;
  isNewTask: boolean;
}> {
  try {
    if (!arrivyClient) {
      throw new Error('Arrivy client not configured. Set ARRIVY_AUTH_KEY and ARRIVY_AUTH_TOKEN environment variables.');
    }

    // Check if task already exists
    const existingTask = await getArrivyTaskByProjectId(projectId);
    
    // Look up or create entity for coordinator if email provided
    let coordinatorEntityId: number | undefined;
    if (projectData.coordinatorEmail) {
      try {
        let entity = await getArrivyEntityByEmail(projectData.coordinatorEmail);
        
        if (!entity) {
          // Create entity if it doesn't exist
          const entityResult = await syncEntityFromQuickBase(
            projectData.coordinatorEmail,
            projectData.coordinatorEmail.split('@')[0], // Use email prefix as name
            undefined,
            undefined
          );
          entity = entityResult.entity;
        }
        
        coordinatorEntityId = entity.arrivy_entity_id;
        logInfo(`[Arrivy] Assigned coordinator entity ${coordinatorEntityId} to task`);
      } catch (error) {
        logError('Failed to sync coordinator entity', error as Error, {
          coordinatorEmail: projectData.coordinatorEmail,
        });
        // Continue without entity assignment if it fails
      }
    }
    
    // Map QuickBase data to Arrivy task parameters
    const taskParams = mapQuickBaseToArrivyTask(projectId, recordId, projectData);
    
    // Add coordinator entity to assigned entities if available
    if (coordinatorEntityId) {
      taskParams.entity_ids = [coordinatorEntityId];
    }

    let arrivyTask: ArrivyTask;
    let isNewTask = false;

    if (existingTask) {
      // Update existing task
      logInfo(`[Arrivy] Updating existing task for project ${projectId}`, {
        arrivy_task_id: existingTask.arrivy_task_id,
      });

      arrivyTask = await arrivyClient.updateTask(existingTask.arrivy_task_id, taskParams);
    } else {
      // Create new task
      logInfo(`[Arrivy] Creating new task for project ${projectId}`);
      
      arrivyTask = await arrivyClient.createTask(taskParams);
      isNewTask = true;
    }

    // Generate tracker URL
    const trackerUrl = getCustomerTrackerUrl(arrivyTask.id, arrivyTask.url_safe_id);

    // Store task in local database
    const taskData: ArrivyTaskData = {
      arrivy_task_id: arrivyTask.id,
      url_safe_id: arrivyTask.url_safe_id,
      quickbase_project_id: projectId,
      quickbase_record_id: recordId,
      customer_name: projectData.customerName,
      customer_phone: projectData.customerPhone,
      customer_email: projectData.customerEmail,
      customer_address: projectData.customerAddress,
      task_type: projectData.taskType,
      scheduled_start: projectData.scheduledStart,
      scheduled_end: projectData.scheduledEnd,
      assigned_entity_ids: arrivyTask.entity_ids || [],
      current_status: arrivyTask.status || 'NOT_STARTED',
      tracker_url: trackerUrl,
      template_id: arrivyTask.template_id?.toString(),
      extra_fields: arrivyTask.extra_fields,
      synced_at: new Date(),
    };

    await upsertArrivyTask(taskData);

    logInfo(`[Arrivy] Successfully synced project ${projectId} to Arrivy`, {
      arrivy_task_id: arrivyTask.id,
      tracker_url: trackerUrl,
      isNewTask,
    });

    return {
      task: arrivyTask,
      trackerUrl,
      isNewTask,
    };
  } catch (error) {
    logError('Failed to sync project to Arrivy', error as Error, {
      projectId,
      recordId,
    });
    throw error;
  }
}

/**
 * Map QuickBase project data to Arrivy task parameters
 */
export function mapQuickBaseToArrivyTask(
  projectId: string,
  recordId: number,
  projectData: {
    customerName?: string;
    customerPhone?: string;
    customerEmail?: string;
    customerAddress?: string;
    city?: string;
    state?: string;
    zipCode?: string;
    taskType: string;
    scheduledStart?: Date;
    scheduledEnd?: Date;
    coordinatorEmail?: string;
    details?: string;
    templateId?: string;
  }
): ArrivyTaskCreateParams {
  const params: ArrivyTaskCreateParams = {
    external_id: projectId,
    customer_name: projectData.customerName,
    customer_phone: projectData.customerPhone,
    customer_email: projectData.customerEmail,
    customer_address_line_1: projectData.customerAddress,
    customer_city: projectData.city,
    customer_state: projectData.state,
    customer_zipcode: projectData.zipCode,
    title: `${projectData.taskType} - ${projectData.customerName || 'Customer'}`,
    details: projectData.details,
    start_datetime: projectData.scheduledStart?.toISOString(),
    end_datetime: projectData.scheduledEnd?.toISOString(),
    notifications: {
      sms: true,
      email: true,
    },
    extra_fields: {
      quickbase_project_id: projectId,
      quickbase_record_id: recordId,
      task_type: projectData.taskType,
    },
  };

  // Add template if specified
  if (projectData.templateId) {
    params.template_id = parseInt(projectData.templateId, 10);
  }

  // Calculate duration if both start and end times are provided
  if (projectData.scheduledStart && projectData.scheduledEnd) {
    const durationMs = projectData.scheduledEnd.getTime() - projectData.scheduledStart.getTime();
    params.duration = Math.round(durationMs / (1000 * 60)); // Convert to minutes
  }

  return params;
}

/**
 * Sync entity from QuickBase user or email
 */
export async function syncEntityFromQuickBase(
  email: string,
  name: string,
  phone?: string,
  quickbaseUserId?: string
): Promise<{
  entity: ArrivyEntityData;
  isNew: boolean;
}> {
  try {
    if (!arrivyClient) {
      throw new Error('Arrivy client not configured');
    }

    // Check if entity already exists in local database
    const existingEntity = await getArrivyEntityByEmail(email);

    if (existingEntity) {
      logInfo(`[Arrivy] Entity already exists for ${email}`, {
        arrivy_entity_id: existingEntity.arrivy_entity_id,
      });
      return {
        entity: existingEntity,
        isNew: false,
      };
    }

    // Create new entity in Arrivy
    logInfo(`[Arrivy] Creating new entity for ${email}`);
    
    const arrivyEntity = await arrivyClient.createEntity({
      name,
      email,
      phone,
      type: 'CREW',
      extra_fields: {
        quickbase_user_id: quickbaseUserId,
      },
    });

    // Store entity in local database
    const entityData: ArrivyEntityData = {
      arrivy_entity_id: arrivyEntity.id,
      name: arrivyEntity.name || name,
      email: arrivyEntity.email || email,
      phone: arrivyEntity.phone || phone,
      entity_type: arrivyEntity.type,
      quickbase_user_id: quickbaseUserId,
      extra_fields: arrivyEntity.extra_fields,
    };

    await upsertArrivyEntity(entityData);

    logInfo(`[Arrivy] Successfully created entity for ${email}`, {
      arrivy_entity_id: arrivyEntity.id,
    });

    return {
      entity: entityData,
      isNew: true,
    };
  } catch (error) {
    logError('Failed to sync entity from QuickBase', error as Error, {
      email,
      name,
    });
    throw error;
  }
}

/**
 * Generate customer tracker URL
 */
export function getCustomerTrackerUrl(taskId: number, urlSafeId: string): string {
  const companyName = process.env.ARRIVY_COMPANY_NAME || 'company';
  return `https://app.arrivy.com/live/track/${encodeURIComponent(companyName)}/${urlSafeId}`;
}

/**
 * Generate business/internal tracker URL for operations team
 */
export function getBusinessTrackerUrl(taskId: number): string {
  return `https://app.arrivy.com/dashboard/task/${taskId}`;
}

/**
 * Check if a task has QuickBase association
 */
export function hasQuickBaseAssociation(task: { quickbase_project_id?: string | null; quickbase_record_id?: number | null }): boolean {
  return !!task.quickbase_project_id && 
         task.quickbase_project_id !== '' && 
         !task.quickbase_project_id.startsWith('ARRIVY-');
}

// =============================================================================
// WEBHOOK PROCESSING
// =============================================================================

/**
 * Ensure task exists in database before processing event
 * Fetches task from Arrivy API if not found locally
 */
async function ensureTaskExists(arrivyTaskId: number, eventType: string): Promise<boolean> {
  try {
    // Check if task exists in database
    let task = await getArrivyTaskByArrivyId(arrivyTaskId);

    if (task) {
      return true; // Task exists, we're good
    }

    // Task doesn't exist - try to fetch from Arrivy API
    logInfo(`[Arrivy] Task ${arrivyTaskId} not found, fetching from API`, {
      arrivy_task_id: arrivyTaskId,
      event_type: eventType,
    });

    if (!arrivyClient) {
      logError('[Arrivy] Cannot fetch task - Arrivy client not configured', new Error('Arrivy client not configured'), {
        arrivy_task_id: arrivyTaskId,
      });
      return false;
    }

    // Fetch task from Arrivy API
    const arrivyTask = await arrivyClient.getTask(arrivyTaskId);
    if (!arrivyTask) {
      logError('[Arrivy] Task not found in Arrivy API', new Error('Task not found in API'), {
        arrivy_task_id: arrivyTaskId,
      });
      return false;
    }

    // Helper function to format address
    const formatAddress = (task: ArrivyTask): string | undefined => {
      const parts = [
        task.customer_address_line_1,
        task.customer_city,
        task.customer_state,
        task.customer_zipcode
      ].filter(Boolean);
      return parts.length > 0 ? parts.join(', ') : undefined;
    };

    // Helper function to extract task type
    const extractTaskType = (task: ArrivyTask): string => {
      if (task.extra_fields?.task_type) {
        return task.extra_fields.task_type;
      }
      // Try to infer from title
      const title = task.title?.toLowerCase() || '';
      if (title.includes('survey')) return 'survey';
      if (title.includes('install')) return 'install';
      if (title.includes('inspection')) return 'inspection';
      return 'service'; // default
    };

    // Generate tracker URL
    const trackerUrl = getCustomerTrackerUrl(arrivyTask.id, arrivyTask.url_safe_id);

    // Create task in database
    const taskData: ArrivyTaskData = {
      arrivy_task_id: arrivyTask.id,
      url_safe_id: arrivyTask.url_safe_id,
      quickbase_project_id: arrivyTask.external_id ?? null,
      quickbase_record_id: null,
      customer_name: arrivyTask.customer_name,
      customer_phone: arrivyTask.customer_phone,
      customer_email: arrivyTask.customer_email,
      customer_address: formatAddress(arrivyTask),
      task_type: extractTaskType(arrivyTask),
      scheduled_start: arrivyTask.start_datetime ? new Date(arrivyTask.start_datetime) : undefined,
      scheduled_end: arrivyTask.end_datetime ? new Date(arrivyTask.end_datetime) : undefined,
      assigned_entity_ids: arrivyTask.entity_ids || [],
      current_status: arrivyTask.status || 'NOT_STARTED',
      tracker_url: trackerUrl,
      template_id: arrivyTask.template_id?.toString(),
      extra_fields: arrivyTask.extra_fields,
      synced_at: new Date(),
    };

    await upsertArrivyTask(taskData);

    logInfo('[Arrivy] Successfully fetched and created task from API', {
      arrivy_task_id: arrivyTaskId,
      customer_name: arrivyTask.customer_name,
    });

    return true;
  } catch (error) {
    logError('[Arrivy] Failed to ensure task exists', error as Error, {
      arrivy_task_id: arrivyTaskId,
      event_type: eventType,
    });
    return false;
  }
}

/**
 * Process incoming webhook event from Arrivy
 */
export async function processWebhookEvent(
  payload: ArrivyWebhookPayload
): Promise<{
  success: boolean;
  eventId: number;
  notificationCreated?: boolean;
  duplicate?: boolean;
}> {
  try {
    const {
      EVENT_ID,
      EVENT_TYPE,
      EVENT_SUB_TYPE,
      EVENT_TIME,
      OBJECT_ID,
      REPORTER_ID,
      REPORTER_NAME,
      TITLE,
      MESSAGE,
      OBJECT_FIELDS,
      EXTRA_FIELDS,
      IS_TRANSIENT_STATUS,
    } = payload;

    // Log webhook event
    logArrivyWebhook(EVENT_TYPE, EVENT_SUB_TYPE, {
      EVENT_ID,
      OBJECT_ID,
      REPORTER_ID,
      REPORTER_NAME,
      payload,
    });

    // Ensure task exists in database before inserting event
    // This prevents foreign key constraint violations
    const taskExists = await ensureTaskExists(OBJECT_ID, EVENT_TYPE);
    if (!taskExists) {
      logError('[Arrivy] Cannot process event - task does not exist and could not be created', new Error('Task unavailable'), {
        event_id: EVENT_ID,
        event_type: EVENT_TYPE,
        arrivy_task_id: OBJECT_ID,
      });
      // Return success to prevent webhook retries
      return {
        success: true,
        eventId: EVENT_ID,
        notificationCreated: false,
      };
    }

    // Store event in database - returns null if duplicate
    const eventData: ArrivyEventData = {
      event_id: EVENT_ID,
      event_type: EVENT_TYPE,
      event_sub_type: EVENT_SUB_TYPE || null,
      event_time: new Date(EVENT_TIME),
      arrivy_task_id: OBJECT_ID,
      reporter_id: REPORTER_ID,
      reporter_name: REPORTER_NAME,
      title: TITLE,
      message: MESSAGE,
      object_fields: OBJECT_FIELDS,
      extra_fields: EXTRA_FIELDS,
      is_transient: IS_TRANSIENT_STATUS || false,
    };

    const insertedEvent = await insertArrivyEvent(eventData);

    // If null, this is a duplicate webhook delivery - skip processing
    if (!insertedEvent) {
      logInfo(`[Arrivy] Skipping duplicate webhook event ${EVENT_ID}`);
      return {
        success: true,
        eventId: EVENT_ID,
        duplicate: true,
      };
    }

    // Handle different event types (TASK_CREATED already handled above)
    let notificationCreated = false;

    switch (EVENT_TYPE) {
      case 'TASK_CREATED':
        // Already handled above to ensure task exists before event insertion
        break;

      case 'TASK_STATUS':
        await handleTaskStatusEvent(payload);
        notificationCreated = await createNotificationForStatusEvent(payload);
        break;

      case 'CREW_ASSIGNED':
        await handleCrewAssignedEvent(payload);
        break;

      case 'ARRIVING':
        // Only normalize status to ENROUTE, don't create notification
        await handleCriticalEvent(payload);
        break;

      case 'LATE':
      case 'NOSHOW':
        await handleCriticalEvent(payload);
        notificationCreated = await createNotificationForCriticalEvent(payload);
        break;

      case 'TASK_RATING':
        await handleTaskRatingEvent(payload);
        break;

      default:
        logInfo(`[Arrivy] Unhandled event type: ${EVENT_TYPE}`);
    }

    return {
      success: true,
      eventId: EVENT_ID,
      notificationCreated,
    };
  } catch (error) {
    logError('Failed to process Arrivy webhook event', error as Error, {
      event_id: payload.EVENT_ID,
      event_type: payload.EVENT_TYPE,
    });
    throw error;
  }
}

/**
 * Handle TASK_STATUS event
 */
async function handleTaskStatusEvent(payload: ArrivyWebhookPayload): Promise<void> {
  const { OBJECT_ID, EVENT_SUB_TYPE, EVENT_TIME, REPORTER_ID, REPORTER_NAME, MESSAGE } = payload;

  // Update task status in database
  if (EVENT_SUB_TYPE) {
    await updateArrivyTaskStatus(OBJECT_ID, EVENT_SUB_TYPE);
  }

  // Store status update
  const statusData: ArrivyTaskStatusData = {
    arrivy_task_id: OBJECT_ID,
    status_type: EVENT_SUB_TYPE || 'UNKNOWN',
    reporter_id: REPORTER_ID,
    reporter_name: REPORTER_NAME,
    reported_at: new Date(EVENT_TIME),
    notes: MESSAGE,
    has_attachments: payload.OBJECT_FIELDS?.HAS_ATTACHMENT || false,
    visible_to_customer: payload.OBJECT_FIELDS?.RESPONSE_FROM_CUSTOMER !== false,
    source: 'webhook',
  };

  await insertArrivyTaskStatus(statusData);

  logInfo(`[Arrivy] Handled TASK_STATUS event`, {
    arrivy_task_id: OBJECT_ID,
    status_type: EVENT_SUB_TYPE,
  });
}

/**
 * Handle TASK_CREATED webhook event
 * 
 * When a new task is created in Arrivy (via web, mobile app, or API),
 * this handler fetches the complete task details and stores them in
 * the local database for dashboard visibility.
 * 
 * @param payload - Webhook payload from Arrivy
 * 
 * @remarks
 * - Fetches full task details via API (webhook payload is minimal)
 * - Generates customer tracker URL for immediate use
 * - Uses upsert to handle duplicate webhook deliveries
 * - Sets QuickBase IDs to null for Arrivy-originated tasks
 * - Does not create notifications (informational event only)
 * 
 * @example
 * // Webhook payload structure:
 * {
 *   EVENT_TYPE: 'TASK_CREATED',
 *   OBJECT_ID: 5678633960079360,
 *   OBJECT_EXTERNAL_ID: 'PROJECT-123',
 *   REPORTER_NAME: 'John Smith',
 *   EVENT_TIME: '2025-10-28T14:30:00Z'
 * }
 */
async function handleTaskCreatedEvent(payload: ArrivyWebhookPayload): Promise<void> {
  const { OBJECT_ID, OBJECT_EXTERNAL_ID, REPORTER_NAME } = payload;

  try {
    // Check if Arrivy client is configured
    if (!arrivyClient) {
      logInfo('[Arrivy] Arrivy client not configured, skipping TASK_CREATED event processing', {
        arrivy_task_id: OBJECT_ID,
      });
      return;
    }

    // Fetch full task details from Arrivy API with retry for eventual consistency
    logInfo('[Arrivy] Fetching task details for TASK_CREATED event', {
      arrivy_task_id: OBJECT_ID,
      external_id: OBJECT_EXTERNAL_ID,
    });

    let task: ArrivyTask | null = null;
    const maxAttempts = 3;
    const retryDelayMs = 400; // 400ms backoff between attempts
    
    for (let attempt = 1; attempt <= maxAttempts; attempt++) {
      try {
        task = await arrivyClient.getTask(OBJECT_ID);
        if (attempt > 1) {
          logInfo('[Arrivy] Successfully fetched task details on retry', {
            arrivy_task_id: OBJECT_ID,
            attempt,
          });
        }
        break; // Success - exit retry loop
      } catch (error) {
        if (attempt === maxAttempts) {
          // Final attempt failed - log and propagate
          logError(`Failed to fetch task details after ${maxAttempts} attempts`, error as Error, {
            arrivy_task_id: OBJECT_ID,
            external_id: OBJECT_EXTERNAL_ID,
          });
          throw error; // Will be caught by outer catch block
        }
        // Not final attempt - log and retry
        logInfo(`[Arrivy] Fetch attempt ${attempt} failed, retrying...`, {
          arrivy_task_id: OBJECT_ID,
          attempt,
          next_attempt_in_ms: retryDelayMs,
        });
        await new Promise(resolve => setTimeout(resolve, retryDelayMs));
      }
    }

    if (!task) {
      throw new Error('Failed to fetch task details - no task returned');
    }

    // Helper function to format address
    const formatAddress = (task: ArrivyTask): string | undefined => {
      const parts = [
        task.customer_address_line_1,
        task.customer_city,
        task.customer_state,
        task.customer_zipcode
      ].filter(Boolean);
      return parts.length > 0 ? parts.join(', ') : undefined;
    };

    // Helper function to extract task type
    const extractTaskType = (task: ArrivyTask): string => {
      if (task.extra_fields?.task_type) {
        return task.extra_fields.task_type;
      }
      // Try to infer from title
      const title = task.title?.toLowerCase() || '';
      if (title.includes('survey')) return 'survey';
      if (title.includes('install')) return 'install';
      if (title.includes('inspection')) return 'inspection';
      return 'service'; // default
    };

    // Generate tracker URL
    const trackerUrl = getCustomerTrackerUrl(task.id, task.url_safe_id);

    // Map task data to database schema
    const taskData: ArrivyTaskData = {
      arrivy_task_id: task.id,
      url_safe_id: task.url_safe_id,
      quickbase_project_id: task.external_id ?? OBJECT_EXTERNAL_ID ?? null,
      quickbase_record_id: null, // Arrivy-originated tasks don't have QB record IDs
      customer_name: task.customer_name,
      customer_phone: task.customer_phone,
      customer_email: task.customer_email,
      customer_address: formatAddress(task),
      task_type: extractTaskType(task),
      scheduled_start: task.start_datetime ? new Date(task.start_datetime) : undefined,
      scheduled_end: task.end_datetime ? new Date(task.end_datetime) : undefined,
      assigned_entity_ids: task.entity_ids || [],
      current_status: task.status || 'NOT_STARTED',
      tracker_url: trackerUrl,
      template_id: task.template_id?.toString(),
      extra_fields: task.extra_fields,
      synced_at: new Date(),
    };

    // Store in database (upsert handles duplicates)
    await upsertArrivyTask(taskData);

    logInfo('[Arrivy] Handled TASK_CREATED event', {
      arrivy_task_id: task.id,
      external_id: task.external_id || 'none',
      customer_name: task.customer_name,
      task_type: taskData.task_type,
    });
  } catch (error) {
    logError('Failed to handle TASK_CREATED event', error as Error, {
      arrivy_task_id: OBJECT_ID,
      external_id: OBJECT_EXTERNAL_ID,
      reporter_name: REPORTER_NAME,
    });
    // Don't throw - prevents webhook retries on non-critical errors
  }
}

/**
 * Handle CREW_ASSIGNED event
 */
async function handleCrewAssignedEvent(payload: ArrivyWebhookPayload): Promise<void> {
  logInfo(`[Arrivy] Crew assigned to task`, {
    arrivy_task_id: payload.OBJECT_ID,
    reporter_name: payload.REPORTER_NAME,
  });

  // Could update assigned entities here if needed
}

/**
 * Handle critical events (ARRIVING, LATE, NOSHOW)
 */
async function handleCriticalEvent(payload: ArrivyWebhookPayload): Promise<void> {
  const { EVENT_TYPE, OBJECT_ID, EVENT_SUB_TYPE } = payload;

  // Normalize ARRIVING to ENROUTE for task progress tracking
  // Keep LATE and NOSHOW as-is since they're exception conditions
  let normalizedStatus: string;
  if (EVENT_TYPE === 'ARRIVING') {
    normalizedStatus = 'ENROUTE';
  } else {
    normalizedStatus = EVENT_TYPE; // LATE or NOSHOW
  }

  // Update task status with normalized value
  await updateArrivyTaskStatus(OBJECT_ID, normalizedStatus);

  logInfo(`[Arrivy] Handled critical event: ${EVENT_TYPE} -> ${normalizedStatus}`, {
    arrivy_task_id: OBJECT_ID,
    event_sub_type: EVENT_SUB_TYPE,
    normalized_status: normalizedStatus,
  });
}

/**
 * Handle TASK_RATING event
 */
async function handleTaskRatingEvent(payload: ArrivyWebhookPayload): Promise<void> {
  logInfo(`[Arrivy] Task rated`, {
    arrivy_task_id: payload.OBJECT_ID,
    rating_type: payload.OBJECT_FIELDS?.RATING_TYPE,
  });

  // Could store rating in database or sync back to QuickBase
}

// =============================================================================
// NOTIFICATION HELPERS
// =============================================================================

/**
 * Get notification type for Arrivy event
 */
function getNotificationTypeForArrivyEvent(eventType: string, eventSubType?: string): string {
  const upperEventType = eventType.toUpperCase();
  const upperSubType = eventSubType?.toUpperCase();
  
  // Critical events
  if (upperEventType === 'LATE') return 'arrivy_task_late';
  if (upperEventType === 'NOSHOW') return 'arrivy_task_noshow';
  
  // Status events
  if (upperEventType === 'STATUS_CHANGE' && upperSubType) {
    if (upperSubType === 'EXCEPTION') return 'arrivy_task_exception';
    if (upperSubType === 'CANCELLED') return 'arrivy_task_cancelled';
    if (upperSubType === 'STARTED') return 'arrivy_task_started';
    if (upperSubType === 'COMPLETE') return 'arrivy_task_complete';
  }
  
  return 'system_alert'; // Fallback
}

/**
 * Get notification title for Arrivy event
 */
function getNotificationTitleForArrivyEvent(
  eventType: string,
  customerName: string,
  taskType: string
): string {
  const upperEventType = eventType.toUpperCase();
  const taskTypeLabel = taskType.charAt(0).toUpperCase() + taskType.slice(1);
  
  switch (upperEventType) {
    case 'LATE':
      return `${taskTypeLabel} Running Late: ${customerName}`;
    case 'NOSHOW':
      return `Customer No-Show: ${customerName}`;
    case 'EXCEPTION':
      return `Field Exception: ${customerName}`;
    case 'CANCELLED':
      return `${taskTypeLabel} Cancelled: ${customerName}`;
    case 'STARTED':
      return `${taskTypeLabel} Started: ${customerName}`;
    case 'COMPLETE':
      return `${taskTypeLabel} Complete: ${customerName}`;
    default:
      return `${taskTypeLabel} Update: ${customerName}`;
  }
}

// =============================================================================
// NOTIFICATION CREATION
// =============================================================================

/**
 * Create notification for status event if needed
 */
async function createNotificationForStatusEvent(payload: ArrivyWebhookPayload): Promise<boolean> {
  const { EVENT_SUB_TYPE, OBJECT_ID, REPORTER_NAME, MESSAGE } = payload;

  // Only create notifications for certain status types
  const notifiableStatuses = ['STARTED', 'COMPLETE', 'EXCEPTION', 'CANCELLED'];

  if (!EVENT_SUB_TYPE || !notifiableStatuses.includes(EVENT_SUB_TYPE)) {
    return false;
  }

  try {
    // Get task to find associated project coordinator
    const task = await getArrivyTaskByArrivyId(OBJECT_ID);
    if (!task) {
      logInfo('[Arrivy] Task not found for status event', { arrivy_task_id: OBJECT_ID });
      return false;
    }

    // Get coordinator email from QuickBase project
    const coordinatorEmail = await getCoordinatorEmailForTask(task);
    if (!coordinatorEmail) {
      logInfo('[Arrivy] No coordinator email found for status event', {
        arrivy_task_id: OBJECT_ID,
        quickbase_project_id: task.quickbase_project_id,
      });
      return false;
    }

    // Determine notification type
    const notificationType = getNotificationTypeForArrivyEvent('STATUS_CHANGE', EVENT_SUB_TYPE);

    // Determine priority
    const priority = ['EXCEPTION', 'CANCELLED'].includes(EVENT_SUB_TYPE) ? 'critical' : 'normal';

    // Get crew member names
    const crewIds = task.assigned_entity_ids || [];
    const crewContacts = await getCrewContactsForTask(crewIds);
    const crewNames = crewContacts.map(c => c.name);

    // Build notification metadata
    const metadata: ArrivyFieldAlertMetadata = {
      arrivy_task_id: task.arrivy_task_id,
      event_type: EVENT_SUB_TYPE,
      task_type: task.task_type || 'service',
      customer_name: task.customer_name || 'Unknown Customer',
      customer_phone: task.customer_phone || null,
      scheduled_start: task.scheduled_start?.toISOString() || new Date().toISOString(),
      current_status: task.current_status || EVENT_SUB_TYPE,
      assigned_crew: crewNames,
      tracker_url: task.tracker_url || '',
      business_tracker_url: getBusinessTrackerUrl(task.arrivy_task_id),
      reporter_name: REPORTER_NAME || null,
      event_message: MESSAGE || `Task status changed to ${EVENT_SUB_TYPE}`,
    };

    // Generate title
    const title = getNotificationTitleForArrivyEvent(
      EVENT_SUB_TYPE,
      metadata.customer_name,
      metadata.task_type
    );

    // Check for duplicate notification
    const recentNotifications = await getNotificationsForUser(coordinatorEmail, { limit: 10 });
    const isDuplicate = recentNotifications.some(
      n => n.type === notificationType && 
           n.metadata?.arrivy_task_id === task.arrivy_task_id &&
           (Date.now() - new Date(n.created_at).getTime()) < 3600000 // Within last hour
    );

    if (isDuplicate) {
      logInfo('[Arrivy] Duplicate notification prevented', {
        coordinator: coordinatorEmail,
        notification_type: notificationType,
        arrivy_task_id: task.arrivy_task_id,
      });
      return false;
    }

    // Create notification
    const notification = await createNotification({
      user_id: coordinatorEmail,
      project_id: task.quickbase_record_id || 0,
      type: notificationType as any,
      priority,
      source: 'arrivy',
      title,
      message: metadata.event_message,
      metadata,
      icon: priority === 'critical' ? 'alert-triangle' : 'info',
      color: priority === 'critical' ? 'red' : 'blue',
      action_url: `/operations/scheduling?task=${task.arrivy_task_id}`,
    });

    logInfo('[Arrivy] Status notification created', {
      notification_id: notification.id,
      coordinator: coordinatorEmail,
      type: notificationType,
    });

    // Send email notification if preferences allow
    // Email sending errors should not block notification creation
    try {
      // Check user preferences for email delivery
      const shouldSendEmail = await shouldSendEmailNotification(coordinatorEmail, notificationType);
      
      if (shouldSendEmail) {
        const { sendArrivyFieldAlertEmail } = await import('@/lib/utils/email-helpers');
        await sendArrivyFieldAlertEmail(
          coordinatorEmail,
          coordinatorEmail.split('@')[0], // Simple name extraction
          EVENT_SUB_TYPE as any,
          metadata.customer_name,
          metadata.task_type,
          new Date(metadata.scheduled_start).toLocaleString(),
          crewNames,
          metadata.event_message,
          metadata.tracker_url,
          metadata.business_tracker_url
        );
      } else {
        logInfo('[Arrivy] Email skipped due to user preferences', {
          coordinator: coordinatorEmail,
          notification_type: notificationType,
          notification_id: notification.id,
        });
      }
    } catch (emailError) {
      logError('[Arrivy] Failed to send status event email', emailError as Error, {
        notification_id: notification.id,
      });
    }

    return true;
  } catch (error) {
    logError('Failed to create notification for status event', error as Error, {
      arrivy_task_id: OBJECT_ID,
      status_type: EVENT_SUB_TYPE,
    });
    return false;
  }
}

/**
 * Create notification for critical event (LATE, NOSHOW, ARRIVING)
 */
async function createNotificationForCriticalEvent(payload: ArrivyWebhookPayload): Promise<boolean> {
  const { EVENT_TYPE, OBJECT_ID, MESSAGE, TITLE, REPORTER_NAME } = payload;

  try {
    // Get task to find associated project
    const task = await getArrivyTaskByArrivyId(OBJECT_ID);
    if (!task) {
      logInfo('[Arrivy] Task not found for critical event', { arrivy_task_id: OBJECT_ID });
      return false;
    }

    // Get coordinator email from QuickBase project
    const coordinatorEmail = await getCoordinatorEmailForTask(task);
    if (!coordinatorEmail) {
      logInfo('[Arrivy] No coordinator email found for critical event', {
        arrivy_task_id: OBJECT_ID,
        quickbase_project_id: task.quickbase_project_id,
      });
      return false;
    }

    // Determine notification type
    const notificationType = getNotificationTypeForArrivyEvent(EVENT_TYPE);

    // All critical events have critical priority except CANCELLED
    const priority = EVENT_TYPE === 'CANCELLED' ? 'normal' : 'critical';

    // Get crew member names
    const crewIds = task.assigned_entity_ids || [];
    const crewContacts = await getCrewContactsForTask(crewIds);
    const crewNames = crewContacts.map(c => c.name);

    // Build notification metadata
    const metadata: ArrivyFieldAlertMetadata = {
      arrivy_task_id: task.arrivy_task_id,
      event_type: EVENT_TYPE,
      task_type: task.task_type || 'service',
      customer_name: task.customer_name || 'Unknown Customer',
      customer_phone: task.customer_phone || null,
      scheduled_start: task.scheduled_start?.toISOString() || new Date().toISOString(),
      current_status: task.current_status || EVENT_TYPE,
      assigned_crew: crewNames,
      tracker_url: task.tracker_url || '',
      business_tracker_url: getBusinessTrackerUrl(task.arrivy_task_id),
      reporter_name: REPORTER_NAME || null,
      event_message: MESSAGE || TITLE || `Critical event: ${EVENT_TYPE}`,
    };

    // Generate title
    const title = getNotificationTitleForArrivyEvent(
      EVENT_TYPE,
      metadata.customer_name,
      metadata.task_type
    );

    // Check for duplicate notification
    const recentNotifications = await getNotificationsForUser(coordinatorEmail, { limit: 10 });
    const isDuplicate = recentNotifications.some(
      n => n.type === notificationType && 
           n.metadata?.arrivy_task_id === task.arrivy_task_id &&
           (Date.now() - new Date(n.created_at).getTime()) < 3600000 // Within last hour
    );

    if (isDuplicate) {
      logInfo('[Arrivy] Duplicate notification prevented', {
        coordinator: coordinatorEmail,
        notification_type: notificationType,
        arrivy_task_id: task.arrivy_task_id,
      });
      return false;
    }

    // Create notification
    const notification = await createNotification({
      user_id: coordinatorEmail,
      project_id: task.quickbase_record_id || 0,
      type: notificationType as any,
      priority,
      source: 'arrivy',
      title,
      message: metadata.event_message,
      metadata,
      icon: 'alert-triangle',
      color: priority === 'critical' ? 'red' : 'orange',
      action_url: `/operations/scheduling?task=${task.arrivy_task_id}`,
    });

    logInfo('[Arrivy] Critical notification created', {
      notification_id: notification.id,
      coordinator: coordinatorEmail,
      type: notificationType,
    });

    // Send email notification if preferences allow
    // Email sending errors should not block notification creation
    try {
      // Check user preferences for email delivery
      const shouldSendEmail = await shouldSendEmailNotification(coordinatorEmail, notificationType);
      
      if (shouldSendEmail) {
        const { sendArrivyFieldAlertEmail } = await import('@/lib/utils/email-helpers');
        await sendArrivyFieldAlertEmail(
          coordinatorEmail,
          coordinatorEmail.split('@')[0], // Simple name extraction
          EVENT_TYPE as any,
          metadata.customer_name,
          metadata.task_type,
          new Date(metadata.scheduled_start).toLocaleString(),
          crewNames,
          metadata.event_message,
          metadata.tracker_url,
          metadata.business_tracker_url
        );
      } else {
        logInfo('[Arrivy] Email skipped due to user preferences', {
          coordinator: coordinatorEmail,
          notification_type: notificationType,
          notification_id: notification.id,
        });
      }
    } catch (emailError) {
      logError('[Arrivy] Failed to send critical event email', emailError as Error, {
        notification_id: notification.id,
      });
    }

    return true;
  } catch (error) {
    logError('Failed to create notification for critical event', error as Error, {
      arrivy_task_id: OBJECT_ID,
      event_type: EVENT_TYPE,
    });
    return false;
  }
}

// =============================================================================
// FIELD TRACKING DATA
// =============================================================================

/**
 * Get field tracking data for dashboard
 */
export async function getFieldTrackingData(
  coordinatorEmail?: string,
  role?: string
): Promise<{
  tasks: FieldTrackingTaskWithDetails[];
  metrics: {
    total: number;
    inProgress: number;
    completedToday: number;
    delayed: number;
    avgCompletionTime?: number;
  };
}> {
  try {
    // Get tasks with optional coordinator filter
    const tasks = await getFieldTrackingTasks({
      coordinatorEmail,
      limit: 100,
    });

    // Calculate metrics
    const now = new Date();
    const todayStart = new Date(now.getFullYear(), now.getMonth(), now.getDate());

    // Calculate average completion time from completed tasks
    let avgCompletionTime: number | undefined;
    const completedTasks = tasks.filter(t => 
      t.current_status === 'COMPLETE' && 
      t.scheduled_start && 
      t.latest_status_time
    );

    if (completedTasks.length > 0) {
      const completionTimes = completedTasks
        .map(t => {
          const start = new Date(t.scheduled_start!).getTime();
          const complete = new Date(t.latest_status_time!).getTime();
          return (complete - start) / (1000 * 60); // Convert to minutes
        })
        .filter(time => time > 0 && time < 24 * 60); // Filter out invalid times
      
      if (completionTimes.length > 0) {
        avgCompletionTime = Math.round(
          completionTimes.reduce((sum, time) => sum + time, 0) / completionTimes.length
        );
      }
    }

    const metrics = {
      total: tasks.length,
      inProgress: tasks.filter(t => ['ENROUTE', 'STARTED'].includes(t.current_status || '')).length,
      completedToday: tasks.filter(t => 
        t.current_status === 'COMPLETE' && 
        t.latest_status_time && 
        new Date(t.latest_status_time) >= todayStart
      ).length,
      delayed: tasks.filter(t => 
        t.scheduled_start && 
        new Date(t.scheduled_start) < now && 
        !['COMPLETE', 'CANCELLED'].includes(t.current_status || '')
      ).length,
      avgCompletionTime,
      linkedToQuickBase: tasks.filter(t => hasQuickBaseAssociation(t)).length,
    };

    return {
      tasks,
      metrics,
    };
  } catch (error) {
    logError('Failed to get field tracking data', error as Error, {
      coordinatorEmail,
      role,
    });
    throw error;
  }
}

