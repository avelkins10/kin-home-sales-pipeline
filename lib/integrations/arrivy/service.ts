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
  type ArrivyTaskData,
  type ArrivyEntityData,
  type ArrivyEventData,
  type ArrivyTaskStatusData,
  type FieldTrackingTaskWithDetails,
} from '@/lib/db/arrivy';
import { logError, logInfo, logArrivyWebhook } from '@/lib/logging/logger';
import { createNotification } from '@/lib/db/notifications';

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
    
    // Map QuickBase data to Arrivy task parameters
    const taskParams = mapQuickBaseToArrivyTask(projectId, recordId, projectData);

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
  return `https://app.arrivy.com/live/track/${companyName}/${urlSafeId}`;
}

// =============================================================================
// WEBHOOK PROCESSING
// =============================================================================

/**
 * Process incoming webhook event from Arrivy
 */
export async function processWebhookEvent(
  payload: ArrivyWebhookPayload
): Promise<{
  success: boolean;
  eventId: number;
  notificationCreated?: boolean;
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

    // Store event in database
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

    await insertArrivyEvent(eventData);

    // Handle different event types
    let notificationCreated = false;

    switch (EVENT_TYPE) {
      case 'TASK_STATUS':
        await handleTaskStatusEvent(payload);
        notificationCreated = await createNotificationForStatusEvent(payload);
        break;

      case 'CREW_ASSIGNED':
        await handleCrewAssignedEvent(payload);
        break;

      case 'ARRIVING':
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

  // Update task status
  await updateArrivyTaskStatus(OBJECT_ID, EVENT_TYPE);

  logInfo(`[Arrivy] Handled critical event: ${EVENT_TYPE}`, {
    arrivy_task_id: OBJECT_ID,
    event_sub_type: EVENT_SUB_TYPE,
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
      return false;
    }

    // Note: In a real implementation, you would look up the coordinator's user_id
    // from the QuickBase project data. For now, we'll skip notification creation
    // unless we have that mapping.

    logInfo(`[Arrivy] Would create notification for status event`, {
      arrivy_task_id: OBJECT_ID,
      status_type: EVENT_SUB_TYPE,
      project_id: task.quickbase_project_id,
    });

    return false; // Set to true when coordinator mapping is implemented
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
  const { EVENT_TYPE, OBJECT_ID, MESSAGE, TITLE } = payload;

  try {
    // Get task to find associated project
    const task = await getArrivyTaskByArrivyId(OBJECT_ID);
    if (!task) {
      return false;
    }

    // Note: In a real implementation, you would create a notification for the
    // project coordinator. For now, we'll just log it.

    logInfo(`[Arrivy] Would create notification for critical event`, {
      arrivy_task_id: OBJECT_ID,
      event_type: EVENT_TYPE,
      project_id: task.quickbase_project_id,
    });

    return false; // Set to true when coordinator mapping is implemented
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

