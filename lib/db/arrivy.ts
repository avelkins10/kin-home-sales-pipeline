// lib/db/arrivy.ts
import { sql } from '@vercel/postgres';
import { logError, logInfo } from '@/lib/logging/logger';
import { 
  CrewPerformanceMetrics, 
  CrewLeaderboard, 
  CrewPerformanceTrend 
} from '@/lib/types/operations';

/**
 * Database access functions for Arrivy integration
 */

// =============================================================================
// ARRIVY TASKS
// =============================================================================

export interface ArrivyTaskData {
  arrivy_task_id: number;
  url_safe_id: string;
  quickbase_project_id?: string | null;
  quickbase_record_id?: number | null;
  customer_name?: string | null;
  customer_phone?: string | null;
  customer_email?: string | null;
  customer_address?: string | null;
  task_type?: string | null;
  scheduled_start?: Date | null;
  scheduled_end?: Date | null;
  start_datetime_window_start?: Date | null;
  start_datetime_window_end?: Date | null;
  assigned_entity_ids?: number[] | null;
  current_status?: string | null;
  tracker_url?: string | null;
  template_id?: string | null;
  extra_fields?: Record<string, any> | null;
  synced_at?: Date | null;
}

export interface ArrivyTaskRecord extends ArrivyTaskData {
  id: number;
  quickbase_project_id: string | null;
  quickbase_record_id: number | null;
  created_at: Date;
  updated_at: Date;
}

/**
 * Insert or update an Arrivy task
 */
export async function upsertArrivyTask(taskData: ArrivyTaskData): Promise<ArrivyTaskRecord> {
  try {
    const result = await sql<ArrivyTaskRecord>`
      INSERT INTO arrivy_tasks (
        arrivy_task_id,
        url_safe_id,
        quickbase_project_id,
        quickbase_record_id,
        customer_name,
        customer_phone,
        customer_email,
        customer_address,
        task_type,
        scheduled_start,
        scheduled_end,
        start_datetime_window_start,
        start_datetime_window_end,
        assigned_entity_ids,
        current_status,
        tracker_url,
        template_id,
        extra_fields,
        synced_at
      ) VALUES (
        ${taskData.arrivy_task_id},
        ${taskData.url_safe_id},
        ${taskData.quickbase_project_id ?? null},
        ${taskData.quickbase_record_id ?? null},
        ${taskData.customer_name ?? null},
        ${taskData.customer_phone ?? null},
        ${taskData.customer_email ?? null},
        ${taskData.customer_address ?? null},
        ${taskData.task_type ?? null},
        ${taskData.scheduled_start ?? null},
        ${taskData.scheduled_end ?? null},
        ${taskData.start_datetime_window_start ?? null},
        ${taskData.start_datetime_window_end ?? null},
        ${taskData.assigned_entity_ids && taskData.assigned_entity_ids.length > 0
          ? taskData.assigned_entity_ids
          : null},
        ${taskData.current_status ?? null},
        ${taskData.tracker_url ?? null},
        ${taskData.template_id ?? null},
        ${taskData.extra_fields ? JSON.stringify(taskData.extra_fields) : null},
        ${taskData.synced_at ?? new Date()}
      )
      ON CONFLICT (arrivy_task_id)
      DO UPDATE SET
        url_safe_id = COALESCE(EXCLUDED.url_safe_id, arrivy_tasks.url_safe_id),
        quickbase_project_id = COALESCE(EXCLUDED.quickbase_project_id, arrivy_tasks.quickbase_project_id),
        quickbase_record_id = COALESCE(EXCLUDED.quickbase_record_id, arrivy_tasks.quickbase_record_id),
        customer_name = COALESCE(EXCLUDED.customer_name, arrivy_tasks.customer_name),
        customer_phone = COALESCE(EXCLUDED.customer_phone, arrivy_tasks.customer_phone),
        customer_email = COALESCE(EXCLUDED.customer_email, arrivy_tasks.customer_email),
        customer_address = COALESCE(EXCLUDED.customer_address, arrivy_tasks.customer_address),
        task_type = COALESCE(EXCLUDED.task_type, arrivy_tasks.task_type),
        scheduled_start = COALESCE(EXCLUDED.scheduled_start, arrivy_tasks.scheduled_start),
        scheduled_end = COALESCE(EXCLUDED.scheduled_end, arrivy_tasks.scheduled_end),
        start_datetime_window_start = COALESCE(EXCLUDED.start_datetime_window_start, arrivy_tasks.start_datetime_window_start),
        start_datetime_window_end = COALESCE(EXCLUDED.start_datetime_window_end, arrivy_tasks.start_datetime_window_end),
        assigned_entity_ids = COALESCE(EXCLUDED.assigned_entity_ids, arrivy_tasks.assigned_entity_ids),
        current_status = COALESCE(EXCLUDED.current_status, arrivy_tasks.current_status),
        tracker_url = COALESCE(EXCLUDED.tracker_url, arrivy_tasks.tracker_url),
        template_id = COALESCE(EXCLUDED.template_id, arrivy_tasks.template_id),
        extra_fields = COALESCE(EXCLUDED.extra_fields, arrivy_tasks.extra_fields),
        synced_at = COALESCE(EXCLUDED.synced_at, arrivy_tasks.synced_at),
        updated_at = NOW()
      RETURNING *
    `;

    return result.rows[0];
  } catch (error) {
    logError('Failed to upsert Arrivy task', error as Error, {
      arrivy_task_id: taskData.arrivy_task_id,
      quickbase_project_id: taskData.quickbase_project_id,
    });
    throw error;
  }
}

/**
 * Get Arrivy task by QuickBase project ID
 */
export async function getArrivyTaskByProjectId(projectId: string): Promise<ArrivyTaskRecord | null> {
  try {
    const result = await sql<ArrivyTaskRecord>`
      SELECT * FROM arrivy_tasks
      WHERE quickbase_project_id = ${projectId}
      ORDER BY synced_at DESC
      LIMIT 1
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to get Arrivy task by project ID', error as Error, { projectId });
    throw error;
  }
}

/**
 * Get Arrivy task by Arrivy task ID
 */
export async function getArrivyTaskByArrivyId(arrivyTaskId: number): Promise<ArrivyTaskRecord | null> {
  try {
    const result = await sql<ArrivyTaskRecord>`
      SELECT * FROM arrivy_tasks
      WHERE arrivy_task_id = ${arrivyTaskId}
      LIMIT 1
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to get Arrivy task by Arrivy ID', error as Error, { arrivyTaskId });
    throw error;
  }
}

/**
 * Get Arrivy task by database ID
 */
export async function getArrivyTaskById(id: number): Promise<ArrivyTaskRecord | null> {
  try {
    const result = await sql<ArrivyTaskRecord>`
      SELECT * FROM arrivy_tasks
      WHERE id = ${id}
      LIMIT 1
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to get Arrivy task by database ID', error as Error, { id });
    throw error;
  }
}

/**
 * Update Arrivy task status
 */
export async function updateArrivyTaskStatus(arrivyTaskId: number, status: string): Promise<void> {
  try {
    await sql`
      UPDATE arrivy_tasks
      SET current_status = ${status}, updated_at = NOW()
      WHERE arrivy_task_id = ${arrivyTaskId}
    `;
  } catch (error) {
    logError('Failed to update Arrivy task status', error as Error, { arrivyTaskId, status });
    throw error;
  }
}

/**
 * Delete Arrivy task
 */
export async function deleteArrivyTask(arrivyTaskId: number): Promise<void> {
  try {
    await sql`
      DELETE FROM arrivy_tasks
      WHERE arrivy_task_id = ${arrivyTaskId}
    `;
  } catch (error) {
    logError('Failed to delete Arrivy task', error as Error, { arrivyTaskId });
    throw error;
  }
}

// =============================================================================
// ARRIVY ENTITIES
// =============================================================================

export interface ArrivyEntityData {
  arrivy_entity_id: number;
  name: string;
  email?: string | null;
  phone?: string | null;
  entity_type?: string | null;
  quickbase_user_id?: string | null;
  extra_fields?: Record<string, any> | null;
}

export interface ArrivyEntityRecord extends ArrivyEntityData {
  id: number;
  created_at: Date;
  updated_at: Date;
}

/**
 * Insert or update an Arrivy entity
 */
export async function upsertArrivyEntity(entityData: ArrivyEntityData): Promise<ArrivyEntityRecord> {
  try {
    const result = await sql<ArrivyEntityRecord>`
      INSERT INTO arrivy_entities (
        arrivy_entity_id,
        name,
        email,
        phone,
        entity_type,
        quickbase_user_id,
        extra_fields
      ) VALUES (
        ${entityData.arrivy_entity_id},
        ${entityData.name},
        ${entityData.email},
        ${entityData.phone},
        ${entityData.entity_type},
        ${entityData.quickbase_user_id},
        ${JSON.stringify(entityData.extra_fields || {})}
      )
      ON CONFLICT (arrivy_entity_id)
      DO UPDATE SET
        name = EXCLUDED.name,
        email = EXCLUDED.email,
        phone = EXCLUDED.phone,
        entity_type = EXCLUDED.entity_type,
        quickbase_user_id = EXCLUDED.quickbase_user_id,
        extra_fields = EXCLUDED.extra_fields,
        updated_at = NOW()
      RETURNING *
    `;

    return result.rows[0];
  } catch (error) {
    logError('Failed to upsert Arrivy entity', error as Error, {
      arrivy_entity_id: entityData.arrivy_entity_id,
      email: entityData.email,
    });
    throw error;
  }
}

/**
 * Get Arrivy entity by email
 */
export async function getArrivyEntityByEmail(email: string): Promise<ArrivyEntityRecord | null> {
  try {
    const result = await sql<ArrivyEntityRecord>`
      SELECT * FROM arrivy_entities
      WHERE email = ${email}
      LIMIT 1
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to get Arrivy entity by email', error as Error, { email });
    throw error;
  }
}

/**
 * Get Arrivy entity by Arrivy entity ID
 */
export async function getArrivyEntityById(entityId: number): Promise<ArrivyEntityRecord | null> {
  try {
    const result = await sql<ArrivyEntityRecord>`
      SELECT * FROM arrivy_entities
      WHERE arrivy_entity_id = ${entityId}
      LIMIT 1
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to get Arrivy entity by ID', error as Error, { entityId });
    throw error;
  }
}

/**
 * List all Arrivy entities
 */
export async function listArrivyEntities(): Promise<ArrivyEntityRecord[]> {
  try {
    const result = await sql<ArrivyEntityRecord>`
      SELECT * FROM arrivy_entities
      ORDER BY name ASC
    `;

    return result.rows;
  } catch (error) {
    logError('Failed to list Arrivy entities', error as Error);
    throw error;
  }
}

// =============================================================================
// ARRIVY EVENTS
// =============================================================================

export interface ArrivyEventData {
  event_id: number;
  event_type: string;
  event_sub_type?: string | null;
  event_time: Date;
  arrivy_task_id?: number | null;
  reporter_id?: number | null;
  reporter_name?: string | null;
  title?: string | null;
  message?: string | null;
  object_fields?: Record<string, any> | null;
  extra_fields?: Record<string, any> | null;
  is_transient?: boolean;
}

export interface ArrivyEventRecord extends ArrivyEventData {
  id: number;
  created_at: Date;
}

/**
 * Insert a new Arrivy event from webhook
 * Returns null if event already exists (duplicate webhook delivery)
 */
export async function insertArrivyEvent(eventData: ArrivyEventData): Promise<ArrivyEventRecord | null> {
  try {
    const result = await sql<ArrivyEventRecord>`
      INSERT INTO arrivy_events (
        event_id,
        event_type,
        event_sub_type,
        event_time,
        arrivy_task_id,
        reporter_id,
        reporter_name,
        title,
        message,
        object_fields,
        extra_fields,
        is_transient
      ) VALUES (
        ${eventData.event_id},
        ${eventData.event_type},
        ${eventData.event_sub_type},
        ${eventData.event_time},
        ${eventData.arrivy_task_id},
        ${eventData.reporter_id},
        ${eventData.reporter_name},
        ${eventData.title},
        ${eventData.message},
        ${JSON.stringify(eventData.object_fields || {})},
        ${JSON.stringify(eventData.extra_fields || {})},
        ${eventData.is_transient || false}
      )
      ON CONFLICT (event_id) DO NOTHING
      RETURNING *
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to insert Arrivy event', error as Error, {
      event_id: eventData.event_id,
      event_type: eventData.event_type,
    });
    throw error;
  }
}

/**
 * Get recent events for a task
 */
export async function getArrivyEventsForTask(arrivyTaskId: number, limit: number = 50): Promise<ArrivyEventRecord[]> {
  try {
    const result = await sql<ArrivyEventRecord>`
      SELECT * FROM arrivy_events
      WHERE arrivy_task_id = ${arrivyTaskId}
      ORDER BY event_time DESC
      LIMIT ${limit}
    `;

    return result.rows;
  } catch (error) {
    logError('Failed to get Arrivy events for task', error as Error, { arrivyTaskId });
    throw error;
  }
}

/**
 * Get recent events by type
 */
export async function getArrivyEventsByType(
  eventType: string,
  limit: number = 50,
  offset: number = 0
): Promise<ArrivyEventRecord[]> {
  try {
    const result = await sql<ArrivyEventRecord>`
      SELECT * FROM arrivy_events
      WHERE event_type = ${eventType}
      ORDER BY event_time DESC
      LIMIT ${limit}
      OFFSET ${offset}
    `;

    return result.rows;
  } catch (error) {
    logError('Failed to get Arrivy events by type', error as Error, { eventType });
    throw error;
  }
}

/**
 * Get all recent events with optional filters
 */
export async function getArrivyEvents(
  filters: {
    taskId?: number;
    eventType?: string;
    startDate?: Date;
    endDate?: Date;
    reporterName?: string;
    taskType?: string;
    search?: string;
    limit?: number;
    offset?: number;
  } = {}
): Promise<ArrivyEventRecord[]> {
  const {
    taskId,
    eventType,
    startDate,
    endDate,
    reporterName,
    taskType,
    search,
    limit = 50,
    offset = 0,
  } = filters;

  try {
    // If taskType or search filters are provided, we need to join with arrivy_tasks
    const needsJoin = taskType || search;
    
    let query = needsJoin 
      ? `SELECT e.* FROM arrivy_events e LEFT JOIN arrivy_tasks t ON e.arrivy_task_id = t.arrivy_task_id WHERE 1=1`
      : `SELECT * FROM arrivy_events WHERE 1=1`;
    
    const params: any[] = [];
    let paramIndex = 1;

    if (taskId) {
      query += needsJoin ? ` AND e.arrivy_task_id = $${paramIndex}` : ` AND arrivy_task_id = $${paramIndex}`;
      params.push(taskId);
      paramIndex++;
    }

    if (eventType) {
      query += needsJoin ? ` AND e.event_type = $${paramIndex}` : ` AND event_type = $${paramIndex}`;
      params.push(eventType);
      paramIndex++;
    }

    if (startDate) {
      query += needsJoin ? ` AND e.event_time >= $${paramIndex}` : ` AND event_time >= $${paramIndex}`;
      params.push(startDate);
      paramIndex++;
    }

    if (endDate) {
      query += needsJoin ? ` AND e.event_time <= $${paramIndex}` : ` AND event_time <= $${paramIndex}`;
      params.push(endDate);
      paramIndex++;
    }

    if (reporterName) {
      query += needsJoin ? ` AND e.reporter_name = $${paramIndex}` : ` AND reporter_name = $${paramIndex}`;
      params.push(reporterName);
      paramIndex++;
    }

    if (taskType) {
      query += ` AND LOWER(t.task_type) = LOWER($${paramIndex})`;
      params.push(taskType);
      paramIndex++;
    }

    if (search) {
      query += ` AND (t.customer_name ILIKE $${paramIndex} OR t.quickbase_project_id ILIKE $${paramIndex} OR t.arrivy_task_id::text ILIKE $${paramIndex} OR t.url_safe_id ILIKE $${paramIndex})`;
      params.push(`%${search}%`);
      paramIndex++;
    }

    query += needsJoin 
      ? ` ORDER BY e.event_time DESC LIMIT $${paramIndex} OFFSET $${paramIndex + 1}`
      : ` ORDER BY event_time DESC LIMIT $${paramIndex} OFFSET $${paramIndex + 1}`;
    params.push(limit, offset);

    const result = await sql.query<ArrivyEventRecord>(query, params);
    return result.rows;
  } catch (error) {
    logError('Failed to get Arrivy events with filters', error as Error, filters);
    throw error;
  }
}

/**
 * Get count of events matching filters (mirrors getArrivyEvents filtering)
 */
export async function getArrivyEventsCount(
  filters: {
    taskId?: number;
    eventType?: string;
    startDate?: Date;
    endDate?: Date;
    reporterName?: string;
    taskType?: string;
    search?: string;
  } = {}
): Promise<number> {
  const {
    taskId,
    eventType,
    startDate,
    endDate,
    reporterName,
    taskType,
    search,
  } = filters;

  try {
    // If taskType or search filters are provided, we need to join with arrivy_tasks
    const needsJoin = taskType || search;
    
    let query = needsJoin 
      ? `SELECT COUNT(*) as count FROM arrivy_events e LEFT JOIN arrivy_tasks t ON e.arrivy_task_id = t.arrivy_task_id WHERE 1=1`
      : `SELECT COUNT(*) as count FROM arrivy_events WHERE 1=1`;
    
    const params: any[] = [];
    let paramIndex = 1;

    if (taskId) {
      query += needsJoin ? ` AND e.arrivy_task_id = $${paramIndex}` : ` AND arrivy_task_id = $${paramIndex}`;
      params.push(taskId);
      paramIndex++;
    }

    if (eventType) {
      query += needsJoin ? ` AND e.event_type = $${paramIndex}` : ` AND event_type = $${paramIndex}`;
      params.push(eventType);
      paramIndex++;
    }

    if (startDate) {
      query += needsJoin ? ` AND e.event_time >= $${paramIndex}` : ` AND event_time >= $${paramIndex}`;
      params.push(startDate);
      paramIndex++;
    }

    if (endDate) {
      query += needsJoin ? ` AND e.event_time <= $${paramIndex}` : ` AND event_time <= $${paramIndex}`;
      params.push(endDate);
      paramIndex++;
    }

    if (reporterName) {
      query += needsJoin ? ` AND e.reporter_name = $${paramIndex}` : ` AND reporter_name = $${paramIndex}`;
      params.push(reporterName);
      paramIndex++;
    }

    if (taskType) {
      query += ` AND LOWER(t.task_type) = LOWER($${paramIndex})`;
      params.push(taskType);
      paramIndex++;
    }

    if (search) {
      query += ` AND (t.customer_name ILIKE $${paramIndex} OR t.quickbase_project_id ILIKE $${paramIndex} OR t.arrivy_task_id::text ILIKE $${paramIndex} OR t.url_safe_id ILIKE $${paramIndex})`;
      params.push(`%${search}%`);
      paramIndex++;
    }

    const result = await sql.query<{ count: string }>(query, params);
    return parseInt(result.rows[0]?.count || '0', 10);
  } catch (error) {
    logError('Failed to get Arrivy events count', error as Error, filters);
    throw error;
  }
}

/**
 * Get unique crew member names for filter dropdown
 * @returns Array of unique reporter names sorted alphabetically
 */
export async function getUniqueCrewMembers(): Promise<string[]> {
  try {
    const result = await sql`
      SELECT DISTINCT reporter_name
      FROM arrivy_events
      WHERE reporter_name IS NOT NULL
      ORDER BY reporter_name ASC
    `;
    return result.rows.map(row => row.reporter_name);
  } catch (error) {
    logError('Failed to get unique crew members', error as Error);
    throw error;
  }
}

/**
 * Get event type counts for filter badges (optional)
 * @param filters - Optional filters to apply before counting
 * @returns Record of event type to count
 */
export async function getEventTypeCounts(
  filters?: {
    startDate?: Date;
    endDate?: Date;
    reporterName?: string;
  }
): Promise<Record<string, number>> {
  try {
    let query = `
      SELECT event_type, COUNT(*) as count
      FROM arrivy_events
      WHERE 1=1
    `;
    const params: any[] = [];
    let paramIndex = 1;

    if (filters?.startDate) {
      query += ` AND event_time >= $${paramIndex}`;
      params.push(filters.startDate);
      paramIndex++;
    }

    if (filters?.endDate) {
      query += ` AND event_time <= $${paramIndex}`;
      params.push(filters.endDate);
      paramIndex++;
    }

    if (filters?.reporterName) {
      query += ` AND reporter_name = $${paramIndex}`;
      params.push(filters.reporterName);
      paramIndex++;
    }

    query += ` GROUP BY event_type ORDER BY count DESC`;

    const result = await sql.query<{ event_type: string; count: string }>(query, params);
    
    return result.rows.reduce((acc, row) => {
      acc[row.event_type] = parseInt(row.count, 10);
      return acc;
    }, {} as Record<string, number>);
  } catch (error) {
    logError('Failed to get event type counts', error as Error, filters);
    throw error;
  }
}

/**
 * Get customer ratings for a task from TASK_RATING events
 * @param arrivyTaskId - Arrivy task ID
 * @returns Array of TaskRating objects
 */
export async function getTaskRatings(arrivyTaskId: number): Promise<import('@/lib/types/operations').TaskRating[]> {
  try {
    const result = await sql`
      SELECT 
        event_id,
        (extra_fields->>'rating')::integer as rating,
        object_fields->>'RATING_TYPE' as rating_type,
        message as feedback,
        reporter_name as customer_name,
        event_time as rated_at
      FROM arrivy_events
      WHERE arrivy_task_id = ${arrivyTaskId}
        AND event_type = 'TASK_RATING'
        AND extra_fields->>'rating' IS NOT NULL
      ORDER BY event_time DESC
    `;

    return result.rows.map(row => ({
      event_id: row.event_id,
      rating: row.rating || 0,
      rating_type: row.rating_type || 'FiveStar',
      feedback: row.feedback,
      customer_name: row.customer_name,
      rated_at: row.rated_at,
    }));
  } catch (error) {
    logError('Failed to get task ratings', error as Error, { arrivyTaskId });
    return [];
  }
}

/**
 * Get customer notes for a task from note-type events
 * @param arrivyTaskId - Arrivy task ID
 * @returns Array of CustomerNote objects
 */
export async function getCustomerNotes(arrivyTaskId: number): Promise<import('@/lib/types/operations').CustomerNote[]> {
  try {
    const result = await sql`
      SELECT 
        event_id,
        message as note,
        reporter_name as customer_name,
        event_time as created_at,
        event_type
      FROM arrivy_events
      WHERE arrivy_task_id = ${arrivyTaskId}
        AND (event_type = 'TASK_NOTE' OR event_type = 'CUSTOMER_NOTE' OR event_type = 'NOTE')
        AND message IS NOT NULL
        AND message != ''
      ORDER BY event_time DESC
    `;

    return result.rows.map(row => ({
      event_id: row.event_id,
      note: row.note,
      customer_name: row.customer_name,
      created_at: row.created_at,
      event_type: row.event_type,
    }));
  } catch (error) {
    logError('Failed to get customer notes', error as Error, { arrivyTaskId });
    return [];
  }
}

/**
 * Get full crew contact information for a task
 * @param entityIds - Array of Arrivy entity IDs
 * @returns Array of CrewContact objects with phone and email
 */
export async function getCrewContactsForTask(entityIds: number[]): Promise<import('@/lib/types/operations').CrewContact[]> {
  if (!entityIds || entityIds.length === 0) {
    return [];
  }

  try {
    const result = await sql`
      SELECT 
        arrivy_entity_id as entity_id,
        name,
        email,
        phone,
        entity_type
      FROM arrivy_entities
      WHERE arrivy_entity_id = ANY(ARRAY[${sql.join(entityIds.map(id => sql`${id}`), sql`, `)}]::bigint[])
      ORDER BY name ASC
    `;

    return result.rows.map(row => ({
      entity_id: row.entity_id,
      name: row.name,
      email: row.email,
      phone: row.phone,
      entity_type: row.entity_type,
    }));
  } catch (error) {
    logError('Failed to get crew contacts', error as Error, { entityIds });
    return [];
  }
}

/**
 * Get coordinator email for an Arrivy task by looking up the associated QuickBase project
 */
export async function getCoordinatorEmailForTask(task: ArrivyTaskRecord): Promise<string | null> {
  try {
    // Check if task has QuickBase association
    if (!task.quickbase_project_id || task.quickbase_project_id.startsWith('ARRIVY-')) {
      logInfo('Task has no QuickBase association', { arrivy_task_id: task.arrivy_task_id });
      return null;
    }

    if (!task.quickbase_record_id || task.quickbase_record_id === 0) {
      logInfo('Task has no valid QuickBase record ID', { arrivy_task_id: task.arrivy_task_id });
      return null;
    }

    // Dynamically import QuickBase functions to avoid circular dependencies
    const { getProjectById } = await import('@/lib/quickbase/queries');
    const { PROJECT_FIELDS } = await import('@/lib/constants/fieldIds');

    // Fetch project from QuickBase
    const project = await getProjectById(task.quickbase_record_id);
    if (!project) {
      logInfo('QuickBase project not found', { 
        arrivy_task_id: task.arrivy_task_id,
        record_id: task.quickbase_record_id 
      });
      return null;
    }

    // Extract coordinator email
    const coordinatorEmail = project[PROJECT_FIELDS.PROJECT_COORDINATOR_EMAIL]?.value;
    if (!coordinatorEmail) {
      logInfo('No coordinator email in QuickBase project', { 
        arrivy_task_id: task.arrivy_task_id,
        record_id: task.quickbase_record_id 
      });
      return null;
    }

    return coordinatorEmail;
  } catch (error) {
    logError('Failed to get coordinator email for task', error as Error, {
      arrivy_task_id: task.arrivy_task_id,
      quickbase_record_id: task.quickbase_record_id,
    });
    return null;
  }
}

/**
 * Calculate duration metrics for a task
 * @param task - Task record from database
 * @param statusHistory - Array of status records
 * @returns TaskDurationMetrics with calculated durations
 */
export function calculateTaskDurationMetrics(
  task: ArrivyTaskRecord,
  statusHistory: ArrivyTaskStatusRecord[]
): import('@/lib/types/operations').TaskDurationMetrics {
  // Find STARTED and COMPLETE statuses
  const startedStatus = statusHistory.find(s => s.status_type === 'STARTED');
  const completedStatus = statusHistory.find(s => s.status_type === 'COMPLETE');

  const started_at = startedStatus?.reported_at || null;
  const completed_at = completedStatus?.reported_at || null;
  const is_completed = !!completedStatus;

  // Calculate scheduled duration
  let scheduled_duration_minutes: number | null = null;
  if (task.scheduled_start && task.scheduled_end) {
    const start = new Date(task.scheduled_start);
    const end = new Date(task.scheduled_end);
    scheduled_duration_minutes = Math.round((end.getTime() - start.getTime()) / (1000 * 60));
  }

  // Calculate actual duration
  let actual_duration_minutes: number | null = null;
  if (started_at && completed_at) {
    const start = new Date(started_at);
    const complete = new Date(completed_at);
    actual_duration_minutes = Math.round((complete.getTime() - start.getTime()) / (1000 * 60));
  }

  // Calculate time to start
  let time_to_start_minutes: number | null = null;
  let is_delayed = false;
  if (task.scheduled_start && started_at) {
    const scheduledStart = new Date(task.scheduled_start);
    const actualStart = new Date(started_at);
    time_to_start_minutes = Math.round((actualStart.getTime() - scheduledStart.getTime()) / (1000 * 60));
    is_delayed = time_to_start_minutes > 0;
  }

  return {
    scheduled_duration_minutes,
    actual_duration_minutes,
    time_to_start_minutes,
    is_completed,
    is_delayed,
    started_at,
    completed_at,
  };
}

// =============================================================================
// ARRIVY TASK STATUS
// =============================================================================

export interface ArrivyTaskStatusData {
  arrivy_task_id: number;
  status_type: string;
  reporter_id?: number | null;
  reporter_name?: string | null;
  reported_at: Date;
  notes?: string | null;
  has_attachments?: boolean;
  visible_to_customer?: boolean;
  source?: string | null;
}

export interface ArrivyTaskStatusRecord extends ArrivyTaskStatusData {
  id: number;
  created_at: Date;
}

/**
 * Insert a new task status update
 * Uses composite key (arrivy_task_id + status_type + reported_at) to prevent duplicates
 * Returns null if status already exists (idempotent)
 */
export async function insertArrivyTaskStatus(statusData: ArrivyTaskStatusData): Promise<ArrivyTaskStatusRecord | null> {
  try {
    // Check if this status already exists to prevent duplicates
    // Use composite check: same task + same status type + same timestamp (within 1 second tolerance)
    const existing = await sql<ArrivyTaskStatusRecord>`
      SELECT * FROM arrivy_task_status
      WHERE arrivy_task_id = ${statusData.arrivy_task_id}
        AND status_type = ${statusData.status_type}
        AND ABS(EXTRACT(EPOCH FROM (reported_at - ${statusData.reported_at}))) < 1
      LIMIT 1
    `;

    if (existing.rows.length > 0) {
      // Status already exists - return existing record (idempotent)
      return existing.rows[0];
    }

    // Insert new status
    const result = await sql<ArrivyTaskStatusRecord>`
      INSERT INTO arrivy_task_status (
        arrivy_task_id,
        status_type,
        reporter_id,
        reporter_name,
        reported_at,
        notes,
        has_attachments,
        visible_to_customer,
        source
      ) VALUES (
        ${statusData.arrivy_task_id},
        ${statusData.status_type},
        ${statusData.reporter_id},
        ${statusData.reporter_name},
        ${statusData.reported_at},
        ${statusData.notes},
        ${statusData.has_attachments || false},
        ${statusData.visible_to_customer !== false},
        ${statusData.source}
      )
      RETURNING *
    `;

    return result.rows[0];
  } catch (error) {
    logError('Failed to insert Arrivy task status', error as Error, {
      arrivy_task_id: statusData.arrivy_task_id,
      status_type: statusData.status_type,
    });
    throw error;
  }
}

/**
 * Get latest status for a task
 */
export async function getLatestTaskStatus(arrivyTaskId: number): Promise<ArrivyTaskStatusRecord | null> {
  try {
    const result = await sql<ArrivyTaskStatusRecord>`
      SELECT * FROM arrivy_task_status
      WHERE arrivy_task_id = ${arrivyTaskId}
      ORDER BY reported_at DESC
      LIMIT 1
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to get latest task status', error as Error, { arrivyTaskId });
    throw error;
  }
}

/**
 * Get status history for a task
 */
export async function getTaskStatusHistory(arrivyTaskId: number, limit: number = 50): Promise<ArrivyTaskStatusRecord[]> {
  try {
    const result = await sql<ArrivyTaskStatusRecord>`
      SELECT * FROM arrivy_task_status
      WHERE arrivy_task_id = ${arrivyTaskId}
      ORDER BY reported_at DESC
      LIMIT ${limit}
    `;

    return result.rows;
  } catch (error) {
    logError('Failed to get task status history', error as Error, { arrivyTaskId });
    throw error;
  }
}

// =============================================================================
// ARRIVY TASK ATTACHMENTS
// =============================================================================

export interface ArrivyTaskAttachmentData {
  arrivy_task_id: number;
  arrivy_status_id: number;
  file_id: number;
  file_path: string;
  filename: string;
  uploaded_by?: string | null;
  uploaded_at: Date;
}

export interface ArrivyTaskAttachmentRecord extends ArrivyTaskAttachmentData {
  id: number;
  created_at: Date;
  updated_at: Date;
}

/**
 * Insert a task attachment (idempotent - uses ON CONFLICT DO NOTHING)
 * Returns null if attachment already exists (duplicate file_id)
 */
export async function insertArrivyTaskAttachment(
  attachmentData: ArrivyTaskAttachmentData
): Promise<ArrivyTaskAttachmentRecord | null> {
  try {
    const result = await sql<ArrivyTaskAttachmentRecord>`
      INSERT INTO arrivy_task_attachments (
        arrivy_task_id,
        arrivy_status_id,
        file_id,
        file_path,
        filename,
        uploaded_by,
        uploaded_at
      ) VALUES (
        ${attachmentData.arrivy_task_id},
        ${attachmentData.arrivy_status_id},
        ${attachmentData.file_id},
        ${attachmentData.file_path},
        ${attachmentData.filename},
        ${attachmentData.uploaded_by},
        ${attachmentData.uploaded_at}
      )
      ON CONFLICT (file_id) DO NOTHING
      RETURNING *
    `;

    return result.rows[0] || null;
  } catch (error) {
    logError('Failed to insert task attachment', error as Error, {
      file_id: attachmentData.file_id,
      task_id: attachmentData.arrivy_task_id,
    });
    throw error;
  }
}

/**
 * Get all attachments for a task
 * Ordered by upload time (newest first)
 */
export async function getTaskAttachments(
  arrivyTaskId: number,
  limit: number = 100
): Promise<ArrivyTaskAttachmentRecord[]> {
  try {
    const result = await sql<ArrivyTaskAttachmentRecord>`
      SELECT * FROM arrivy_task_attachments
      WHERE arrivy_task_id = ${arrivyTaskId}
      ORDER BY uploaded_at DESC
      LIMIT ${limit}
    `;

    return result.rows;
  } catch (error) {
    logError('Failed to get task attachments', error as Error, { arrivyTaskId });
    return []; // Return empty array on error (non-fatal)
  }
}

/**
 * Get attachment count for a task
 */
export async function getTaskAttachmentCount(arrivyTaskId: number): Promise<number> {
  try {
    const result = await sql`
      SELECT COUNT(*) as count
      FROM arrivy_task_attachments
      WHERE arrivy_task_id = ${arrivyTaskId}
    `;

    return result.rows[0]?.count || 0;
  } catch (error) {
    logError('Failed to get task attachment count', error as Error, { arrivyTaskId });
    return 0;
  }
}

// =============================================================================
// TASK EXCEPTIONS & ISSUES
// =============================================================================

export interface TaskException {
  event_id: number;
  exception_type: 'EXCEPTION' | 'LATE' | 'NOSHOW';
  occurred_at: Date;
  reporter_name: string | null;
  description: string | null;
  exception_details?: {
    type?: string;
    notes?: string;
    reason?: string;
  } | null;
}

/**
 * Get exceptions/issues for a task
 * Fetches EXCEPTION, LATE, and NOSHOW events from arrivy_events
 */
export async function getTaskExceptions(arrivyTaskId: number, limit: number = 50): Promise<TaskException[]> {
  try {
    const result = await sql`
      SELECT
        event_id,
        event_type as exception_type,
        event_time as occurred_at,
        reporter_name,
        message as description,
        extra_fields
      FROM arrivy_events
      WHERE arrivy_task_id = ${arrivyTaskId}
        AND event_type IN ('EXCEPTION', 'LATE', 'NOSHOW')
      ORDER BY event_time DESC
      LIMIT ${limit}
    `;

    return result.rows.map(row => {
      // Try to parse exception details from extra_fields.EXCEPTION
      let exception_details = null;
      if (row.extra_fields && typeof row.extra_fields === 'object') {
        const exceptionField = (row.extra_fields as any).EXCEPTION;
        if (exceptionField && typeof exceptionField === 'object') {
          exception_details = {
            type: exceptionField.type || null,
            notes: exceptionField.notes || null,
            reason: exceptionField.reason || null,
          };
        }
      }

      return {
        event_id: row.event_id,
        exception_type: row.exception_type,
        occurred_at: row.occurred_at,
        reporter_name: row.reporter_name,
        description: row.description,
        exception_details,
      };
    });
  } catch (error) {
    logError('Failed to get task exceptions', error as Error, { arrivyTaskId });
    return []; // Return empty array on error (non-fatal)
  }
}

// =============================================================================
// FIELD TRACKING DASHBOARD QUERIES
// =============================================================================

export interface FieldTrackingTaskWithDetails extends ArrivyTaskRecord {
  latest_status?: string | null;
  latest_status_time?: Date | null;
  entity_names?: string[] | null;
}

/**
 * Get field tracking tasks with filters for dashboard
 */
export async function getFieldTrackingTasks(filters: {
  coordinatorEmail?: string;
  taskType?: string;
  status?: string;
  dateRange?: { start: Date; end: Date };
  search?: string;
  limit?: number;
  offset?: number;
}): Promise<FieldTrackingTaskWithDetails[]> {
  const {
    coordinatorEmail,
    taskType,
    status,
    dateRange,
    search,
    limit = 50,
    offset = 0,
  } = filters;

  try {
    let query = `
      SELECT 
        t.id,
        t.arrivy_task_id,
        t.url_safe_id,
        t.quickbase_project_id,
        t.quickbase_record_id,
        t.customer_name,
        t.customer_phone,
        t.customer_email,
        t.customer_address,
        t.task_type,
        t.scheduled_start,
        t.scheduled_end,
        t.start_datetime_window_start,
        t.start_datetime_window_end,
        t.assigned_entity_ids,
        -- CRITICAL: Use latest status from history table, fall back to current_status column
        COALESCE(s.status_type, t.current_status, 'NOT_STARTED') as current_status,
        t.tracker_url,
        t.business_tracker_url,
        t.template_id,
        t.extra_fields,
        t.created_at,
        t.updated_at,
        t.synced_at,
        s.status_type as latest_status,
        s.reported_at as latest_status_time,
        ARRAY_AGG(e.name) FILTER (WHERE e.name IS NOT NULL) as entity_names
      FROM arrivy_tasks t
      LEFT JOIN LATERAL (
        SELECT status_type, reported_at
        FROM arrivy_task_status
        WHERE arrivy_task_id = t.arrivy_task_id
        ORDER BY reported_at DESC
        LIMIT 1
      ) s ON true
      LEFT JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
      LEFT JOIN arrivy_entities e ON e.arrivy_entity_id = te.arrivy_entity_id
      WHERE 1=1
    `;

    const params: any[] = [];
    let paramIndex = 1;

    if (coordinatorEmail) {
      query += ` AND EXISTS (
        SELECT 1 FROM arrivy_task_entities te2
        INNER JOIN arrivy_entities e2 ON e2.arrivy_entity_id = te2.arrivy_entity_id
        WHERE te2.arrivy_task_id = t.arrivy_task_id
        AND e2.email = $${paramIndex}
      )`;
      params.push(coordinatorEmail);
      paramIndex++;
    }

    if (taskType) {
      // Support both exact match and category-based filtering
      // If taskType is simple (survey, install, etc), do partial match
      // If taskType is detailed (Surveys - Site Survey), do exact match
      const isDetailedType = taskType.includes(' - ');
      if (isDetailedType) {
        query += ` AND LOWER(t.task_type) = LOWER($${paramIndex})`;
        params.push(taskType);
      } else {
        query += ` AND LOWER(t.task_type) LIKE LOWER($${paramIndex})`;
        params.push(`%${taskType}%`);
      }
      paramIndex++;
    }

    if (status) {
      // Filter using the computed status (from history) not the stale column
      query += ` AND COALESCE(s.status_type, t.current_status, 'NOT_STARTED') = $${paramIndex}`;
      params.push(status);
      paramIndex++;
    }

    if (dateRange) {
      query += ` AND t.scheduled_start >= $${paramIndex} AND t.scheduled_start <= $${paramIndex + 1}`;
      params.push(dateRange.start, dateRange.end);
      paramIndex += 2;
    }

    if (search) {
      query += ` AND (
        t.customer_name ILIKE $${paramIndex}
        OR (t.quickbase_project_id IS NOT NULL AND t.quickbase_project_id ILIKE $${paramIndex})
        OR t.customer_phone ILIKE $${paramIndex}
      )`;
      params.push(`%${search}%`);
      paramIndex++;
    }

    query += ` GROUP BY t.id, s.status_type, s.reported_at`;
    query += ` ORDER BY t.scheduled_start DESC`;
    query += ` LIMIT $${paramIndex} OFFSET $${paramIndex + 1}`;
    params.push(limit, offset);

    const result = await sql.query<FieldTrackingTaskWithDetails>(query, params);
    return result.rows;
  } catch (error) {
    logError('Failed to get field tracking tasks', error as Error, filters);
    throw error;
  }
}

// ============================================================================
// CREW PERFORMANCE ANALYTICS QUERIES
// ============================================================================

/**
 * Get comprehensive performance metrics for all crew members
 * Aggregates task completion, duration, ratings, and on-time data from Arrivy
 */
export async function getCrewPerformanceMetrics(filters: {
  timeRange: '7days' | '30days' | '90days' | 'all';
  crewId?: number;
  taskType?: string;
}): Promise<CrewPerformanceMetrics[]> {
  try {
    const { timeRange, crewId, taskType } = filters;

    const params: any[] = [];
    let paramIndex = 1;
    
    let dateFilter = '';
    if (timeRange === '7days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '7 days'`;
    } else if (timeRange === '30days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '30 days'`;
    } else if (timeRange === '90days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '90 days'`;
    }

    let taskTypeFilter = '';
    if (taskType && taskType !== 'all') {
      taskTypeFilter = `AND LOWER(t.task_type) = LOWER($${paramIndex})`;
      params.push(taskType);
      paramIndex++;
    }

    let crewFilter = '';
    if (crewId !== undefined) {
      crewFilter = `WHERE e.arrivy_entity_id = $${paramIndex}`;
      params.push(crewId);
      paramIndex++;
    }

    const query = `
      WITH all_completed_tasks AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          t.arrivy_task_id,
          s.reported_at
        FROM arrivy_tasks t
        JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s.status_type = 'COMPLETE'
        ${taskTypeFilter}
      ),
      today_tasks AS (
        SELECT entity_id, COUNT(*) as count
        FROM all_completed_tasks
        WHERE reported_at::date = CURRENT_DATE
        GROUP BY entity_id
      ),
      week_tasks AS (
        SELECT entity_id, COUNT(*) as count
        FROM all_completed_tasks
        WHERE reported_at::date >= DATE_TRUNC('week', CURRENT_DATE)
        GROUP BY entity_id
      ),
      month_tasks AS (
        SELECT entity_id, COUNT(*) as count
        FROM all_completed_tasks
        WHERE reported_at::date >= DATE_TRUNC('month', CURRENT_DATE)
        GROUP BY entity_id
      ),
      completed_tasks AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          t.arrivy_task_id
        FROM arrivy_tasks t
        JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s.status_type = 'COMPLETE'
        ${dateFilter}
        ${taskTypeFilter}
      ),
      task_durations AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          t.arrivy_task_id,
          EXTRACT(EPOCH FROM (s_end.reported_at - s_start.reported_at)) / 60 as duration_minutes,
          CASE 
            WHEN s_start.reported_at <= t.scheduled_start THEN 0
            ELSE 1
          END as is_delayed
        FROM arrivy_tasks t
        JOIN arrivy_task_status s_start ON t.arrivy_task_id = s_start.arrivy_task_id AND s_start.status_type = 'STARTED'
        JOIN arrivy_task_status s_end ON t.arrivy_task_id = s_end.arrivy_task_id AND s_end.status_type = 'COMPLETE'
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s_end.reported_at IS NOT NULL
        ${dateFilter.replace('s.reported_at', 's_end.reported_at')}
        ${taskTypeFilter}
      ),
      ratings AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          AVG((ev.extra_fields->>'rating')::numeric) as avg_rating,
          COUNT(*) as rating_count
        FROM arrivy_events ev
        JOIN arrivy_tasks t ON ev.arrivy_task_id = t.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE ev.event_type = 'TASK_RATING'
        AND ev.extra_fields->>'rating' IS NOT NULL
        ${dateFilter.replace('s.reported_at', 'ev.event_time')}
        ${taskTypeFilter}
        GROUP BY entity_id
      ),
      active_tasks AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          COUNT(*) as active_count
        FROM arrivy_tasks t
        JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s.status_type IN ('NOT_STARTED', 'ENROUTE', 'STARTED')
        AND s.reported_at = (
          SELECT MAX(s2.reported_at) 
          FROM arrivy_task_status s2 
          WHERE s2.arrivy_task_id = t.arrivy_task_id
        )
        ${taskTypeFilter}
        GROUP BY entity_id
      ),
      exceptions AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          COUNT(*) FILTER (WHERE ev.event_type = 'EXCEPTION') as exception_count,
          COUNT(*) FILTER (WHERE ev.event_type = 'NOSHOW') as noshow_count
        FROM arrivy_events ev
        JOIN arrivy_tasks t ON ev.arrivy_task_id = t.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE ev.event_type IN ('EXCEPTION', 'NOSHOW')
        ${dateFilter.replace('s.reported_at', 'ev.event_time')}
        ${taskTypeFilter}
        GROUP BY entity_id
      )
      SELECT 
        e.arrivy_entity_id as entity_id,
        e.name as crew_name,
        e.email as crew_email,
        e.phone as crew_phone,
        COALESCE(tt.count, 0)::int as tasks_completed_today,
        COALESCE(wt.count, 0)::int as tasks_completed_week,
        COALESCE(mt.count, 0)::int as tasks_completed_month,
        COALESCE(COUNT(DISTINCT ct.arrivy_task_id), 0)::int as tasks_completed_total,
        ROUND(AVG(td.duration_minutes)::numeric, 2) as avg_completion_time_minutes,
        CASE 
          WHEN COUNT(td.arrivy_task_id) > 0 
          THEN ROUND((1.0 - (SUM(td.is_delayed)::numeric / COUNT(td.arrivy_task_id))) * 100, 2)
          ELSE 0 
        END as on_time_percentage,
        r.avg_rating as customer_rating_avg,
        COALESCE(r.rating_count, 0)::int as customer_rating_count,
        COALESCE(at.active_count, 0)::int as tasks_currently_assigned,
        (
          SELECT COUNT(DISTINCT te2.arrivy_task_id)
          FROM arrivy_task_entities te2
          WHERE te2.arrivy_entity_id = e.arrivy_entity_id
        )::int as total_tasks_assigned,
        COALESCE(SUM(td.is_delayed), 0)::int as delayed_tasks_count,
        COALESCE(ex.exception_count, 0)::int as exception_count,
        COALESCE(ex.noshow_count, 0)::int as noshow_count
      FROM arrivy_entities e
      LEFT JOIN today_tasks tt ON e.arrivy_entity_id = tt.entity_id
      LEFT JOIN week_tasks wt ON e.arrivy_entity_id = wt.entity_id
      LEFT JOIN month_tasks mt ON e.arrivy_entity_id = mt.entity_id
      LEFT JOIN completed_tasks ct ON e.arrivy_entity_id = ct.entity_id
      LEFT JOIN task_durations td ON e.arrivy_entity_id = td.entity_id
      LEFT JOIN ratings r ON e.arrivy_entity_id = r.entity_id
      LEFT JOIN active_tasks at ON e.arrivy_entity_id = at.entity_id
      LEFT JOIN exceptions ex ON e.arrivy_entity_id = ex.entity_id
      ${crewFilter}
      GROUP BY e.arrivy_entity_id, e.name, e.email, e.phone, tt.count, wt.count, mt.count, r.avg_rating, r.rating_count, at.active_count, ex.exception_count, ex.noshow_count
      HAVING COUNT(DISTINCT ct.arrivy_task_id) > 0 OR at.active_count > 0
      ORDER BY tasks_completed_total DESC
    `;

    const result = await sql.query<CrewPerformanceMetrics>(query, params);
    return result.rows;
  } catch (error) {
    logError('Failed to get crew performance metrics', error as Error, filters);
    throw error;
  }
}

/**
 * Calculate team-wide average metrics for comparison
 */
export async function getCrewTeamAverages(filters: {
  timeRange: '7days' | '30days' | '90days' | 'all';
  taskType?: string;
}): Promise<Omit<CrewPerformanceMetrics, 'entity_id' | 'crew_name' | 'crew_email' | 'crew_phone'>> {
  try {
    const { timeRange, taskType } = filters;

    let dateFilter = '';
    const params: any[] = [];
    let paramIndex = 1;
    
    if (timeRange === '7days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '7 days'`;
    } else if (timeRange === '30days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '30 days'`;
    } else if (timeRange === '90days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '90 days'`;
    }

    let taskTypeFilter = '';
    if (taskType && taskType !== 'all') {
      taskTypeFilter = `AND LOWER(t.task_type) = LOWER($${paramIndex})`;
      params.push(taskType);
      paramIndex++;
    }

    const query = `
      WITH all_completed_tasks AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          t.arrivy_task_id,
          s.reported_at
        FROM arrivy_tasks t
        JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s.status_type = 'COMPLETE'
        ${taskTypeFilter}
      ),
      today_tasks AS (
        SELECT entity_id, COUNT(*) as count
        FROM all_completed_tasks
        WHERE reported_at::date = CURRENT_DATE
        GROUP BY entity_id
      ),
      week_tasks AS (
        SELECT entity_id, COUNT(*) as count
        FROM all_completed_tasks
        WHERE reported_at::date >= DATE_TRUNC('week', CURRENT_DATE)
        GROUP BY entity_id
      ),
      month_tasks AS (
        SELECT entity_id, COUNT(*) as count
        FROM all_completed_tasks
        WHERE reported_at::date >= DATE_TRUNC('month', CURRENT_DATE)
        GROUP BY entity_id
      ),
      completed_tasks AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          t.arrivy_task_id
        FROM arrivy_tasks t
        JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s.status_type = 'COMPLETE'
        ${dateFilter}
        ${taskTypeFilter}
      ),
      task_durations AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          t.arrivy_task_id,
          EXTRACT(EPOCH FROM (s_end.reported_at - s_start.reported_at)) / 60 as duration_minutes,
          CASE 
            WHEN s_start.reported_at <= t.scheduled_start THEN 0
            ELSE 1
          END as is_delayed
        FROM arrivy_tasks t
        JOIN arrivy_task_status s_start ON t.arrivy_task_id = s_start.arrivy_task_id AND s_start.status_type = 'STARTED'
        JOIN arrivy_task_status s_end ON t.arrivy_task_id = s_end.arrivy_task_id AND s_end.status_type = 'COMPLETE'
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s_end.reported_at IS NOT NULL
        ${dateFilter.replace('s.reported_at', 's_end.reported_at')}
        ${taskTypeFilter}
      ),
      ratings AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          AVG((ev.extra_fields->>'rating')::numeric) as avg_rating,
          COUNT(*) as rating_count
        FROM arrivy_events ev
        JOIN arrivy_tasks t ON ev.arrivy_task_id = t.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE ev.event_type = 'TASK_RATING'
        AND ev.extra_fields->>'rating' IS NOT NULL
        ${dateFilter.replace('s.reported_at', 'ev.event_time')}
        ${taskTypeFilter}
        GROUP BY entity_id
      ),
      active_tasks AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          COUNT(*) as active_count
        FROM arrivy_tasks t
        JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE s.status_type IN ('NOT_STARTED', 'ENROUTE', 'STARTED')
        AND s.reported_at = (
          SELECT MAX(s2.reported_at) 
          FROM arrivy_task_status s2 
          WHERE s2.arrivy_task_id = t.arrivy_task_id
        )
        ${taskTypeFilter}
        GROUP BY entity_id
      ),
      exceptions AS (
        SELECT 
          te.arrivy_entity_id as entity_id,
          COUNT(*) FILTER (WHERE ev.event_type = 'EXCEPTION') as exception_count,
          COUNT(*) FILTER (WHERE ev.event_type = 'NOSHOW') as noshow_count
        FROM arrivy_events ev
        JOIN arrivy_tasks t ON ev.arrivy_task_id = t.arrivy_task_id
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE ev.event_type IN ('EXCEPTION', 'NOSHOW')
        ${dateFilter.replace('s.reported_at', 'ev.event_time')}
        ${taskTypeFilter}
        GROUP BY entity_id
      )
      SELECT 
        ROUND(AVG(tasks_today)::numeric, 2)::float as tasks_completed_today,
        ROUND(AVG(tasks_week)::numeric, 2)::float as tasks_completed_week,
        ROUND(AVG(tasks_month)::numeric, 2)::float as tasks_completed_month,
        ROUND(AVG(tasks_total)::numeric, 2)::float as tasks_completed_total,
        ROUND(AVG(avg_duration)::numeric, 2) as avg_completion_time_minutes,
        ROUND(AVG(on_time_pct)::numeric, 2) as on_time_percentage,
        CASE 
          WHEN COUNT(rating) FILTER (WHERE rating IS NOT NULL) > 0 
          THEN ROUND(AVG(rating)::numeric, 2)
          ELSE NULL 
        END as customer_rating_avg,
        ROUND(AVG(rating_cnt)::numeric, 2)::int as customer_rating_count,
        ROUND(AVG(active)::numeric, 2)::int as tasks_currently_assigned,
        ROUND(AVG(total)::numeric, 2)::int as total_tasks_assigned,
        ROUND(AVG(delayed)::numeric, 2)::int as delayed_tasks_count,
        ROUND(AVG(exceptions_cnt)::numeric, 2)::int as exception_count,
        ROUND(AVG(noshows)::numeric, 2)::int as noshow_count
      FROM (
        SELECT 
          e.arrivy_entity_id as entity_id,
          COALESCE(tt.count, 0)::int as tasks_today,
          COALESCE(wt.count, 0)::int as tasks_week,
          COALESCE(mt.count, 0)::int as tasks_month,
          COUNT(DISTINCT ct.arrivy_task_id)::int as tasks_total,
          AVG(td.duration_minutes) as avg_duration,
          CASE 
            WHEN COUNT(td.arrivy_task_id) > 0 
            THEN (1.0 - (SUM(td.is_delayed)::numeric / COUNT(td.arrivy_task_id))) * 100
            ELSE 0 
          END as on_time_pct,
          r.avg_rating as rating,
          COALESCE(r.rating_count, 0)::int as rating_cnt,
          COALESCE(at.active_count, 0)::int as active,
          (
            SELECT COUNT(DISTINCT te2.arrivy_task_id)
            FROM arrivy_task_entities te2
            WHERE te2.arrivy_entity_id = e.arrivy_entity_id
          )::int as total,
          SUM(td.is_delayed)::int as delayed,
          COALESCE(ex.exception_count, 0)::int as exceptions_cnt,
          COALESCE(ex.noshow_count, 0)::int as noshows
        FROM arrivy_entities e
        LEFT JOIN today_tasks tt ON e.arrivy_entity_id = tt.entity_id
        LEFT JOIN week_tasks wt ON e.arrivy_entity_id = wt.entity_id
        LEFT JOIN month_tasks mt ON e.arrivy_entity_id = mt.entity_id
        LEFT JOIN completed_tasks ct ON e.arrivy_entity_id = ct.entity_id
        LEFT JOIN task_durations td ON e.arrivy_entity_id = td.entity_id
        LEFT JOIN ratings r ON e.arrivy_entity_id = r.entity_id
        LEFT JOIN active_tasks at ON e.arrivy_entity_id = at.entity_id
        LEFT JOIN exceptions ex ON e.arrivy_entity_id = ex.entity_id
        GROUP BY e.arrivy_entity_id, tt.count, wt.count, mt.count, r.avg_rating, r.rating_count, at.active_count, ex.exception_count, ex.noshow_count
        HAVING COUNT(DISTINCT ct.arrivy_task_id) > 0 OR at.active_count > 0
      ) crew_stats
    `;

    const result = await sql.query(query, params);
    return result.rows[0] || {
      tasks_completed_today: 0,
      tasks_completed_week: 0,
      tasks_completed_month: 0,
      tasks_completed_total: 0,
      avg_completion_time_minutes: null,
      on_time_percentage: 0,
      customer_rating_avg: null,
      customer_rating_count: 0,
      tasks_currently_assigned: 0,
      total_tasks_assigned: 0,
      delayed_tasks_count: 0,
      exception_count: 0,
      noshow_count: 0
    };
  } catch (error) {
    logError('Failed to get crew team averages', error as Error, filters);
    throw error;
  }
}

/**
 * Identify top performers and crew members needing support
 */
export async function getCrewLeaderboard(filters: {
  timeRange: '7days' | '30days' | '90days' | 'all';
  taskType?: string;
}): Promise<CrewLeaderboard> {
  try {
    const crewMetrics = await getCrewPerformanceMetrics(filters);

    // Top performers by tasks completed
    const topPerformers = crewMetrics
      .sort((a, b) => b.tasks_completed_total - a.tasks_completed_total)
      .slice(0, 5)
      .map(crew => ({
        crew_name: crew.crew_name,
        metric_value: crew.tasks_completed_total,
        metric_name: 'Tasks Completed'
      }));

    // Identify crew members needing support
    const needsSupport: Array<{ crew_name: string; metric_value: number; issue: string }> = [];

    crewMetrics.forEach(crew => {
      // Low on-time percentage
      if (crew.on_time_percentage < 70 && crew.tasks_completed_total > 0) {
        needsSupport.push({
          crew_name: crew.crew_name,
          metric_value: crew.on_time_percentage,
          issue: `Low on-time percentage: ${crew.on_time_percentage.toFixed(1)}% - Review scheduling`
        });
      }

      // High exception count
      if (crew.exception_count > 3) {
        needsSupport.push({
          crew_name: crew.crew_name,
          metric_value: crew.exception_count,
          issue: `High exception count: ${crew.exception_count} - Field challenges identified`
        });
      }

      // Low customer ratings
      if (crew.customer_rating_avg && crew.customer_rating_avg < 3.5 && crew.customer_rating_count >= 3) {
        needsSupport.push({
          crew_name: crew.crew_name,
          metric_value: crew.customer_rating_avg,
          issue: `Low customer rating: ${crew.customer_rating_avg.toFixed(1)}/5 - Customer service training needed`
        });
      }

      // Zero tasks completed in time range
      if (crew.tasks_completed_total === 0 && crew.total_tasks_assigned > 0) {
        needsSupport.push({
          crew_name: crew.crew_name,
          metric_value: 0,
          issue: 'No tasks completed in time range - Investigate activity'
        });
      }
    });

    return {
      top_performers: topPerformers,
      needs_support: needsSupport.slice(0, 5)
    };
  } catch (error) {
    logError('Failed to get crew leaderboard', error as Error, filters);
    throw error;
  }
}

/**
 * Get daily performance trends for a specific crew member
 */
export async function getCrewPerformanceTrends(
  entityId: number,
  days: number
): Promise<CrewPerformanceTrend[]> {
  try {
    const query = `
      WITH date_series AS (
        SELECT generate_series(
          CURRENT_DATE - INTERVAL '${days} days',
          CURRENT_DATE,
          INTERVAL '1 day'
        )::date as date
      ),
      daily_completions AS (
        SELECT 
          s.reported_at::date as date,
          COUNT(*) as tasks_completed,
          AVG(EXTRACT(EPOCH FROM (s_end.reported_at - s_start.reported_at)) / 60) as avg_duration,
          (1.0 - (SUM(CASE WHEN s_start.reported_at > t.scheduled_start THEN 1 ELSE 0 END)::numeric / COUNT(*))) * 100 as on_time_pct
        FROM arrivy_tasks t
        JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id AND s.status_type = 'COMPLETE'
        LEFT JOIN arrivy_task_status s_start ON t.arrivy_task_id = s_start.arrivy_task_id AND s_start.status_type = 'STARTED'
        LEFT JOIN arrivy_task_status s_end ON t.arrivy_task_id = s_end.arrivy_task_id AND s_end.status_type = 'COMPLETE'
        JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
        WHERE te.arrivy_entity_id = ${entityId}
        AND s.reported_at >= CURRENT_DATE - INTERVAL '${days} days'
        GROUP BY s.reported_at::date
      )
      SELECT 
        ds.date::text,
        COALESCE(e.name, 'Unknown') as crew_name,
        COALESCE(dc.tasks_completed, 0)::int as tasks_completed,
        COALESCE(ROUND(dc.avg_duration::numeric, 2), 0)::float as avg_completion_time,
        COALESCE(ROUND(dc.on_time_pct::numeric, 2), 0)::float as on_time_percentage
      FROM date_series ds
      LEFT JOIN daily_completions dc ON ds.date = dc.date
      CROSS JOIN arrivy_entities e
      WHERE e.arrivy_entity_id = ${entityId}
      ORDER BY ds.date
    `;

    const result = await sql.query<CrewPerformanceTrend>(query);
    return result.rows;
  } catch (error) {
    logError('Failed to get crew performance trends', error as Error, { entityId, days });
    throw error;
  }
}

/**
 * Get detailed task completion breakdown for a single crew member
 */
export async function getCrewTaskCompletionStats(
  entityId: number,
  timeRange: '7days' | '30days' | '90days' | 'all'
): Promise<{
  by_type: Record<string, number>;
  by_status: Record<string, number>;
  by_day: Array<{ date: string; count: number }>;
}> {
  try {
    let dateFilter = '';
    if (timeRange === '7days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '7 days'`;
    } else if (timeRange === '30days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '30 days'`;
    } else if (timeRange === '90days') {
      dateFilter = `AND s.reported_at >= NOW() - INTERVAL '90 days'`;
    }

    // By type
    const typeQuery = `
      SELECT 
        t.task_type,
        COUNT(*) as count
      FROM arrivy_tasks t
      JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
      JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
      WHERE te.arrivy_entity_id = ${entityId}
      AND s.status_type = 'COMPLETE'
      ${dateFilter}
      GROUP BY t.task_type
    `;

    // By status
    const statusQuery = `
      SELECT 
        s.status_type,
        COUNT(*) as count
      FROM arrivy_tasks t
      JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
      JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
      WHERE te.arrivy_entity_id = ${entityId}
      ${dateFilter}
      GROUP BY s.status_type
    `;

    // By day
    const dayQuery = `
      SELECT 
        s.reported_at::date as date,
        COUNT(*) as count
      FROM arrivy_tasks t
      JOIN arrivy_task_status s ON t.arrivy_task_id = s.arrivy_task_id
      JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
      WHERE te.arrivy_entity_id = ${entityId}
      AND s.status_type = 'COMPLETE'
      ${dateFilter}
      GROUP BY s.reported_at::date
      ORDER BY date
    `;

    const [typeResult, statusResult, dayResult] = await Promise.all([
      sql.query<{ task_type: string; count: string }>(typeQuery),
      sql.query<{ status_type: string; count: string }>(statusQuery),
      sql.query<{ date: Date; count: string }>(dayQuery)
    ]);

    const by_type: Record<string, number> = {};
    typeResult.rows.forEach(row => {
      by_type[row.task_type] = parseInt(row.count);
    });

    const by_status: Record<string, number> = {};
    statusResult.rows.forEach(row => {
      by_status[row.status_type] = parseInt(row.count);
    });

    const by_day = dayResult.rows.map(row => ({
      date: row.date.toISOString().split('T')[0],
      count: parseInt(row.count)
    }));

    return { by_type, by_status, by_day };
  } catch (error) {
    logError('Failed to get crew task completion stats', error as Error, { entityId, timeRange });
    throw error;
  }
}

// =============================================================================
// FIELD OPERATIONS ANALYTICS QUERIES
// =============================================================================

/**
 * Get comprehensive task performance analytics
 * Includes task type breakdown, duration stats, completion rates, delay patterns
 */
export async function getTaskPerformanceAnalytics(filters: {
  startDate?: string;
  endDate?: string;
  taskType?: string;
  entityId?: number;
}): Promise<{
  overview: {
    total_tasks: number;
    completed: number;
    cancelled: number;
    no_shows: number;
    completion_rate: number;
    avg_duration_minutes: number;
    on_time_percentage: number;
  };
  by_task_type: Array<{
    task_type: string;
    count: number;
    completed: number;
    avg_duration: number;
    completion_rate: number;
  }>;
  duration_distribution: Array<{
    task_type: string;
    min_duration: number;
    avg_duration: number;
    max_duration: number;
    p50_duration: number; // median
    p90_duration: number;
  }>;
  daily_trends: Array<{
    date: string;
    total: number;
    completed: number;
    completion_rate: number;
  }>;
  delay_patterns: Array<{
    hour_of_day: number;
    avg_delay_minutes: number;
    delayed_count: number;
    total_count: number;
  }>;
}> {
  try {
    const { startDate, endDate, taskType, entityId } = filters;

    // Build date filter
    let dateFilter = '';
    if (startDate && endDate) {
      dateFilter = `AND t.scheduled_start >= '${startDate}' AND t.scheduled_start < '${endDate}'`;
    } else if (startDate) {
      dateFilter = `AND t.scheduled_start >= '${startDate}'`;
    }

    // Build task type filter
    const taskTypeFilter = taskType ? `AND t.task_type = '${taskType}'` : '';

    // Build entity filter
    const entityFilter = entityId
      ? `AND EXISTS (
          SELECT 1 FROM arrivy_task_entities te
          WHERE te.arrivy_task_id = t.arrivy_task_id
          AND te.arrivy_entity_id = ${entityId}
        )`
      : '';

    // Overview query
    const overviewQuery = `
      WITH task_durations AS (
        SELECT
          t.arrivy_task_id,
          t.current_status,
          EXTRACT(EPOCH FROM (s_complete.reported_at - s_start.reported_at)) / 60 as duration_minutes,
          CASE
            WHEN s_complete.reported_at <= t.scheduled_end THEN true
            ELSE false
          END as on_time
        FROM arrivy_tasks t
        LEFT JOIN arrivy_task_status s_start ON s_start.arrivy_task_id = t.arrivy_task_id
          AND s_start.status_type = 'STARTED'
        LEFT JOIN arrivy_task_status s_complete ON s_complete.arrivy_task_id = t.arrivy_task_id
          AND s_complete.status_type = 'COMPLETE'
        WHERE t.scheduled_start IS NOT NULL
        ${dateFilter}
        ${taskTypeFilter}
        ${entityFilter}
      )
      SELECT
        COUNT(*) as total_tasks,
        SUM(CASE WHEN current_status = 'COMPLETE' THEN 1 ELSE 0 END) as completed,
        SUM(CASE WHEN current_status = 'CANCELLED' THEN 1 ELSE 0 END) as cancelled,
        SUM(CASE WHEN current_status = 'NOSHOW' THEN 1 ELSE 0 END) as no_shows,
        ROUND(
          SUM(CASE WHEN current_status = 'COMPLETE' THEN 1 ELSE 0 END)::numeric /
          NULLIF(COUNT(*), 0) * 100,
          1
        ) as completion_rate,
        ROUND(AVG(duration_minutes), 1) as avg_duration_minutes,
        ROUND(
          SUM(CASE WHEN on_time = true THEN 1 ELSE 0 END)::numeric /
          NULLIF(SUM(CASE WHEN duration_minutes IS NOT NULL THEN 1 ELSE 0 END), 0) * 100,
          1
        ) as on_time_percentage
      FROM task_durations
    `;

    // By task type query
    const byTypeQuery = `
      WITH task_durations AS (
        SELECT
          t.task_type,
          t.current_status,
          EXTRACT(EPOCH FROM (s_complete.reported_at - s_start.reported_at)) / 60 as duration_minutes
        FROM arrivy_tasks t
        LEFT JOIN arrivy_task_status s_start ON s_start.arrivy_task_id = t.arrivy_task_id
          AND s_start.status_type = 'STARTED'
        LEFT JOIN arrivy_task_status s_complete ON s_complete.arrivy_task_id = t.arrivy_task_id
          AND s_complete.status_type = 'COMPLETE'
        WHERE t.scheduled_start IS NOT NULL
        AND t.task_type IS NOT NULL
        ${dateFilter}
        ${taskTypeFilter}
        ${entityFilter}
      )
      SELECT
        task_type,
        COUNT(*) as count,
        SUM(CASE WHEN current_status = 'COMPLETE' THEN 1 ELSE 0 END) as completed,
        ROUND(AVG(duration_minutes), 1) as avg_duration,
        ROUND(
          SUM(CASE WHEN current_status = 'COMPLETE' THEN 1 ELSE 0 END)::numeric /
          COUNT(*) * 100,
          1
        ) as completion_rate
      FROM task_durations
      GROUP BY task_type
      ORDER BY count DESC
    `;

    // Duration distribution query
    const durationDistQuery = `
      WITH task_durations AS (
        SELECT
          t.task_type,
          EXTRACT(EPOCH FROM (s_complete.reported_at - s_start.reported_at)) / 60 as duration_minutes
        FROM arrivy_tasks t
        LEFT JOIN arrivy_task_status s_start ON s_start.arrivy_task_id = t.arrivy_task_id
          AND s_start.status_type = 'STARTED'
        LEFT JOIN arrivy_task_status s_complete ON s_complete.arrivy_task_id = t.arrivy_task_id
          AND s_complete.status_type = 'COMPLETE'
        WHERE t.scheduled_start IS NOT NULL
        AND t.task_type IS NOT NULL
        AND s_start.reported_at IS NOT NULL
        AND s_complete.reported_at IS NOT NULL
        ${dateFilter}
        ${taskTypeFilter}
        ${entityFilter}
      )
      SELECT
        task_type,
        MIN(duration_minutes) as min_duration,
        ROUND(AVG(duration_minutes), 1) as avg_duration,
        MAX(duration_minutes) as max_duration,
        PERCENTILE_CONT(0.5) WITHIN GROUP (ORDER BY duration_minutes) as p50_duration,
        PERCENTILE_CONT(0.9) WITHIN GROUP (ORDER BY duration_minutes) as p90_duration
      FROM task_durations
      GROUP BY task_type
      ORDER BY task_type
    `;

    // Daily trends query
    const dailyTrendsQuery = `
      SELECT
        t.scheduled_start::date as date,
        COUNT(*) as total,
        SUM(CASE WHEN t.current_status = 'COMPLETE' THEN 1 ELSE 0 END) as completed,
        ROUND(
          SUM(CASE WHEN t.current_status = 'COMPLETE' THEN 1 ELSE 0 END)::numeric /
          COUNT(*) * 100,
          1
        ) as completion_rate
      FROM arrivy_tasks t
      WHERE t.scheduled_start IS NOT NULL
      ${dateFilter}
      ${taskTypeFilter}
      ${entityFilter}
      GROUP BY t.scheduled_start::date
      ORDER BY date
    `;

    // Delay patterns query
    const delayPatternsQuery = `
      WITH delays AS (
        SELECT
          EXTRACT(HOUR FROM t.scheduled_start) as hour_of_day,
          CASE
            WHEN s_complete.reported_at > t.scheduled_end
            THEN EXTRACT(EPOCH FROM (s_complete.reported_at - t.scheduled_end)) / 60
            ELSE 0
          END as delay_minutes
        FROM arrivy_tasks t
        LEFT JOIN arrivy_task_status s_complete ON s_complete.arrivy_task_id = t.arrivy_task_id
          AND s_complete.status_type = 'COMPLETE'
        WHERE t.scheduled_start IS NOT NULL
        AND t.scheduled_end IS NOT NULL
        ${dateFilter}
        ${taskTypeFilter}
        ${entityFilter}
      )
      SELECT
        hour_of_day::integer,
        ROUND(AVG(delay_minutes), 1) as avg_delay_minutes,
        SUM(CASE WHEN delay_minutes > 0 THEN 1 ELSE 0 END)::integer as delayed_count,
        COUNT(*)::integer as total_count
      FROM delays
      WHERE hour_of_day IS NOT NULL
      GROUP BY hour_of_day
      ORDER BY hour_of_day
    `;

    const [overviewResult, byTypeResult, durationDistResult, dailyTrendsResult, delayPatternsResult] = await Promise.all([
      sql.query(overviewQuery),
      sql.query(byTypeQuery),
      sql.query(durationDistQuery),
      sql.query(dailyTrendsQuery),
      sql.query(delayPatternsQuery)
    ]);

    return {
      overview: overviewResult.rows[0] || {
        total_tasks: 0,
        completed: 0,
        cancelled: 0,
        no_shows: 0,
        completion_rate: 0,
        avg_duration_minutes: 0,
        on_time_percentage: 0,
      },
      by_task_type: byTypeResult.rows.map((row: any) => ({
        task_type: row.task_type,
        count: parseInt(row.count),
        completed: parseInt(row.completed),
        avg_duration: parseFloat(row.avg_duration) || 0,
        completion_rate: parseFloat(row.completion_rate) || 0,
      })),
      duration_distribution: durationDistResult.rows.map((row: any) => ({
        task_type: row.task_type,
        min_duration: parseFloat(row.min_duration) || 0,
        avg_duration: parseFloat(row.avg_duration) || 0,
        max_duration: parseFloat(row.max_duration) || 0,
        p50_duration: parseFloat(row.p50_duration) || 0,
        p90_duration: parseFloat(row.p90_duration) || 0,
      })),
      daily_trends: dailyTrendsResult.rows.map((row: any) => ({
        date: row.date.toISOString().split('T')[0],
        total: parseInt(row.total),
        completed: parseInt(row.completed),
        completion_rate: parseFloat(row.completion_rate) || 0,
      })),
      delay_patterns: delayPatternsResult.rows.map((row: any) => ({
        hour_of_day: row.hour_of_day,
        avg_delay_minutes: parseFloat(row.avg_delay_minutes) || 0,
        delayed_count: row.delayed_count,
        total_count: row.total_count,
      })),
    };
  } catch (error) {
    logError('Failed to get task performance analytics', error as Error, filters);
    throw error;
  }
}

/**
 * Get exception and quality insights analytics
 * Includes exception breakdown, trends, crew correlation, quality scores
 */
export async function getExceptionAnalytics(filters: {
  startDate?: string;
  endDate?: string;
  entityId?: number;
}): Promise<{
  overview: {
    total_exceptions: number;
    exception_rate: number;
    late_count: number;
    no_show_count: number;
    other_exception_count: number;
    avg_quality_score: number;
  };
  by_exception_type: Array<{
    event_type: string;
    event_sub_type: string | null;
    count: number;
    percentage: number;
  }>;
  exception_trends: Array<{
    date: string;
    total_exceptions: number;
    late: number;
    no_show: number;
    other: number;
  }>;
  by_crew: Array<{
    entity_name: string;
    total_exceptions: number;
    late_count: number;
    no_show_count: number;
    exception_rate: number;
    quality_score: number;
  }>;
  root_causes: Array<{
    cause: string;
    count: number;
  }>;
}> {
  try {
    const { startDate, endDate, entityId } = filters;

    // Build date filter
    let dateFilter = '';
    if (startDate && endDate) {
      dateFilter = `AND e.event_time >= '${startDate}' AND e.event_time < '${endDate}'`;
    } else if (startDate) {
      dateFilter = `AND e.event_time >= '${startDate}'`;
    }

    // Build entity filter
    const entityFilter = entityId
      ? `AND EXISTS (
          SELECT 1 FROM arrivy_task_entities te
          WHERE te.arrivy_task_id = e.arrivy_task_id
          AND te.arrivy_entity_id = ${entityId}
        )`
      : '';

    // Overview query
    const overviewQuery = `
      WITH task_counts AS (
        SELECT COUNT(DISTINCT arrivy_task_id) as total_tasks
        FROM arrivy_events e
        WHERE 1=1
        ${dateFilter}
        ${entityFilter}
      ),
      exception_counts AS (
        SELECT
          COUNT(*) as total_exceptions,
          SUM(CASE WHEN event_type = 'LATE' THEN 1 ELSE 0 END) as late_count,
          SUM(CASE WHEN event_type = 'NOSHOW' THEN 1 ELSE 0 END) as no_show_count,
          SUM(CASE WHEN event_type NOT IN ('LATE', 'NOSHOW') AND event_type LIKE '%EXCEPTION%' THEN 1 ELSE 0 END) as other_exception_count
        FROM arrivy_events e
        WHERE event_type IN ('LATE', 'NOSHOW')
           OR event_type LIKE '%EXCEPTION%'
        ${dateFilter}
        ${entityFilter}
      ),
      ratings AS (
        SELECT AVG(CAST(extra_fields->>'rating' AS NUMERIC)) as avg_rating
        FROM arrivy_events e
        WHERE event_type = 'TASK_RATING'
        AND extra_fields->>'rating' IS NOT NULL
        ${dateFilter}
        ${entityFilter}
      )
      SELECT
        ec.total_exceptions,
        ROUND(
          ec.total_exceptions::numeric /
          NULLIF(tc.total_tasks, 0) * 100,
          1
        ) as exception_rate,
        ec.late_count,
        ec.no_show_count,
        ec.other_exception_count,
        ROUND(COALESCE(r.avg_rating * 20, 75), 1) as avg_quality_score
      FROM exception_counts ec
      CROSS JOIN task_counts tc
      LEFT JOIN ratings r ON true
    `;

    // By exception type query
    const byTypeQuery = `
      WITH total AS (
        SELECT COUNT(*) as total_count
        FROM arrivy_events e
        WHERE event_type IN ('LATE', 'NOSHOW')
           OR event_type LIKE '%EXCEPTION%'
        ${dateFilter}
        ${entityFilter}
      )
      SELECT
        e.event_type,
        e.event_sub_type,
        COUNT(*) as count,
        ROUND(COUNT(*)::numeric / t.total_count * 100, 1) as percentage
      FROM arrivy_events e
      CROSS JOIN total t
      WHERE e.event_type IN ('LATE', 'NOSHOW')
         OR e.event_type LIKE '%EXCEPTION%'
      ${dateFilter}
      ${entityFilter}
      GROUP BY e.event_type, e.event_sub_type, t.total_count
      ORDER BY count DESC
    `;

    // Exception trends query
    const trendsQuery = `
      SELECT
        e.event_time::date as date,
        COUNT(*) as total_exceptions,
        SUM(CASE WHEN event_type = 'LATE' THEN 1 ELSE 0 END) as late,
        SUM(CASE WHEN event_type = 'NOSHOW' THEN 1 ELSE 0 END) as no_show,
        SUM(CASE WHEN event_type NOT IN ('LATE', 'NOSHOW') THEN 1 ELSE 0 END) as other
      FROM arrivy_events e
      WHERE event_type IN ('LATE', 'NOSHOW')
         OR event_type LIKE '%EXCEPTION%'
      ${dateFilter}
      ${entityFilter}
      GROUP BY e.event_time::date
      ORDER BY date
    `;

    // By crew query
    const byCrewQuery = `
      WITH crew_tasks AS (
        SELECT
          ent.name as entity_name,
          COUNT(DISTINCT t.arrivy_task_id) as total_tasks,
          SUM(CASE WHEN t.current_status = 'COMPLETE' THEN 1 ELSE 0 END) as completed_tasks
        FROM arrivy_entities ent
        JOIN arrivy_task_entities te ON te.arrivy_entity_id = ent.arrivy_entity_id
        JOIN arrivy_tasks t ON t.arrivy_task_id = te.arrivy_task_id
        WHERE t.scheduled_start IS NOT NULL
        ${startDate ? `AND t.scheduled_start >= '${startDate}'` : ''}
        ${endDate ? `AND t.scheduled_start < '${endDate}'` : ''}
        GROUP BY ent.name
      ),
      crew_exceptions AS (
        SELECT
          ent.name as entity_name,
          COUNT(*) as total_exceptions,
          SUM(CASE WHEN e.event_type = 'LATE' THEN 1 ELSE 0 END) as late_count,
          SUM(CASE WHEN e.event_type = 'NOSHOW' THEN 1 ELSE 0 END) as no_show_count
        FROM arrivy_entities ent
        JOIN arrivy_task_entities te ON te.arrivy_entity_id = ent.arrivy_entity_id
        JOIN arrivy_events e ON e.arrivy_task_id = te.arrivy_task_id
        WHERE e.event_type IN ('LATE', 'NOSHOW')
           OR e.event_type LIKE '%EXCEPTION%'
        ${dateFilter}
        GROUP BY ent.name
      ),
      crew_ratings AS (
        SELECT
          ent.name as entity_name,
          AVG(CAST(e.extra_fields->>'rating' AS NUMERIC)) as avg_rating
        FROM arrivy_entities ent
        JOIN arrivy_task_entities te ON te.arrivy_entity_id = ent.arrivy_entity_id
        JOIN arrivy_events e ON e.arrivy_task_id = te.arrivy_task_id
        WHERE e.event_type = 'TASK_RATING'
        AND e.extra_fields->>'rating' IS NOT NULL
        ${dateFilter}
        GROUP BY ent.name
      )
      SELECT
        ct.entity_name,
        COALESCE(ce.total_exceptions, 0) as total_exceptions,
        COALESCE(ce.late_count, 0) as late_count,
        COALESCE(ce.no_show_count, 0) as no_show_count,
        ROUND(
          COALESCE(ce.total_exceptions, 0)::numeric /
          NULLIF(ct.total_tasks, 0) * 100,
          1
        ) as exception_rate,
        ROUND(
          COALESCE(cr.avg_rating * 20, 75) +
          (25 - (COALESCE(ce.total_exceptions, 0)::numeric / NULLIF(ct.total_tasks, 0) * 100)),
          1
        ) as quality_score
      FROM crew_tasks ct
      LEFT JOIN crew_exceptions ce ON ce.entity_name = ct.entity_name
      LEFT JOIN crew_ratings cr ON cr.entity_name = ct.entity_name
      ORDER BY exception_rate DESC
    `;

    // Root causes query (extract from event messages)
    const rootCausesQuery = `
      SELECT
        LOWER(message) as cause,
        COUNT(*) as count
      FROM arrivy_events e
      WHERE (event_type IN ('LATE', 'NOSHOW') OR event_type LIKE '%EXCEPTION%')
      AND message IS NOT NULL
      ${dateFilter}
      ${entityFilter}
      GROUP BY LOWER(message)
      ORDER BY count DESC
      LIMIT 10
    `;

    const [overviewResult, byTypeResult, trendsResult, byCrewResult, rootCausesResult] = await Promise.all([
      sql.query(overviewQuery),
      sql.query(byTypeQuery),
      sql.query(trendsQuery),
      sql.query(byCrewQuery),
      sql.query(rootCausesQuery)
    ]);

    return {
      overview: overviewResult.rows[0] || {
        total_exceptions: 0,
        exception_rate: 0,
        late_count: 0,
        no_show_count: 0,
        other_exception_count: 0,
        avg_quality_score: 75,
      },
      by_exception_type: byTypeResult.rows.map((row: any) => ({
        event_type: row.event_type,
        event_sub_type: row.event_sub_type,
        count: parseInt(row.count),
        percentage: parseFloat(row.percentage),
      })),
      exception_trends: trendsResult.rows.map((row: any) => ({
        date: row.date.toISOString().split('T')[0],
        total_exceptions: parseInt(row.total_exceptions),
        late: parseInt(row.late),
        no_show: parseInt(row.no_show),
        other: parseInt(row.other),
      })),
      by_crew: byCrewResult.rows.map((row: any) => ({
        entity_name: row.entity_name,
        total_exceptions: parseInt(row.total_exceptions),
        late_count: parseInt(row.late_count),
        no_show_count: parseInt(row.no_show_count),
        exception_rate: parseFloat(row.exception_rate),
        quality_score: parseFloat(row.quality_score),
      })),
      root_causes: rootCausesResult.rows.map((row: any) => ({
        cause: row.cause,
        count: parseInt(row.count),
      })),
    };
  } catch (error) {
    logError('Failed to get exception analytics', error as Error, filters);
    throw error;
  }
}

/**
 * Get workload distribution analytics
 * Shows how tasks are distributed across crew, capacity utilization, balance metrics
 */
export async function getWorkloadDistribution(filters: {
  date?: string; // Specific date to analyze
  includeScheduled?: boolean; // Include future scheduled tasks
}): Promise<{
  overview: {
    total_active_tasks: number;
    total_crew_members: number;
    avg_tasks_per_crew: number;
    workload_imbalance_score: number; // std deviation
    over_capacity_count: number;
    under_capacity_count: number;
  };
  by_crew: Array<{
    entity_name: string;
    active_tasks: number;
    scheduled_tasks: number;
    completed_today: number;
    capacity_percentage: number; // vs ideal distribution
    status: 'over' | 'balanced' | 'under';
  }>;
  hourly_distribution: Array<{
    hour: number;
    total_scheduled: number;
    crew_count: number;
    avg_tasks_per_crew: number;
  }>;
}> {
  try {
    const { date, includeScheduled } = filters;
    const targetDate = date || new Date().toISOString().split('T')[0];

    // Overview query
    const overviewQuery = `
      WITH crew_workload AS (
        SELECT
          ent.arrivy_entity_id,
          COUNT(DISTINCT t.arrivy_task_id) FILTER (
            WHERE t.current_status NOT IN ('COMPLETE', 'CANCELLED', 'NOSHOW')
            AND t.scheduled_start::date <= '${targetDate}'::date
          ) as active_tasks
        FROM arrivy_entities ent
        LEFT JOIN arrivy_task_entities te ON te.arrivy_entity_id = ent.arrivy_entity_id
        LEFT JOIN arrivy_tasks t ON t.arrivy_task_id = te.arrivy_task_id
        GROUP BY ent.arrivy_entity_id
      ),
      workload_stats AS (
        SELECT
          COUNT(*) as total_crew,
          SUM(active_tasks) as total_active,
          AVG(active_tasks) as avg_tasks,
          STDDEV(active_tasks) as stddev_tasks
        FROM crew_workload
      )
      SELECT
        total_active::integer,
        total_crew::integer,
        ROUND(avg_tasks, 1) as avg_tasks_per_crew,
        ROUND(COALESCE(stddev_tasks, 0), 2) as workload_imbalance_score,
        (SELECT COUNT(*) FROM crew_workload WHERE active_tasks > avg_tasks * 1.5)::integer as over_capacity_count,
        (SELECT COUNT(*) FROM crew_workload WHERE active_tasks < avg_tasks * 0.5 AND avg_tasks > 0)::integer as under_capacity_count
      FROM workload_stats
    `;

    // By crew query
    const byCrewQuery = `
      WITH ideal_load AS (
        SELECT AVG(task_count) as ideal
        FROM (
          SELECT
            COUNT(DISTINCT t.arrivy_task_id) as task_count
          FROM arrivy_entities ent
          LEFT JOIN arrivy_task_entities te ON te.arrivy_entity_id = ent.arrivy_entity_id
          LEFT JOIN arrivy_tasks t ON t.arrivy_task_id = te.arrivy_task_id
            AND t.current_status NOT IN ('COMPLETE', 'CANCELLED', 'NOSHOW')
            AND t.scheduled_start::date <= '${targetDate}'::date
          GROUP BY ent.arrivy_entity_id
        ) counts
      )
      SELECT
        ent.name as entity_name,
        COUNT(DISTINCT t.arrivy_task_id) FILTER (
          WHERE t.current_status NOT IN ('COMPLETE', 'CANCELLED', 'NOSHOW')
          AND t.scheduled_start::date <= '${targetDate}'::date
        )::integer as active_tasks,
        ${includeScheduled ? `
        COUNT(DISTINCT t.arrivy_task_id) FILTER (
          WHERE t.current_status NOT IN ('COMPLETE', 'CANCELLED', 'NOSHOW')
          AND t.scheduled_start::date > '${targetDate}'::date
        )::integer as scheduled_tasks,
        ` : '0 as scheduled_tasks,'}
        COUNT(DISTINCT s.arrivy_task_id) FILTER (
          WHERE s.status_type = 'COMPLETE'
          AND s.reported_at::date = '${targetDate}'::date
        )::integer as completed_today,
        ROUND(
          (COUNT(DISTINCT t.arrivy_task_id) FILTER (
            WHERE t.current_status NOT IN ('COMPLETE', 'CANCELLED', 'NOSHOW')
            AND t.scheduled_start::date <= '${targetDate}'::date
          )::numeric / NULLIF(ideal.ideal, 0)) * 100,
          1
        ) as capacity_percentage,
        CASE
          WHEN COUNT(DISTINCT t.arrivy_task_id) FILTER (
            WHERE t.current_status NOT IN ('COMPLETE', 'CANCELLED', 'NOSHOW')
            AND t.scheduled_start::date <= '${targetDate}'::date
          ) > ideal.ideal * 1.5 THEN 'over'
          WHEN COUNT(DISTINCT t.arrivy_task_id) FILTER (
            WHERE t.current_status NOT IN ('COMPLETE', 'CANCELLED', 'NOSHOW')
            AND t.scheduled_start::date <= '${targetDate}'::date
          ) < ideal.ideal * 0.5 AND ideal.ideal > 0 THEN 'under'
          ELSE 'balanced'
        END as status
      FROM arrivy_entities ent
      CROSS JOIN ideal_load ideal
      LEFT JOIN arrivy_task_entities te ON te.arrivy_entity_id = ent.arrivy_entity_id
      LEFT JOIN arrivy_tasks t ON t.arrivy_task_id = te.arrivy_task_id
      LEFT JOIN arrivy_task_status s ON s.arrivy_task_id = t.arrivy_task_id
      GROUP BY ent.name, ideal.ideal
      ORDER BY active_tasks DESC
    `;

    // Hourly distribution query
    const hourlyQuery = `
      SELECT
        EXTRACT(HOUR FROM t.scheduled_start)::integer as hour,
        COUNT(DISTINCT t.arrivy_task_id)::integer as total_scheduled,
        COUNT(DISTINCT te.arrivy_entity_id)::integer as crew_count,
        ROUND(
          COUNT(DISTINCT t.arrivy_task_id)::numeric /
          NULLIF(COUNT(DISTINCT te.arrivy_entity_id), 0),
          1
        ) as avg_tasks_per_crew
      FROM arrivy_tasks t
      JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
      WHERE t.scheduled_start::date = '${targetDate}'::date
      GROUP BY EXTRACT(HOUR FROM t.scheduled_start)
      ORDER BY hour
    `;

    const [overviewResult, byCrewResult, hourlyResult] = await Promise.all([
      sql.query(overviewQuery),
      sql.query(byCrewQuery),
      sql.query(hourlyQuery)
    ]);

    return {
      overview: overviewResult.rows[0] || {
        total_active_tasks: 0,
        total_crew_members: 0,
        avg_tasks_per_crew: 0,
        workload_imbalance_score: 0,
        over_capacity_count: 0,
        under_capacity_count: 0,
      },
      by_crew: byCrewResult.rows.map((row: any) => ({
        entity_name: row.entity_name,
        active_tasks: row.active_tasks,
        scheduled_tasks: row.scheduled_tasks,
        completed_today: row.completed_today,
        capacity_percentage: parseFloat(row.capacity_percentage) || 0,
        status: row.status as 'over' | 'balanced' | 'under',
      })),
      hourly_distribution: hourlyResult.rows.map((row: any) => ({
        hour: row.hour,
        total_scheduled: row.total_scheduled,
        crew_count: row.crew_count,
        avg_tasks_per_crew: parseFloat(row.avg_tasks_per_crew) || 0,
      })),
    };
  } catch (error) {
    logError('Failed to get workload distribution', error as Error, filters);
    throw error;
  }
}

