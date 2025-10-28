// lib/db/arrivy.ts
import { sql } from '@vercel/postgres';
import { logError } from '@/lib/logging/logger';

/**
 * Database access functions for Arrivy integration
 */

// =============================================================================
// ARRIVY TASKS
// =============================================================================

export interface ArrivyTaskData {
  arrivy_task_id: number;
  url_safe_id: string;
  quickbase_project_id: string;
  quickbase_record_id: number;
  customer_name?: string | null;
  customer_phone?: string | null;
  customer_email?: string | null;
  customer_address?: string | null;
  task_type?: string | null;
  scheduled_start?: Date | null;
  scheduled_end?: Date | null;
  assigned_entity_ids?: number[] | null;
  current_status?: string | null;
  tracker_url?: string | null;
  template_id?: string | null;
  extra_fields?: Record<string, any> | null;
  synced_at?: Date | null;
}

export interface ArrivyTaskRecord extends ArrivyTaskData {
  id: number;
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
        assigned_entity_ids,
        current_status,
        tracker_url,
        template_id,
        extra_fields,
        synced_at
      ) VALUES (
        ${taskData.arrivy_task_id},
        ${taskData.url_safe_id},
        ${taskData.quickbase_project_id},
        ${taskData.quickbase_record_id},
        ${taskData.customer_name},
        ${taskData.customer_phone},
        ${taskData.customer_email},
        ${taskData.customer_address},
        ${taskData.task_type},
        ${taskData.scheduled_start},
        ${taskData.scheduled_end},
        ${taskData.assigned_entity_ids || []},
        ${taskData.current_status},
        ${taskData.tracker_url},
        ${taskData.template_id},
        ${JSON.stringify(taskData.extra_fields || {})},
        ${taskData.synced_at || new Date()}
      )
      ON CONFLICT (arrivy_task_id)
      DO UPDATE SET
        url_safe_id = EXCLUDED.url_safe_id,
        customer_name = EXCLUDED.customer_name,
        customer_phone = EXCLUDED.customer_phone,
        customer_email = EXCLUDED.customer_email,
        customer_address = EXCLUDED.customer_address,
        task_type = EXCLUDED.task_type,
        scheduled_start = EXCLUDED.scheduled_start,
        scheduled_end = EXCLUDED.scheduled_end,
        assigned_entity_ids = EXCLUDED.assigned_entity_ids,
        current_status = EXCLUDED.current_status,
        tracker_url = EXCLUDED.tracker_url,
        template_id = EXCLUDED.template_id,
        extra_fields = EXCLUDED.extra_fields,
        synced_at = EXCLUDED.synced_at,
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
 */
export async function insertArrivyEvent(eventData: ArrivyEventData): Promise<ArrivyEventRecord> {
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

    return result.rows[0];
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
    limit?: number;
    offset?: number;
  } = {}
): Promise<ArrivyEventRecord[]> {
  const {
    taskId,
    eventType,
    startDate,
    endDate,
    limit = 50,
    offset = 0,
  } = filters;

  try {
    let query = `SELECT * FROM arrivy_events WHERE 1=1`;
    const params: any[] = [];
    let paramIndex = 1;

    if (taskId) {
      query += ` AND arrivy_task_id = $${paramIndex}`;
      params.push(taskId);
      paramIndex++;
    }

    if (eventType) {
      query += ` AND event_type = $${paramIndex}`;
      params.push(eventType);
      paramIndex++;
    }

    if (startDate) {
      query += ` AND event_time >= $${paramIndex}`;
      params.push(startDate);
      paramIndex++;
    }

    if (endDate) {
      query += ` AND event_time <= $${paramIndex}`;
      params.push(endDate);
      paramIndex++;
    }

    query += ` ORDER BY event_time DESC LIMIT $${paramIndex} OFFSET $${paramIndex + 1}`;
    params.push(limit, offset);

    const result = await sql.query<ArrivyEventRecord>(query, params);
    return result.rows;
  } catch (error) {
    logError('Failed to get Arrivy events with filters', error as Error, filters);
    throw error;
  }
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
 */
export async function insertArrivyTaskStatus(statusData: ArrivyTaskStatusData): Promise<ArrivyTaskStatusRecord> {
  try {
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
        t.*,
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
      LEFT JOIN unnest(t.assigned_entity_ids) entity_id ON true
      LEFT JOIN arrivy_entities e ON e.arrivy_entity_id = entity_id
      WHERE 1=1
    `;

    const params: any[] = [];
    let paramIndex = 1;

    if (coordinatorEmail) {
      query += ` AND EXISTS (
        SELECT 1 FROM arrivy_entities
        WHERE arrivy_entity_id = ANY(t.assigned_entity_ids)
        AND email = $${paramIndex}
      )`;
      params.push(coordinatorEmail);
      paramIndex++;
    }

    if (taskType) {
      query += ` AND t.task_type = $${paramIndex}`;
      params.push(taskType);
      paramIndex++;
    }

    if (status) {
      query += ` AND t.current_status = $${paramIndex}`;
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
        OR t.quickbase_project_id ILIKE $${paramIndex}
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

