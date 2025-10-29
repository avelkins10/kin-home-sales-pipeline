# Join Table Migration - Implementation Guide

**Status:** Partially Complete - Code updates required  
**Date:** October 28, 2025

---

## ✅ What's Complete

### 1. Database Schema
- ✅ Migration 015 created `arrivy_task_entities` join table
- ✅ Foreign keys established with CASCADE delete
- ✅ Unique constraint on (arrivy_task_id, arrivy_entity_id)
- ✅ 4 indexes created for performance
- ✅ Backfill logic included to migrate existing array data

### 2. Database Functions
- ✅ `assignEntityToTask()` - Assign single entity
- ✅ `unassignEntityFromTask()` - Remove assignment
- ✅ `getTaskEntities()` - Get entities for a task (uses JOIN)
- ✅ `getEntityTasks()` - Get tasks for an entity (uses JOIN)
- ✅ `setTaskEntities()` - Bulk replace assignments (atomic)

### 3. Type Definitions
- ✅ `FieldTrackingTask` updated to use `assigned_entities` array of objects
- ✅ `ArrivyTaskEntityData` interface defined
- ✅ `ArrivyTaskEntityRecord` interface defined

---

## ⚠️ What's Remaining

### 1. Update upsertArrivyTask in lib/db/arrivy.ts

**Current code writes to `assigned_entity_ids` array:**
```typescript
assigned_entity_ids: (taskData.assigned_entity_ids && taskData.assigned_entity_ids.length > 0) 
  ? sql`ARRAY[${sql.join(taskData.assigned_entity_ids.map(id => sql`${id}`), sql`, `)}]::bigint[]`
  : sql`ARRAY[]::bigint[]`,
```

**Needs to become:**
```typescript
// Step 1: Upsert task WITHOUT assigned_entity_ids
const result = await sql<ArrivyTaskRecord>`
  INSERT INTO arrivy_tasks (
    arrivy_task_id,
    url_safe_id,
    -- all other fields except assigned_entity_ids
  ) VALUES (
    ${taskData.arrivy_task_id},
    ${taskData.url_safe_id},
    -- all other values
  )
  ON CONFLICT (arrivy_task_id) DO UPDATE SET
    -- all fields except assigned_entity_ids
  RETURNING *
`;

// Step 2: Sync entity assignments via join table
if (taskData.entity_ids && taskData.entity_ids.length > 0) {
  await setTaskEntities(
    taskData.arrivy_task_id,
    taskData.entity_ids,
    'system_sync'
  );
}

return result.rows[0];
```

### 2. Update getFieldTrackingTasks query

**Current query includes:**
```sql
LEFT JOIN LATERAL unnest(t.assigned_entity_ids) AS eid(id) ON true
```

**Needs to become:**
```sql
LEFT JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
LEFT JOIN arrivy_entities e ON e.arrivy_entity_id = te.arrivy_entity_id
```

### 3. Update API endpoint app/api/operations/field-tracking/tasks/[id]/route.ts

**Current code:**
```typescript
const entities = await getArrivyEntitiesByIds(task.assigned_entity_ids);
entityNames = entities.map(e => e.name);
```

**Needs to become:**
```typescript
const entities = await getTaskEntities(task.arrivy_task_id);
const assigned_entities = entities.map(e => ({
  id: e.arrivy_entity_id,
  name: e.name,
  email: e.email
}));
const entity_names = entities.map(e => e.name);

return NextResponse.json({ 
  task: {
    ...task,
    assigned_entities,
    entity_names,
  },
  statusHistory,
  events,
}, { status: 200 });
```

### 4. Update service layer lib/integrations/arrivy/service.ts

When syncing from Arrivy webhooks, use `setTaskEntities()` instead of passing `assigned_entity_ids` to upsert:

```typescript
// After upserting task
await setTaskEntities(
  arrivyTaskId,
  webhookPayload.entity_ids || [],
  'arrivy_webhook'
);
```

### 5. Remove assigned_entity_ids references

Search for all occurrences of `assigned_entity_ids` and replace with join table logic:

```bash
# Find all references
grep -r "assigned_entity_ids" --include="*.ts" --include="*.tsx" .

# Expected files to update:
# - lib/db/arrivy.ts (upsertArrivyTask, getFieldTrackingTasks)
# - lib/integrations/arrivy/service.ts (syncTaskFromArrivy)
# - app/api/operations/field-tracking/* (any endpoints)
# - components/operations/* (if any direct references)
```

---

## 📋 Step-by-Step Migration

### Step 1: Run Migration 015 (If not done)
```bash
psql $DATABASE_URL -f lib/db/migrations/015_create_arrivy_task_entities_join_table.sql
```

**Verify:**
```sql
SELECT COUNT(*) FROM arrivy_task_entities;
-- Should match total assignments from arrays
```

### Step 2: Update lib/db/arrivy.ts

#### A. Update upsertArrivyTask function

Remove `assigned_entity_ids` from INSERT and UPDATE:

```typescript
export async function upsertArrivyTask(taskData: ArrivyTaskData): Promise<ArrivyTaskRecord> {
  try {
    // Step 1: Upsert task without entity assignments
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
        -- REMOVED: assigned_entity_ids
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
        -- REMOVED: assigned_entity_ids array
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
        -- REMOVED: assigned_entity_ids
        current_status = EXCLUDED.current_status,
        tracker_url = EXCLUDED.tracker_url,
        template_id = EXCLUDED.template_id,
        extra_fields = EXCLUDED.extra_fields,
        synced_at = EXCLUDED.synced_at,
        updated_at = NOW()
      RETURNING *
    `;

    // Step 2: Sync entity assignments via join table
    // Note: entity_ids should be passed separately now
    // This will be called from the service layer after upsert

    return result.rows[0];
  } catch (error) {
    logError('Failed to upsert Arrivy task', error as Error, {
      arrivy_task_id: taskData.arrivy_task_id,
      quickbase_project_id: taskData.quickbase_project_id,
    });
    throw error;
  }
}
```

#### B. Update getFieldTrackingTasks query

Replace array unnest with proper JOINs:

```typescript
export async function getFieldTrackingTasks(filters: {
  coordinatorEmail?: string;
  taskType?: string;
  status?: string;
  dateRange?: { start: Date; end: Date };
  search?: string;
  limit?: number;
  offset?: number;
}): Promise<FieldTrackingTaskWithDetails[]> {
  // ... filter setup code ...

  try {
    let query = `
      SELECT 
        t.*,
        s.status_type as latest_status,
        s.reported_at as latest_status_time,
        ARRAY_AGG(
          DISTINCT jsonb_build_object(
            'id', e.arrivy_entity_id,
            'name', e.name,
            'email', e.email
          )
        ) FILTER (WHERE e.arrivy_entity_id IS NOT NULL) as assigned_entities,
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

    // ... rest of query with filters ...
    
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
```

### Step 3: Update Service Layer

In `lib/integrations/arrivy/service.ts`, after upserting task, sync entities:

```typescript
export async function syncTaskFromArrivy(arrivyTask: ArrivyTask): Promise<ArrivyTaskRecord> {
  // Step 1: Upsert task
  const task = await upsertArrivyTask({
    arrivy_task_id: arrivyTask.id,
    url_safe_id: arrivyTask.url_safe_id,
    // ... other fields ...
    // DON'T pass assigned_entity_ids
  });

  // Step 2: Sync entity assignments via join table
  if (arrivyTask.entity_ids && arrivyTask.entity_ids.length > 0) {
    await setTaskEntities(
      arrivyTask.id,
      arrivyTask.entity_ids,
      'arrivy_sync'
    );
  } else {
    // Clear assignments if no entities
    await setTaskEntities(arrivyTask.id, [], 'arrivy_sync');
  }

  return task;
}
```

### Step 4: Update API Endpoints

In `app/api/operations/field-tracking/tasks/[id]/route.ts`:

```typescript
// In GET handler, replace entity fetching:
const entities = await getTaskEntities(task.arrivy_task_id);
const assigned_entities = entities.map(e => ({
  id: e.arrivy_entity_id,
  name: e.name,
  email: e.email
}));
const entity_names = entities.map(e => e.name);

return NextResponse.json({ 
  task: {
    ...task,
    assigned_entities,
    entity_names,
  },
  statusHistory,
  events,
}, { status: 200 });

// In PUT handler, update entity assignments:
if (updatedTask.entity_ids) {
  await setTaskEntities(
    task.arrivy_task_id,
    updatedTask.entity_ids,
    auth.session.user.email
  );
}
```

### Step 5: Test Thoroughly

```bash
# 1. Test task creation
curl -X POST http://localhost:3000/api/operations/field-tracking/tasks \
  -d '{"projectId":"TEST","taskType":"survey","entity_ids":[1,2]}'

# 2. Verify join table
psql $DATABASE_URL -c "SELECT * FROM arrivy_task_entities WHERE arrivy_task_id = <task_id>;"

# 3. Test task retrieval
curl http://localhost:3000/api/operations/field-tracking/tasks/TEST

# 4. Test dashboard
open http://localhost:3000/operations/scheduling

# 5. Test entity reassignment
curl -X PUT http://localhost:3000/api/operations/field-tracking/tasks/TEST \
  -d '{"entity_ids":[3,4]}'

# 6. Verify old assignments removed
psql $DATABASE_URL -c "SELECT * FROM arrivy_task_entities WHERE arrivy_task_id = <task_id>;"
```

### Step 6: Run Migration 018 (Drop Array Column)

**ONLY after verifying everything works:**

```bash
psql $DATABASE_URL -f lib/db/migrations/018_drop_assigned_entity_ids_array.sql
```

**This is IRREVERSIBLE without a backup.**

---

## 🎯 Benefits After Migration

### Performance
- ✅ Proper JOINs instead of array operations
- ✅ Indexes on join table for fast lookups
- ✅ No N+1 queries when fetching entity names

### Data Integrity
- ✅ Foreign key constraints enforce referential integrity
- ✅ CASCADE delete removes orphaned assignments
- ✅ UNIQUE constraint prevents duplicate assignments

### Flexibility
- ✅ Can track assignment history (assigned_at, assigned_by)
- ✅ Can add notes per assignment
- ✅ Can query "which entities are assigned most"
- ✅ Can query "which tasks has entity X"

### Maintainability
- ✅ Standard SQL patterns
- ✅ Easier to understand and modify
- ✅ Better query optimization by database

---

## 📊 Migration Checklist

### Pre-Migration
- [ ] Backup database
- [ ] Run migration 015
- [ ] Verify backfill completed
- [ ] Document current array usage

### Code Updates
- [ ] Update `ArrivyTaskData` interface
- [ ] Update `upsertArrivyTask()` function
- [ ] Update `getFieldTrackingTasks()` query
- [ ] Update service layer sync functions
- [ ] Update API endpoints (GET, PUT)
- [ ] Update dashboard components if needed

### Testing
- [ ] Test task creation with entities
- [ ] Test task update (change entities)
- [ ] Test entity deletion (cascade works)
- [ ] Test dashboard display
- [ ] Test webhook processing
- [ ] Load test with 100+ tasks

### Post-Migration
- [ ] Run migration 018 (drop array column)
- [ ] Monitor performance
- [ ] Update documentation
- [ ] Deploy to production

---

## 🚨 Rollback Plan

If issues occur BEFORE running migration 018:

```sql
-- 1. Repopulate array from join table
UPDATE arrivy_tasks t
SET assigned_entity_ids = (
  SELECT ARRAY_AGG(arrivy_entity_id)
  FROM arrivy_task_entities
  WHERE arrivy_task_id = t.arrivy_task_id
);

-- 2. Drop join table
DROP TABLE IF EXISTS arrivy_task_entities;

-- 3. Revert code changes
```

If issues occur AFTER running migration 018:

```sql
-- 1. Restore from backup
-- or
-- 2. Recreate array column
ALTER TABLE arrivy_tasks ADD COLUMN assigned_entity_ids BIGINT[];

-- 3. Repopulate from join table
UPDATE arrivy_tasks t
SET assigned_entity_ids = (
  SELECT ARRAY_AGG(arrivy_entity_id)
  FROM arrivy_task_entities
  WHERE arrivy_task_id = t.arrivy_task_id
);
```

---

##Status: Implementation In Progress

**Next Step:** Update `lib/db/arrivy.ts` `upsertArrivyTask()` function to remove `assigned_entity_ids` and use `setTaskEntities()` instead.

