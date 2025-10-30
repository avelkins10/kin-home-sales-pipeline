# Arrivy Status & Type Detection Fix

## Root Cause Analysis

### Issue 1: Task Status Not Syncing (Tania Morris showing "Unknown" instead of "COMPLETE")

**Problem:** Tasks show COMPLETE in Arrivy but display as "Unknown" in our app.

**Root Cause:**
1. `task.status` from `GET /tasks/{id}` can be stale or incorrect
2. The TRUE current status is in the status history array: `GET /tasks/{id}/status`
3. The latest entry in the status history array is the actual current status
4. Our cron sync was using `task.status` instead of checking status history

**Solution:**
- Updated cron sync to fetch status history and use the latest status entry
- Status update priority: `latest_status_from_history` > `task.status` > `NOT_STARTED`
- Webhook handler already correct (uses `EVENT_SUB_TYPE`)

### Issue 2: Task Type Misclassification (Shawn Knutsen showing "Service" instead of "Survey")

**Problem:** Tasks incorrectly classified as "Service - General" when they should be "Surveys - Site Survey".

**Root Cause:**
- Task type detection logic may not be checking all available signals
- Need to verify what data Arrivy actually returns for these tasks

**Solution:**
- Created debug endpoint: `/api/admin/fix-task-status-and-type?customer=Knutsen`
- Fetches fresh data from Arrivy API and shows all available fields
- Helps diagnose why tasks are misclassified

## Arrivy API Understanding

### Task Status Lifecycle

According to Arrivy API docs:

1. **Task Creation:** `POST /tasks/new` → Status: `NOT_STARTED`
2. **Status Updates:** `POST /tasks/{id}/status/new` → Creates status history entry
3. **Status History:** `GET /tasks/{id}/status` → Returns array of all status updates
4. **Current Status:** The **latest entry** in status history array is the TRUE current status

**Key Insight:** `task.status` from `GET /tasks/{id}` may not reflect the latest status!
Always check `GET /tasks/{id}/status` for the most recent status.

### Task Type Detection Priority

Our detection logic (in order of reliability):

1. **extra_fields.task_type** (explicit) - Rarely set
2. **extra_fields keys** (e.g., "Notes for Surveyor") - MOST RELIABLE indicator
3. **Group membership** (task.group.name)
4. **Template name** (task.template or task.template_id → lookup)
5. **Title keywords** (task.title)
6. **Default:** "Service - General"

### Status Update Flow

```
Arrivy Mobile App/Web → Status Change
  ↓
Arrivy API → Webhook POST /api/webhooks/arrivy
  ↓
handleTaskStatusEvent() → updateArrivyTaskStatus()
  ↓
UPDATE arrivy_tasks SET current_status = EVENT_SUB_TYPE
```

**OR**

```
Cron Sync (hourly) → GET /tasks (list)
  ↓
For each task: GET /tasks/{id}/status (history)
  ↓
Find latest status entry → UPDATE current_status
```

## Files Changed

1. **app/api/cron/sync-arrivy/route.ts**
   - Now fetches status history for each task
   - Updates `current_status` from latest status history entry
   - Falls back to `task.status` if no history exists

2. **app/api/admin/fix-task-status-and-type/route.ts** (NEW)
   - Debug endpoint to force sync specific tasks
   - Shows what data Arrivy returns vs what's in database
   - Helps diagnose classification issues

## Testing

1. **Test Status Sync:**
   ```bash
   # Wait for cron sync or trigger manually
   curl "https://kin-home-sales-pipeline.vercel.app/api/cron/sync-arrivy"
   
   # Or force sync specific tasks
   curl "https://kin-home-sales-pipeline.vercel.app/api/admin/fix-task-status-and-type?customer=Tania&updateStatus=true"
   ```

2. **Test Task Type Detection:**
   ```bash
   # Check what Arrivy has for Knutsen
   curl "https://kin-home-sales-pipeline.vercel.app/api/admin/fix-task-status-and-type?customer=Knutsen&updateType=true"
   ```

3. **Verify in App:**
   - Refresh Field Operations page
   - Tania Morris should show "COMPLETE" status
   - Shawn Knutsen should show correct task type (if Arrivy has the data)

## Next Steps

1. ✅ Status sync fixed - cron now uses status history
2. ✅ Debug endpoint created - can diagnose task type issues
3. ⏳ Run fix endpoint for Knutsen to see what Arrivy data shows
4. ⏳ Update task type detection if needed based on actual Arrivy data

