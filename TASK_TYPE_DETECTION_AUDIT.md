# Task Type Detection Audit

## Current Problem
All tasks showing as "SERVICE - GENERAL" even though they should be "Surveys - Site Survey", "Installations", etc.

## Root Cause Analysis

### What We Found

#### 1. Database vs. Arrivy API Data Mismatch
**Example: Justin Lambert task**

**Database has:**
```json
{
  "arrivy_task_id": "5045922937044992",
  "customer_name": "Justin Lambert",
  "task_type": "Service - General",  // ❌ WRONG
  "template_id": null,                // ❌ NOT STORED!
  "extra_fields": {
    "task_color": "#03a9f4",
    "Notes for Surveyor": "none"      // ✅ This indicates Survey!
  }
}
```

**Arrivy API has:**
```json
{
  "id": 5045922937044992,
  "title": "Justin Lambert",
  "template": 6088550194479104,       // ✅ Template ID exists!
  "extra_fields": {
    "Notes for Surveyor": "none",     // ✅ Survey indicator
    "task_color": "#03a9f4"
  }
}
```

### 2. Key Issues Identified

#### Issue #1: Template ID Not Being Stored
- **Problem:** `template_id` is `null` in database but exists in Arrivy API
- **Impact:** Can't use template-based detection
- **Location:** Task sync code not saving `template` field

#### Issue #2: Template is Numeric, Code Expects String
- **Problem:** `task.template` is a number (6088550194479104) but code calls `.toLowerCase()` on it
- **Fixed:** Added type checking in utils.ts
- **Result:** Still doesn't help because we're not looking up template names

#### Issue #3: Reliable Detection Signals Not Being Used
**Most Reliable Signals (in order):**
1. ✅ **extra_fields keys** - "Notes for Surveyor" = Survey, "Notes for Installer" = Install
2. ❌ **template name** - Not accessible (only have numeric ID, not template name)
3. ❌ **template_id mapping** - Would need to fetch template details from API
4. ⚠️ **title** - Unreliable (customer names, not task types)
5. ⚠️ **extra_fields.task_type** - Not consistently set

### 3. Current Detection Logic (lib/integrations/arrivy/utils.ts)

```typescript
export function extractTaskType(task: ArrivyTask): string {
  // 1. Check extra_fields.task_type (rarely set)
  // 2. Check forms array (not available in most tasks)
  // 3. Check group name (not available)
  // 4. Check template (numeric ID, can't use)
  // 5. Check title (customer name, not helpful)
  // 6. Default to 'Service - General'
}
```

## Recommended Solution

### Strategy: Use extra_fields Keys as Primary Signal

The **most reliable** indicator is the presence of specific keys in `extra_fields`:

| extra_fields Key | Task Type |
|-----------------|-----------|
| "Notes for Surveyor" | Surveys - Site Survey |
| "Notes for Installer" | Installations - General |
| "Notes for Inspector" | Inspections - General |
| "Notes for Service Tech" | Service - General |
| Contains "solar" or "pv" | Installations - Solar |
| Contains "electrical" or "mpu" | Installations - Electrical Upgrade (MPU) |

### Why This Works
1. ✅ **Already in database** - No need to fetch from Arrivy API
2. ✅ **Highly reliable** - These are template-specific form fields
3. ✅ **No rate limiting** - Read from local database
4. ✅ **Fast** - Simple key existence check

## Implementation Plan

### Phase 1: Update extractTaskType Logic (PRIORITY)
```typescript
export function extractTaskType(task: ArrivyTask): string {
  const extraFields = task.extra_fields || {};
  const fieldKeys = Object.keys(extraFields).join(' ').toLowerCase();

  // Check form field indicators (most reliable)
  if (fieldKeys.includes('surveyor') || fieldKeys.includes('notes for surveyor')) {
    return 'Surveys - Site Survey';
  }
  if (fieldKeys.includes('installer') || fieldKeys.includes('notes for installer')) {
    return 'Installations - General';
  }
  if (fieldKeys.includes('inspector') || fieldKeys.includes('notes for inspector')) {
    return 'Inspections - General';
  }

  // ... rest of logic
}
```

### Phase 2: Fix Database Sync to Store template_id
Update all sync endpoints to save the `template` field from Arrivy API as `template_id` in database.

### Phase 3: Create Template ID Mapping (Future)
If needed, create a mapping table:
```sql
CREATE TABLE arrivy_templates (
  template_id BIGINT PRIMARY KEY,
  template_name TEXT,
  task_category TEXT -- 'survey', 'installation', 'inspection', 'service'
);
```

## Files That Need Updates

1. ✅ **lib/integrations/arrivy/utils.ts** - Update extractTaskType logic (PRIORITY)
2. **lib/db/arrivy.ts** - Update upsertArrivyTask to store template field
3. **app/api/cron/sync-arrivy/route.ts** - Ensure template is saved
4. **lib/integrations/arrivy/service.ts** - Save template in all upsert calls

## Next Steps

1. Update extractTaskType to prioritize extra_fields keys
2. Run fast-fix endpoint to update existing tasks
3. Test with Justin Lambert, Tania Morris, Shawn Knutsen tasks
4. Verify all tasks show correct types
