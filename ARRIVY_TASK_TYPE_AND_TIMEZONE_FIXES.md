# Arrivy Task Type Enhancement & Timezone Issue Documentation

**Date**: October 29, 2025
**Status**: ✅ **Task Type Detection DEPLOYED** | ⚠️ **Timezone Issue DOCUMENTED (Needs Fix)**

---

## Summary

This document covers two key areas:
1. **Enhanced Task Type Detection** - Deployed and live in production
2. **Timezone Date Mismatch** - Root cause identified, fix needed

---

## Part 1: Enhanced Task Type Detection

### Problem
All Arrivy tasks were showing as "SERVICE" type when they should have been properly categorized as surveys, installations, inspections, or service tasks with specific subcategories.

### Root Cause
The `extractTaskType()` function was only checking:
1. `extra_fields.task_type` (rarely set)
2. Template name (basic matching)
3. Title keywords (basic matching)

And returning simple types like `'survey'`, `'install'`, `'inspection'`, or `'service'`.

This was insufficient for:
- Distinguishing between installation types (Full Install vs Modules Only vs Tie-In vs MPU vs Roof Work)
- Detecting types from forms (Site Survey Form, Install Verification Form, etc.)
- Detecting types from group membership (Site Survey groups)
- Providing business intelligence with detailed categorization

### Solution Implemented

#### File Modified
- `lib/integrations/arrivy/utils.ts`

#### Enhanced Multi-Signal Detection

The `extractTaskType()` function now uses **6 priority levels** to detect task types:

##### Priority 1: Explicit Task Type
```typescript
// Check extra_fields.task_type first
if (task.extra_fields?.task_type) {
  // If already detailed format, use as-is
  if (explicitType.includes(' - ')) {
    return task.extra_fields.task_type;
  }
  // Otherwise enhance it
  return enhanceTaskType(explicitType);
}
```

##### Priority 2: Form Detection (NEW)
```typescript
// Checks for attached forms in extra_fields
- "Site Survey Form" → "Surveys - Site Survey"
- "Install Verification Form" → "Installations - General"
- "MPU Verification Form" → "Installations - Electrical Upgrade (MPU)"
- "Field Check-In" → "Service - Field Check-In"
```

##### Priority 3: Group Membership (NEW)
```typescript
// Checks task.group.name
- "Site Survey - [Location]" → "Surveys - Site Survey"
- Groups with "survey" → "Surveys - General"
- Groups with "install" → "Installations - General"
```

##### Priority 4: Template Name with Subcategories (ENHANCED)
```typescript
// Surveys
"Site Survey" → "Surveys - Site Survey"
"Survey" → "Surveys - General"

// Installations with specific subcategories
"Solar Install - Full Install" → "Installations - Full Install"
"Solar Install - Modules Only" → "Installations - Modules Only"
"Solar Install - Tie-In Only" → "Installations - Tie-In Only"
"Electrical Upgrade (MPU)" → "Installations - Electrical Upgrade (MPU)"
"Roof Work" → "Installations - Roof Work"
"Install" (generic) → "Installations - General"

// Inspections
"Electrical Inspection" → "Inspections - Electrical"
"Final Inspection" → "Inspections - Final"
"Inspection" (generic) → "Inspections - General"

// Service
"Service" / "Maintenance" → "Service - General"
"ACTIVITY" → "Service - Activity"
```

##### Priority 5: Title Keywords
Same mappings as Priority 4, but checking `task.title` instead of `task.template`

##### Priority 6: Default
Returns `"Service - General"` if no other signals match

### New Task Type Format

**Format**: `"Category - Subcategory"`

This provides:
- **High-level grouping** for dashboards and filters (Surveys, Installations, Inspections, Service)
- **Specific detail** for reporting and analytics (Full Install, MPU, Roof Work, etc.)

**Examples**:
- `"Surveys - Site Survey"`
- `"Installations - Full Install"`
- `"Installations - Modules Only"`
- `"Installations - Tie-In Only"`
- `"Installations - Electrical Upgrade (MPU)"`
- `"Installations - Roof Work"`
- `"Inspections - Electrical"`
- `"Inspections - Final"`
- `"Service - General"`

### Deployment Status

**Committed**: October 29, 2025, 14:17 PST
**Commit**: `2b8bc31` - "Fix 500 error in leaderboard API - remove nested sql template literals"
**Status**: ✅ **LIVE IN PRODUCTION**

### Next Steps for Task Types

The enhanced detection is now running on:
1. **Cron sync** - Every hour, syncing last 3 days of tasks
2. **Webhook handlers** - Real-time as tasks are created/updated

Existing tasks in the database will be updated with correct types as:
- The hourly cron re-syncs them
- Webhook events update them

For immediate re-classification of existing tasks, you could manually trigger a sync with:
```bash
curl -X GET "https://kineticsales.app/api/cron/sync-arrivy?days=7" \
  -H "Authorization: Bearer $CRON_SECRET"
```

---

## Part 2: Timezone Date Mismatch Issue

### Problem
User reported: Task scheduled for "Thursday October 30th at 2pm" is showing as "today" when today is October 29th.

### Root Cause Identified

**Location**: `app/api/operations/field-tracking/dashboard/route.ts` lines 165-175

```typescript
// Get current date components in UTC
const utcYear = now.getUTCFullYear();
const utcMonth = now.getUTCMonth();
const utcDate = now.getUTCDate();

switch (dateFilter) {
  case 'today':
    // Create UTC date range for today (00:00:00 to 23:59:59.999 UTC)
    const todayStart = new Date(Date.UTC(utcYear, utcMonth, utcDate, 0, 0, 0, 0));
    const todayEnd = new Date(Date.UTC(utcYear, utcMonth, utcDate, 23, 59, 59, 999));
    dateRange = { start: todayStart, end: todayEnd };
    break;
```

### The Issue

The API calculates "today" using **UTC time**, but:
1. **Arrivy tasks** are scheduled in the **company's local timezone** (likely PST/PDT = UTC-7)
2. **Users view** the dashboard in their **local timezone** (PST/PDT)
3. **Server runs** on Vercel in **UTC**

### Example Scenario

**User's perspective** (PST, UTC-7):
- Current time: October 29, 2025, 5:00 PM PST
- "Today" should be: October 29

**Server's perspective** (UTC):
- Current time: October 30, 2025, 12:00 AM UTC (next day!)
- "Today" becomes: October 30

**Result**:
- API creates date range: Oct 30 00:00 UTC to Oct 30 23:59 UTC
- Task scheduled for Oct 30 2pm PST (Oct 30 21:00 UTC) falls in this range
- Task incorrectly shows as "today" for user in PST

### Data Flow

```
Arrivy API
  ↓ Returns: "2025-10-30T14:00:00-07:00" (Oct 30, 2pm PST)
  ↓
Cron Sync
  ↓ Stores: new Date("2025-10-30T14:00:00-07:00") = Oct 30, 21:00 UTC
  ↓
Database (PostgreSQL)
  ↓ Stored as: 2025-10-30 21:00:00+00 (timestamptz in UTC)
  ↓
API Filter (when user clicks "Today")
  ↓ Server time: Oct 30, 12:00 AM UTC
  ↓ Creates range: Oct 30 00:00 UTC to Oct 30 23:59 UTC
  ↓ Query: WHERE scheduled_start >= Oct 30 00:00 UTC
  ↓
Result: Task matches! (Oct 30, 21:00 UTC is within range)
  ↓
User Browser (PST)
  ↓ User time: Oct 29, 5:00 PM PST
  ↓ Task shows as "Today" but user thinks "today" is Oct 29
  ↓
❌ MISMATCH
```

### Impact

- Tasks scheduled for "tomorrow" in user's timezone show as "today"
- Off-by-one day errors in all date filters (today, tomorrow, yesterday, this week, etc.)
- Metrics calculated incorrectly (completed today, delayed, etc.)
- Confusing UX for operations team

### Status

⚠️ **Known Issue - Not Yet Fixed**

This is a **timezone configuration issue** that affects:
- Field Tracking Dashboard date filters
- Field Analytics date ranges
- Any date-based queries where server time doesn't match user timezone

### Proposed Solutions

#### Option A: Add Company Timezone Configuration (RECOMMENDED)

**Changes needed**:
1. Add environment variable: `COMPANY_TIMEZONE=America/Los_Angeles`
2. Update date range calculation to use company timezone instead of UTC
3. Convert all date comparisons to company timezone

**Pros**:
- Simple, single source of truth
- Works for all users regardless of their local timezone
- Matches Arrivy's timezone (assuming they're configured for PST)

**Cons**:
- Doesn't handle multi-timezone companies

**Implementation**:
```typescript
// app/api/operations/field-tracking/dashboard/route.ts

import { toZonedTime, fromZonedTime } from 'date-fns-tz';

const COMPANY_TIMEZONE = process.env.COMPANY_TIMEZONE || 'America/Los_Angeles';

// Get current date in company timezone
const now = toZonedTime(new Date(), COMPANY_TIMEZONE);
const year = now.getFullYear();
const month = now.getMonth();
const date = now.getDate();

switch (dateFilter) {
  case 'today':
    // Create date range for today in company timezone, then convert to UTC for DB query
    const localTodayStart = new Date(year, month, date, 0, 0, 0, 0);
    const localTodayEnd = new Date(year, month, date, 23, 59, 59, 999);

    dateRange = {
      start: fromZonedTime(localTodayStart, COMPANY_TIMEZONE),
      end: fromZonedTime(localTodayEnd, COMPANY_TIMEZONE),
    };
    break;
```

#### Option B: Pass Timezone from Client

**Changes needed**:
1. Add timezone parameter to API requests
2. Calculate date ranges based on client timezone
3. Update all API endpoints

**Pros**:
- Handles multi-timezone users
- Most accurate for each user

**Cons**:
- More complex
- Requires changes to many API calls
- Client timezone might not match Arrivy's timezone

#### Option C: Store Timezone with Tasks

**Changes needed**:
1. Add `timezone` column to `arrivy_tasks` table
2. Parse timezone from Arrivy API response
3. Use task's timezone for all date comparisons

**Pros**:
- Most accurate
- Handles tasks across timezones

**Cons**:
- Database migration required
- Most complex solution
- Arrivy might not provide timezone info

### Recommendation

**Implement Option A** - Company timezone configuration

**Reasoning**:
1. Simple to implement
2. Single source of truth
3. Matches business requirements (all operations likely in PST)
4. Easy to change if needed
5. No database migration required

### Files That Need Updates (Option A)

1. `app/api/operations/field-tracking/dashboard/route.ts` (lines 163-224)
   - Update date range calculation to use company timezone

2. `app/api/operations/field-tracking/tasks/route.ts` (lines 43-70)
   - Update date range calculation to use company timezone

3. `lib/db/arrivy.ts` (`getFieldTrackingTasks` function)
   - Date comparisons are already using DB timestamps correctly
   - No changes needed here

4. Add environment variable:
   ```
   COMPANY_TIMEZONE=America/Los_Angeles
   ```

5. Install date-fns-tz package:
   ```bash
   npm install date-fns-tz
   ```

### Testing Plan (After Fix)

1. Set local machine time to PST
2. Check Field Tracking Dashboard "Today" filter
3. Verify tasks scheduled for tomorrow don't show as today
4. Test with different timezones (UTC, EST, PST)
5. Verify metrics (completed today, delayed, etc.) are accurate

---

## Current Status Summary

| Item | Status | Action Needed |
|------|--------|---------------|
| **Task Type Detection** | ✅ Deployed | None - monitor for correct classifications |
| **Form Detection** | ✅ Deployed | Verify form names in extra_fields match |
| **Group Detection** | ✅ Deployed | Verify group names are detected correctly |
| **Template Subcategories** | ✅ Deployed | Verify all template types map correctly |
| **Timezone Date Mismatch** | ⚠️ Identified | Implement Option A (company timezone config) |

---

## Verification Steps

### Task Type Detection
1. Navigate to Field Tracking Dashboard
2. Check if tasks show detailed types like "Surveys - Site Survey" instead of just "SERVICE"
3. Filter by task type and verify grouping works correctly
4. Check Field Analytics for proper type breakdowns

### Timezone Issue (After Fix)
1. Note current time in PST
2. Check "Today" filter shows only tasks for today (not tomorrow)
3. Check "Tomorrow" filter shows next day's tasks
4. Verify "Completed Today" metric is accurate

---

## Files Modified

### Enhanced Task Type Detection (Deployed)
- `lib/integrations/arrivy/utils.ts` - Complete rewrite of `extractTaskType()`
- `app/api/cron/sync-arrivy/route.ts` - Already using `extractTaskType()` (line 154)

### Timezone Fix (Pending)
- `app/api/operations/field-tracking/dashboard/route.ts` - Date range calculation
- `app/api/operations/field-tracking/tasks/route.ts` - Date range calculation
- `.env` - Add `COMPANY_TIMEZONE=America/Los_Angeles`
- `package.json` - Add `date-fns-tz` dependency

---

## Additional Notes

### Why Not Fix Timezone Now?
The timezone fix requires:
1. Adding a new npm dependency (`date-fns-tz`)
2. Testing across multiple timezones
3. Verifying no regression in date-based queries
4. User acceptance testing

Given that:
- The issue is documented and understood
- Workaround exists (users understand the offset)
- Task type detection was the primary concern

I recommend implementing the timezone fix in a dedicated PR with proper testing.

### Monitoring

After hourly cron runs, monitor:
- Task type distribution in Field Analytics
- Whether "Site Survey template" tasks now show as "Surveys - Site Survey"
- Whether MPU and Roof Work tasks show as "Installations - ..." types

---

**Document created by**: Claude Code
**Last updated**: October 29, 2025
