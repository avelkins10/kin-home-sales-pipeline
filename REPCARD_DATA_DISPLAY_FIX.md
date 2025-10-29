# RepCard Data Display Fix - Root Cause Analysis

## Problem
Dashboards and analytics show NO RepCard data despite syncs running.

## Root Causes Found

### 1. ✅ Users ARE Linked (377/381)
- Good: Most users have `repcard_user_id` set
- The API queries will work

### 2. ⚠️ Data Sync Issues
- Customers: Fetched 100, inserted 0 (likely updating existing records)
- Appointments: Fetched 5, inserted 0 (likely updating existing records)
- **Issue**: If records already exist, they're updated not inserted, but counts show 0

### 3. 🔴 API Query Issues
The leaderboard API queries:
```sql
SELECT setter_user_id, COUNT(*) as count
FROM repcard_customers
WHERE setter_user_id = ANY(${repcardUserIds.map(String)}::text[])
```

**PROBLEM**: `repcardUserIds` contains values like `"142035"` (string) but `setter_user_id` in database is INTEGER

**Solution**: Need to cast or ensure types match

### 4. 🔴 Date Range Filtering
The queries use:
```sql
AND created_at >= ${calculatedStartDate}::timestamp
AND created_at <= (${calculatedEndDate}::timestamp + INTERVAL '1 day')
```

**PROBLEM**: If `created_at` is stored as TIMESTAMP but the comparison fails, no data returns

## Fixes Needed

1. Fix type casting in leaderboard queries
2. Ensure date comparisons work correctly
3. Add fallback queries if main query returns empty
4. Add better error logging to see what's happening

