# RepCard Leaderboard Fixes - 2025-11-21

## Executive Summary

Fixed critical issues with RepCard Leaderboard showing "0 RepCard users found" by addressing data source mismatches, NULL role handling, office sync errors, and rate limiting issues.

## Issues Identified

### 1. Data Source Mismatch (CRITICAL)
**Problem**: Leaderboard was querying `repcard_users` table (100 users) instead of `users` table (384 RepCard-linked users)

**Current State**:
- `repcard_users` table: **100 users** (32 active, 68 inactive)
- `users` table: **390 users**, **384 linked to RepCard** via `repcard_user_id`
- App users have proper roles: 126 setters, 260 closers
- **ALL** RepCard users have `role = NULL` in `repcard_users` table

**Root Cause**:
- RepCard user sync only fetches 100 users (pagination stops after page 1)
- API response shows `currentPage = 1, lastPage = 1`, causing `hasMore = false`
- Either RepCard API only has 100 users OR pagination is broken

### 2. NULL Role Handling
**Problem**: All RepCard users have `role = NULL`, causing role filters to fail

**Previous Query**:
```sql
WHERE ru.role = ${role} OR u.role = ${role}
```
This failed because `NULL = 'setter'` returns false in SQL

**Fix Applied**: Use `COALESCE()` to prefer app user roles
```sql
WHERE COALESCE(u.role, ru.role) = ${role}
```

### 3. Office Sync Errors
**Problem**: 7 offices (IDs: 3173, 3222, 3814, 3821, 4166, 4437, 4438) had NULL names, violating NOT NULL constraint

**Fix Applied**: Skip offices without names during sync
```typescript
if (!office.name || office.name.trim() === '') {
  console.warn(`[RepCard Sync] Skipping office ${office.id} - missing name`);
  recordsFailed++;
  continue;
}
```

### 4. Rate Limiting
**Observed**: API rate limits during status log sync
```
[warning] [RepCard] Rate limit low: 1 remaining, resets at null
[warning] [RepCard] Rate limit low: 0 remaining, resets at null
[info] [RepCard] Rate limited, retrying in 1000ms (attempt 1/3)
```

## Fixes Applied

### Fix 1: Leaderboard Data Source (app/api/repcard/leaderboard/route.ts)

**Changed** from querying `repcard_users` as primary source:
```sql
FROM repcard_users ru
LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
WHERE ru.status = 1
```

**To** querying `users` table as primary source:
```sql
FROM users u
LEFT JOIN repcard_users ru ON ru.repcard_user_id = u.repcard_user_id
WHERE u.repcard_user_id IS NOT NULL
  AND u.role = ${role}
```

**Impact**: Leaderboard will now show **384 RepCard-linked users** instead of just 32

### Fix 2: Office Sync NULL Handling (lib/repcard/comprehensive-sync.ts:451-456)

Added validation before inserting offices:
```typescript
if (!office.name || office.name.trim() === '') {
  console.warn(`[RepCard Sync] Skipping office ${office.id} - missing name`);
  recordsFailed++;
  continue;
}
```

### Fix 3: Pagination Debug Logging (lib/repcard/comprehensive-sync.ts:170-179, 387-391)

Added comprehensive pagination logging:
```typescript
const paginationInfo = {
  currentPage: response.result.currentPage,
  lastPage: response.result.lastPage,
  total: response.result.total,
  perPage: response.result.perPage
};
console.log(`[RepCard Sync] Pagination info:`, JSON.stringify(paginationInfo));
console.log(`[RepCard Sync Users] hasMore = ${hasMore} (currentPage: ${currentPage}, lastPage: ${lastPage})`);
```

## Testing

### Before Fix
```
[RepCard Leaderboard] Found 22 valid metrics from database
[RepCard Leaderboard] Date range: 2025-11-21 to 2025-11-21 (timeRange: today)
[RepCard Leaderboard] ⚠️ No RepCard users found
```

### After Fix (Expected)
- Leaderboard should show **384 RepCard-linked users**
- Users should have proper roles (126 setters, 260 closers)
- Office filtering should work based on app users' `sales_office`
- Pagination logging will show why only 100 users are synced from RepCard API

## Next Steps & Recommendations

### Immediate Actions
1. **Test Leaderboard**: Verify leaderboard now shows all 384 users
2. **Trigger Full Sync**: Run a full sync to see pagination debug logs
   ```bash
   curl -X POST "https://your-domain.com/api/admin/repcard/sync?type=full" \
     -H "Authorization: Bearer YOUR_TOKEN"
   ```

### Investigation Required
1. **Pagination Issue**:
   - Check RepCard API pagination response structure
   - Verify if RepCard actually has more than 100 users
   - Review sync logs to see pagination debug output

2. **Role Population**:
   - Consider creating a migration to populate `repcard_users.role` from `users.role`
   - Or continue relying on app users table for role data

3. **Rate Limiting**:
   - Implement request throttling between API calls
   - Consider increasing cache TTL from 30min to 1 hour
   - Add backoff strategy for rate limit retries

### Database Migration (Optional)
Create migration to populate NULL roles from app users:
```sql
UPDATE repcard_users ru
SET role = u.role
FROM users u
WHERE u.repcard_user_id = ru.repcard_user_id
  AND ru.role IS NULL
  AND u.role IS NOT NULL;
```

## Files Modified

1. `app/api/repcard/leaderboard/route.ts` (Lines 303-415, 444-458)
   - Changed to query `users` table as primary source
   - Updated userMap creation to use app user data

2. `lib/repcard/comprehensive-sync.ts` (Lines 451-456)
   - Added NULL name validation for office sync

3. `lib/repcard/comprehensive-sync.ts` (Lines 170-179, 387-391)
   - Added pagination debug logging

## Diagnostic Scripts Created

1. `scripts/check-repcard-users.ts` - Check RepCard users table status
2. `scripts/check-sync-history.ts` - View sync logs and database stats
3. `scripts/check-repcard-api.ts` - Test RepCard API pagination (requires API key)

## Database Stats (As of 2025-11-21)

| Table | Total | Active | Linked | With Role |
|-------|-------|--------|--------|-----------|
| `repcard_users` | 100 | 32 | 35 | 0 |
| `users` | 390 | N/A | 384 | 386 |

**Sync History**: Last 10 syncs all fetched exactly 100 users (0 inserted, 100 updated, 0 failed)

## Success Criteria

- [ ] Leaderboard shows 384 users (or appropriate subset based on role filter)
- [ ] Office filtering works without errors
- [ ] No "No RepCard users found" messages
- [ ] Pagination logs show why only 100 users are synced
- [ ] Rate limiting handled gracefully
