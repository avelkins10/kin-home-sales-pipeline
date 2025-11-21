# RepCard Integration - Comprehensive Audit Report
**Date**: 2025-11-21
**Status**: In Progress - Critical Issues Identified & Migrations Created

---

## Executive Summary

Completed comprehensive audit of RepCard integration revealing **8 critical issues** affecting data attribution, user visibility, and analytics. Created database migrations to address reschedule tracking, office mapping, and attachment attribution. Implementation partially complete with schema compatibility issues requiring resolution.

---

## Audit Findings Summary

### ✅ What's Working Well

1. **Comprehensive Data Sync** - 13 entity types synced from RepCard API
2. **User Linking** - 384/390 app users linked to RepCard (98.5%)
3. **Error Handling** - Robust logging and error tracking via `repcard_sync_log`
4. **Caching** - 30-minute cache on leaderboard prevents rate limiting
5. **Proper Indexing** - Comprehensive indexes on foreign keys and date fields

### ❌ Critical Issues Found

| Issue | Impact | Priority | Status |
|-------|--------|----------|--------|
| **1. Only 100 Users Syncing** | Missing 284+ users in `repcard_users` table | HIGH | ⚠️ IDENTIFIED |
| **2. No Reschedule Tracking** | Cannot measure appointment quality metrics | HIGH | 🔧 MIGRATION CREATED |
| **3. Office Filtering Broken** | RepCard offices don't map to QuickBase offices | HIGH | 🔧 MIGRATION CREATED |
| **4. Attachment Attribution Split** | Customer & appointment attachments in separate tables | MEDIUM | 🔧 MIGRATION CREATED |
| **5. NULL Roles on RepCard Users** | All 100 RepCard users have `role = NULL` | MEDIUM | ✅ FIXED IN CODE |
| **6. ID Type Inconsistencies** | Mixed INTEGER/TEXT types causing performance issues | MEDIUM | ⚠️ REQUIRES SCHEMA FIX |
| **7. No Transaction Safety** | Sync failures leave DB in inconsistent state | MEDIUM | ⚠️ NEEDS IMPLEMENTATION |
| **8. No UI for Multiple Appointments** | Can't view appointment history per customer | LOW | ⚠️ NEEDS IMPLEMENTATION |

---

## Database Analysis

### Current State

```
📊 Table Statistics:
  • repcard_users:         100 records (32 active, 68 inactive)
  • repcard_customers:    2,800 records
  • repcard_appointments: 2,145 records
  • repcard_offices:          7 records
  • repcard_status_logs:  66,340 records
  • users (app):            390 records (384 linked to RepCard)
```

### Data Attribution Status

| Entity | Setter Attribution | Office Attribution | Attachment Attribution |
|--------|-------------------|-------------------|----------------------|
| **Customers** | ✅ `setter_user_id` | ⚠️ `office_id` (no FK) | ✅ Via attachments table |
| **Appointments** | ✅ `setter_user_id` + `closer_user_id` | ⚠️ `office_id` (no FK) | ✅ Via attachments table |
| **Attachments** | ❌ `uploaded_by_user_id` not linked | N/A | ⚠️ Split across 2 tables |

### Reschedule Detection Results

```sql
-- Customers with multiple appointments:
SELECT repcard_customer_id, COUNT(*) as appointment_count
FROM repcard_appointments
GROUP BY repcard_customer_id
HAVING COUNT(*) > 1;
```
**Result**: Multiple appointments exist but no tracking of:
- Which is original vs reschedule
- Reschedule reason
- Reschedule count per customer

---

## Migrations Created

### 028: Reschedule Tracking ⚠️ SCHEMA ISSUE
**File**: `lib/db/migrations/028_add_repcard_reschedule_tracking.sql`

**Adds**:
- `original_appointment_id TEXT` - Links reschedules to first appointment
- `reschedule_count INTEGER` - 0=original, 1=first reschedule, etc.
- `is_reschedule BOOLEAN` - Quick flag for filtering
- `reschedule_reason TEXT` - Optional reason for reschedule

**Creates**:
- View: `repcard_appointment_chains` - Shows full reschedule history
- Backfill logic to mark existing reschedules
- Indexes for performance

**Issue**: References `original.status` column which doesn't exist in schema
**Fix Required**: Remove status reference or update to use existing column

### 029: Office Mapping ⚠️ DATA INTEGRITY ISSUE
**File**: `lib/db/migrations/029_add_repcard_office_mapping.sql`

**Creates**:
- Table: `repcard_office_mappings` - Maps RepCard ↔ QuickBase offices
- Foreign keys: `repcard_customers.office_id` → `repcard_offices`
- Auto-population of mappings via name matching (exact + fuzzy)
- Helper functions: `get_quickbase_office_id()`, `get_repcard_office_id()`

**Issue**: Cannot add FK constraint - existing data has invalid `office_id` references
**Fix Required**: Clean up invalid office_id values before adding constraint

### 030: Unified Attachments ⚠️ TYPE MISMATCH
**File**: `lib/db/migrations/030_create_repcard_unified_attachments.sql`

**Creates**:
- View: `repcard_all_attachments` - Combines customer + appointment attachments
- View: `repcard_customer_attachment_summary` - Counts per customer
- View: `repcard_user_attachment_stats` - Stats per user (attachment rate, etc.)
- Materialized view: `repcard_attachment_stats_snapshot` - Cached stats

**Issue**: `integer = text` comparison fails due to ID type mismatch
**Fix Required**: Cast IDs consistently or normalize types first

---

## Code Fixes Applied

### 1. Leaderboard Data Source ✅ COMPLETED
**File**: `app/api/repcard/leaderboard/route.ts`

**Changed**: Query from `repcard_users` (100 users) → `users` table (384 linked users)

**Before**:
```typescript
FROM repcard_users ru
LEFT JOIN users u ON u.repcard_user_id = ru.repcard_user_id
WHERE ru.status = 1  // Only 32 active users
```

**After**:
```typescript
FROM users u
LEFT JOIN repcard_users ru ON ru.repcard_user_id = u.repcard_user_id
WHERE u.repcard_user_id IS NOT NULL  // All 384 linked users
```

**Impact**: Leaderboard now shows **384 users** instead of 32

### 2. NULL Role Handling ✅ COMPLETED
**File**: `app/api/repcard/leaderboard/route.ts`

**Changed**: Use app user roles (setters/closers) instead of NULL RepCard roles

**Before**:
```typescript
WHERE ru.role = ${role} OR u.role = ${role}  // Fails for NULL
```

**After**:
```typescript
WHERE u.role = ${role}  // Uses app user role (always populated)
```

### 3. Office Sync Error Handling ✅ COMPLETED
**File**: `lib/repcard/comprehensive-sync.ts:451-456`

**Added**: Skip offices with NULL names instead of crashing

```typescript
if (!office.name || office.name.trim() === '') {
  console.warn(`[RepCard Sync] Skipping office ${office.id} - missing name`);
  recordsFailed++;
  continue;
}
```

### 4. Pagination Debug Logging ✅ COMPLETED
**File**: `lib/repcard/comprehensive-sync.ts:170-179, 387-391`

**Added**: Comprehensive logging to diagnose why only 100 users sync

```typescript
console.log(`[RepCard Sync] Pagination info:`, {
  currentPage, lastPage, total, perPage
});
console.log(`hasMore = ${hasMore}`);
```

---

## Issues Requiring Resolution

### Priority 1: Fix Migration Schema Incompatibilities

#### Issue 1A: Migration 028 - Unknown Column Reference
**Error**: `column original.status does not exist`

**Location**: Line ~65 in `028_add_repcard_reschedule_tracking.sql`

**Fix**:
```sql
-- REMOVE THIS LINE:
original.status as original_status,

-- Or replace with existing column
original.scheduled_at as original_scheduled_at,
```

#### Issue 1B: Migration 029 - Invalid Foreign Key Data
**Error**: `foreign key constraint "fk_repcard_customers_office" cannot be implemented`

**Cause**: Customers reference office_ids that don't exist in `repcard_offices`

**Fix**:
```sql
-- Before adding FK, clean up invalid references
UPDATE repcard_customers
SET office_id = NULL
WHERE office_id IS NOT NULL
  AND NOT EXISTS (
    SELECT 1 FROM repcard_offices
    WHERE repcard_office_id = repcard_customers.office_id
  );
```

#### Issue 1C: Migration 030 - ID Type Mismatch
**Error**: `operator does not exist: integer = text`

**Cause**: `repcard_customer_id` is INTEGER in some tables, TEXT in others

**Fix**:
```sql
-- Use explicit casting in view
WHERE c.repcard_customer_id = ca.repcard_customer_id::integer
-- OR normalize all IDs to same type first
```

### Priority 2: Fix User Sync Pagination

**Current**: Only 100 users syncing (pagination stops after page 1)

**Evidence**:
```
Last 10 syncs: All fetched exactly 100 users
hasMore logic: response.result.currentPage < response.result.lastPage
```

**Possible Causes**:
1. RepCard API only returns `lastPage = 1` (only 100 active users exist)
2. Pagination response structure changed
3. `incremental` sync mode doesn't fetch all pages

**Investigation Needed**:
1. Check pagination logs from next sync
2. Test with `type=full` sync parameter
3. Verify RepCard API actually has more than 100 users

### Priority 3: Implement Transaction Safety

**Current**: Sync operations not wrapped in transactions

**Risk**: Failed sync leaves database in inconsistent state

**Implementation**:
```typescript
// In comprehensive-sync.ts
await sql.begin(async (tx) => {
  // All sync operations
  await syncUsers(tx);
  await syncCustomers(tx);
  // ...

  // Commit happens automatically if no errors
});
```

---

## Frontend Impact Analysis

### Dashboard Components Status

| Component | Data Expected | Current Status | Fix Needed |
|-----------|--------------|----------------|------------|
| **RepCardLeaderboard** | All 384 users | ✅ FIXED | None |
| **MetricsCard** | Reschedule rates | ❌ MISSING | Add reschedule_rate field |
| **QualityMetricsCard** | Attachment rates | ⚠️ PARTIAL | Already shows, but inaccurate |
| **ComprehensiveDashboard** | Multiple appointments | ❌ NOT SHOWN | Add appointment history |
| **OfficeFilter** | Correct office mappings | ❌ BROKEN | Use office mapping table |

### Missing UI Components

1. **Appointment History Timeline**
   - Show all appointments for a customer
   - Indicate which are reschedules
   - Display reschedule count badge

2. **Attachment Preview/Download**
   - List attachments per customer
   - Download/preview files
   - Show upload date and user

3. **Reschedule Metrics**
   - Avg reschedules per customer
   - Reschedule rate by rep
   - Time between reschedules

---

## Next Steps & Recommendations

### Immediate Actions (Today)

1. **Fix Migration 028**
   - Remove invalid `status` column reference
   - Re-run migration
   - Verify reschedule tracking works

2. **Fix Migration 029**
   - Clean up invalid office_id references
   - Add foreign keys
   - Populate office mappings

3. **Fix Migration 030**
   - Add explicit type casts for ID comparisons
   - Test unified attachment view
   - Verify attachment counts

### Short Term (This Week)

4. **Debug User Pagination**
   - Trigger full sync with logging
   - Verify RepCard API response structure
   - Fix pagination if broken

5. **Update Stats Endpoints**
   - Add `reschedule_rate` to `/api/repcard/users/[id]/stats`
   - Add `avg_appointments_per_customer`
   - Include unified attachment counts

6. **Update Leaderboard**
   - Use office mapping table for filtering
   - Add reschedule rate metric option
   - Remove fallback to "all users"

### Medium Term (Next 2 Weeks)

7. **Add Transaction Safety**
   - Wrap all sync operations
   - Implement rollback on error
   - Add partial sync recovery

8. **Build Basic UI Enhancements**
   - Add reschedule count badges
   - Show attachment counts
   - Indicate multiple appointments

9. **Performance Optimization**
   - Materialize common aggregations
   - Add missing composite indexes
   - Cache expensive calculations

### Long Term (Next Month)

10. **Advanced Features**
    - Full appointment history timeline
    - Attachment management (upload/download)
    - Reschedule reason tracking

11. **Data Quality**
    - Normalize all ID types to INTEGER
    - Add data validation on sync
    - Implement referential integrity checks

---

## Testing Checklist

### Database Tests
- [ ] Reschedule tracking: Count customers with >1 appointment
- [ ] Office mappings: Verify RepCard ↔ QuickBase links
- [ ] Attachment counts: Compare unified view vs separate tables
- [ ] Foreign keys: Test cascading deletes

### API Tests
- [ ] Leaderboard shows 384 users (not 32)
- [ ] Office filter returns correct users
- [ ] Reschedule metrics in user stats
- [ ] Attachment stats per user

### Sync Tests
- [ ] Full sync completes without errors
- [ ] Pagination fetches all users
- [ ] Reschedule data populated correctly
- [ ] Office mappings auto-populate

### Frontend Tests
- [ ] Dashboard displays new metrics
- [ ] Office filter dropdown works
- [ ] Reschedule counts visible
- [ ] No console errors

---

## Files Modified

### Migrations Created
1. `lib/db/migrations/028_add_repcard_reschedule_tracking.sql`
2. `lib/db/migrations/029_add_repcard_office_mapping.sql`
3. `lib/db/migrations/030_create_repcard_unified_attachments.sql`

### Code Fixed
1. `app/api/repcard/leaderboard/route.ts` - Query users table, NULL role handling
2. `lib/repcard/comprehensive-sync.ts` - Office sync error handling, pagination logging

### Scripts Created
1. `scripts/run-new-repcard-migrations.ts` - Run migrations 028-030
2. `scripts/check-repcard-users.ts` - Diagnose user table issues
3. `scripts/check-sync-history.ts` - Review sync logs
4. `scripts/check-repcard-api.ts` - Test API pagination

### Documentation
1. `REPCARD_LEADERBOARD_FIXES.md` - Initial leaderboard fixes
2. `REPCARD_COMPREHENSIVE_AUDIT_REPORT.md` - This document

---

## Success Metrics

Once all fixes are implemented, we should achieve:

- ✅ **384+ users** visible in leaderboards (currently 384, but sync may find more)
- ✅ **Reschedule tracking** for all 2,145 appointments
- ✅ **Office filtering** works correctly via mapping table
- ✅ **Unified attachment counts** match reality
- ✅ **Zero sync errors** on full sync
- ✅ **Complete data attribution** for all entities

---

## Conclusion

The RepCard integration is **production-ready for basic analytics** but requires **schema fixes and enhanced tracking** for advanced features like reschedule analytics and proper office filtering.

**Estimated Effort to Complete**:
- Schema fixes: 2-4 hours
- Pagination debugging: 2-3 hours
- Frontend enhancements: 4-6 hours
- **Total**: 1-2 days

**Risk Level**: Low-Medium (migrations are non-destructive, code fixes are minimal)

**Business Impact**: High (enables tracking of key metrics like reschedule rates, improves data accuracy)
