# RepCard Data Display Fixes - Action Plan

**Date:** 2025-01-28  
**Priority:** HIGH - Fixing RepCard data display issues

---

## 🔍 Issues Identified

### 1. Type Casting Issues (Performance)
**Problem:** 243+ occurrences of `::TEXT` casts in queries
- Causes: Type mismatches between INTEGER and TEXT columns
- Impact: Prevents index usage, 2-3x slower queries
- Status: Migration 018 should fix this, but code still has casts

### 2. INNER JOINs Hiding Zero Metrics
**Problem:** Users with zero doors knocked/appointments don't appear in leaderboards
- Location: `app/api/repcard/leaderboard/route.ts` lines 255, 499+
- Impact: Incomplete leaderboard data
- Status: Some fallback logic exists but not comprehensive

### 3. Data Source Confusion
**Problem:** Queries sometimes use `repcard_users` (100 users) instead of `users` (384 users)
- Impact: Missing 284 users in some queries
- Status: Partially fixed, but needs verification

### 4. Missing Reschedule Metrics Display
**Problem:** Reschedule tracking exists in DB but not displayed in UI
- Impact: Can't see appointment quality metrics
- Status: Schema ready, UI missing

---

## 🎯 Fix Plan

### Phase 1: Verify Database Schema (5 min)
Check if migration 018 has been applied:
```sql
SELECT column_name, data_type 
FROM information_schema.columns 
WHERE table_name IN ('users', 'repcard_customers', 'repcard_appointments')
  AND column_name IN ('repcard_user_id', 'setter_user_id', 'closer_user_id');
```

**Expected:** All should be `integer` (not `text`)

### Phase 2: Remove Type Casts (If Migration Applied) (30 min)
If types are INTEGER, remove all `::TEXT` casts:
- Search: `repcard_user_id::TEXT` → `repcard_user_id`
- Search: `setter_user_id::TEXT` → `setter_user_id`
- Search: `closer_user_id::TEXT` → `closer_user_id`

### Phase 3: Fix INNER JOINs (30 min)
Change all INNER JOINs to LEFT JOINs in leaderboard queries:
- `app/api/repcard/leaderboard/route.ts`
- Ensure users with zero metrics appear

### Phase 4: Verify Data Sources (15 min)
Ensure all queries use `users` table as primary source:
- Check leaderboard queries
- Check stats queries
- Verify office filtering works

### Phase 5: Add Reschedule Metrics Display (1 hour)
- Add reschedule rate to `RepCardMetricsCard`
- Add reschedule stats to leaderboard
- Create reschedule analytics component

---

## 📋 Files to Fix

1. **app/api/repcard/leaderboard/route.ts**
   - Remove type casts (if migration applied)
   - Change INNER JOINs to LEFT JOINs
   - Verify data source (users table)

2. **app/api/repcard/users/[userId]/stats/route.ts**
   - Remove type casts
   - Add reschedule metrics

3. **components/dashboard/RepCardMetricsCard.tsx**
   - Add reschedule rate display
   - Improve error handling

4. **components/analytics/ConfigurableLeaderboard.tsx**
   - Verify data display
   - Add reschedule metric option

---

## 🧪 Testing Checklist

After fixes:
- [ ] Leaderboard shows all users (even with 0 metrics)
- [ ] Type casts removed (if migration applied)
- [ ] Queries use INTEGER comparisons (no casts)
- [ ] Reschedule metrics display correctly
- [ ] Office filtering works
- [ ] Date range filtering works
- [ ] Performance improved (faster queries)

---

## 🚀 Next Steps

1. Check database schema
2. Apply fixes based on schema state
3. Test all RepCard displays
4. Verify data accuracy
