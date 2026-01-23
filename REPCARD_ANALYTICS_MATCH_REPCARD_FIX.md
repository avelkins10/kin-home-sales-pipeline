# RepCard Analytics - Match RepCard Dashboard Counting Logic

**Date:** 2025-01-28  
**Status:** ✅ **FIXED**

---

## 🔴 Problem

Analytics dashboard showed **7 appointments** for "Today" but RepCard dashboard (source of truth) shows **5 appointments**.

**Discrepancy:** 2 appointments difference

---

## ✅ Root Cause

The analytics dashboard was counting appointments differently than RepCard:

1. **Including cancelled appointments** - RepCard excludes these
2. **Including no-show appointments** - RepCard excludes these  
3. **Including appointments with NULL scheduled_at** - Fixed earlier, but needed verification

RepCard's "Appointments Set" metric only counts:
- ✅ Appointments with `scheduled_at` in date range
- ✅ NOT reschedules (`is_reschedule = FALSE`)
- ✅ NOT cancelled (`status_category != 'cancelled'`)
- ✅ NOT no-show (`status_category != 'no_show'`)

---

## ✅ Fixes Applied

### 1. Updated Quality Metrics Query

**File:** `app/api/repcard/unified-dashboard/route.ts` (lines 125-142)

**Before:**
```sql
COUNT(*)::int as total_appointments,
COUNT(*) FILTER (WHERE is_within_48_hours = TRUE)::int as within_48h,
-- ... no exclusion of cancelled/no-show
```

**After:**
```sql
COUNT(*) FILTER (
  WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
  AND status_category NOT IN ('cancelled', 'no_show')
  AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
)::int as total_appointments,
-- ... same exclusion logic for all metrics
```

### 2. Updated Setter Leaderboard Query

**File:** `app/api/repcard/unified-dashboard/route.ts` (lines 635-640)

**Before:**
```sql
COUNT(DISTINCT a.id) FILTER (WHERE a.is_reschedule = FALSE OR a.is_reschedule IS NULL)::int as appointments_set,
```

**After:**
```sql
COUNT(DISTINCT a.id) FILTER (
  WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
  AND a.status_category NOT IN ('cancelled', 'no_show')
  AND (a.disposition IS NULL OR a.disposition NOT ILIKE '%cancel%' AND a.disposition NOT ILIKE '%no.show%' AND a.disposition NOT ILIKE '%no_show%')
)::int as appointments_set,
```

### 3. Updated All Appointment Count Queries

Applied the same exclusion logic to:
- ✅ Office performance queries
- ✅ Top doors knocked queries
- ✅ Appointment counts CTEs
- ✅ Quality metrics (48h, power bill, both, neither)

---

## 📊 Matching Logic

**RepCard Dashboard Counting:**
```
Appointments Set = 
  scheduled_at in date range
  AND is_reschedule = FALSE
  AND status_category NOT IN ('cancelled', 'no_show')
  AND disposition NOT LIKE '%cancel%'
  AND disposition NOT LIKE '%no.show%'
  AND disposition NOT LIKE '%no_show%'
```

**Our Analytics (Now):**
```
Appointments Set = 
  scheduled_at in date range
  AND (is_reschedule = FALSE OR is_reschedule IS NULL)
  AND status_category NOT IN ('cancelled', 'no_show')
  AND (disposition IS NULL OR disposition NOT ILIKE '%cancel%' AND disposition NOT ILIKE '%no.show%' AND disposition NOT ILIKE '%no_show%')
```

✅ **Now matches RepCard's counting logic**

---

## 🔍 Why This Matters

1. **Accuracy:** Analytics should match the source of truth (RepCard dashboard)
2. **Trust:** Users expect consistent numbers across systems
3. **Business Logic:** "Appointments Set" should only count actual appointments, not cancelled/no-show ones

---

## ✅ Verification

After the fix:
- ✅ Analytics dashboard should show **5 appointments** for "Today" (matching RepCard)
- ✅ Cache count should match displayed count
- ✅ All quality metrics calculated on the same filtered set
- ✅ Setter leaderboard matches RepCard's counts

---

## 📝 Files Modified

1. `app/api/repcard/unified-dashboard/route.ts`
   - Quality metrics query (lines 125-142)
   - Setter leaderboard query (lines 635-640)
   - Top doors knocked query (lines 524-534, 577-583)
   - Office performance queries (lines 300, 344, 387, 405)
   - All appointment counting CTEs

---

**Status:** 🟢 **FIXED - READY FOR TESTING**

The analytics dashboard should now match RepCard's "Appointments Set" count of 5 for today.
