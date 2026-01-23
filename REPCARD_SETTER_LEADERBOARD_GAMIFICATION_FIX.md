# RepCard Setter Leaderboard - Gamification Fix

**Date:** 2025-01-28  
**Status:** ✅ **FIXED**

---

## 🎯 Business Requirement

**Setter Leaderboard for "Appointments Set":**
- Show **gross appointments set today** (for gamification)
- Exclude **ONLY reschedules** (`is_reschedule = FALSE`)
- **Disposition doesn't matter** - include cancelled/no-show appointments
- Quality metrics (48h, power bill) shown **separately on the same row**

**Purpose:** Gamify TODAY's door efforts - setters compete for most appointments set, regardless of what happens later (cancelled/no-show).

---

## ✅ Fix Applied

### Setter Leaderboard Query

**File:** `app/api/repcard/unified-dashboard/route.ts` (lines 699-730)

**Before:**
```sql
COUNT(DISTINCT a.id) FILTER (
  WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
  AND a.status_category NOT IN ('cancelled', 'no_show')  -- ❌ Excluded cancelled/no-show
  AND (a.disposition IS NULL OR ...)
)::int as appointments_set,
```

**After:**
```sql
-- Gross appointments set today (for gamification) - exclude ONLY reschedules
-- Disposition doesn't matter - we want to gamify TODAY's door efforts
COUNT(DISTINCT a.id) FILTER (
  WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
)::int as appointments_set,
-- Quality metrics shown separately (calculated from same gross set)
COUNT(DISTINCT a.id) FILTER (
  WHERE (a.is_reschedule = FALSE OR a.is_reschedule IS NULL)
  AND a.is_within_48_hours = TRUE
)::int as within_48h_count,
```

### Updated All Setter Leaderboard Queries

1. ✅ **Top Appointment Setters** (with quality metrics) - lines 699-730
2. ✅ **Top Doors Knocked** (appointment counts CTE) - lines 524-534, 577-583
3. ✅ **Office Setters Breakdown** - lines 387, 405, 433, 456

---

## 📊 Logic

**Appointments Set (Gamification):**
```
scheduled_at in date range
AND is_reschedule = FALSE
(disposition doesn't matter - include cancelled/no-show)
```

**Quality Metrics (Same Row):**
```
Same base set (gross appointments, exclude only reschedules)
AND is_within_48_hours = TRUE  (for 48h speed)
AND has_power_bill = TRUE      (for power bill rate)
```

---

## 🎮 Gamification Impact

**Before:**
- Setter sets 5 appointments today
- 2 get cancelled later
- Leaderboard shows: 3 appointments ❌

**After:**
- Setter sets 5 appointments today
- 2 get cancelled later
- Leaderboard shows: 5 appointments ✅
- Quality metrics show: "3 48h, 4 PB" (separate column)

**Result:** Setters are rewarded for their door efforts TODAY, regardless of what happens later.

---

## ✅ Verification

After the fix:
- ✅ Setter leaderboard shows gross appointments set (exclude only reschedules)
- ✅ Quality metrics (48h, power bill) calculated from same gross set
- ✅ Quality metrics shown separately on the same row
- ✅ Cancelled/no-show appointments included in count (for gamification)
- ✅ Reschedules excluded (only new appointments count)

---

**Status:** 🟢 **FIXED - READY FOR TESTING**

Setter leaderboard now gamifies TODAY's door efforts correctly!
