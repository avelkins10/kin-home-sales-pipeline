# RepCard Display Fixes - Final Summary

**Date:** 2025-01-28  
**Status:** ✅ **ALL FIXES APPLIED**

---

## 🎯 Problem

User reported seeing **0.0%** for:
- **48-Hour Speed** (should show actual percentage)
- **Power Bill Rate** (should show actual percentage)

Even though there are **465 appointments** in the system.

Also seeing **"0 in 48h"** and **"0 w/PB"** in leaderboards.

---

## ✅ Root Causes Identified

1. **Quality Metrics API**: Using RepCard API instead of database queries
2. **Unified Dashboard API**: Using EXISTS queries on attachment tables instead of `has_power_bill` column
3. **Leaderboard Queries**: Using EXISTS queries instead of `has_power_bill` column
4. **Missing Data**: Columns might not be populated (need backfill)

---

## ✅ Fixes Applied

### 1. User Stats Route (`app/api/repcard/users/[userId]/stats/route.ts`)

**Changed:** Replaced API-based quality metrics with **database queries**

**Now Uses:**
- ✅ `is_within_48_hours` column directly
- ✅ `has_power_bill` column directly
- ✅ Role-based queries (setter vs closer)

### 2. Unified Dashboard API - Quality Metrics (`app/api/repcard/unified-dashboard/route.ts`)

**Changed:** Replaced EXISTS queries with **direct column usage**

**Before:**
```sql
COUNT(DISTINCT a.id) FILTER (
  WHERE EXISTS (
    SELECT 1 FROM repcard_appointment_attachments aa
    WHERE aa.repcard_appointment_id::TEXT = a.repcard_appointment_id::TEXT
  ) OR EXISTS (
    SELECT 1 FROM repcard_customer_attachments ca
    WHERE ca.repcard_customer_id::TEXT = a.repcard_customer_id::TEXT
  )
)::int as with_power_bill
```

**After:**
```sql
COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill
```

### 3. Unified Dashboard API - Leaderboard (`app/api/repcard/unified-dashboard/route.ts`)

**Changed:** Fixed top appointment setters query to use `has_power_bill` column

**Before:**
```sql
COUNT(DISTINCT a.id) FILTER (
  WHERE EXISTS (...)
)::int as with_power_bill_count
```

**After:**
```sql
COUNT(DISTINCT a.id) FILTER (WHERE a.has_power_bill = TRUE)::int as with_power_bill_count
```

---

## 📊 Database Columns Used

### `repcard_appointments` Table
- ✅ `is_within_48_hours` BOOLEAN - Calculated by trigger (migration 031)
- ✅ `has_power_bill` BOOLEAN - Calculated by trigger (migration 015)
- ✅ `is_reschedule` BOOLEAN - Tracks reschedules
- ✅ `scheduled_at` TIMESTAMP - Used for date filtering

---

## 🔧 Backfill Script Created

**File:** `scripts/backfill-repcard-metrics.ts`

**Purpose:** Recalculates `is_within_48_hours` and `has_power_bill` for all appointments

**Usage:**
```bash
npx tsx scripts/backfill-repcard-metrics.ts
```

**What it does:**
1. Updates `is_within_48_hours` based on `scheduled_at - customer.created_at <= 48 hours`
2. Updates `has_power_bill` based on attachment existence
3. Verifies results

---

## 🧪 Testing Checklist

After deployment and backfill:
- [ ] Appointment Quality card shows correct percentages (not 0%)
- [ ] Leaderboards show correct "in 48h" and "w/PB" counts (not 0)
- [ ] Percentages match actual data in database
- [ ] Date range filtering works correctly

---

## 📝 Files Modified

1. ✅ `app/api/repcard/users/[userId]/stats/route.ts` - Database-based quality metrics
2. ✅ `app/api/repcard/unified-dashboard/route.ts` - Direct column usage (2 places)
3. ✅ `scripts/backfill-repcard-metrics.ts` - Created backfill script

---

## 🚀 Next Steps

1. **Deploy fixes** ✅ (code changes complete)
2. **Run backfill script** ⚠️ (needs to be run on production database)
   ```bash
   npx tsx scripts/backfill-repcard-metrics.ts
   ```
3. **Verify results** - Check dashboard shows correct percentages

---

## 🎉 Summary

**Before:** 0% for both metrics (using API/EXISTS queries)  
**After:** Correct percentages using database columns (once backfilled)

**Status:** ✅ **CODE FIXES COMPLETE - READY FOR BACKFILL!**

---

## ⚠️ Important Note

The code fixes are complete, but **existing appointments may still show 0%** until the backfill script is run. The triggers will automatically calculate these values for **new appointments**, but existing ones need to be backfilled.
