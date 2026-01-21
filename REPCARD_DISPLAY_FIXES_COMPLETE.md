# RepCard Display Fixes - Complete

**Date:** 2025-01-28  
**Status:** ✅ **FIXES APPLIED**

---

## 🎯 Problem

User reported seeing **0%** for:
- Appointments within 48 hours (should show actual percentage)
- Power bill rate (should show actual percentage)

Even though there are **465 appointments** in the system.

---

## ✅ Root Cause

1. **Quality Metrics Calculation**: The `getQualityMetricsForUser` function was using RepCard API instead of database queries
2. **Unified Dashboard API**: Was checking attachment tables with EXISTS queries instead of using the `has_power_bill` column
3. **Missing Database Queries**: Not using the pre-calculated `is_within_48_hours` and `has_power_bill` columns

---

## ✅ Fixes Applied

### 1. User Stats Route (`app/api/repcard/users/[userId]/stats/route.ts`)

**Changed:** Replaced API-based quality metrics with **database queries**

**Now Uses:**
- ✅ `is_within_48_hours` column directly from `repcard_appointments`
- ✅ `has_power_bill` column directly from `repcard_appointments`
- ✅ Role-based queries (setter vs closer)
- ✅ Proper date filtering

**Calculations:**
- **Appointment Speed**: `COUNT(*) FILTER (WHERE is_within_48_hours = TRUE) / total_appointments * 100`
- **Power Bill Rate**: `COUNT(*) FILTER (WHERE has_power_bill = TRUE) / total_appointments * 100` (for closers) or `customers_with_attachments / total_customers * 100` (for setters)

### 2. Unified Dashboard API (`app/api/repcard/unified-dashboard/route.ts`)

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

**Benefits:**
- ✅ Much faster (uses index)
- ✅ Uses pre-calculated values
- ✅ More accurate

---

## 📊 Database Columns Used

### `repcard_appointments` Table
- ✅ `is_within_48_hours` BOOLEAN - Calculated by trigger (migration 031)
- ✅ `has_power_bill` BOOLEAN - Calculated by trigger (migration 015)
- ✅ `is_reschedule` BOOLEAN - Tracks reschedules
- ✅ `scheduled_at` TIMESTAMP - Used for date filtering
- ✅ `setter_user_id` INTEGER - For setter attribution
- ✅ `closer_user_id` INTEGER - For closer attribution

---

## 🧪 Testing

After deployment, verify:
- [ ] Appointment speed shows correct percentage (not 0%)
- [ ] Power bill rate shows correct percentage (not 0%)
- [ ] Percentages match actual data in database
- [ ] Role-based filtering works (setters vs closers)

---

## 📝 Files Modified

1. ✅ `app/api/repcard/users/[userId]/stats/route.ts` - Database-based quality metrics
2. ✅ `app/api/repcard/unified-dashboard/route.ts` - Direct column usage for power bill

---

## 🎉 Summary

**Before:** 0% for both metrics (using API/EXISTS queries)  
**After:** Correct percentages using database columns

**Status:** ✅ **READY FOR TESTING!**

---

## 🔍 Debugging

If percentages still show 0%, check:
1. **Migration 015** applied? (`has_power_bill` column exists)
2. **Migration 031** applied? (`is_within_48_hours` calculation correct)
3. **Triggers active?** (auto-calculate on insert/update)
4. **Data populated?** (run backfill if needed)
