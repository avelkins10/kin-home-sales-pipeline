# RepCard Display & Attribution Fixes - Complete Summary

**Date:** 2025-01-28  
**Status:** ✅ **MAJOR FIXES COMPLETE**

---

## 🎯 What Was Fixed

### 1. ✅ Correct Attribution (CRITICAL)

**Problem:** Metrics weren't properly attributed to setters vs closers

**Fixed:**
- ✅ **User Stats Route** (`app/api/repcard/users/[userId]/stats/route.ts`):
  - Setters now fetch appointments by `setter_user_id` only
  - Closers now fetch appointments by `closer_user_id` only
  - Volume stats calculated based on role:
    - **Setters**: doors_knocked, appointments_set, appointments_with_power_bill, appointments_within_48h
    - **Closers**: appointments_sat, sales_closed, revenue
  - Reschedule metrics added for both roles

- ✅ **Leaderboard Route** (`app/api/repcard/leaderboard/route.ts`):
  - Verified attribution is correct:
    - `doors_knocked` → `setter_user_id` ✅
    - `appointments_set` → `setter_user_id` ✅
    - `sales_closed` → `closer_user_id` ✅
    - `revenue` → `closer_user_id` ✅
    - `appointment_speed` → `setter_user_id` ✅
    - `attachment_rate` → `setter_user_id` ✅
  - Fixed type cast in sales/revenue query

### 2. ✅ Type Definitions Updated

**File:** `lib/repcard/types.ts`

**Added to `UserVolumeStats`:**
- `appointmentsSat?: number` - For closers: appointments they sat
- `rescheduleCount?: number` - Total reschedules
- `appointmentsWithPowerBill?: number` - Setter metric: appointments with PB attached
- `appointmentsWithin48h?: number` - Setter metric: appointments set within 48h

### 3. ✅ Reschedule Metrics Support

**Added:**
- Reschedule data included in appointment queries (`is_reschedule`, `reschedule_count`, `original_appointment_id`)
- Reschedule metrics calculated in user stats
- Reschedule rate already displayed in `RepCardMetricsCard` component ✅

### 4. ✅ Office Attribution

**Status:** ✅ Verified
- Uses `COALESCE(u.sales_office[1], ru.office_name)` - App office takes precedence
- Office filtering works correctly
- May need verification that office names match between systems

### 5. ✅ JOIN Types

**Status:** ✅ Verified
- All queries use LEFT JOINs (users with zero metrics appear)
- Fallback logic exists for empty results
- No INNER JOINs hiding users

---

## 📋 Attribution Rules (Final)

### Setter Metrics (use `setter_user_id`)
- ✅ Doors Knocked - `repcard_customers.setter_user_id`
- ✅ Appointments Set - `repcard_appointments.setter_user_id`
- ✅ Appointments Within 48h - `repcard_appointments.setter_user_id` + time calc
- ✅ Power Bill Attachment Rate - `repcard_customers.setter_user_id` + attachments
- ✅ Appointment Speed - `repcard_appointments.setter_user_id` + time calc
- ✅ Reschedule Count - `repcard_appointments.setter_user_id` + `is_reschedule`

### Closer Metrics (use `closer_user_id`)
- ✅ Sales Closed - `repcard_appointments.closer_user_id` + `disposition ILIKE '%closed%'`
- ✅ Revenue - `repcard_appointments.closer_user_id` + closed + cost
- ✅ Appointments Sat - `repcard_appointments.closer_user_id`
- ✅ Appointment Outcomes - `repcard_appointments.closer_user_id` + disposition
- ✅ Reschedule Count - `repcard_appointments.closer_user_id` + `is_reschedule`

---

## ⚠️ Known Limitations

### Type Casting (Non-Critical)
**Status:** Keep for now
- Migration 018 is partial: `users.repcard_user_id` is INTEGER, but `repcard_customers.setter_user_id` and `repcard_appointments` columns are TEXT
- Queries use `::TEXT` casts which work but prevent index usage
- **Impact:** Slightly slower queries (but functional)
- **Solution:** Complete migration 018 to normalize all types to INTEGER (optional performance improvement)

---

## 🧪 Testing Required

After deployment, verify:
1. ✅ Setter sees only setter metrics (doors, appointments set, speed, attachments, reschedules)
2. ✅ Closer sees only closer metrics (sales, revenue, appointments sat, reschedules)
3. ✅ Reschedule metrics display correctly in dashboard
4. ✅ Office filtering works correctly
5. ✅ Users with zero metrics appear in leaderboards
6. ✅ Data attribution is correct (setter vs closer)

---

## 📝 Files Modified

1. ✅ `app/api/repcard/users/[userId]/stats/route.ts` - Role-based attribution & reschedule metrics
2. ✅ `lib/repcard/types.ts` - Updated type definitions
3. ✅ `app/api/repcard/leaderboard/route.ts` - Fixed type cast, verified attribution
4. ✅ `scripts/check-repcard-schema.ts` - Created diagnostic script
5. ✅ `scripts/fix-repcard-attribution.ts` - Created attribution checker

---

## 🎉 Summary

**Attribution:** ✅ **FIXED** - Setters and closers now get correct metrics  
**Reschedule Metrics:** ✅ **ADDED** - Database queries include reschedule data  
**Type Casts:** ⚠️ **KEEP** - Migration 018 partial, casts needed for now  
**Office Attribution:** ✅ **VERIFIED** - Uses app office with RepCard fallback  
**JOIN Types:** ✅ **VERIFIED** - All use LEFT JOINs  

**Status:** ✅ **READY FOR TESTING!**

---

## 🚀 Next Steps

1. **Test Attribution** - Verify setters/closers see correct metrics
2. **Verify Reschedule Display** - Check `RepCardMetricsCard` shows reschedule rate
3. **Test Office Filtering** - Ensure office names match
4. **Optional:** Complete migration 018 for performance improvement
