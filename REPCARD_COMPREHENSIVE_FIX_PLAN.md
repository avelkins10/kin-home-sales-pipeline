# RepCard Comprehensive Fix Plan - Complete Attribution & Display Fixes

**Date:** 2025-01-28  
**Priority:** CRITICAL  
**Status:** In Progress

---

## 🎯 Fix Objectives

1. ✅ **Correct Attribution** - Setters get setter metrics, closers get closer metrics
2. ✅ **Fix Type Casts** - Keep for now (migration 018 partial), document for future removal
3. ✅ **Fix JOINs** - Ensure LEFT JOINs show users with zero metrics
4. ✅ **Fix Office Attribution** - Ensure correct office mapping
5. ✅ **Add Reschedule Metrics** - Display in dashboard components

---

## 📋 Attribution Rules (CRITICAL)

### Setter Metrics (use `setter_user_id`)
- **Doors Knocked**: `repcard_customers.setter_user_id` ✅
- **Appointments Set**: `repcard_appointments.setter_user_id` ✅
- **Appointments Within 48h**: `repcard_appointments.setter_user_id` + `is_within_48_hours` ✅
- **Power Bill Attachment**: `repcard_customers.setter_user_id` + attachments ✅
- **Appointment Speed**: `repcard_appointments.setter_user_id` + time calc ✅

### Closer Metrics (use `closer_user_id`)
- **Sales Closed**: `repcard_appointments.closer_user_id` + `disposition ILIKE '%closed%'` ✅
- **Revenue**: `repcard_appointments.closer_user_id` + closed + cost ✅
- **Appointment Outcomes**: `repcard_appointments.closer_user_id`:
  - `sat_closed`, `sat_no_close`, `no_show`, `cancelled` ✅
- **Sit/Close Rate**: `repcard_appointments.closer_user_id` ✅

### Both Metrics
- **Quality Score**: Composite (setter + closer) ✅
- **Reschedule Rate**: Track for both ✅

---

## 🔧 Files to Fix

### 1. `app/api/repcard/leaderboard/route.ts` (1367 lines)
**Critical Fixes:**
- ✅ Line 500: `appointment_speed` - Uses `setter_user_id` (CORRECT)
- ✅ Line 557: `attachment_rate` - Uses `setter_user_id` (CORRECT)
- ✅ Line 1232-1287: `sales_closed`/`revenue` - Uses `closer_user_id` (CORRECT)
- ⚠️ Line 707-815: `doors_knocked` - Uses `setter_user_id` (CORRECT) but has type casts
- ⚠️ Line 946-1018: `appointments_set` - Uses `setter_user_id` (CORRECT) but has type casts
- ⚠️ Office attribution: Uses `COALESCE(u.sales_office[1], ru.office_name)` (NEEDS VERIFICATION)

**Type Casts:**
- Keep `::TEXT` casts for now (migration 018 partial)
- Document for removal after migration completes

**JOINs:**
- All use LEFT JOIN ✅ (good)
- Fallback logic exists ✅ (good)

### 2. `app/api/repcard/users/[userId]/stats/route.ts`
**Fixes Needed:**
- Add role-based metric filtering
- Add reschedule metrics
- Ensure setter/closer attribution correct

### 3. Dashboard Components
**Fixes Needed:**
- `RepCardMetricsCard`: Show role-appropriate metrics
- Add reschedule rate display
- Fix attribution display

---

## ✅ Verification Checklist

After fixes:
- [ ] Setter sees only setter metrics
- [ ] Closer sees only closer metrics  
- [ ] Office filtering works
- [ ] Users with zero metrics appear
- [ ] Reschedule metrics display
- [ ] Type casts work (migration partial)
- [ ] Queries use correct user_id fields

---

## 🚀 Implementation Order

1. **Verify Current State** - Run diagnostic scripts
2. **Fix Leaderboard Attribution** - Ensure correct user_id usage
3. **Fix User Stats Route** - Add role-based filtering
4. **Add Reschedule Metrics** - Display in UI
5. **Test** - Verify all metrics attributed correctly

---

## 📝 Notes

- Migration 018 is **partial** - `users.repcard_user_id` is INTEGER, but `repcard_customers.setter_user_id` and `repcard_appointments` columns are TEXT
- **Keep type casts** (`::TEXT`) for now until migration completes
- Attribution logic is **mostly correct** - just needs verification and reschedule metrics
