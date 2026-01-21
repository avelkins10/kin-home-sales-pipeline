# RepCard Attribution Fixes - Comprehensive Plan

**Date:** 2025-01-28  
**Priority:** CRITICAL - Fixing data attribution to correct users

---

## 🎯 Attribution Rules

### Setter Metrics (use `setter_user_id`)
- ✅ **Doors Knocked** - `repcard_customers.setter_user_id`
- ✅ **Appointments Set** - `repcard_appointments.setter_user_id`
- ✅ **Appointments Within 48 Hours** - `repcard_appointments.setter_user_id` + `is_within_48_hours`
- ✅ **Power Bill Attachment Rate** - `repcard_customers.setter_user_id` + attachments check
- ✅ **Customer Attachment Rate** - `repcard_customers.setter_user_id` + `repcard_customer_attachments`
- ✅ **Appointment Speed** - `repcard_appointments.setter_user_id` + time from customer creation

### Closer Metrics (use `closer_user_id`)
- ✅ **Sales Closed** - `repcard_appointments.closer_user_id` + `disposition ILIKE '%closed%'`
- ✅ **Revenue** - `repcard_appointments.closer_user_id` + closed appointments + customer cost
- ✅ **Appointment Outcomes** - `repcard_appointments.closer_user_id`:
  - `sat_closed` - Sat and closed
  - `sat_no_close` - Sat but didn't close
  - `no_show` - Customer didn't show
  - `cancelled` - Appointment cancelled
- ✅ **Sit/Close Rate** - `repcard_appointments.closer_user_id` (sat_closed / total sat)
- ✅ **Follow-up Consistency** - `repcard_appointments.closer_user_id` + follow-up tracking

### Both Metrics (can apply to both)
- ✅ **Quality Score** - Composite of setter + closer metrics
- ✅ **Reschedule Rate** - Can track for both (setter sets, closer may reschedule)

---

## 🔧 Files to Fix

### 1. Leaderboard Route (`app/api/repcard/leaderboard/route.ts`)
**Issues:**
- Some queries use wrong user_id (setter vs closer)
- Type casts (`::TEXT`) everywhere
- INNER JOINs hiding zero metrics
- Office attribution may be wrong

**Fixes:**
- `doors_knocked`: Use `setter_user_id` ✅
- `appointments_set`: Use `setter_user_id` ✅
- `sales_closed`: Use `closer_user_id` ✅
- `revenue`: Use `closer_user_id` ✅
- `appointment_speed`: Use `setter_user_id` ✅
- `attachment_rate`: Use `setter_user_id` ✅
- Remove all `::TEXT` casts (if migration 018 applied)
- Change INNER JOINs to LEFT JOINs
- Fix office attribution

### 2. User Stats Route (`app/api/repcard/users/[userId]/stats/route.ts`)
**Issues:**
- May not properly separate setter vs closer metrics
- Missing reschedule metrics

**Fixes:**
- Add setter-specific metrics section
- Add closer-specific metrics section
- Add reschedule metrics

### 3. Dashboard Components
**Issues:**
- May show wrong metrics for wrong roles
- Missing reschedule display

**Fixes:**
- `RepCardMetricsCard`: Show role-appropriate metrics
- Add reschedule rate display
- Fix attribution display

---

## 📋 Implementation Steps

1. **Check Schema** - Run diagnostic script
2. **Fix Leaderboard Attribution** - Ensure correct user_id usage
3. **Remove Type Casts** - If migration 018 applied
4. **Fix JOINs** - Change INNER to LEFT
5. **Fix Office Attribution** - Ensure correct office mapping
6. **Add Reschedule Metrics** - Display in UI
7. **Test** - Verify all metrics attributed correctly

---

## 🧪 Testing Checklist

After fixes:
- [ ] Setter sees only setter metrics (doors, appointments set, speed, attachments)
- [ ] Closer sees only closer metrics (sales, revenue, outcomes, sit/close rate)
- [ ] Office filtering works correctly
- [ ] Users with zero metrics appear in leaderboards
- [ ] Reschedule metrics display correctly
- [ ] Type casts removed (if migration applied)
- [ ] Queries use correct user_id fields
