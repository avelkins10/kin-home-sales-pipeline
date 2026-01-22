# RepCard Database Architecture Analysis

**Date:** 2026-01-22  
**Purpose:** Analyze database structure, relationships, and potential redundancies

---

## 📊 Current Database Structure

### Core Tables

1. **`repcard_customers`** (Leads/Door Knocks)
   - `id` (TEXT, PK) - Internal UUID
   - `repcard_customer_id` (INTEGER, UNIQUE) - RepCard's customer ID
   - `setter_user_id` (INTEGER) - RepCard user ID who created the lead
   - `office_id` (INTEGER) - RepCard office ID
   - `created_at` (TIMESTAMP) - When customer was created (for 48h calculation)

2. **`repcard_appointments`** (Appointments)
   - `id` (TEXT, PK) - Internal UUID
   - `repcard_appointment_id` (INTEGER, UNIQUE) - RepCard's appointment ID
   - `customer_id` (TEXT, FK → `repcard_customers.id`) - Internal customer link
   - `repcard_customer_id` (INTEGER) - RepCard customer ID (for easier lookups)
   - `setter_user_id` (INTEGER) - Inherited from customer
   - `closer_user_id` (INTEGER) - Rep assigned to run appointment
   - `office_id` (INTEGER) - RepCard office ID
   - `scheduled_at` (TIMESTAMP) - When appointment is scheduled
   - `completed_at` (TIMESTAMP) - When appointment actually happened
   - `is_within_48_hours` (BOOLEAN) - Calculated metric
   - `has_power_bill` (BOOLEAN) - Calculated metric

3. **`repcard_users`** (RepCard User Data)
   - `id` (TEXT, PK) - Internal UUID
   - `repcard_user_id` (INTEGER, UNIQUE) - RepCard's user ID
   - `office_id` (INTEGER) - RepCard office ID
   - `email`, `first_name`, `last_name`, etc.

4. **`repcard_offices`** (RepCard Office Data)
   - `id` (TEXT, PK) - Internal UUID
   - `repcard_office_id` (INTEGER, UNIQUE) - RepCard's office ID
   - `name`, `address`, `city`, `state`, etc.

5. **`repcard_customer_attachments`** (Customer Attachments)
   - `repcard_attachment_id` (INTEGER, UNIQUE)
   - `customer_id` (TEXT, FK → `repcard_customers.id`)
   - `repcard_customer_id` (INTEGER) - For easier lookups

6. **`repcard_appointment_attachments`** (Appointment Attachments)
   - `repcard_attachment_id` (INTEGER, UNIQUE)
   - `appointment_id` (TEXT, FK → `repcard_appointments.id`)
   - `repcard_appointment_id` (INTEGER) - For easier lookups
   - `customer_id` (TEXT, FK → `repcard_customers.id`)
   - `repcard_customer_id` (INTEGER) - For easier lookups

7. **`repcard_office_mappings`** (RepCard ↔ QuickBase Office Mapping)
   - Maps `repcard_office_id` → `quickbase_office_id`

8. **`users`** (App Users - Links to RepCard)
   - `repcard_user_id` (INTEGER) - Links to `repcard_users.repcard_user_id`

---

## 🔍 How Data is Currently Joined

### Customer → Appointment
```sql
-- Primary link (foreign key)
repcard_appointments.customer_id → repcard_customers.id (TEXT)

-- Secondary link (for easier lookups)
repcard_appointments.repcard_customer_id = repcard_customers.repcard_customer_id (INTEGER)
```

### Appointment → Customer Created At (for 48h calculation)
```sql
-- Current trigger uses:
repcard_appointments.repcard_customer_id → repcard_customers.repcard_customer_id
-- Then gets: repcard_customers.created_at
```

### Attachments → Customer/Appointment
```sql
-- Customer attachments
repcard_customer_attachments.repcard_customer_id → repcard_customers.repcard_customer_id

-- Appointment attachments  
repcard_appointment_attachments.repcard_appointment_id → repcard_appointments.repcard_appointment_id
repcard_appointment_attachments.repcard_customer_id → repcard_customers.repcard_customer_id (also stored!)
```

### Users → App Users
```sql
users.repcard_user_id → repcard_users.repcard_user_id (INTEGER)
```

### Offices
```sql
repcard_customers.office_id → repcard_offices.repcard_office_id
repcard_appointments.office_id → repcard_offices.repcard_office_id
repcard_users.office_id → repcard_offices.repcard_office_id
```

---

## ⚠️ Potential Issues & Redundancies

### 1. **Dual Customer Linking (Intentional but Could Be Confusing)**
- ✅ `customer_id` (TEXT) - Foreign key, proper relational link
- ✅ `repcard_customer_id` (INTEGER) - RepCard's ID, used for lookups
- **Status:** This is intentional and correct. The TEXT `customer_id` is for proper foreign keys, INTEGER `repcard_customer_id` is for easier joins without needing the TEXT UUID.

### 2. **Attachment Tables Store Both customer_id and repcard_customer_id**
- `repcard_appointment_attachments` has:
  - `appointment_id` (TEXT, FK)
  - `repcard_appointment_id` (INTEGER)
  - `customer_id` (TEXT, FK) ← **Redundant?**
  - `repcard_customer_id` (INTEGER) ← **Redundant?**
- **Issue:** If we have `appointment_id`, we can get `customer_id` from the appointment. Storing it separately could lead to inconsistencies.
- **Recommendation:** Keep `repcard_customer_id` for easier lookups, but `customer_id` might be redundant.

### 3. **Office ID Type Consistency**
- All office IDs are INTEGER (good!)
- Foreign keys properly set up (good!)
- Mapping table exists for QuickBase ↔ RepCard (good!)

### 4. **User ID Type Consistency**
- All user IDs normalized to INTEGER (migration 018)
- ✅ `repcard_users.repcard_user_id` (INTEGER)
- ✅ `repcard_customers.setter_user_id` (INTEGER)
- ✅ `repcard_appointments.setter_user_id` (INTEGER)
- ✅ `repcard_appointments.closer_user_id` (INTEGER)
- ✅ `users.repcard_user_id` (INTEGER)
- **Status:** All consistent! ✅

### 5. **48-Hour Calculation Join**
- Currently uses: `repcard_appointments.repcard_customer_id` → `repcard_customers.repcard_customer_id`
- This is correct and efficient (uses INTEGER join, not TEXT UUID)
- ✅ No issues here

### 6. **Power Bill Calculation Join**
- Uses: `repcard_customer_attachments.repcard_customer_id` (INTEGER)
- Uses: `repcard_appointment_attachments.repcard_appointment_id` (INTEGER)
- This is correct and efficient
- ✅ No issues here

---

## ✅ What's Working Well

1. **Proper Foreign Keys:** `customer_id` (TEXT) properly references `repcard_customers.id`
2. **Efficient Lookups:** `repcard_customer_id` (INTEGER) allows fast joins without UUID lookups
3. **Type Consistency:** All RepCard IDs are INTEGER (normalized in migration 018)
4. **Office Mapping:** Proper mapping table for RepCard ↔ QuickBase offices
5. **User Linking:** Proper linking between `users` and `repcard_users` by email

---

## 🤔 Potential Improvements

### 1. **Attachment Redundancy**
**Current:** `repcard_appointment_attachments` stores both `customer_id` and `repcard_customer_id`

**Question:** Do we need both? We can get customer from appointment:
```sql
SELECT a.repcard_customer_id 
FROM repcard_appointments a 
WHERE a.id = appointment_attachment.appointment_id
```

**Recommendation:** Keep `repcard_customer_id` for easier lookups (it's used in triggers), but `customer_id` (TEXT FK) might be redundant. However, it doesn't hurt to have it for data integrity.

### 2. **Office Inheritance**
**Current:** Appointments inherit `office_id` from customer, setter, or closer (via trigger)

**Status:** ✅ This is correct and working

### 3. **Setter/Closer Attribution**
**Current:**
- `setter_user_id` - Who knocked the door (from customer)
- `closer_user_id` - Who runs the appointment

**Status:** ✅ This is correct

---

## 🎯 Recommendations

### Keep As-Is (Working Correctly):
1. ✅ Dual customer linking (`customer_id` TEXT + `repcard_customer_id` INTEGER)
2. ✅ User ID normalization (all INTEGER)
3. ✅ Office ID consistency (all INTEGER)
4. ✅ Foreign key relationships

### Consider (Minor Optimizations):
1. **Attachment `customer_id` redundancy:** Could remove TEXT `customer_id` from `repcard_appointment_attachments` since we can get it from `appointment_id`, but keeping it doesn't hurt and provides data integrity.

2. **Index optimization:** Already have good indexes, but could add composite indexes for common query patterns (already done in migration 020).

---

## 📝 Summary

**The database structure is actually quite good!** The dual linking (TEXT UUID + INTEGER RepCard ID) is intentional and provides:
- Proper relational integrity (TEXT foreign keys)
- Fast lookups (INTEGER joins)
- Flexibility (can join either way)

**No major architectural issues found.** The structure supports:
- ✅ Proper customer → appointment relationships
- ✅ Efficient attachment lookups
- ✅ User/office attribution
- ✅ Timezone-aware calculations (now with Eastern Time)

**The 0% metrics issue is likely:**
1. Backfill ran before attachments were synced
2. Timezone calculation needs to be re-run with Eastern Time fix
3. Not a database structure problem

---

## 🔧 Next Steps

1. Re-run Migration 032 (to apply Eastern Time fix)
2. Re-run backfill (to recalculate with current attachments)
3. Use Deep Dive diagnostic to verify fixes worked

The database architecture is solid - the issue is data calculation, not structure.
