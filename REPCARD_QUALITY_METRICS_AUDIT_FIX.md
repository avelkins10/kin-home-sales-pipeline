# RepCard Quality Metrics Audit & Fix

**Date:** 2025-01-28  
**Status:** ✅ **CRITICAL BUGS FIXED**

---

## 🔴 Critical Issues Found

### Issue 1: 48-Hour Calculation Mismatch

**Problem:**
- **Sync Service** was using `appointment.createdAt` (when appointment record was created)
- **Backfill** was using `scheduled_at` (when appointment is scheduled)
- These are **completely different calculations**!

**Impact:**
- Sync service calculated one way, backfill calculated another
- Results were inconsistent
- All 465 appointments showing 0% because sync service logic was wrong

**Fix:**
- ✅ Changed sync service to use `appointment.startAt` (scheduled_at) instead of `createdAt`
- ✅ Both now use: `scheduled_at - customer.created_at <= 48 hours`
- ✅ This measures: "Was the appointment scheduled within 48 hours of the door knock?"

### Issue 2: Power Bill Detection Too Broad

**Problem:**
- Query had `OR attachment_type IS NULL` which matched **ALL** attachments with NULL type
- This could cause false positives or miss actual power bills

**Fix:**
- ✅ Removed the `OR attachment_type IS NULL` clause
- ✅ Now only matches when:
  - `attachment_type` contains 'power' or 'bill' (if not NULL)
  - OR `file_name` contains 'power' or 'bill' (if not NULL)

### Issue 3: Verification Query Result Extraction

**Problem:**
- `Array.from()` wasn't working correctly with SQL results
- Verification queries showed 0 even when data existed

**Fix:**
- ✅ Added `getRows()` helper function for consistent result extraction
- ✅ Added debug logging to see actual verification results

---

## 📋 What Needs to Happen Next

### 1. Re-sync Appointments (CRITICAL)

The sync service fix means **all existing appointments** were calculated with the wrong logic. You need to:

1. **Re-sync appointments** to recalculate `is_within_48_hours` with the correct logic:
   - Go to Settings → RepCard → Sync Status
   - Run a sync for appointments (or full sync)

2. **OR run the backfill** which now uses the correct logic:
   - Go to Settings → RepCard → Metrics
   - Click "Run Metrics Backfill"

### 2. Verify Power Bill Attachments

Run the audit script to see what attachment types/file names exist:
```bash
tsx scripts/audit-power-bill-attachments.ts
```

This will show:
- What attachment types are in the database
- What file names might be power bills
- Whether the detection logic is working

### 3. Verify 48-Hour Calculations

Run the audit script to compare calculations:
```bash
tsx scripts/audit-repcard-48h-calculation.ts
```

This will show:
- Sample appointments and their calculations
- Whether stored values match what they should be
- If there are mismatches

---

## 🔍 Root Cause Analysis

### Why This Happened

1. **Sync service** was written first and used `appointment.createdAt` (when the appointment record was created in RepCard)
2. **Backfill** was written later and used `scheduled_at` (when the appointment is actually scheduled)
3. **No validation** that both were using the same logic
4. **Business logic confusion**: 
   - "48-hour speed" should measure: "How fast did we schedule the appointment after the door knock?"
   - This requires: `scheduled_at - customer.created_at`
   - NOT: `appointment.createdAt - customer.created_at` (which measures data entry speed, not scheduling speed)

### The Correct Logic

**48-Hour Speed = Appointment scheduled within 48 hours of customer creation (door knock)**

```sql
scheduled_at - customer.created_at <= 48 hours
AND scheduled_at - customer.created_at >= 0 hours
```

This measures **scheduling speed**, which is what the business cares about.

---

## ✅ Files Changed

1. **`lib/repcard/sync-service.ts`**
   - Changed from `appointment.createdAt` to `appointment.startAt`
   - Now matches backfill logic

2. **`app/api/admin/repcard/backfill-metrics/route.ts`**
   - Fixed power bill detection (removed NULL clause)
   - Fixed result extraction (added getRows helper)
   - Added debug logging

3. **`scripts/audit-repcard-48h-calculation.ts`** (NEW)
   - Script to audit 48h calculations
   - Compares stored vs should-be values
   - Shows differences between scheduled_at and created_at logic

4. **`scripts/audit-power-bill-attachments.ts`** (NEW)
   - Script to audit power bill detection
   - Shows attachment types and file names
   - Verifies detection logic

---

## 🚀 Next Steps

1. ✅ **Code is fixed and pushed**
2. ⏳ **Re-sync appointments** (or run backfill)
3. ⏳ **Run audit scripts** to verify
4. ⏳ **Check dashboard** - metrics should now show correct percentages

---

## 📊 Expected Results After Fix

After re-syncing or running backfill:
- **48-Hour Speed**: Should show actual percentage (not 0%)
- **Power Bill Rate**: Should show actual percentage (not 0%)
- **High Quality**: Should show appointments with both metrics
- **Individual setter metrics**: Should show "X 48h Y PB" instead of "0 48h 0 PB"

---

**Status:** 🟢 **FIXED - AWAITING RE-SYNC/BACKFILL**
