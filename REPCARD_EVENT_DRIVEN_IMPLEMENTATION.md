# RepCard Event-Driven Metrics Implementation - Complete ✅

**Date:** 2025-01-20  
**Status:** ✅ **IMPLEMENTATION COMPLETE**

---

## 🎯 What Was Implemented

Complete redesign of RepCard metrics calculation using **event-driven architecture**. Metrics are now calculated automatically by database triggers when data changes, ensuring accuracy and eliminating NULL values.

---

## ✅ Implementation Summary

### Phase 1 & 2: Event-Driven Triggers with Audit Trail ✅

**Migration:** `lib/db/migrations/032_repcard_event_driven_metrics.sql`

**What it does:**
- Creates `repcard_metric_audit` table to track why metrics are TRUE/FALSE
- Creates enhanced trigger functions that calculate metrics automatically
- Triggers recalculate when:
  - Appointments are inserted/updated
  - Attachments are added/removed
  - Customer `created_at` changes
- **Ensures NO NULL values** - triggers always set TRUE or FALSE

**Key Functions:**
- `calculate_is_within_48_hours()` - Calculates 48h speed with audit trail
- `calculate_has_power_bill()` - Calculates power bill status with audit trail
- `update_appointment_metrics()` - Main trigger function
- `recalculate_power_bill_on_attachment_change()` - Recalculates when attachments change
- `recalculate_48h_on_customer_change()` - Recalculates when customer.created_at changes

### Phase 3: Simplified Sync Service ✅

**File:** `lib/repcard/sync-service.ts`

**Changes:**
- Removed all calculation code for `is_within_48_hours` and `has_power_bill`
- Now just inserts/updates data - triggers handle calculations
- Single source of truth: triggers always calculate correctly

### Phase 4: Simplified Backfill ✅

**Files:**
- `app/api/admin/repcard/backfill-metrics/route.ts`
- `app/api/cron/repcard-backfill-metrics/route.ts`

**Changes:**
- Backfill now just updates appointments to trigger recalculation
- Uses same trigger logic (consistent)
- No complex queries needed

### Phase 6: Enhanced Debugging ✅

**New Endpoints:**
- `/api/repcard/debug-metric?appointmentId=123&metric=is_within_48_hours`
- `/api/repcard/diagnose-metrics?startDate=2026-01-20&endDate=2026-01-20`

**Features:**
- Shows why metrics are TRUE/FALSE
- Displays audit history
- Compares stored vs calculated values
- Identifies mismatches

### Phase 7: Dashboard Queries ✅

**File:** `app/api/repcard/unified-dashboard/route.ts`

**Status:** Already using stored values (no changes needed)

---

## 🚀 How to Use

### Step 1: Run Migration 032

**Option A: Via Web UI (Recommended)**
1. Go to **Settings → RepCard → Metrics** tab
2. Click **"Run Migration 032"** button
3. Wait for completion (~10-30 seconds)
4. Verify success message shows audit table and triggers created

**Option B: Via API**
```bash
POST /api/admin/repcard/run-migration-032
```

**Option C: Via Script**
```bash
npx tsx scripts/run-migration-032.ts
```

### Step 2: Run Backfill

**Via Web UI:**
1. After migration completes, click **"Run Metrics Backfill"** button
2. This triggers recalculation for all existing appointments
3. Wait for completion (~1-2 minutes)
4. Verify metrics show correct percentages

**What happens:**
- Updates all appointments (triggers `updated_at = NOW()`)
- Triggers automatically recalculate `is_within_48_hours` and `has_power_bill`
- All calculations logged to `repcard_metric_audit` table

### Step 3: Verify

**Check Dashboard:**
- Go to **Analytics → RepCard** tab
- Metrics should show accurate percentages (not 0%)
- 48-Hour Speed and Power Bill Rate should be > 0% if data exists

**Check Debug Endpoint:**
```
GET /api/repcard/debug-metric?appointmentId=123&metric=is_within_48_hours
```

**Check Audit Table:**
```sql
SELECT * FROM repcard_metric_audit 
WHERE appointment_id = 'your-appointment-id'
ORDER BY calculated_at DESC
LIMIT 10;
```

---

## 📊 Key Benefits

1. **Accuracy**: Metrics calculated once, stored, never recalculated incorrectly
2. **Debuggability**: Audit trail shows exactly why each metric is TRUE/FALSE
3. **Simplicity**: Single calculation point (triggers), simple queries
4. **Reliability**: Can't have NULL values (triggers ensure they're set)
5. **Performance**: Fast queries (just read stored values)

---

## 🔍 Debugging Tools

### Debug Single Metric
```
GET /api/repcard/debug-metric?appointmentId=123&metric=is_within_48_hours
```

**Response:**
```json
{
  "appointment": { ... },
  "metric": {
    "name": "is_within_48_hours",
    "currentValue": true,
    "shouldBeValue": true,
    "isCorrect": true,
    "shouldBeReason": "scheduled_at - customer.created_at = 23.5 hours (within 48h)",
    "rawData": {
      "scheduledAt": "2026-01-20T10:00:00Z",
      "customerCreatedAt": "2026-01-19T10:30:00Z",
      "hoursDiff": 23.5
    }
  },
  "auditHistory": [ ... ],
  "diagnosis": {
    "matchesExpected": true,
    "issue": "Metric value is correct",
    "recommendation": "No action needed"
  }
}
```

### Diagnose All Metrics
```
GET /api/repcard/diagnose-metrics?startDate=2026-01-20&endDate=2026-01-20
```

Shows:
- Database state (TRUE/FALSE/NULL counts)
- Sample appointments with calculated vs stored values
- Mismatches between what should be TRUE and what is stored

---

## 📝 Files Created/Modified

### New Files
- `lib/db/migrations/032_repcard_event_driven_metrics.sql` - Migration with triggers and audit table
- `app/api/admin/repcard/run-migration-032/route.ts` - API endpoint to run migration
- `app/api/repcard/debug-metric/route.ts` - Debug endpoint for single metrics
- `app/api/repcard/diagnose-metrics/route.ts` - Comprehensive diagnostic endpoint
- `scripts/run-migration-032.ts` - Script to run migration

### Modified Files
- `lib/repcard/sync-service.ts` - Removed calculation code
- `app/api/admin/repcard/backfill-metrics/route.ts` - Simplified to trigger-based
- `app/api/cron/repcard-backfill-metrics/route.ts` - Simplified to trigger-based
- `components/settings/RepCardManagementTab.tsx` - Added migration button and improved UI

---

## ⚠️ Important Notes

1. **Migration must be run first** - Backfill won't work correctly without triggers
2. **One-time setup** - Migration only needs to be run once
3. **Backfill can be run anytime** - To recalculate existing appointments
4. **Auto-backfill cron** - Still runs every 10 minutes, now uses trigger-based approach
5. **No more NULL values** - Triggers ensure metrics are always TRUE or FALSE

---

## 🎉 Expected Results

After running migration and backfill:

- ✅ Dashboard shows accurate 48-Hour Speed percentages
- ✅ Dashboard shows accurate Power Bill Rate percentages
- ✅ No NULL values in database
- ✅ Easy to debug - can see why any metric is TRUE/FALSE
- ✅ Metrics automatically update when data changes
- ✅ No manual backfill needed going forward

---

## 🔄 Ongoing Operation

**Automatic:**
- Triggers calculate metrics on every insert/update
- Auto-backfill cron runs every 10 minutes (uses triggers)
- Metrics stay accurate automatically

**Manual (if needed):**
- Run backfill to recalculate all appointments
- Use debug endpoints to investigate specific issues
- Check audit table to see calculation history

---

## 📚 Related Documentation

- Migration file: `lib/db/migrations/032_repcard_event_driven_metrics.sql`
- Debug endpoint: `app/api/repcard/debug-metric/route.ts`
- Diagnostic endpoint: `app/api/repcard/diagnose-metrics/route.ts`
- Backfill endpoint: `app/api/admin/repcard/backfill-metrics/route.ts`
