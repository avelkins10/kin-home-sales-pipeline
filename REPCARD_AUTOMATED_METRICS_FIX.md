# RepCard Automated Metrics System

## ✅ Fully Automated - No Manual Steps Required!

The system is now **fully automated** and will handle all historical and future metrics automatically. Here's how it works:

---

## 🔄 Automated Processes

### 1. **Every 5 Minutes: Data Sync + Auto-Linking**
**Cron:** `/api/cron/repcard-sync` (runs every 5 minutes)

**What it does:**
1. Syncs customers from RepCard
2. Syncs appointments from RepCard
3. **Automatically links appointments to customers** (new!)
4. Database triggers automatically recalculate metrics when data changes

**Result:** New appointments are immediately linked to their customers, and metrics are calculated automatically.

---

### 2. **Every 10 Minutes: Metrics Recalculation**
**Cron:** `/api/cron/repcard-backfill-metrics` (runs every 10 minutes)

**What it does:**
1. **Auto-links any remaining appointments to customers** (catches any missed links)
2. Triggers metric recalculation for all appointments
3. Ensures `is_within_48_hours` and `has_power_bill` are always correct

**Result:** Metrics stay accurate even if there were timing issues during sync.

---

### 3. **Database Triggers (Event-Driven)**
**Migration:** `032_repcard_event_driven_metrics.sql`

**What it does:**
- Automatically calculates `is_within_48_hours` when:
  - An appointment is inserted/updated
  - A customer's `created_at` changes
- Automatically calculates `has_power_bill` when:
  - An appointment is inserted/updated
  - An attachment is added/removed
- Logs all calculations to `repcard_metric_audit` table for debugging

**Result:** Metrics are calculated in real-time as data changes, no manual steps needed.

---

## 📊 What This Means for You

### ✅ Historical Data
- **Automatically fixed:** As the syncs run every 5-10 minutes, they'll gradually link all existing appointments to customers
- **No action needed:** The system will catch up on its own

### ✅ Future Data
- **Automatically linked:** New appointments are immediately linked to customers during sync
- **Automatically calculated:** Metrics are calculated by database triggers in real-time
- **Always accurate:** The 10-minute backfill ensures nothing is missed

### ✅ No Manual Steps
- **No button clicking required** after the initial one-time fix
- **No manual backfills needed**
- **No manual linking needed**

---

## 🚀 One-Time Initial Fix (Optional)

If you want to fix the existing 558 appointments **immediately** instead of waiting for the automated syncs:

1. Go to **Settings → RepCard → Metrics tab**
2. Click **"Fix Missing Customers (Sync + Link + Recalculate)"**
3. Wait for it to complete (~1-2 minutes)

**After this one-time fix, everything is automatic!**

---

## 🔍 How to Verify It's Working

### Check Sync Status
1. Go to **Settings → RepCard → Sync Status tab**
2. Look for recent syncs (should show syncs every 5 minutes)
3. Check that customers and appointments are being synced

### Check Metrics
1. Go to **Settings → RepCard → Overview tab**
2. Look at "Quality Metrics (Last 30 Days)"
3. The percentages should gradually improve as appointments get linked

### Check Customer Links
1. Go to **Settings → RepCard → Metrics tab**
2. Click **"Check: Why Customers Missing created_at"**
3. The "Without Customer" count should decrease over time

---

## 📝 Technical Details

### Auto-Linking Logic
```sql
UPDATE repcard_appointments a
SET customer_id = c.id, updated_at = NOW()
FROM repcard_customers c
WHERE a.repcard_customer_id::text = c.repcard_customer_id::text
  AND a.customer_id IS NULL
  AND c.id IS NOT NULL
```

This runs:
- After every sync (in `repcard-sync` cron)
- Before every metrics backfill (in `repcard-backfill-metrics` cron)
- After comprehensive sync completes (in `runComprehensiveSync`)

### Metric Calculation
- **48-Hour Speed:** Calculated by trigger `calculate_is_within_48_hours()` using Eastern Time
- **Power Bill Rate:** Calculated by trigger `calculate_has_power_bill()` checking for any attachments
- **Both use event-driven architecture:** Metrics are calculated automatically when data changes

---

## ⚠️ Important Notes

1. **First sync may take time:** If this is the first time running, the initial sync might take 5-10 minutes to fetch all historical data.

2. **Metrics improve gradually:** As the automated syncs run, they'll gradually link appointments to customers. You should see metrics improve over the next 10-20 minutes.

3. **Database triggers required:** Make sure Migration 032 has been run. If not, metrics won't be calculated automatically.

4. **No manual intervention needed:** Once the system is running, you don't need to click any buttons. Everything happens automatically.

---

## 🎯 Summary

**Yes, it's fully automated!** 

- ✅ Historical data: Fixed automatically by syncs
- ✅ Future data: Linked and calculated automatically
- ✅ No manual steps: Everything happens in the background
- ✅ Always accurate: Multiple layers of automation ensure data integrity

The only manual step is the **one-time initial fix** if you want to speed things up, but even that's optional - the automated syncs will handle it eventually.
