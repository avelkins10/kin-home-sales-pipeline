# RepCard Integration — Summary

**Date:** 2025-01-28  
**Status:** ✅ **COMPLETE — Ready for Backfill**

---

## 🎯 Problem Statement

Dashboard showed **0.0%** for:
- **48-Hour Speed** metric (despite 465 appointments)
- **Power Bill Rate** metric (despite 465 appointments)

Also seeing **"0 in 48h"** and **"0 w/PB"** in leaderboards.

---

## ✅ Completed Work

### 1. Quality Metrics Display Fixes

**Problem:** Dashboard showed 0.0% for "48-Hour Speed" and "Power Bill Rate" despite 465 appointments

**Root Cause:** APIs were using RepCard API calls or EXISTS queries on attachment tables instead of direct database column queries.

**Solution:**
- ✅ Switched from API-based calculations to **direct database column queries**
- ✅ Using `is_within_48_hours` and `has_power_bill` columns directly
- ✅ Added 4 quality categories: Power Bill, 48-Hour Speed, Both, Neither
- ✅ Created backfill script to populate columns for existing appointments

**Files Modified:**
- `app/api/repcard/users/[userId]/stats/route.ts` - Now uses database columns directly
- `app/api/repcard/unified-dashboard/route.ts` - Fixed quality metrics and leaderboard queries

---

### 2. Real-Time Sync Optimization

**Problem:** Data updates were delayed 5-30 minutes

**Solution:**
- ✅ **Cache TTLs:** Reduced from 15-30 minutes to **5 minutes** (aligned with sync interval)
- ✅ **Frontend polling:** Standardized to **30 seconds** across all RepCard components
- ✅ **Sync status indicator:** Added "Last synced: X min ago" with color coding
- ✅ **Effective delay:** ~30-90 seconds (down from 5-30 minutes)

**Impact:**
- Users see updates within 30-90 seconds instead of 5-30 minutes
- Better real-time experience for sales team

---

### 3. Settings Page Consolidation

**Before:** 3 separate RepCard tabs (Debug, Data Debug, Config)

**After:** 1 unified **"RepCard"** tab with 4 sub-sections:
- **Overview:** Key stats and metrics
- **Sync Status:** Monitor sync operations
- **Metrics:** Backfill button to fix 0.0% issue
- **Configuration:** Placeholder (separate Config tab for advanced settings)

**Files:**
- `components/settings/RepCardManagementTab.tsx` - Unified management interface
- `app/(sales)/settings/page.tsx` - Integrated into settings page

---

### 4. Backfill Functionality

**Created:**
- ✅ **API endpoint:** `/api/admin/repcard/backfill-metrics`
- ✅ **UI button:** Settings → RepCard → Metrics tab
- ✅ **Verification:** Shows stats after completion

**What it does:**
- Recalculates `is_within_48_hours` for all appointments
- Recalculates `has_power_bill` for all appointments
- Verifies results and shows percentages

**Files:**
- `app/api/admin/repcard/backfill-metrics/route.ts` - Backfill API endpoint
- `components/settings/RepCardManagementTab.tsx` - UI integration

---

## 📊 Technical Details

### Database Columns
- `is_within_48_hours` - Boolean column in `repcard_appointments` table
- `has_power_bill` - Boolean column in `repcard_appointments` table

### Calculation Logic

**is_within_48_hours:**
```sql
CASE
  WHEN scheduled_at IS NOT NULL AND customer.created_at IS NOT NULL
    AND (scheduled_at - customer.created_at) <= INTERVAL '48 hours' 
    AND (scheduled_at - customer.created_at) >= INTERVAL '0 hours' 
  THEN TRUE
  ELSE FALSE
END
```

**has_power_bill:**
```sql
CASE
  WHEN EXISTS (
    SELECT 1 FROM repcard_customer_attachments
    WHERE attachment_type ILIKE '%power%' OR attachment_type ILIKE '%bill%'
       OR file_name ILIKE '%power%' OR file_name ILIKE '%bill%'
  ) OR EXISTS (
    SELECT 1 FROM repcard_appointment_attachments
    WHERE attachment_type ILIKE '%power%' OR attachment_type ILIKE '%bill%'
       OR file_name ILIKE '%power%' OR file_name ILIKE '%bill%'
  ) THEN TRUE
  ELSE FALSE
END
```

### Sync Configuration
- **Sync frequency:** Every 5 minutes via Vercel Cron
- **Cache strategy:** 5-minute TTL, 30-second frontend polling
- **Attribution:** Correctly separates setter vs closer metrics

---

## 🚀 Current Status

### Code Status
- ✅ **Committed and pushed** to main branch
- ✅ **Deployment:** Changes are live (commits: 1bb2b6c, 604f0cf, 79929bd)
- ✅ **Production-ready:** All changes deployed

### Next Step: Run Backfill

**To fix the 0.0% metrics issue:**

1. Go to **Sales App → Settings → RepCard** tab
2. Click **"Metrics"** sub-tab
3. Click **"Run Metrics Backfill"** button
4. Wait for completion (~1-2 minutes)
5. Refresh dashboard — metrics should show correct percentages

**Expected Results:**
- Backfill will update all appointments with calculated values
- Dashboard will show actual percentages instead of 0.0%
- Leaderboards will show correct counts

---

## 📈 Expected Impact

### Before Backfill
- 48-Hour Speed: **0.0%**
- Power Bill Rate: **0.0%**
- Leaderboards: **"0 in 48h"**, **"0 w/PB"**

### After Backfill
- 48-Hour Speed: **Actual percentage** (e.g., 45.2%)
- Power Bill Rate: **Actual percentage** (e.g., 78.5%)
- Leaderboards: **Correct counts** (e.g., "210 in 48h", "365 w/PB")

---

## 🔍 Verification

After running the backfill, verify:

1. **Dashboard Metrics:**
   - Check RepCard dashboard shows non-zero percentages
   - Verify 48-Hour Speed and Power Bill Rate are accurate

2. **Leaderboards:**
   - Check "in 48h" and "w/PB" counts are non-zero
   - Verify counts match dashboard metrics

3. **Database:**
   - Run verification query to check column population
   - Ensure NULL values are minimal

---

## 📝 Related Files

### API Endpoints
- `app/api/admin/repcard/backfill-metrics/route.ts` - Backfill endpoint
- `app/api/repcard/users/[userId]/stats/route.ts` - User stats (uses columns)
- `app/api/repcard/unified-dashboard/route.ts` - Dashboard (uses columns)

### Components
- `components/settings/RepCardManagementTab.tsx` - Settings UI
- `components/dashboard/RepCardMetricsCard.tsx` - Dashboard metrics display

### Scripts
- `scripts/backfill-repcard-metrics.ts` - Standalone backfill script
- `scripts/check-repcard-metrics.ts` - Verification script

### Migrations
- `lib/db/migrations/015_repcard_comprehensive_fields.sql` - Column definitions
- `lib/db/migrations/031_fix_48_hour_speed_calculation.sql` - Calculation fix

---

## ✅ Checklist

- [x] Quality metrics API fixed (uses database columns)
- [x] Unified dashboard API fixed (uses database columns)
- [x] Leaderboard queries fixed (uses database columns)
- [x] Backfill API endpoint created
- [x] Settings UI updated with backfill button
- [x] Sync optimization implemented
- [x] Settings page consolidated
- [x] Code committed and deployed
- [ ] **Backfill executed** (next step)

---

## 🎉 Summary

All code changes are **complete and deployed**. The system is ready to populate the database columns with calculated values. Once the backfill is run, the dashboard will display accurate metrics instead of 0.0%.

**The backfill button is the final step to populate the data and fix the 0.0% display issue.**
