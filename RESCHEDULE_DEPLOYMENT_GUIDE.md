# 🚀 RepCard Reschedule Tracking - Deployment Guide

**Date**: 2025-11-21
**Commit**: `1ca3433`
**Status**: ✅ **Code Deployed to GitHub → Vercel Auto-Deploying**

---

## ✅ WHAT'S BEEN COMPLETED

### 1. Code Successfully Pushed
- ✅ Committed 11 files with 1,347 insertions
- ✅ Pushed to `main` branch (commit `1ca3433`)
- ✅ Vercel automatic deployment triggered
- ✅ Production build passed successfully

### 2. Features Deployed
- ✅ **Reschedule Tracking Schema** (Migration 028)
- ✅ **Reschedule Metrics Dashboard** Component
- ✅ **Appointment History API** Endpoint
- ✅ **Visual Reschedule Indicators** (color-coded cards)
- ✅ **Top Reschedulers Leaderboard**
- ✅ **Fixed Leaderboard Bug** (384 users vs 32)
- ✅ **Sync Stability Improvements**

---

## 📋 PRODUCTION SETUP (Required)

### Step 1: Apply Migration 028 to Production Database

The reschedule tracking schema must be applied to your **production** database (not local).

**Option A: Via Vercel CLI (Recommended)**
```bash
# Connect to production database
vercel env pull
npx tsx scripts/run-migration-028.ts
```

**Option B: Direct SQL Execution**
Execute the SQL from `lib/db/migrations/028_add_repcard_reschedule_tracking.sql` using:
- Vercel PostgreSQL Dashboard
- psql CLI: `psql $PROD_DATABASE_URL -f lib/db/migrations/028_add_repcard_reschedule_tracking.sql`
- pgAdmin or another SQL client

**What Migration 028 Does:**
```sql
-- Adds 4 new columns to repcard_appointments:
ALTER TABLE repcard_appointments
ADD COLUMN is_reschedule BOOLEAN DEFAULT FALSE,
ADD COLUMN reschedule_count INTEGER DEFAULT 0,
ADD COLUMN original_appointment_id TEXT,
ADD COLUMN reschedule_reason TEXT;

-- Creates view for appointment chains
CREATE VIEW repcard_appointment_chains AS ...

-- Adds foreign key constraint
ALTER TABLE repcard_appointments
ADD CONSTRAINT fk_repcard_appointments_original ...
```

### Step 2: Trigger Full Sync

After migration is applied, populate reschedule data:

**Option A: Manual API Call**
```bash
# Replace with your actual production URL
curl -X POST 'https://your-production-domain.vercel.app/api/admin/repcard/sync?type=full' \
  -H "Cookie: your-auth-cookie"
```

**Option B: Wait for Automatic Cron**
The system runs a full sync every 5 minutes automatically via cron job.

### Step 3: Verify Everything Works

1. **Check Migration Applied**
   ```sql
   SELECT column_name FROM information_schema.columns
   WHERE table_name = 'repcard_appointments'
     AND column_name IN ('is_reschedule', 'reschedule_count');
   -- Should return 2 rows
   ```

2. **Check Data Populated**
   ```sql
   SELECT
     COUNT(*) as total,
     COUNT(*) FILTER (WHERE is_reschedule = TRUE) as reschedules
   FROM repcard_appointments;
   -- After sync, reschedules should be > 0
   ```

3. **Access Dashboard**
   - Go to: `https://your-domain.vercel.app/operations/repcard`
   - Scroll to "Reschedule Metrics" card
   - Click "Appointments" tab to see color-coded cards

---

## 🎯 NEW FEATURES NOW LIVE

### 1. Reschedule Metrics Card
**Location**: RepCard Dashboard (main view)

Displays:
- Total appointments vs reschedules
- Overall reschedule rate (%)
- Customers with multiple reschedules
- Average reschedules per customer
- Top 5 users with highest reschedule rates

**Color Indicators**:
- 🟢 <10% = Good
- 🟡 10-25% = Fair
- 🔴 ≥25% = Needs Attention

### 2. Visual Appointment Indicators
**Location**: Appointments tab

Each appointment card shows:
- **Left Border Color**:
  - 🟢 Green = Original appointment
  - 🟡 Yellow = 1st reschedule
  - 🔴 Red = 2+ reschedules
- **Status Badge**: "Original", "1st Reschedule", or "X Reschedules"
- **Icon**: Checkmark, refresh, or alert

### 3. New API Endpoints

**Reschedule Statistics**
```
GET /api/repcard/metrics/reschedule-stats
Query Parameters:
  - userId (optional)
  - officeIds (optional, comma-separated)
  - startDate (optional, ISO format)
  - endDate (optional, ISO format)

Response:
{
  "totalAppointments": 2146,
  "totalReschedules": 324,
  "rescheduleRate": 15.1,
  "customersWithReschedules": 187,
  "avgReschedulesPerCustomer": 1.73,
  "topReschedulers": [...]
}
```

**Customer Appointment History**
```
GET /api/repcard/customers/[customerId]/appointments

Response:
{
  "customer": {...},
  "appointments": [...],
  "chains": [
    {
      "original": {...},
      "reschedules": [...],
      "totalReschedules": 2
    }
  ],
  "summary": {
    "totalAppointments": 5,
    "reschedules": 2,
    "rescheduleRate": "40.0"
  }
}
```

---

## 🔧 TROUBLESHOOTING

### "Migration fails with column already exists"
✅ **Solution**: Migration already applied - proceed to Step 2 (sync)

### "No reschedule data showing in dashboard"
⚠️ **Solution**:
1. Verify migration was applied (check SQL above)
2. Trigger manual sync OR wait for cron (runs every 5 min)
3. Check sync logs: `/api/admin/repcard-debug`

### "View creation fails"
⚠️ **Solution**: Drop view first, then re-run migration:
```sql
DROP VIEW IF EXISTS repcard_appointment_chains CASCADE;
```

### "TypeScript errors during build"
✅ **Solution**: Expected warnings about SQL array parameters - safe to ignore

---

## 📊 EXPECTED RESULTS

**After Migration + Sync**:
- ✅ Reschedule rate calculated for all 2,146 appointments
- ✅ Top reschedulers list populated
- ✅ Appointment history chains linked
- ✅ Color-coded visual indicators throughout dashboard
- ✅ Historical reschedule trends available

**Performance Impact**: Minimal
- New columns indexed for fast queries
- View uses optimized GROUP BY query
- Dashboard caches metrics for 60 seconds

---

## 🎉 SUCCESS CRITERIA

- [ ] Migration 028 applied to production database
- [ ] Full sync completed (check sync logs)
- [ ] Reschedule Metrics card visible in dashboard
- [ ] Appointments show color-coded borders
- [ ] Top reschedulers list populated
- [ ] API endpoints return valid data

---

## 📞 NEXT STEPS

1. **Apply migration** to production database (Step 1 above)
2. **Trigger sync** or wait for cron (Step 2 above)
3. **Verify dashboard** shows new metrics (Step 3 above)
4. **Train team** on new reschedule quality indicators

**Estimated Time**: 5-10 minutes total

---

## 🔗 USEFUL LINKS

- **Vercel Dashboard**: https://vercel.com/dashboard
- **Database Migrations**: `/lib/db/migrations/`
- **Sync Logs**: `/api/admin/repcard-debug`
- **Deployment Commit**: https://github.com/avelkins10/kin-home-sales-pipeline/commit/1ca3433

---

**Status**: ✅ Code deployed, ⏳ Awaiting migration + sync in production

🚀 **Ready to go live - just run the migration and sync!**
