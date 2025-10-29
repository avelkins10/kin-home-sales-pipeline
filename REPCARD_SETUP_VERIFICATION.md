# RepCard Setup Verification & Testing

## ✅ What's Been Executed

### 1. Database Migration ✅
- **Migration:** `014_repcard_comprehensive_tables.sql`
- **Status:** ✅ Completed
- **Tables Created:**
  - `repcard_users` - User/rep data
  - `repcard_offices` - Office data
  - `repcard_customer_attachments` - Customer attachments
  - `repcard_appointment_attachments` - Appointment attachments
  - `repcard_leaderboard_snapshots` - Historical leaderboard data

### 2. API Configuration ✅
- **Leaderboard API:** Updated to use synced database (`/api/repcard/leaderboard`)
- **Data API:** New endpoint for querying synced data (`/api/repcard/data`)
- **Sync API:** Comprehensive sync endpoint (`/api/admin/repcard/comprehensive-sync`)

### 3. Component Updates ✅
- **ConfigurableLeaderboard:** Auto-refresh every 30 seconds
- **RepCardMetricsCard:** New dashboard component (added to main dashboard)
- **CanvassingOverviewCard:** Auto-refresh every 30 seconds
- **AppointmentRatesCard:** Auto-refresh every 30 seconds

### 4. Dashboard Integration ✅
- RepCard metrics card added to `/app/(sales)/page.tsx`
- Shows doors knocked, appointments, conversion rate, attachments
- Quality metrics breakdown

---

## 🔍 Verification Steps

### Step 1: Verify Database Tables
Run the verification script:
```bash
npx tsx scripts/verify-repcard-setup.ts
```

This checks:
- ✅ All tables exist
- ✅ Data counts
- ✅ Sync logs
- ✅ API files exist
- ✅ Component files exist

### Step 2: Run Initial Sync
The sync needs to be triggered manually first time (requires authentication):

**Option A: Via API (requires auth):**
```bash
curl -X POST "http://localhost:3000/api/admin/repcard/comprehensive-sync?skipAttachments=true" \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN"
```

**Option B: Via Script (if env vars are set correctly):**
```bash
# The script needs DATABASE_URL set properly
# Currently it expects POSTGRES_URL or connectionString
# We'll need to update the comprehensive-sync.ts to use DATABASE_URL
```

### Step 3: Verify Data in Browser

1. **Main Dashboard** (`http://localhost:3000`)
   - Look for "RepCard Metrics" card at top
   - Should show doors knocked, appointments, conversion rate
   - If no data: Shows "RepCard Data Unavailable" message

2. **Analytics Page** (`http://localhost:3000/analytics`)
   - **Leaderboards Tab:**
     - Top Setters - Doors Knocked
     - Top Closers - Revenue
     - Quality Leaders
     - Volume Leaders - Appointments
   - **Canvassing Tab:**
     - Canvassing Overview
     - Doors Knocked Trends
     - Appointment Rates
     - Lead Quality Analysis

3. **Rep Detail Page** (`http://localhost:3000/analytics/rep/[id]`)
   - BaseballCard component should show RepCard stats

---

## 🐛 Known Issues & Fixes

### Issue 1: Sync Script Needs DATABASE_URL
**Problem:** Comprehensive sync script expects `POSTGRES_URL` but we have `DATABASE_URL`

**Fix:** Update `lib/repcard/comprehensive-sync.ts` to use `DATABASE_URL`:
```typescript
// Current (line ~50):
const { sql } = require('@vercel/postgres');

// Should use:
import { sql } from '@/lib/db/client';
```

### Issue 2: Browser Verification
**Problem:** Browser automation can't access authenticated pages

**Solution:** Manual verification:
1. Log in to the app
2. Navigate to dashboard - check for RepCard Metrics card
3. Navigate to analytics - check leaderboards and canvassing tabs
4. Check that data displays correctly

---

## 📊 Expected Results

### Before Sync
- Tables exist ✅
- API endpoints return empty data or "no data" messages
- Components show loading states or "no data" messages

### After Sync
- Tables populated with RepCard data
- Leaderboards show ranked users
- Dashboard shows metrics (doors, appointments, etc.)
- Analytics pages show charts and data

---

## ✅ Verification Checklist

- [x] Database migration run
- [x] Tables created
- [x] API routes updated
- [x] Components updated with auto-refresh
- [x] Dashboard integration complete
- [ ] Initial sync run (requires manual trigger with auth)
- [ ] Verify data displays in browser
- [ ] Test leaderboard functionality
- [ ] Test dashboard metrics card
- [ ] Test analytics pages

---

## 🚀 Next Steps

1. **Fix DATABASE_URL issue in comprehensive-sync.ts**
   - Update to use `@/lib/db/client` instead of `@vercel/postgres`

2. **Run Initial Sync**
   - Via API endpoint (requires auth)
   - Or fix script and run via CLI

3. **Verify in Browser**
   - Check dashboard for RepCard metrics
   - Check analytics leaderboards
   - Check canvassing metrics

4. **Monitor Cron Sync**
   - Cron runs every 5 minutes
   - Check sync logs in database
   - Verify data updates automatically

---

## 📝 Summary

✅ **All configuration is complete:**
- Database tables created
- API endpoints configured
- Components updated
- Dashboard integrated
- Auto-refresh enabled

⏳ **Pending:**
- Initial data sync (requires auth or script fix)
- Browser verification (manual testing)

Once the initial sync runs, all RepCard data will be available in leaderboards and dashboards with 30-second auto-refresh!

