# RepCard Production Fix Guide

## Problem
RepCard metrics are not displaying in production after deployment.

---

## Quick Fix Checklist

### ✅ Step 1: Add RepCard API Key to Vercel Production

**CRITICAL**: The RepCard API key must be set in Vercel production environment variables.

#### Via Vercel Dashboard (Recommended)

1. **Go to**: https://vercel.com/[your-project]/settings/environment-variables
2. **Add Environment Variable**:
   - **Key**: `REPCARD_API_KEY`
   - **Value**: Your RepCard API key (get from https://www.repcard.com/settings/api)
   - **Environment**: Select **Production** (and Preview/Development if needed)
   - **Click**: Save
3. **Redeploy**: After adding the env var, trigger a new deployment:
   ```bash
   # Option 1: Via Dashboard
   # Go to Deployments → Click "Redeploy" on latest deployment
   
   # Option 2: Via CLI
   vercel --prod
   ```

#### Via Vercel CLI

```bash
# Add to production
vercel env add REPCARD_API_KEY production
# Paste your API key when prompted

# Verify it was added
vercel env ls

# Redeploy
vercel --prod
```

**⚠️ IMPORTANT**: After adding environment variables, you MUST redeploy for them to take effect!

---

### ✅ Step 2: Verify Database Tables Exist in Production

RepCard tables must exist in your production database.

#### Check via Admin Dashboard

1. **Navigate to**: `https://your-domain.com/admin/repcard-sync`
2. **Look for errors** about missing tables
3. **If tables are missing**, run migrations (see Step 3)

#### Check via Database Connection

```bash
# Connect to production database
psql "$DATABASE_URL"

# List RepCard tables
\dt repcard_*

# Expected tables:
# - repcard_users
# - repcard_offices
# - repcard_customers
# - repcard_appointments
# - repcard_status_logs
# - repcard_customer_attachments
# - repcard_appointment_attachments
# - repcard_customer_notes
# - repcard_customer_statuses
# - repcard_calendars
# - repcard_custom_fields
# - repcard_leaderboard_snapshots
# - repcard_teams
# - repcard_metric_definitions
# - repcard_leaderboard_config
# - repcard_analytics_config
```

---

### ✅ Step 3: Run Database Migrations (If Needed)

If tables don't exist, run migrations on production database:

#### Option A: Via Script (Recommended)

```bash
# Pull production env vars
vercel env pull .env.production --environment=production

# Run migrations
./scripts/run-migrations-production.sh
```

#### Option B: Manual SQL

```bash
# Connect to production database
psql "$DATABASE_URL"

# Run migrations
\i lib/db/migrations/014_repcard_comprehensive_tables.sql
\i lib/db/migrations/016_repcard_complete_data.sql
\i lib/db/migrations/017_repcard_settings.sql
```

**⚠️ WARNING**: Only run migrations on production if tables are actually missing!

---

### ✅ Step 4: Run Initial Data Sync in Production

After API key is set and tables exist, sync RepCard data:

#### Option A: Via Admin Dashboard (Recommended)

1. **Navigate to**: `https://your-domain.com/admin/repcard-sync`
2. **Click**: "Run Full Sync" button
3. **Wait**: 2-5 minutes for sync to complete
4. **Verify**: Check that records appear in sync results

#### Option B: Via API Endpoint

```bash
# Get your auth token first (from browser dev tools or session)
curl -X POST https://your-domain.com/api/admin/repcard/sync \
  -H "Authorization: Bearer YOUR_AUTH_TOKEN" \
  -H "Content-Type: application/json"
```

#### Option C: Wait for Automatic Sync

The cron job runs every 5 minutes automatically:
- **Endpoint**: `/api/cron/repcard-sync`
- **Schedule**: Every 5 minutes (configured in `vercel.json`)
- **Requires**: `CRON_SECRET` environment variable set in Vercel

**Note**: Automatic sync will only work if:
- `REPCARD_API_KEY` is set ✅
- `CRON_SECRET` is set ✅
- Cron job is enabled in Vercel ✅

---

### ✅ Step 5: Link Users to RepCard

Users need their `repcard_user_id` populated to see RepCard data.

#### Check User Linking Status

```sql
-- Connect to production database
psql "$DATABASE_URL"

-- Check how many users are linked
SELECT 
  COUNT(*) FILTER (WHERE repcard_user_id IS NOT NULL) as linked,
  COUNT(*) as total
FROM users;

-- Show users without RepCard IDs
SELECT id, name, email, role
FROM users
WHERE repcard_user_id IS NULL
LIMIT 10;
```

#### Auto-Link Users by Email

```sql
-- Link users by matching email addresses
UPDATE users u
SET repcard_user_id = ru.repcard_user_id::text
FROM repcard_users ru
WHERE LOWER(u.email) = LOWER(ru.email)
  AND u.repcard_user_id IS NULL;

-- Verify linking worked
SELECT COUNT(*) as newly_linked
FROM users
WHERE repcard_user_id IS NOT NULL;
```

---

### ✅ Step 6: Verify Production API Endpoints

Test that RepCard API endpoints are working in production:

#### Test User Stats Endpoint

```bash
# Replace YOUR_USER_ID with an actual user ID
curl https://your-domain.com/api/repcard/users/YOUR_USER_ID/stats?timeRange=month \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN"
```

**Expected Response**:
```json
{
  "user": { ... },
  "volumeStats": {
    "doorsKnocked": 123,
    "appointmentsSet": 45,
    ...
  },
  ...
}
```

**If you get `hasRepcardData: false`**:
- User's `repcard_user_id` is NULL → Link user (Step 5)
- No data synced → Run sync (Step 4)

#### Test Leaderboard Endpoint

```bash
curl "https://your-domain.com/api/repcard/leaderboard?metric=doors_knocked&timeRange=month&role=all" \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN"
```

**Expected Response**:
```json
{
  "leaderboard": [
    { "rank": 1, "userName": "...", "metricValue": 123, ... },
    ...
  ],
  ...
}
```

---

### ✅ Step 7: Check Production Logs

If metrics still don't show, check Vercel logs:

#### Via Vercel Dashboard

1. **Go to**: https://vercel.com/[your-project]/deployments
2. **Click**: Latest deployment
3. **Click**: "Functions" tab
4. **Check logs** for errors:
   - `RepCard API key is required` → API key not set
   - `Table "repcard_*" does not exist` → Run migrations
   - `No data synced` → Run sync
   - `User not linked to RepCard` → Link users

#### Via Vercel CLI

```bash
# View function logs
vercel logs --follow

# Filter for RepCard errors
vercel logs --follow | grep -i repcard
```

---

## Common Production Issues

### Issue: "RepCard API key is required" Error

**Cause**: `REPCARD_API_KEY` not set in Vercel production environment.

**Fix**:
1. Add `REPCARD_API_KEY` to Vercel production env vars (Step 1)
2. **Redeploy** after adding env var
3. Verify in logs that error is gone

---

### Issue: "Table does not exist" Error

**Cause**: RepCard database tables not created in production.

**Fix**:
1. Run migrations on production database (Step 3)
2. Verify tables exist: `\dt repcard_*`
3. Retry sync

---

### Issue: Metrics Show "0" or Empty

**Cause**: No data synced from RepCard yet.

**Fix**:
1. Verify `REPCARD_API_KEY` is correct and valid
2. Run manual sync via admin dashboard (Step 4)
3. Check sync logs for errors
4. Verify RepCard account has data

---

### Issue: "User not linked to RepCard"

**Cause**: User's `repcard_user_id` is NULL.

**Fix**:
1. Link users by email (Step 5)
2. Verify user exists in RepCard
3. Check email matches between systems

---

### Issue: Cron Job Not Running

**Cause**: `CRON_SECRET` not set or cron job disabled.

**Fix**:
1. Add `CRON_SECRET` to Vercel production env vars:
   ```bash
   vercel env add CRON_SECRET production
   # Generate secret: openssl rand -base64 32
   ```
2. Verify cron is enabled in `vercel.json`
3. Check Vercel cron logs: https://vercel.com/[project]/settings/cron

---

## Production Verification Checklist

After completing all steps, verify:

- [ ] `REPCARD_API_KEY` is set in Vercel production environment
- [ ] Production deployment completed successfully (after adding env var)
- [ ] RepCard database tables exist in production
- [ ] Initial data sync completed successfully
- [ ] Users are linked to RepCard (`repcard_user_id` populated)
- [ ] API endpoints return data (test `/api/repcard/users/[id]/stats`)
- [ ] Dashboard displays RepCard metrics
- [ ] Cron job is running (check Vercel cron logs)

---

## Quick Production Diagnostic

Run this to check production status:

```bash
# 1. Check if API key is set (via Vercel CLI)
vercel env ls | grep REPCARD_API_KEY

# 2. Test API endpoint (replace with your domain and user ID)
curl https://your-domain.com/api/repcard/users/USER_ID/stats?timeRange=month

# 3. Check database (if you have access)
psql "$DATABASE_URL" -c "SELECT COUNT(*) FROM repcard_customers;"
```

---

## Need Help?

**Check Logs**:
- Vercel Dashboard → Deployments → Functions → Logs
- Filter for: `repcard`, `RepCard`, `REPCARD`

**Common Files**:
- API Client: `lib/repcard/client.ts`
- Sync Service: `lib/repcard/comprehensive-sync.ts`
- Cron Job: `app/api/cron/repcard-sync/route.ts`
- Stats Endpoint: `app/api/repcard/users/[userId]/stats/route.ts`

**Documentation**:
- Implementation: `REPCARD_FINAL_IMPLEMENTATION_STATUS.md`
- Sync Guide: `REPCARD_COMPREHENSIVE_SYNC.md`
- Fix Guide: `REPCARD_FIX_GUIDE.md`

---

## Summary

**Most Common Issue**: `REPCARD_API_KEY` not set in Vercel production environment.

**Quick Fix**:
1. Add `REPCARD_API_KEY` to Vercel production env vars
2. Redeploy
3. Run sync via admin dashboard
4. Link users by email

After these steps, RepCard metrics should display in production! 🎉




