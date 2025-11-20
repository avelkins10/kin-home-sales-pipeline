# RepCard Integration Fix Guide

## Problem Diagnosis

The RepCard integration is built and ready, but **no data is displaying** because the RepCard API credentials are not configured. Without these credentials:

- ❌ The RepCard API client can't authenticate
- ❌ No data syncs from RepCard
- ❌ Database tables are empty
- ❌ Dashboard shows no metrics

## Root Cause

Missing environment variables:
```bash
REPCARD_API_KEY=<your-api-key>
REPCARD_API_URL=https://api.repcard.com  # Optional (has default)
```

---

## Fix Steps

### Step 1: Get RepCard API Credentials

1. **Log in to RepCard Dashboard**: https://www.repcard.com/login
2. **Navigate to Settings** → **API** or **Integrations**
3. **Generate or Copy API Key**
4. **Save the API key** (you'll need it for the next step)

**Note**: You'll need admin/owner access to RepCard to generate API keys. Contact your RepCard account owner if you don't have access.

---

### Step 2: Add Credentials to Environment

#### Local Development (.env.local)

Create or update `.env.local`:

```bash
# RepCard API Configuration
REPCARD_API_KEY=<paste-your-api-key-here>
REPCARD_API_URL=https://api.repcard.com
```

#### Production (Vercel)

Add environment variables to Vercel:

```bash
# Option 1: Via Vercel CLI
vercel env add REPCARD_API_KEY production
# Paste your API key when prompted

# Option 2: Via Vercel Dashboard
# 1. Go to: https://vercel.com/<your-project>/settings/environment-variables
# 2. Add: REPCARD_API_KEY = <your-api-key>
# 3. Select: Production
# 4. Save
```

**After adding environment variables in Vercel**:
```bash
# Redeploy to apply new env vars
vercel --prod
```

---

### Step 3: Verify Database Tables Exist

Check if RepCard tables were created:

```bash
# Connect to your database
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

If tables don't exist, run migrations:

```bash
# Run RepCard migrations
npm run migrate

# Or manually:
psql "$DATABASE_URL" -f lib/db/migrations/014_repcard_comprehensive_tables.sql
psql "$DATABASE_URL" -f lib/db/migrations/016_repcard_complete_data.sql
psql "$DATABASE_URL" -f lib/db/migrations/017_repcard_settings.sql
```

---

### Step 4: Run Initial Data Sync

After configuring credentials, sync RepCard data:

#### Option A: Via Admin Dashboard (Recommended)

1. **Navigate to**: http://localhost:3000/admin/repcard-sync (or your production URL)
2. **Click**: "Run Full Sync"
3. **Wait**: 2-5 minutes for initial sync to complete
4. **Verify**: Check that records appear in the sync results

#### Option B: Via Script

```bash
# Full sync of all data
npx tsx scripts/run-comprehensive-repcard-sync.ts

# Incremental sync (faster, only new/updated records)
npx tsx scripts/run-comprehensive-repcard-sync.ts --incremental
```

#### Option C: Via API

```bash
# Trigger sync via API (requires auth token)
curl -X POST https://your-domain.com/api/admin/repcard/sync \
  -H "Authorization: Bearer YOUR_AUTH_TOKEN" \
  -H "Content-Type: application/json"
```

---

### Step 5: Verify Data Synced

Check that data was synced successfully:

```bash
# Check record counts
psql "$DATABASE_URL" -c "
SELECT 
  (SELECT COUNT(*) FROM repcard_users) as users,
  (SELECT COUNT(*) FROM repcard_customers) as customers,
  (SELECT COUNT(*) FROM repcard_appointments) as appointments,
  (SELECT COUNT(*) FROM repcard_status_logs) as status_logs;
"

# Expected output (example):
# users | customers | appointments | status_logs
# ------+-----------+--------------+-------------
#   45  |    1250   |     892      |    3421
```

If counts are 0, check sync logs for errors.

---

### Step 6: Link Users to RepCard

Users need their `repcard_user_id` field populated to see RepCard data:

```bash
# Check which users have RepCard IDs
psql "$DATABASE_URL" -c "
SELECT 
  id, 
  name, 
  email, 
  repcard_user_id,
  role
FROM users
WHERE repcard_user_id IS NOT NULL
LIMIT 10;
"
```

If users don't have `repcard_user_id`, you can:

1. **Auto-link by email** (recommended):
```bash
npx tsx scripts/link-users-to-repcard.ts  # Run this script if it exists
```

2. **Manually update** (for testing):
```sql
-- Link a specific user by email match
UPDATE users u
SET repcard_user_id = ru.repcard_user_id
FROM repcard_users ru
WHERE LOWER(u.email) = LOWER(ru.email)
  AND u.repcard_user_id IS NULL;
```

---

### Step 7: Test Dashboard Display

1. **Navigate to Dashboard**: http://localhost:3000 (or your production URL)
2. **Check RepCard Metrics Card**: Should show:
   - Doors Knocked
   - Appointments Set
   - Conversion Rate
   - Attachments
   - Quality Metrics
3. **Check Analytics Page**: http://localhost:3000/analytics
   - RepCard Leaderboards should populate
   - Quality metrics should display

If data still doesn't show:
- Check browser console for errors
- Check server logs for API errors
- Verify user has `repcard_user_id` populated
- Verify sync completed successfully

---

## Troubleshooting

### Issue: "RepCard API Key not configured"

**Cause**: REPCARD_API_KEY environment variable is missing or not loaded.

**Fix**:
1. Verify `.env.local` has `REPCARD_API_KEY=<your-key>`
2. Restart Next.js dev server: `npm run dev`
3. For production, redeploy after adding env var to Vercel

---

### Issue: "No RepCard data for user"

**Cause**: User's `repcard_user_id` is NULL.

**Fix**:
1. Run user linking script (see Step 6)
2. Or manually set `repcard_user_id` in database
3. Verify email matches between systems

---

### Issue: Sync runs but no data appears

**Causes**:
1. API key is invalid
2. RepCard account has no data
3. Date range filters are excluding data

**Debug**:
```bash
# Test API connection
npx tsx scripts/test-repcard-connection.ts

# Check RepCard API directly
curl -H "x-api-key: YOUR_KEY" https://api.repcard.com/api/users/minimal

# View sync logs
psql "$DATABASE_URL" -c "SELECT * FROM sync_runs ORDER BY started_at DESC LIMIT 5;"
```

---

### Issue: Rate limit errors (429)

**Cause**: RepCard API has rate limits (typically 100 requests/period).

**Fix**:
1. Use incremental sync instead of full sync
2. Sync less frequently (5-10 minute intervals recommended)
3. Increase delays between API calls (configured in client)

---

## Automated Sync Schedule

Once initial sync completes, RepCard data will auto-sync:

**Frequency**: Every 5 minutes (configurable in `vercel.json`)

**Endpoint**: `/api/cron/repcard-sync`

**What syncs**:
- Users (if changed)
- New customers (door knocks)
- New/updated appointments
- Status changes
- Attachments (hourly, not every 5 min)

**Vercel Cron Configuration** (`vercel.json`):
```json
{
  "crons": [
    {
      "path": "/api/cron/repcard-sync",
      "schedule": "*/5 * * * *"
    }
  ]
}
```

**Environment variable required**:
```bash
CRON_SECRET=<random-secret>  # Protects cron endpoint
```

---

## Quick Diagnostic Script

Save this as `scripts/diagnose-repcard-issue.ts`:

```typescript
#!/usr/bin/env tsx
import { sql } from '@/lib/db/client';
import { repcardClient } from '@/lib/repcard/client';

async function diagnose() {
  console.log('🔍 RepCard Integration Diagnostic\n');
  
  // 1. Check environment
  console.log('1. Environment Variables:');
  console.log(`   REPCARD_API_KEY: ${process.env.REPCARD_API_KEY ? '✅ Set' : '❌ Missing'}`);
  console.log(`   REPCARD_API_URL: ${process.env.REPCARD_API_URL || 'Using default'}`);
  console.log(`   DATABASE_URL: ${process.env.DATABASE_URL ? '✅ Set' : '❌ Missing'}\n`);
  
  // 2. Test API connection
  console.log('2. Testing RepCard API Connection:');
  try {
    const users = await repcardClient.getUsersMinimal({ page: 1, perPage: 5 });
    console.log(`   ✅ Connected! Found ${users.result.pagination.total} users\n`);
  } catch (error) {
    console.log(`   ❌ Failed: ${error}\n`);
  }
  
  // 3. Check database tables
  console.log('3. Checking Database Tables:');
  try {
    const tables = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_name LIKE 'repcard_%'
      ORDER BY table_name;
    `;
    console.log(`   ✅ Found ${tables.length} RepCard tables`);
    tables.forEach((t: any) => console.log(`      - ${t.table_name}`));
    console.log();
  } catch (error) {
    console.log(`   ❌ Failed: ${error}\n`);
  }
  
  // 4. Check data counts
  console.log('4. Checking Data Counts:');
  try {
    const counts = await sql`
      SELECT 
        (SELECT COUNT(*) FROM repcard_users) as users,
        (SELECT COUNT(*) FROM repcard_customers) as customers,
        (SELECT COUNT(*) FROM repcard_appointments) as appointments,
        (SELECT COUNT(*) FROM repcard_status_logs) as status_logs;
    `;
    const c = counts[0];
    console.log(`   Users: ${c.users}`);
    console.log(`   Customers: ${c.customers}`);
    console.log(`   Appointments: ${c.appointments}`);
    console.log(`   Status Logs: ${c.status_logs}\n`);
  } catch (error) {
    console.log(`   ❌ Failed: ${error}\n`);
  }
  
  // 5. Check user linking
  console.log('5. Checking User Linking:');
  try {
    const linked = await sql`
      SELECT COUNT(*) as count
      FROM users
      WHERE repcard_user_id IS NOT NULL;
    `;
    const total = await sql`SELECT COUNT(*) as count FROM users;`;
    console.log(`   Linked: ${linked[0].count} / ${total[0].count} users\n`);
  } catch (error) {
    console.log(`   ❌ Failed: ${error}\n`);
  }
  
  console.log('✅ Diagnostic complete!\n');
  
  // Summary
  if (!process.env.REPCARD_API_KEY) {
    console.log('⚠️  ACTION REQUIRED: Set REPCARD_API_KEY environment variable');
  }
}

diagnose().catch(console.error).finally(() => process.exit(0));
```

Run it:
```bash
npx tsx scripts/diagnose-repcard-issue.ts
```

---

## Summary Checklist

- [ ] Get RepCard API key from RepCard dashboard
- [ ] Add `REPCARD_API_KEY` to `.env.local` (local) and Vercel (production)
- [ ] Verify database tables exist (run migrations if needed)
- [ ] Run initial data sync
- [ ] Link users to RepCard by email
- [ ] Test dashboard displays RepCard metrics
- [ ] Verify automated sync is running (check cron logs)

Once complete, RepCard metrics will display throughout the dashboard! 🎉

---

## Need Help?

**Documentation**:
- RepCard API Docs: https://www.repcard.com/api-docs
- Implementation Summary: `REPCARD_FINAL_IMPLEMENTATION_STATUS.md`
- Sync Guide: `REPCARD_COMPREHENSIVE_SYNC.md`

**Common Files**:
- API Client: `lib/repcard/client.ts`
- Sync Service: `lib/repcard/comprehensive-sync.ts`
- Cron Job: `app/api/cron/repcard-sync/route.ts`
- Dashboard Component: `components/dashboard/RepCardMetricsCard.tsx`




