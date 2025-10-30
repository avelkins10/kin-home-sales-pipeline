# Deployment Instructions - RepCard Complete Integration

## ✅ Code Deployed

All code changes have been committed and pushed to `main` branch. Vercel will automatically deploy.

## 📋 Post-Deployment Steps

### Step 1: Run Database Migrations

**Option A: Using the deployment script**
```bash
./scripts/deploy-repcard-integration.sh
```

**Option B: Manual migration**
```bash
# Connect to production database
psql $DATABASE_URL

# Run migrations
\i lib/db/migrations/016_repcard_complete_data.sql
\i lib/db/migrations/017_repcard_settings.sql
```

**Option C: Using Vercel CLI**
```bash
vercel env pull .env.production
psql $DATABASE_URL -f lib/db/migrations/016_repcard_complete_data.sql
psql $DATABASE_URL -f lib/db/migrations/017_repcard_settings.sql
```

### Step 2: Run Initial Sync

After migrations are complete, trigger a comprehensive sync:

```bash
# Using curl
curl -X POST https://your-domain.com/api/admin/repcard/comprehensive-sync \
  -H "Cookie: your-session-cookie"

# Or skip rarely-changing data for faster first sync
curl -X POST "https://your-domain.com/api/admin/repcard/comprehensive-sync?skipCustomerStatuses=true&skipCalendars=true&skipCustomFields=true" \
  -H "Cookie: your-session-cookie"
```

### Step 3: Configure Leaderboards

1. Navigate to **Settings → RepCard Config** (super admin only)
2. Go to **"Leaderboards"** tab
3. Create default leaderboard:
   - Name: "Default D2D Leaderboard"
   - Type: "d2d"
   - Rank By: "doors_knocked"
   - Enabled Metrics: Select desired metrics
   - Set as default: ✅

### Step 4: Verify Deployment

1. ✅ Check Vercel deployment succeeded
2. ✅ Verify migrations ran successfully
3. ✅ Test sync endpoint
4. ✅ Test settings UI
5. ✅ Test leaderboard with configId

## 🔍 Verification Checklist

- [ ] Migrations completed without errors
- [ ] New tables exist: `repcard_customer_notes`, `repcard_customer_statuses`, `repcard_calendars`, `repcard_custom_fields`, `repcard_leaderboard_snapshots`, `repcard_teams`
- [ ] Settings tables exist: `repcard_leaderboard_config`, `repcard_analytics_config`, `repcard_metric_definitions`
- [ ] Metric definitions are pre-populated (check count: `SELECT COUNT(*) FROM repcard_metric_definitions`)
- [ ] Comprehensive sync runs successfully
- [ ] Settings UI is accessible at `/settings` → RepCard Config tab
- [ ] Leaderboard API works with and without configId

## 🚨 Troubleshooting

### Migration Errors
If migrations fail, check:
- Database connection string is correct
- Tables don't already exist (safe to re-run migrations)
- User has CREATE TABLE permissions

### Sync Errors
- Check REPCARD_API_KEY is set in environment variables
- Verify API key has correct permissions
- Check rate limits (should be fine with incremental sync)

### Settings UI Not Showing
- Verify user role is `super_admin`
- Check browser console for errors
- Verify API endpoints are accessible

## 📊 Next Steps After Deployment

1. **Monitor sync**: Check sync logs in `repcard_sync_log` table
2. **Create configurations**: Set up default leaderboards and analytics widgets
3. **Train users**: Show team how to use new settings
4. **Monitor performance**: Check API response times

## 🎉 Success!

Your RepCard integration is now live! All 13 data types are syncing, and you have a fully configurable leaderboard and analytics system.

