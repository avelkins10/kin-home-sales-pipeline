# 🚀 Deployment Instructions - RepCard Enhancements

## ✅ Code Status
- **All code committed**: ✅ Yes
- **All code pushed**: ⚠️ Needs manual push (authentication required)
- **Migrations ready**: ✅ Yes
- **API endpoints created**: ✅ Yes

## 📋 Steps to Deploy

### Step 1: Push Code to GitHub
```bash
git push origin main
```

If authentication is required, you may need to:
- Use SSH: `git remote set-url origin git@github.com:your-org/Rep_Dashboard.git`
- Or authenticate with GitHub CLI: `gh auth login`
- Or use a personal access token

### Step 2: Wait for Auto-Deployment
Once pushed, GitHub Actions will:
1. Run tests
2. Build the project
3. Deploy to Vercel production

Check deployment status at: https://github.com/your-org/Rep_Dashboard/actions

### Step 3: Run Migrations in Production

**Option A: Via Admin API (Recommended)**
1. Log in as super_admin
2. Go to: `https://your-production-url.com/api/admin/run-migrations-034-035`
3. Make a POST request (or use the admin dashboard if available)

**Option B: Via Main Migrations Endpoint**
1. POST to: `https://your-production-url.com/api/admin/run-migrations`
2. This will run all pending migrations including 034 and 035

**Option C: Via Script (if you have production DB access)**
```bash
export DATABASE_URL="your-production-database-url"
npx tsx scripts/run-migrations-direct.ts
```

### Step 4: Verify Deployment

1. **Check Code Deployment**:
   - Visit Vercel dashboard
   - Verify latest deployment succeeded
   - Check build logs

2. **Verify Migrations**:
   ```bash
   # Via API
   GET https://your-production-url.com/api/admin/run-migrations-034-035
   
   # Or check database directly
   psql "$DATABASE_URL" -c "SELECT column_name FROM information_schema.columns WHERE table_name='repcard_metric_audit' AND column_name='repcard_appointment_id';"
   ```

3. **Test Features**:
   - Visit `/analytics` → RepCard tab
   - Check office breakdown view (should show expandable office cards)
   - Verify webhook processing (check logs)
   - Test sync missing customers endpoint

## 🎯 What's Being Deployed

### Code Changes
- ✅ Attachment sync on webhooks
- ✅ Webhook field extraction (appointment_link, remind_at, contact_source, etc.)
- ✅ Office breakdown view with individual rep metrics
- ✅ Fixed sync missing customers (rate limits, timeouts)
- ✅ All API endpoints updated

### Database Migrations
- ✅ Migration 034: Fix metric audit FK constraint
- ✅ Migration 035: Add useful webhook fields

### New API Endpoints
- ✅ `/api/admin/run-migrations-034-035` - Run new migrations
- ✅ Updated `/api/admin/run-migrations` - Includes 034 and 035

## ⚠️ Important Notes

1. **Migrations are idempotent**: Safe to run multiple times
2. **Rate Limits**: Sync operations now respect RepCard API limits
3. **Timeouts**: Sync limited to 500 customers per run
4. **Backward Compatible**: All changes are backward compatible

## 📞 Support

If deployment fails:
1. Check GitHub Actions logs
2. Check Vercel deployment logs
3. Verify database connection
4. Check migration status via API

---

**Ready to deploy!** Just push to main and run migrations in production.
