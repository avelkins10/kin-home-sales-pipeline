# RepCard Production Quick Fix

## 🚨 Problem
RepCard metrics are not displaying in production after deployment.

## ✅ Solution (3 Steps)

### Step 1: Add RepCard API Key to Vercel Production

**This is the most common issue!**

1. Go to: https://vercel.com/[your-project]/settings/environment-variables
2. Click "Add New"
3. Add:
   - **Key**: `REPCARD_API_KEY`
   - **Value**: Your RepCard API key (get from https://www.repcard.com/settings/api)
   - **Environment**: Select **Production** ✅
   - Click "Save"
4. **IMPORTANT**: Redeploy after adding the env var
   - Go to Deployments → Click "Redeploy" on latest deployment
   - Or run: `vercel --prod`

### Step 2: Run Initial Data Sync

After the API key is set and deployment completes:

1. Navigate to: `https://your-domain.com/admin/repcard-sync`
2. Click "Run Full Sync" button
3. Wait 2-5 minutes for sync to complete
4. Verify records appear in sync results

### Step 3: Link Users to RepCard

Users need their `repcard_user_id` populated:

1. Connect to production database:
   ```bash
   psql "$DATABASE_URL"
   ```

2. Link users by email:
   ```sql
   UPDATE users u
   SET repcard_user_id = ru.repcard_user_id::text
   FROM repcard_users ru
   WHERE LOWER(u.email) = LOWER(ru.email)
     AND u.repcard_user_id IS NULL;
   ```

3. Verify:
   ```sql
   SELECT COUNT(*) FROM users WHERE repcard_user_id IS NOT NULL;
   ```

---

## 🔍 Verify It's Working

After completing all steps:

1. **Check Dashboard**: Navigate to your production dashboard
2. **Look for RepCard Metrics Card**: Should show doors knocked, appointments, etc.
3. **If you see "No RepCard Data"**: User isn't linked (Step 3)
4. **If you see "RepCard Data Unavailable"**: Check Vercel logs for errors

---

## 📋 Common Issues

### Issue: "RepCard API key is required"
**Fix**: Add `REPCARD_API_KEY` to Vercel production env vars (Step 1)

### Issue: "Table does not exist"
**Fix**: Run migrations:
```bash
./scripts/run-migrations-production.sh
```

### Issue: Metrics show "0" or empty
**Fix**: Run data sync (Step 2)

### Issue: "User not linked to RepCard"
**Fix**: Link users by email (Step 3)

---

## 🛠️ Need More Help?

- **Detailed Guide**: See `REPCARD_PRODUCTION_FIX.md`
- **Check Status**: Run `npx tsx scripts/check-production-repcard.ts`
- **Vercel Logs**: https://vercel.com/[project]/deployments → Functions → Logs

---

## ✅ Checklist

- [ ] `REPCARD_API_KEY` added to Vercel production environment
- [ ] Production deployment completed (after adding env var)
- [ ] Initial data sync completed successfully
- [ ] Users linked to RepCard (`repcard_user_id` populated)
- [ ] Dashboard displays RepCard metrics

After completing this checklist, RepCard metrics should display in production! 🎉




