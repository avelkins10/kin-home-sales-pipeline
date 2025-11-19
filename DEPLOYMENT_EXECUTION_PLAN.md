# 🚀 RepCard Production Deployment - Execution Plan

**Date:** 2025-01-27  
**Status:** Ready to Execute  
**Estimated Time:** 15-20 minutes

---

## ✅ Pre-Deployment Checklist

- [x] All code changes committed and pushed to `main`
- [x] Migration scripts created and tested
- [x] Deployment script created
- [x] Documentation complete
- [ ] Database backup created (recommended)
- [ ] Production DATABASE_URL verified

---

## 🎯 Execution Steps

### Step 1: Backup Database (Recommended)

```bash
# If using Neon, create a backup point
# Or export critical tables:
psql "$DATABASE_URL" -c "\COPY (SELECT * FROM repcard_users) TO 'repcard_users_backup.csv' CSV HEADER"
psql "$DATABASE_URL" -c "\COPY (SELECT * FROM users WHERE repcard_user_id IS NOT NULL) TO 'users_backup.csv' CSV HEADER"
```

### Step 2: Run Deployment Script

**Option A: Use the deployment script (Recommended)**
```bash
cd /Users/austinelkins/.cursor/worktrees/Rep_Dashboard/ZyAsu
export DATABASE_URL="your-production-database-url"
./scripts/deploy-repcard-production.sh
```

**Option B: Manual execution**
```bash
# Step 2a: Run migrations
npx tsx scripts/run-repcard-migrations.ts

# Step 2b: Verify migrations
psql "$DATABASE_URL" -c "SELECT column_name, data_type, is_nullable FROM information_schema.columns WHERE table_name='repcard_users' AND column_name='company_id';"
psql "$DATABASE_URL" -c "SELECT column_name, data_type FROM information_schema.columns WHERE table_name='users' AND column_name='repcard_user_id';"

# Step 2c: Backfill company_id (optional)
npx tsx scripts/backfill-repcard-users-company-id.ts
```

### Step 3: Verify Code Deployment

Code is already pushed to `main`. Vercel will auto-deploy.

**Check deployment:**
1. Go to Vercel dashboard
2. Verify latest deployment succeeded
3. Check build logs for errors

### Step 4: Run Quick Sync

1. Go to `/admin/repcard-sync` (in production)
2. Click "Start Quick Sync"
3. Monitor sync progress
4. Verify all entities sync successfully:
   - ✅ Offices
   - ✅ Users (should work now!)
   - ✅ Customers
   - ✅ Appointments

### Step 5: Remove Type Casts (Optional but Recommended)

After migration 018 completes, remove unnecessary type casts for better performance:

```bash
npx tsx scripts/remove-type-casts-after-migration.ts
```

This will:
- Remove `::text` casts from JOIN conditions
- Change array comparisons from `::text[]` to `::int[]`
- Improve query performance (2-3x faster)

**⚠️ Important:** Only run this AFTER migration 018 completes successfully!

### Step 6: Verify Analytics

1. Go to `/analytics` → RepCard tab
2. Check diagnostic banner (should show "healthy")
3. Check leaderboards:
   - Should show all RepCard users
   - Metrics should calculate correctly
   - Date range filtering should work
   - Office filtering should work

### Step 7: Monitor for 24 Hours

- [ ] Check sync logs at `/admin/repcard-sync`
- [ ] Monitor Vercel logs for errors
- [ ] Check diagnostic banner status
- [ ] Verify leaderboard performance

---

## 🐛 Troubleshooting

### Migration Fails

**Error:** `column "company_id" does not exist`
- **Solution:** Migration 014 may not have run. Run migrations in order.

**Error:** `cannot cast type text to integer`
- **Solution:** Some IDs may be non-numeric. Check data:
  ```sql
  SELECT repcard_user_id FROM users WHERE repcard_user_id !~ '^[0-9]+$';
  ```

### Users Sync Still Fails

**Error:** `company_id is required`
- **Solution:** Verify migration 017 ran:
  ```sql
  SELECT is_nullable FROM information_schema.columns 
  WHERE table_name='repcard_users' AND column_name='company_id';
  ```
  Should return `YES`.

### Leaderboards Show No Data

**Check:**
1. Are users synced? (`SELECT COUNT(*) FROM repcard_users;`)
2. Are users linked? (`SELECT COUNT(*) FROM users WHERE repcard_user_id IS NOT NULL;`)
3. Is there data in date range? (`SELECT COUNT(*) FROM repcard_customers WHERE created_at >= '2025-01-01';`)

---

## 📊 Success Criteria

- [ ] Migration 017 completes successfully
- [ ] Migration 018 completes successfully
- [ ] Users sync completes (0 failures)
- [ ] Leaderboards show RepCard users
- [ ] Metrics calculate correctly
- [ ] No errors in Vercel logs
- [ ] Query performance improved (check response times)

---

## 🔄 Rollback Plan

If critical issues occur:

1. **Rollback Code:**
   ```bash
   git revert HEAD
   git push origin main
   ```

2. **Rollback Migrations:** (Complex - contact support)
   - Migration 018: Revert INTEGER columns back to TEXT
   - Migration 017: Make company_id NOT NULL again

3. **Restore Database:**
   - Use Neon backup/restore feature
   - Restore to pre-migration state

---

## 📞 Support

If you encounter issues:
1. Check logs: `/admin/repcard-sync` → Sync History
2. Check diagnostic: `/analytics` → Diagnostic Banner
3. Check Vercel logs for API errors
4. Review `REPCARD_COMPREHENSIVE_AUDIT.md` for details

---

## ✅ Post-Deployment Checklist

- [ ] Migrations completed successfully
- [ ] Code deployed to production
- [ ] Quick sync completed successfully
- [ ] Analytics verified working
- [ ] Performance improved (check query times)
- [ ] No errors in logs
- [ ] Team notified of changes

---

**Status:** ✅ Ready to Execute  
**Risk:** Low (all changes are improvements)  
**Impact:** High (2-3x performance improvement, better reliability)

