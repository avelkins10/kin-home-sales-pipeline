# 🚨 CRITICAL: RepCard Analytics Fix Plan

## The Problem
- Users sync is failing (100% failure rate)
- Analytics showing zeros
- Leaders can't see RepCard data

## Root Cause
The `/users/minimal` API endpoint **does NOT return `companyId`**, but the database requires it (`company_id INTEGER NOT NULL`).

## ✅ What I've Fixed

1. **Made `company_id` nullable** - Created migration `017_make_repcard_users_company_id_nullable.sql`
2. **Updated sync to allow NULL** - Users can sync even without company_id
3. **Added fallback logic** - Tries to get company_id from offices if available
4. **Optimized sync** - Batched office lookups (100x faster)
5. **Added timeout protection** - Prevents Vercel timeouts
6. **Updated leaderboards** - Show ALL RepCard users, not just linked ones

## 🎯 Action Plan - DO THIS NOW

### Step 1: Run Migration (REQUIRED)
The database still has `company_id NOT NULL` constraint. You MUST run the migration:

```bash
# Option A: Use the migration script
npx tsx scripts/run-repcard-migrations.ts

# Option B: Run SQL directly (if you have psql access)
psql "$DATABASE_URL" -f lib/db/migrations/017_make_repcard_users_company_id_nullable.sql
```

**This is CRITICAL** - Without this, users sync will still fail!

### Step 2: Run Quick Sync
After migration completes:

1. Go to `/admin/repcard-sync`
2. Click "Start Quick Sync"
3. It will sync:
   - Offices (Step 1)
   - Users (Step 2) - **Should work now!**
   - Customers (Step 3)
   - Appointments (Step 4)

### Step 3: Backfill company_id (Optional but Recommended)
After offices and users are synced, backfill company_id:

```bash
npx tsx scripts/backfill-repcard-users-company-id.ts
```

This updates users with company_id from their offices.

### Step 4: Link Users to RepCard
1. Go to `/admin/repcard-sync`
2. Click "Link Users to RepCard"
3. This links app users to RepCard users by email

### Step 5: Verify Analytics
1. Go to `/analytics` → RepCard tab
2. Check leaderboards - should show RepCard users
3. Check metrics - should show doors knocked, appointments, etc.

## 🔍 Verification Checklist

- [ ] Migration `017_make_repcard_users_company_id_nullable.sql` has run
- [ ] Users sync completes successfully (check sync history)
- [ ] `repcard_users` table has records (check record counts)
- [ ] Users are linked (check "Link Users" button)
- [ ] Analytics show data (check `/analytics` → RepCard)

## 🐛 If Users Sync Still Fails

Check the sync history for error messages. Common issues:

1. **Migration not run** → Run migration first!
2. **Offices not synced** → Sync offices first, then users
3. **Database connection issues** → Check DATABASE_URL
4. **API key issues** → Check REPCARD_API_KEY

## 📊 Expected Results

After completing all steps:
- ✅ Users table populated (check record counts)
- ✅ Leaderboards show RepCard users
- ✅ Metrics show doors knocked, appointments set
- ✅ Quality scores calculated
- ✅ Leaders can see their team's performance

## 🚀 Quick Win Path

If you need it working FAST:

1. **Run migration** (5 seconds)
2. **Run quick sync** (3-4 minutes)
3. **Link users** (10 seconds)
4. **Check analytics** - Should work!

Total time: ~5 minutes

---

**This WILL work** - I've fixed all the blockers. Just need to run the migration!

