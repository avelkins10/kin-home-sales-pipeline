# RepCard Production Fix Checklist

## ✅ What I've Created For You

I've created all the scripts and documentation you need. Here's what's ready:

### Scripts Created:
1. ✅ `scripts/link-users-to-repcard.sql` - Links users to RepCard by email
2. ✅ `scripts/check-repcard-tables.sql` - Checks if tables exist
3. ✅ `scripts/verify-production-setup.sh` - Verifies entire setup
4. ✅ `scripts/trigger-production-sync.sh` - Instructions for triggering sync
5. ✅ `scripts/check-production-repcard.ts` - TypeScript diagnostic tool

### Documentation Created:
1. ✅ `REPCARD_PRODUCTION_FIX.md` - Complete production fix guide
2. ✅ `REPCARD_PRODUCTION_QUICK_FIX.md` - Quick reference
3. ✅ `REPCARD_FIX_GUIDE.md` - General troubleshooting guide

### Code Improvements:
1. ✅ Improved error handling in `RepCardMetricsCard.tsx`
2. ✅ Better error messages for users

---

## 🔑 What I Need From You

To complete the fix, I need these 3 things:

### 1. RepCard API Key ⚠️ CRITICAL

**What**: Your RepCard API key  
**Where to get it**: https://www.repcard.com/settings/api (or your RepCard dashboard)  
**What I'll do with it**: Add it to Vercel production environment variables

**OR** you can add it yourself:
- Go to: https://vercel.com/[your-project]/settings/environment-variables
- Add: `REPCARD_API_KEY` = `<your-key>`
- Environment: **Production**
- **Redeploy** after adding

---

### 2. Production Database Access

**What**: Access to your production database  
**Why**: To check tables, link users, verify data

**Options**:
- **Option A**: Give me the `DATABASE_URL` (I'll use it securely)
- **Option B**: You run the SQL scripts I created:
  ```bash
  # Check tables
  psql "$DATABASE_URL" -f scripts/check-repcard-tables.sql
  
  # Link users
  psql "$DATABASE_URL" -f scripts/link-users-to-repcard.sql
  ```

---

### 3. Production Domain

**What**: Your production domain (e.g., `app.yourcompany.com`)  
**Why**: To verify API endpoints and trigger sync

**OR** you can trigger sync yourself:
- Navigate to: `https://your-domain.com/admin/repcard-sync`
- Click "Run Full Sync"

---

## 🚀 Quick Start (If You Have Everything)

If you have the RepCard API key, database access, and domain, I can:

1. ✅ Verify environment variables are set
2. ✅ Check database tables exist
3. ✅ Run migrations if needed
4. ✅ Link users to RepCard
5. ✅ Trigger initial sync
6. ✅ Verify everything works

**Just provide**:
- RepCard API key
- Database URL (or confirm you'll run the SQL scripts)
- Production domain

---

## 📋 Step-by-Step (If You Want to Do It Yourself)

### Step 1: Add RepCard API Key to Vercel

```bash
# Via CLI
vercel env add REPCARD_API_KEY production
# Paste your key when prompted

# Or via Dashboard
# https://vercel.com/[project]/settings/environment-variables
```

**Then redeploy:**
```bash
vercel --prod
```

### Step 2: Verify Database Tables

```bash
# Check if tables exist
psql "$DATABASE_URL" -f scripts/check-repcard-tables.sql
```

If tables are missing, run migrations:
```bash
./scripts/run-migrations-production.sh
```

### Step 3: Run Initial Sync

**Via Admin Dashboard** (easiest):
1. Go to: `https://your-domain.com/admin/repcard-sync`
2. Click "Run Full Sync"
3. Wait 2-5 minutes

**Or wait for automatic sync** (runs every 5 minutes if cron is set up)

### Step 4: Link Users

```bash
# Link users by email
psql "$DATABASE_URL" -f scripts/link-users-to-repcard.sql
```

### Step 5: Verify

```bash
# Run verification script
./scripts/verify-production-setup.sh
```

---

## 🎯 What's Most Likely Wrong

Based on the code, the **most common issue** is:

**❌ `REPCARD_API_KEY` not set in Vercel production**

This causes:
- API calls to fail
- No data to sync
- Empty database tables
- Dashboard shows no metrics

**Fix**: Add the API key to Vercel production environment variables (Step 1 above)

---

## ✅ After Everything is Fixed

You should see:
- ✅ RepCard metrics card on dashboard showing doors knocked, appointments, etc.
- ✅ Leaderboards populated with data
- ✅ Quality metrics displaying
- ✅ No error messages

---

## 📞 Ready to Proceed?

**Option 1**: Give me the info and I'll fix it all
- RepCard API key
- Database URL (or I'll guide you through SQL)
- Production domain

**Option 2**: Follow the step-by-step guide above

**Option 3**: Run the verification script to see what's missing:
```bash
./scripts/verify-production-setup.sh
```

Let me know which option you prefer! 🚀



