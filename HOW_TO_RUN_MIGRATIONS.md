# ✅ How to Run RepCard Migrations - Simple Guide

## 🚀 Easy Method (No psql Required!)

Since you don't have `psql` installed, use the Node.js script:

```bash
# Make sure you're in the project directory
cd /Users/austinelkins/Rep_Dashboard

# Run migrations
npm run migrate:repcard-complete
```

That's it! The script will:
1. ✅ Read your `.env.local` file for DATABASE_URL
2. ✅ Run both migrations (016 and 017)
3. ✅ Verify everything was created correctly
4. ✅ Tell you what to do next

---

## 📋 Step-by-Step Instructions

### Step 1: Verify .env.local exists
```bash
# Check if file exists
ls -la .env.local

# Should show: ✅ .env.local exists
```

### Step 2: Run migrations
```bash
npm run migrate:repcard-complete
```

You should see output like:
```
🚀 RepCard Complete Integration - Migrations
==========================================

✅ DATABASE_URL configured

📦 Migration 016_repcard_complete_data
   Reading migration file...
   Executing migration...
✅ 016_repcard_complete_data completed successfully

📦 Migration 017_repcard_settings
   Reading migration file...
   Executing migration...
✅ 017_repcard_settings completed successfully

🔍 Verifying migrations...
📊 Found 9 RepCard tables:
   ✅ repcard_calendars
   ✅ repcard_custom_fields
   ✅ repcard_customer_notes
   ✅ repcard_customer_statuses
   ✅ repcard_leaderboard_snapshots
   ✅ repcard_teams
   ✅ repcard_leaderboard_config
   ✅ repcard_analytics_config
   ✅ repcard_metric_definitions

📈 Metric definitions: 20 (expected ~20)

✅ All migrations completed successfully!
```

### Step 3: Run Sync (After Deployment)

After Vercel finishes deploying, trigger the sync:

**Option A: Via Browser**
1. Log in as super admin
2. Go to: `https://your-domain.com/api/admin/repcard/comprehensive-sync`
3. Or use the admin UI

**Option B: Via curl**
```bash
curl -X POST https://your-domain.com/api/admin/repcard/comprehensive-sync \
  -H "Cookie: your-session-cookie"
```

**Option C: Wait for Cron**
- Sync runs automatically every 5 minutes via cron job

### Step 4: Configure Leaderboards

1. Log in as super admin
2. Go to **Settings** → **RepCard Config** tab
3. Create your first leaderboard configuration

---

## 🔧 Troubleshooting

### "DATABASE_URL not set"
```bash
# Check if .env.local exists
cat .env.local | grep DATABASE_URL

# If missing, get it from Vercel:
# Dashboard → Settings → Environment Variables → Copy DATABASE_URL
```

### "Module not found"
```bash
# Install dependencies
npm install
```

### "Tables already exist"
✅ **This is fine!** Migrations are safe to run multiple times. If tables already exist, the script will continue.

### Migration fails
1. Check your DATABASE_URL is correct
2. Verify database user has CREATE TABLE permissions
3. Check error message for specific issues

---

## 🎯 Quick Reference

```bash
# Run migrations
npm run migrate:repcard-complete

# Verify tables exist (optional)
npm run migrate:repcard-complete  # Will verify automatically

# Trigger sync (after deployment)
# Visit: /api/admin/repcard/comprehensive-sync
```

---

## ✅ Success Checklist

After running migrations, verify:
- [ ] Script completed without errors
- [ ] See "✅ All migrations completed successfully!"
- [ ] 9 tables listed in verification
- [ ] ~20 metric definitions loaded

Then:
- [ ] Wait for Vercel deployment
- [ ] Run sync
- [ ] Configure leaderboards

**That's it! You're done!** 🎉
