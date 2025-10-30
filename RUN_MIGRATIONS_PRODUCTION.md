# Running Migrations in Production

## ⚠️ Important: Production vs Local Database

The migration script reads from `.env.local`, which might point to your **local development database**. 

**You need to run migrations against your PRODUCTION database!**

---

## 🎯 Best Practices

### Option 1: Use Vercel CLI (Recommended)

This pulls production environment variables and runs migrations safely:

```bash
# Install Vercel CLI if you haven't
npm i -g vercel

# Login to Vercel
vercel login

# Pull production environment variables
vercel env pull .env.production --environment=production

# Run migrations with production DATABASE_URL
export $(cat .env.production | grep DATABASE_URL | xargs)
npm run migrate:repcard-complete

# Or directly:
DATABASE_URL=$(grep DATABASE_URL .env.production | cut -d '=' -f2-) npm run migrate:repcard-complete
```

### Option 2: Set Production DATABASE_URL Manually

```bash
# Get DATABASE_URL from Vercel Dashboard:
# Settings → Environment Variables → Copy DATABASE_URL

# Set it temporarily
export DATABASE_URL="your-production-database-url"

# Run migrations
npm run migrate:repcard-complete

# Unset after (for safety)
unset DATABASE_URL
```

### Option 3: Run via Vercel CLI Directly

```bash
# Run migrations in production environment
vercel env pull .env.production
source .env.production  # Load production vars
npm run migrate:repcard-complete
```

### Option 4: Create Production-Specific Script

Create a script that explicitly uses production:

```bash
#!/bin/bash
# scripts/run-migrations-production.sh

echo "⚠️  WARNING: This will run migrations on PRODUCTION database"
read -p "Are you sure? (yes/no): " confirm

if [ "$confirm" != "yes" ]; then
  echo "Cancelled"
  exit 1
fi

# Pull production env vars
vercel env pull .env.production --environment=production

# Run migrations
export $(cat .env.production | grep DATABASE_URL | xargs)
npm run migrate:repcard-complete
```

---

## ✅ Safe Migration Checklist

Before running migrations in production:

- [ ] ✅ **Backup database** (if possible)
- [ ] ✅ **Verify DATABASE_URL points to production**
- [ ] ✅ **Test migrations locally first** (using local DATABASE_URL)
- [ ] ✅ **Run during low-traffic period** (if possible)
- [ ] ✅ **Have rollback plan** (migrations are idempotent - safe to re-run)

---

## 🔍 How to Verify You're Using Production Database

After running migrations, verify:

```bash
# Connect to production database
psql "$DATABASE_URL"

# Check database name/host
SELECT current_database(), inet_server_addr();

# Or check tables
SELECT table_name FROM information_schema.tables 
WHERE table_name LIKE 'repcard_%';
```

---

## 🚨 Important Notes

1. **Migrations are safe to re-run** - If tables already exist, they'll be skipped
2. **No data loss** - These migrations only CREATE tables, don't modify existing data
3. **Backward compatible** - Old code continues to work
4. **Can run anytime** - No downtime required

---

## 📋 Recommended Approach

**For production deployment:**

1. **Local test first:**
   ```bash
   # Test with local database
   npm run migrate:repcard-complete
   ```

2. **Then run in production:**
   ```bash
   # Pull production env vars
   vercel env pull .env.production
   
   # Run with production DATABASE_URL
   DATABASE_URL=$(grep DATABASE_URL .env.production | cut -d '=' -f2-) \
     npm run migrate:repcard-complete
   ```

3. **Verify:**
   - Check Vercel logs
   - Verify tables exist in production database
   - Test sync endpoint

---

## 🎯 Quick Production Command

```bash
# One-liner to run migrations in production
vercel env pull .env.production && \
  DATABASE_URL=$(grep DATABASE_URL .env.production | cut -d '=' -f2-) \
  npm run migrate:repcard-complete
```

This ensures you're using production DATABASE_URL!

