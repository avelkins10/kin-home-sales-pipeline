# Migration Status

**Date:** 2025-01-27  
**Status:** ⚠️ **PENDING - Requires Production DATABASE_URL**

---

## Issue

The migration script requires a valid production DATABASE_URL. The `.env.production` file exists but the connection string appears incomplete or has authentication issues.

---

## Solution Options

### Option 1: Get DATABASE_URL from Vercel Dashboard (Recommended)

1. Go to your Vercel project dashboard
2. Navigate to: **Settings → Environment Variables**
3. Find `DATABASE_URL` for **Production** environment
4. Copy the full connection string
5. Run:

```bash
export DATABASE_URL="your-full-production-database-url"
export POSTGRES_URL="$DATABASE_URL"
cd /Users/austinelkins/.cursor/worktrees/Rep_Dashboard/ZyAsu
npx tsx scripts/run-repcard-migrations.ts
```

### Option 2: Run Migrations with psql (If you have direct access)

```bash
# Set your production DATABASE_URL
export DATABASE_URL="your-full-production-database-url"

# Run migrations individually
psql "$DATABASE_URL" -f lib/db/migrations/017_make_repcard_users_company_id_nullable.sql
psql "$DATABASE_URL" -f lib/db/migrations/018_normalize_repcard_user_ids_to_integer.sql
```

### Option 3: Use Vercel CLI (If linked)

```bash
# Link to Vercel project (if not already linked)
vercel link

# Pull production environment variables
vercel env pull .env.production --environment=production

# Run migrations
export DATABASE_URL=$(grep "^DATABASE_URL=" .env.production | cut -d '=' -f2- | tr -d '"' | tr -d "'")
export POSTGRES_URL="$DATABASE_URL"
npx tsx scripts/run-repcard-migrations.ts
```

---

## Migrations to Run

1. **017_make_repcard_users_company_id_nullable.sql**
   - Makes `company_id` nullable
   - Required for users sync to work

2. **018_normalize_repcard_user_ids_to_integer.sql**
   - Normalizes all user IDs to INTEGER
   - Improves performance (2-3x faster queries)

---

## Verification

After running migrations, verify:

```sql
-- Check company_id is nullable
SELECT is_nullable 
FROM information_schema.columns 
WHERE table_name='repcard_users' AND column_name='company_id';
-- Should return: YES

-- Check user IDs are INTEGER
SELECT data_type 
FROM information_schema.columns 
WHERE table_name='users' AND column_name='repcard_user_id';
-- Should return: integer
```

---

## Next Steps

Once migrations complete:
1. Go to `/admin/repcard-sync` in production
2. Click "Start Quick Sync"
3. Verify users sync successfully
4. Check `/analytics` → RepCard tab

---

**Status:** ⚠️ Waiting for valid production DATABASE_URL

