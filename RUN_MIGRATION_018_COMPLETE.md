# Run Migration 018 Complete - Quick Guide

## What This Does

Completes the partial migration 018 by converting remaining TEXT user IDs to INTEGER:
- `repcard_customers.setter_user_id`: TEXT → INTEGER
- `repcard_appointments.setter_user_id`: TEXT → INTEGER  
- `repcard_appointments.closer_user_id`: TEXT → INTEGER

## Quick Run (Choose One Method)

### Method 1: Direct DATABASE_URL (Fastest)

```bash
# Get DATABASE_URL from Vercel Dashboard:
# Settings → Environment Variables → Production → Copy DATABASE_URL

# Then run:
cd /Users/austinelkins/.cursor/worktrees/Rep_Dashboard/Aghcz
DATABASE_URL="your-production-database-url-here" npx tsx scripts/run-complete-migration-018.ts
```

### Method 2: Via Vercel CLI

```bash
cd /Users/austinelkins/.cursor/worktrees/Rep_Dashboard/Aghcz

# Link to Vercel project (if not already linked)
vercel link

# Pull production environment variables
vercel env pull .env.production --environment=production

# Run migration
source .env.production
npx tsx scripts/run-complete-migration-018.ts
```

### Method 3: Manual SQL (If you have psql)

```bash
# Get DATABASE_URL from Vercel Dashboard
export DATABASE_URL="your-production-database-url"

# Run migration directly
psql "$DATABASE_URL" -f lib/db/migrations/018_complete_normalize_user_ids.sql
```

## Verification

After running, verify the schema changed:

```sql
SELECT 
  table_name,
  column_name,
  data_type
FROM information_schema.columns
WHERE table_schema = 'public'
  AND table_name IN ('repcard_customers', 'repcard_appointments')
  AND column_name IN ('setter_user_id', 'closer_user_id')
ORDER BY table_name, column_name;
```

Expected result:
- All columns should show `data_type = 'integer'`

## After Migration

Once migration completes successfully:
1. ✅ Schema will be consistent (all INTEGER)
2. ⚡ Can remove `::text` casts from queries for better performance
3. 🚀 Queries will be faster (can use indexes)

## Safety

- ✅ Migration is idempotent (safe to re-run)
- ✅ Uses `USING CASE` to handle invalid values gracefully
- ✅ No data loss (only converts valid numeric strings)
- ✅ Creates indexes for performance


