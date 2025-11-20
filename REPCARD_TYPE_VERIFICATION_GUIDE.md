# RepCard Type Verification Guide

## What Type of Issue Is This?

This is a **PostgreSQL/Neon database schema type mismatch** issue. It's NOT:
- ❌ A RepCard API issue (API returns numbers correctly)
- ❌ A JavaScript/TypeScript issue (types are correct)
- ✅ A **database schema vs query type mismatch** (PostgreSQL/Neon)

## Where to Verify

### 1. **RepCard API Documentation** ✅ (Already Verified)
**Location:** `lib/repcard/types.ts` and `lib/repcard/client.ts`

**What to Check:**
```typescript
// RepCard API returns IDs as numbers:
export interface RepCardUser {
  id: number;  // ✅ Number, not string
  ...
}

export interface RepCardCustomer {
  id: number;  // ✅ Number, not string
  assignedUserId?: number;  // ✅ Number, not string
  ...
}
```

**Conclusion:** RepCard API returns numeric IDs (not hex strings). ✅

**API Docs:** https://www.repcard.com/api-docs

---

### 2. **Database Schema (Migrations)** 🔍 (This is where the issue is)

**Location:** `lib/db/migrations/`

**Migration History:**
1. **Migration 012** (`012_repcard_sync_tables.sql`):
   - Created tables with `INTEGER` types ✅
   ```sql
   repcard_customer_id INTEGER UNIQUE NOT NULL,
   setter_user_id INTEGER,
   ```

2. **Migration 013** (`013_fix_repcard_id_types.sql`):
   - Changed to `TEXT` (incorrect assumption about hex IDs) ❌
   ```sql
   ALTER COLUMN repcard_customer_id TYPE TEXT,
   ALTER COLUMN setter_user_id TYPE TEXT,
   ```

3. **Migration 018** (`018_normalize_repcard_user_ids_to_integer.sql`):
   - Changed user IDs back to `INTEGER` (correct) ✅
   ```sql
   ALTER COLUMN setter_user_id TYPE INTEGER,
   ALTER COLUMN repcard_user_id TYPE INTEGER,
   ```

**The Problem:** Migration 018 may not have run in production, so columns are still `TEXT`.

---

### 3. **PostgreSQL/Neon Type Casting** 📚 (Reference)

**PostgreSQL Documentation:**
- Type Casting: https://www.postgresql.org/docs/current/sql-expressions.html#SQL-SYNTAX-TYPE-CASTS
- Type Conversion: https://www.postgresql.org/docs/current/typeconv.html

**Neon Documentation:**
- Uses standard PostgreSQL, so same rules apply
- https://neon.tech/docs

**Key Points:**
- PostgreSQL is **strictly typed** - you can't compare `INTEGER = TEXT` without casting
- Casting syntax: `column::text` or `column::integer`
- Casting prevents index usage (performance hit)

---

### 4. **How to Check Your Production Database** 🔍

**Option 1: Check via SQL Query**
```sql
-- Check actual column types in production
SELECT 
  table_name,
  column_name,
  data_type
FROM information_schema.columns
WHERE table_schema = 'public'
  AND table_name IN ('users', 'repcard_customers', 'repcard_appointments')
  AND column_name LIKE '%repcard%' OR column_name LIKE '%user_id%'
ORDER BY table_name, column_name;
```

**Option 2: Check Migration Status**
```sql
-- Check if migration 018 has run
SELECT * FROM schema_migrations 
WHERE version = '018_normalize_repcard_user_ids_to_integer';
```

**Option 3: Use the Diagnostic Script**
```bash
npx tsx scripts/check-production-repcard.ts
```

---

### 5. **Current Fix Strategy** ✅

**What We Did:**
- Added `::text` casts to handle both cases (TEXT or INTEGER)
- Works whether migration 018 has run or not
- Trade-off: Slightly slower queries (can't use indexes), but works

**Example:**
```sql
-- Before (fails if types don't match):
LEFT JOIN repcard_appointments a ON u.repcard_user_id = a.setter_user_id

-- After (works with both TEXT and INTEGER):
LEFT JOIN repcard_appointments a ON u.repcard_user_id::text = a.setter_user_id::text
```

---

### 6. **Best Long-Term Solution** 🎯

**Run Migration 018 in Production:**
```bash
# Connect to production database
psql "$DATABASE_URL" -f lib/db/migrations/018_normalize_repcard_user_ids_to_integer.sql

# Then remove the ::text casts for better performance
```

**After Migration 018 Runs:**
- All user ID columns will be `INTEGER`
- Can remove `::text` casts
- Queries will be faster (can use indexes)
- Type-safe comparisons

---

## Summary

| Source | Type | Status |
|--------|------|--------|
| RepCard API | `number` | ✅ Correct |
| TypeScript Types | `number` | ✅ Correct |
| Migration 012 | `INTEGER` | ✅ Correct (initial) |
| Migration 013 | `TEXT` | ❌ Wrong assumption |
| Migration 018 | `INTEGER` | ✅ Correct (fix) |
| Production DB | `TEXT` or `INTEGER` | ❓ Unknown (needs check) |
| Current Queries | Cast to `TEXT` | ✅ Works (temporary fix) |

**Next Steps:**
1. ✅ Verify RepCard API returns numbers (done)
2. 🔍 Check production database schema
3. 🔧 Run migration 018 if needed
4. ⚡ Remove casts after migration for performance


