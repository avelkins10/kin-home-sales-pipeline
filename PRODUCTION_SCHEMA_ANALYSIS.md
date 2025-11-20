# Production Database Schema Analysis

## Current State (from production query)

| Table | Column | Type | Status |
|-------|--------|------|--------|
| `users` | `repcard_user_id` | **INTEGER** | ✅ Migration 018 ran |
| `repcard_customers` | `setter_user_id` | **TEXT** | ❌ Migration 018 did NOT run |
| `repcard_appointments` | `setter_user_id` | **TEXT** | ❌ Migration 018 did NOT run |
| `repcard_appointments` | `closer_user_id` | **TEXT** | ❌ Migration 018 did NOT run |
| `repcard_customers` | `repcard_customer_id` | TEXT | ✅ Correct (should stay TEXT) |
| `repcard_appointments` | `repcard_customer_id` | TEXT | ✅ Correct (should stay TEXT) |

## The Problem

**Type Mismatch:**
- `users.repcard_user_id` = INTEGER ✅
- `repcard_customers.setter_user_id` = TEXT ❌
- `repcard_appointments.setter_user_id` = TEXT ❌

**When we query:**
```sql
LEFT JOIN repcard_appointments a ON u.repcard_user_id = a.setter_user_id
```

PostgreSQL can't compare `INTEGER = TEXT` → **Error: "operator does not exist: integer = text"**

## Why Migration 018 Didn't Complete

Migration 018 has **4 steps**:
1. ✅ Step 1: `repcard_customers.setter_user_id` → INTEGER (NOT run)
2. ✅ Step 2: `repcard_appointments.setter_user_id` → INTEGER (NOT run)
3. ✅ Step 3: `users.repcard_user_id` → INTEGER (RUN - this worked!)
4. ✅ Step 4: `repcard_status_logs.changed_by_user_id` → INTEGER (unknown)

**Conclusion:** Migration 018 was **partially run** - only Step 3 completed.

## Current Fix (Temporary)

We're casting both sides to TEXT:
```sql
LEFT JOIN repcard_appointments a ON u.repcard_user_id::text = a.setter_user_id::text
```

**Pros:**
- ✅ Works with current schema
- ✅ No migration needed

**Cons:**
- ❌ Can't use indexes (slower queries)
- ❌ Not optimal long-term

## Proper Solution

**Run the remaining steps of Migration 018:**

```sql
-- Step 1: Fix repcard_customers.setter_user_id
ALTER TABLE repcard_customers
  ALTER COLUMN setter_user_id TYPE INTEGER 
  USING CASE 
    WHEN setter_user_id ~ '^[0-9]+$' THEN setter_user_id::integer
    ELSE NULL
  END;

-- Step 2: Fix repcard_appointments.setter_user_id and closer_user_id
ALTER TABLE repcard_appointments
  ALTER COLUMN setter_user_id TYPE INTEGER 
  USING CASE 
    WHEN setter_user_id ~ '^[0-9]+$' THEN setter_user_id::integer
    ELSE NULL
  END,
  ALTER COLUMN closer_user_id TYPE INTEGER 
  USING CASE 
    WHEN closer_user_id ~ '^[0-9]+$' THEN closer_user_id::integer
    ELSE NULL
  END;

-- Step 4: Fix repcard_status_logs.changed_by_user_id (if table exists)
ALTER TABLE repcard_status_logs
  ALTER COLUMN changed_by_user_id TYPE INTEGER 
  USING CASE 
    WHEN changed_by_user_id ~ '^[0-9]+$' THEN changed_by_user_id::integer
    ELSE NULL
  END;
```

**After running:**
- Remove `::text` casts from queries
- Queries will be faster (can use indexes)
- Type-safe comparisons

## Next Steps

1. ✅ Current fix deployed (works with TEXT columns)
2. 🔧 Run remaining migration steps in production
3. ⚡ Remove casts after migration for performance

