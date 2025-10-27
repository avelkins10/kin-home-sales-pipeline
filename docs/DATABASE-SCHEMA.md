# Database Schema Reference

This document provides a comprehensive reference for the database schema used in the Kin Home Sales Pipeline Dashboard.

## Table of Contents
- [Users Table](#users-table)
- [Offices Table](#offices-table)
- [Common Query Patterns](#common-query-patterns)
- [SQL Best Practices](#sql-best-practices)
- [Common Pitfalls](#common-pitfalls)

## Users Table

The `users` table is the core table for authentication and user management.

### Schema

```sql
CREATE TABLE users (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  email TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL,
  name TEXT NOT NULL,
  role TEXT NOT NULL CHECK (role IN ('closer', 'setter', 'coordinator', 'team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin', 'operations_coordinator', 'operations_manager')),
  quickbase_user_id TEXT,
  repcard_user_id TEXT,
  sales_office TEXT[],  -- ⚠️ THIS IS AN ARRAY, not a string!
  phone TEXT,
  region TEXT,
  last_login_at TIMESTAMP,
  last_project_date TIMESTAMP,
  invited_at TIMESTAMP,
  invite_token TEXT UNIQUE,
  invite_accepted_at TIMESTAMP,
  is_active BOOLEAN DEFAULT true,
  timezone TEXT DEFAULT 'America/New_York',
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
```

### Critical Field Notes

#### `sales_office` - TEXT[] (Array)
**⚠️ IMPORTANT**: This is a TEXT ARRAY, not a simple string!

**Why it's an array**: Users can belong to multiple offices (e.g., office leaders managing multiple locations)

**Common mistake**:
```sql
-- ❌ WRONG - This will fail with "column office does not exist"
SELECT id, name, office FROM users

-- ✅ CORRECT - Extract first element and alias
SELECT id, name, sales_office[1] AS office FROM users
```

**Filtering by office**:
```sql
-- ✅ Check if office is in the array
WHERE 'Phoenix Office' = ANY(sales_office)

-- ✅ Join with offices table for ID-based filtering
JOIN offices o ON o.name = ANY(u.sales_office)
WHERE o.quickbase_office_id = 123
```

#### `repcard_user_id` - TEXT
Links to RepCard API user ID for canvassing data. Can be NULL if user not linked to RepCard.

#### `quickbase_user_id` - TEXT
Links to QuickBase Contacts table user ID. Can be NULL for new users pending sync.

## Offices Table

```sql
CREATE TABLE offices (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  name TEXT UNIQUE NOT NULL,
  region TEXT CHECK (region IN ('southwest', 'southeast', 'midwest', 'northeast', 'west')),
  leader_id TEXT REFERENCES users(id) ON DELETE SET NULL,
  quickbase_office_id INTEGER UNIQUE,  -- Stable ID from QuickBase
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);
```

### Key Fields

#### `quickbase_office_id` - INTEGER
Stable reference ID from QuickBase (Field 810). Use this for filtering, not office names (which can change).

## Common Query Patterns

### Pattern 1: Select User with Office

```sql
-- Basic pattern - extract first office
SELECT
  id,
  name,
  email,
  sales_office[1] AS office,
  role
FROM users
WHERE id = $1;
```

### Pattern 2: Filter Users by Office Name

```sql
-- Check if office is in user's array
SELECT id, name, email
FROM users
WHERE $1 = ANY(sales_office);
```

### Pattern 3: Filter Users by Office ID (Recommended)

```sql
-- More robust - uses stable QuickBase ID
SELECT DISTINCT u.id, u.name, u.email, u.sales_office[1] AS office
FROM users u
JOIN offices o ON o.name = ANY(u.sales_office)
WHERE o.quickbase_office_id = ANY($1::int[]);
```

### Pattern 4: Filter Users by Multiple Office IDs

```sql
-- Convert JavaScript array to PostgreSQL array format
const officeIdsArray = `{${officeIds.join(',')}}`;

SELECT DISTINCT u.id, u.name, u.email
FROM users u
JOIN offices o ON o.name = ANY(u.sales_office)
WHERE o.quickbase_office_id = ANY(${officeIdsArray}::int[]);
```

### Pattern 5: Get All User Office Assignments

```sql
-- Get full office array
SELECT
  id,
  name,
  sales_office,  -- Returns entire array
  array_length(sales_office, 1) AS office_count
FROM users;
```

## SQL Best Practices

### 1. Always Check Migration Files First
Before writing SQL queries, check `/lib/db/migrations/` to see the actual schema:
- `001_initial_schema.sql` - Core tables (users, sessions, project_cache)
- `002_settings_schema.sql` - Notification settings
- `003_offices_schema.sql` - Offices table
- `006_user_hierarchies_schema.sql` - Team lead relationships
- `010_add_operations_roles.sql` - Operations roles

### 2. Use Prepared Statements
```typescript
// ✅ GOOD - Prevents SQL injection
const users = await sql`SELECT * FROM users WHERE email = ${email}`;

// ❌ BAD - SQL injection risk
const users = await sql`SELECT * FROM users WHERE email = '${email}'`;
```

### 3. Handle Arrays Properly
```typescript
// ✅ GOOD - Create array in JavaScript
const salesOfficeArray = office ? [office] : null;
await sql`INSERT INTO users (...) VALUES (..., ${salesOfficeArray}, ...)`;

// ❌ BAD - Let SQL try to infer type
await sql`INSERT INTO users (...) VALUES (..., ARRAY[$1], ...)`;
```

### 4. Use Meaningful Aliases
```sql
-- ✅ GOOD - Clear aliases
SELECT u.sales_office[1] AS office
FROM users u
JOIN offices o ON o.name = ANY(u.sales_office)

-- ❌ BAD - Confusing table names
SELECT users.sales_office[1] AS office
FROM users
JOIN offices ON offices.name = ANY(users.sales_office)
```

### 5. Check Existing Code Patterns
Before writing new queries, search for similar examples:
```bash
# Find SQL queries for users table
grep -r "FROM users" app/api --include="*.ts"

# Find office filtering patterns
grep -r "sales_office" app/api --include="*.ts"
```

## Common Pitfalls

### Pitfall 1: Using `office` Instead of `sales_office`
```sql
-- ❌ ERROR: column "office" does not exist
SELECT office FROM users;

-- ✅ CORRECT
SELECT sales_office[1] AS office FROM users;
```

**Why this happens**: The column is actually named `sales_office` (an array). Some older docs/examples may incorrectly refer to it as `office`.

**How to avoid**: Always check the migration files or existing code before writing SQL queries.

### Pitfall 2: Treating `sales_office` as a String
```sql
-- ❌ ERROR: operator does not exist: text[] = text
WHERE sales_office = 'Phoenix Office'

-- ✅ CORRECT
WHERE 'Phoenix Office' = ANY(sales_office)
```

### Pitfall 3: Array Parameter Type Inference
```typescript
// ❌ ERROR: Argument of type 'number[]' is not assignable to parameter of type 'Primitive'
const users = await sql`
  WHERE office_id = ANY(${officeIds})
`;

// ✅ CORRECT - Convert to PostgreSQL array format
const officeIdsArray = `{${officeIds.join(',')}}`;
const users = await sql`
  WHERE office_id = ANY(${officeIdsArray}::int[])
`;
```

### Pitfall 4: Forgetting NULL Checks
```sql
-- ❌ Can fail if sales_office is NULL
SELECT sales_office[1] FROM users;

-- ✅ CORRECT - Handle NULL
SELECT COALESCE(sales_office[1], 'Unassigned') AS office FROM users;
```

### Pitfall 5: Office Name vs Office ID
```sql
-- ❌ FRAGILE - Office names can change
WHERE 'Phoenix Office' = ANY(sales_office)

-- ✅ ROBUST - Use stable QuickBase office ID
JOIN offices o ON o.name = ANY(sales_office)
WHERE o.quickbase_office_id = 123
```

## Testing Your Queries

### Local Testing
```bash
# Test with your local database
psql $DATABASE_URL -c "SELECT sales_office FROM users LIMIT 5;"
```

### Integration Tests
Always add integration tests for new API endpoints that query the database:

```typescript
// tests/integration/yourEndpoint.spec.ts
import { test, expect } from '@playwright/test';

test('should fetch users with office correctly', async ({ request }) => {
  const response = await request.get('/api/your-endpoint');
  expect(response.ok()).toBeTruthy();
  const data = await response.json();
  expect(data.users[0].office).toBeDefined();
});
```

## Quick Reference

| Need to... | Pattern |
|-----------|---------|
| Get user's first office | `sales_office[1] AS office` |
| Check if user in office | `WHERE 'Office' = ANY(sales_office)` |
| Filter by office ID | `JOIN offices o ON o.name = ANY(u.sales_office) WHERE o.quickbase_office_id = ?` |
| Handle office array param | `const arr = \`{${ids.join(',')}}\`; WHERE id = ANY(${arr}::int[])` |
| Get all user offices | `SELECT sales_office FROM users` |
| Count user's offices | `array_length(sales_office, 1)` |

## Migration History

Key migrations affecting schema:

- `001_initial_schema.sql` - Created users table with `sales_office TEXT[]`
- `003_offices_schema.sql` - Created offices table
- `006_user_hierarchies_schema.sql` - Added team lead relationships
- `009_add_office_quickbase_id.sql` - Added stable office IDs
- `010_add_operations_roles.sql` - Extended role enum

## Additional Resources

- [Vercel Postgres Documentation](https://vercel.com/docs/storage/vercel-postgres)
- [PostgreSQL Array Functions](https://www.postgresql.org/docs/current/functions-array.html)
- Migration files: `/lib/db/migrations/`
- Example queries: Search codebase with `grep -r "FROM users" app/api`

---

**Last Updated**: October 2025
**Maintainer**: Development Team
**Questions?**: Check existing code patterns or ask in team chat
