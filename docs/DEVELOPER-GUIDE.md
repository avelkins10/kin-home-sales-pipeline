# Developer Guide

Welcome to the Kin Home Sales Pipeline Dashboard development guide. This document provides essential information for developers contributing to this codebase.

## Table of Contents
- [Getting Started](#getting-started)
- [Code Organization](#code-organization)
- [Database Development](#database-development)
- [API Development](#api-development)
- [Testing Requirements](#testing-requirements)
- [Common Patterns](#common-patterns)
- [Troubleshooting](#troubleshooting)

## Getting Started

### Prerequisites
- Node.js 18+
- npm
- PostgreSQL (via Neon)
- QuickBase account access

### Initial Setup
```bash
# 1. Clone and install
npm install

# 2. Setup environment
cp env.example .env.local
# Edit .env.local with your credentials

# 3. Run automated setup
npm run setup:all

# 4. Start development
npm run dev
```

See [SETUP.md](../SETUP.md) for detailed setup instructions.

## Code Organization

```
├── app/                      # Next.js App Router
│   ├── (auth)/              # Authentication pages
│   ├── (sales)/             # Sales app route group
│   ├── (operations)/        # Operations app route group
│   └── api/                 # API routes
├── components/              # React components
│   ├── ui/                  # shadcn/ui components
│   ├── analytics/           # Analytics-specific components
│   ├── operations/          # Operations-specific components
│   └── layout/              # Layout components
├── lib/                     # Business logic
│   ├── auth/               # Authentication logic
│   ├── db/                 # Database client and migrations
│   ├── quickbase/          # QuickBase API integration
│   ├── repcard/            # RepCard API integration
│   ├── types/              # TypeScript types
│   └── utils/              # Utility functions
├── docs/                    # Documentation
└── tests/                   # Test files
    ├── unit/               # Vitest unit tests
    └── integration/        # Playwright E2E tests
```

## Database Development

### 🚨 CRITICAL: Always Check Schema First

**Before writing ANY SQL query**, check the actual database schema:

1. **Check migration files**: `/lib/db/migrations/*.sql`
2. **Read DATABASE-SCHEMA.md**: [docs/DATABASE-SCHEMA.md](./DATABASE-SCHEMA.md)
3. **Search existing code**: `grep -r "FROM users" app/api --include="*.ts"`

### Common Schema Mistake Example

**❌ WRONG** (causes 500 errors in production):
```typescript
const users = await sql`
  SELECT id, name, email, office  -- ❌ column "office" does not exist!
  FROM users
  WHERE role = ${role}
`;
```

**✅ CORRECT**:
```typescript
const users = await sql`
  SELECT id, name, email, sales_office[1] AS office  -- ✅ extract from array
  FROM users
  WHERE role = ${role}
`;
```

**Why this happens**: The column is `sales_office` (a TEXT array), not `office`.

### Database Best Practices

1. **Use the sql template tag from @vercel/postgres**
   ```typescript
   import { sql } from '@/lib/db/client';

   // ✅ GOOD - Prevents SQL injection
   const result = await sql`SELECT * FROM users WHERE email = ${email}`;
   ```

2. **Handle NULL values**
   ```typescript
   // ✅ GOOD - Use COALESCE for NULL handling
   SELECT COALESCE(sales_office[1], 'Unassigned') AS office FROM users
   ```

3. **Use proper array handling**
   ```typescript
   // ✅ GOOD - Create array in JavaScript
   const officeArray = office ? [office] : null;
   await sql`INSERT INTO users (sales_office) VALUES (${officeArray})`;

   // ✅ GOOD - PostgreSQL array parameter
   const ids = [1, 2, 3];
   const idsArray = `{${ids.join(',')}}`;
   await sql`WHERE id = ANY(${idsArray}::int[])`;
   ```

4. **Type your query results**
   ```typescript
   const users = await sql`SELECT * FROM users` as unknown as User[];
   ```

5. **Use transactions for multi-step operations**
   ```typescript
   await sql.query('BEGIN');
   try {
     await sql`INSERT INTO ...`;
     await sql`UPDATE ...`;
     await sql.query('COMMIT');
   } catch (error) {
     await sql.query('ROLLBACK');
     throw error;
   }
   ```

### Creating Migrations

When you need to change the database schema:

1. **Create a new migration file**:
   ```bash
   # Naming: XXX_description.sql (where XXX is next number)
   touch lib/db/migrations/012_your_change_description.sql
   ```

2. **Write idempotent SQL**:
   ```sql
   BEGIN;

   -- Use IF NOT EXISTS to make migration idempotent
   ALTER TABLE users ADD COLUMN IF NOT EXISTS your_column TEXT;

   -- Create indexes
   CREATE INDEX IF NOT EXISTS idx_users_your_column ON users(your_column);

   -- Add comments
   COMMENT ON COLUMN users.your_column IS 'Description of what this column does';

   COMMIT;
   ```

3. **Test locally**:
   ```bash
   # Run your migration
   npm run migrate:your-migration

   # Verify schema
   psql $DATABASE_URL -c "\d users"
   ```

4. **Update DATABASE-SCHEMA.md** with your changes

## API Development

### Creating New API Routes

1. **Check authentication**:
   ```typescript
   import { requireAuth, requireRole } from '@/lib/auth/guards';

   export async function GET(request: NextRequest) {
     // Require authentication
     const auth = await requireAuth();
     if (!auth.authorized) return auth.response;

     // OR require specific role
     const auth = await requireRole(['super_admin']);
     if (!auth.authorized) return auth.response;

     // Your logic here...
   }
   ```

2. **Validate inputs**:
   ```typescript
   import { z } from 'zod';

   const schema = z.object({
     email: z.string().email(),
     role: z.enum(['closer', 'setter', 'office_leader'])
   });

   const validated = schema.parse(body);
   ```

3. **Add logging**:
   ```typescript
   import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

   export async function GET(request: NextRequest) {
     const start = Date.now();
     const reqId = request.headers.get('x-request-id') || `req-${Date.now()}`;

     try {
       logApiRequest('GET', path, { endpoint: 'my-endpoint', requestId: reqId });

       // Your logic...

       const duration = Date.now() - start;
       logApiResponse('GET', path, duration, { status: 200, requestId: reqId });
       return NextResponse.json(data);
     } catch (error) {
       logError('my-endpoint', error as Error, { requestId: reqId });
       return NextResponse.json({ error: 'Internal server error' }, { status: 500 });
     }
   }
   ```

4. **Handle errors gracefully**:
   ```typescript
   try {
     const data = await fetchData();
     if (!data) {
       return NextResponse.json(
         { error: 'Not found' },
         { status: 404 }
       );
     }
     return NextResponse.json(data);
   } catch (error) {
     logError('endpoint-name', error as Error);
     return NextResponse.json(
       {
         error: 'Internal server error',
         message: error instanceof Error ? error.message : 'Unknown error'
       },
       { status: 500 }
     );
   }
   ```

### API Route Patterns

Refer to existing endpoints for patterns:

| Pattern | Example File |
|---------|-------------|
| User-specific data | `app/api/user/profile/route.ts` |
| Admin operations | `app/api/admin/users/route.ts` |
| QuickBase queries | `lib/quickbase/queries.ts` |
| RepCard integration | `lib/repcard/client.ts` |
| Caching | `app/api/repcard/leaderboard/route.ts` |

## Testing Requirements

### Before Submitting a PR

**You MUST**:
1. ✅ Run type checking: `npm run type-check` or `npx tsc --noEmit`
2. ✅ Run linting: `npm run lint`
3. ✅ Run unit tests: `npm run test:unit`
4. ✅ Test your changes locally in the browser
5. ✅ Check for SQL errors in browser console

**You SHOULD**:
1. Add unit tests for business logic
2. Add integration tests for new API endpoints
3. Test edge cases (NULL values, empty arrays, etc.)
4. Test with different user roles

### Writing Tests

#### Unit Tests (Vitest)
```typescript
// tests/unit/myFeature.test.ts
import { describe, it, expect } from 'vitest';
import { myFunction } from '@/lib/utils/myFeature';

describe('myFunction', () => {
  it('should handle normal case', () => {
    expect(myFunction('input')).toBe('expected');
  });

  it('should handle NULL', () => {
    expect(myFunction(null)).toBe('default');
  });
});
```

Run with: `npm run test:unit`

#### Integration Tests (Playwright)
```typescript
// tests/integration/myEndpoint.spec.ts
import { test, expect } from '@playwright/test';

test('should fetch data successfully', async ({ request }) => {
  const response = await request.get('/api/my-endpoint');
  expect(response.ok()).toBeTruthy();

  const data = await response.json();
  expect(data.users).toBeDefined();
  expect(data.users.length).toBeGreaterThan(0);
});
```

Run with: `npm run test:integration`

## Common Patterns

### Pattern 1: Fetching Data with TanStack Query

```typescript
// In a component
import { useQuery } from '@tanstack/react-query';

export function MyComponent() {
  const { data, isLoading, error } = useQuery({
    queryKey: ['my-data', userId],
    queryFn: async () => {
      const res = await fetch(`/api/my-endpoint?userId=${userId}`);
      if (!res.ok) throw new Error('Failed to fetch');
      return res.json();
    },
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  if (isLoading) return <div>Loading...</div>;
  if (error) return <div>Error: {error.message}</div>;

  return <div>{JSON.stringify(data)}</div>;
}
```

### Pattern 2: Office-Based Filtering

```typescript
// Get users filtered by office IDs
async function getUsersByOffices(officeIds: number[]) {
  if (officeIds.length === 0) {
    // No filter - get all users
    return await sql`
      SELECT id, name, sales_office[1] AS office
      FROM users
    `;
  }

  // Filter by office IDs
  const idsArray = `{${officeIds.join(',')}}`;
  return await sql`
    SELECT DISTINCT u.id, u.name, u.sales_office[1] AS office
    FROM users u
    JOIN offices o ON o.name = ANY(u.sales_office)
    WHERE o.quickbase_office_id = ANY(${idsArray}::int[])
  `;
}
```

### Pattern 3: Role-Based Authorization

```typescript
// In API route
import { requireRole } from '@/lib/auth/guards';

export async function GET(request: NextRequest) {
  const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
  if (!auth.authorized) return auth.response;

  const user = auth.session?.user;

  // Office leaders can only see their offices
  let officeFilter: string[] = [];
  if (user.role === 'office_leader') {
    officeFilter = user.sales_office || [];
  }

  // Fetch data with office filter...
}
```

## Troubleshooting

### Common Issues

#### Issue 1: "Column does not exist" SQL Error

**Symptoms**: 500 error, PostgreSQL error "column X does not exist"

**Cause**: Using wrong column name (e.g., `office` instead of `sales_office`)

**Solution**:
1. Check [DATABASE-SCHEMA.md](./DATABASE-SCHEMA.md)
2. Look at migration files in `/lib/db/migrations/`
3. Search existing code: `grep -r "FROM users" app/api`

#### Issue 2: "Argument of type X not assignable to Primitive"

**Symptoms**: TypeScript error when passing arrays to SQL

**Cause**: Vercel Postgres client can't infer array types automatically

**Solution**:
```typescript
// Convert to PostgreSQL array format
const arr = `{${ids.join(',')}}`;
await sql`WHERE id = ANY(${arr}::int[])`;
```

#### Issue 3: Missing RepCard Data

**Symptoms**: Components show "Not linked to RepCard"

**Cause**: User doesn't have `repcard_user_id` set

**Solution**:
1. Check if user is synced from QuickBase Contacts
2. Run user sync: `npm run sync:contacts`
3. Verify RepCard ID in database: `psql $DATABASE_URL -c "SELECT repcard_user_id FROM users WHERE email = 'user@example.com'"`

#### Issue 4: Office Filter Not Working

**Symptoms**: Users seeing data they shouldn't see

**Causes**:
- Using office name instead of ID (names can change)
- Not joining with offices table
- Forgetting to check user's role for office filtering

**Solution**: See "Pattern 2: Office-Based Filtering" above

### Debugging Tools

```bash
# Check database schema
psql $DATABASE_URL -c "\d users"

# Test SQL query directly
psql $DATABASE_URL -c "SELECT sales_office FROM users LIMIT 5"

# View logs (local development)
npm run dev # Logs appear in terminal

# View logs (production)
vercel logs [deployment-url]

# Run type checking
npx tsc --noEmit

# Check for SQL errors
npm run build # Builds project, catches some SQL template errors
```

## Resources

- [DATABASE-SCHEMA.md](./DATABASE-SCHEMA.md) - Complete database schema reference
- [SETUP.md](../SETUP.md) - Initial setup instructions
- [README.md](../README.md) - Project overview and features
- [TESTING.md](./QA-CHECKLIST.md) - Testing guidelines
- [Next.js Documentation](https://nextjs.org/docs)
- [TanStack Query](https://tanstack.com/query/latest)
- [Vercel Postgres](https://vercel.com/docs/storage/vercel-postgres)
- [QuickBase API](https://developer.quickbase.com/)

## Getting Help

1. **Search existing code**: Most patterns already exist in the codebase
2. **Check documentation**: Start with DATABASE-SCHEMA.md and this guide
3. **Look at similar files**: Find similar functionality and copy the pattern
4. **Ask the team**: If stuck, ask in team chat with:
   - What you're trying to do
   - What you've tried
   - Error messages (full stack trace)
   - Relevant code snippets

---

**Last Updated**: October 2025
**Maintainer**: Development Team
