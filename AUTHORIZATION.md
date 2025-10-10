# Authorization System Documentation

## Overview

This application uses **EMAIL-BASED** authorization for project access. This document explains how authorization works and how to modify it safely.

## ⚠️ CRITICAL: Single Source of Truth

**ALL** authorization logic is centralized in these modules:

1. `/lib/auth/userIdentity.ts` - Defines how users are identified
2. `/lib/auth/projectAuthorization.ts` - Defines project access rules

**DO NOT duplicate authorization logic elsewhere!**

## How It Works

### User Identification

Users are identified by their **email address** (not QuickBase User ID). This allows:
- Users with multiple QuickBase User IDs to see all their projects
- Admin/ops users without QuickBase accounts to access the system
- Consistent authorization across all features

### Project Access Rules

Authorization hierarchy (most to least permissive):

1. **super_admin & regional**: See ALL projects (no filtering)
2. **office_leader, area_director, divisional**: See all projects in assigned office(s)
3. **team_lead**: See projects for managed users (by email)
4. **closer & setter**: See only projects where email matches CLOSER_EMAIL or SETTER_EMAIL
5. **coordinator**: Limited support (legacy - needs email field added to QuickBase)

### QuickBase Field Mapping

Projects are filtered using these QuickBase fields:
- Field 518 = CLOSER_EMAIL
- Field 331 = SETTER_EMAIL
- Field 287 = SALES_OFFICE (for office-based filtering)

## Code Structure

### Centralized Modules

#### `/lib/auth/userIdentity.ts`

Provides functions to extract user information from session:
- `getUserEmail(session)` - Get user's email (normalized to lowercase)
- `getUserRole(session)` - Get user's role
- `getUserDatabaseId(session)` - Get database UUID
- `isAdmin(session)` - Check if user is admin
- `isRep(session)` - Check if user is closer/setter

#### `/lib/auth/projectAuthorization.ts`

Provides authorization functions:
- `canUserAccessProject(session, project)` - Check if user can access a specific project
- `hasUnrestrictedAccess(session)` - Check if user has blanket access (optimization)
- `buildProjectAccessClause(email, role, offices, managedEmails)` - Build QuickBase WHERE clause
- `canUserAdministerProjects(session)` - Check if user can perform admin actions

### Authorization Guards

#### `/lib/auth/guards.ts`

Provides guard functions for API routes:
- `requireAuth()` - Require authenticated user
- `requireRole(roles[])` - Require specific role(s)
- `requireProjectAccessById(projectId)` - Require access to specific project

### QuickBase Queries

#### `/lib/quickbase/queries.ts`

All project queries use centralized authorization:
- Imports `buildProjectAccessClause` from projectAuthorization
- No authorization logic duplicated here
- All queries filter by email-based access rules

## API Routes

### Projects API

#### `/app/api/projects/route.ts` (List)
- Uses database user ID to look up email
- Calls QuickBase with email-based filtering
- Returns only accessible projects

#### `/app/api/projects/[id]/route.ts` (Detail)
- Uses `requireProjectAccessById` guard
- Checks email match before returning data
- Returns 403 if no access

### Dashboard APIs

#### `/app/api/dashboard/metrics/route.ts`
- Uses database user ID
- Filters metrics by email-based access
- Returns aggregated data for accessible projects only

## Changing Authorization

### When to Update

You need to update authorization when:
- Changing how users are identified (e.g., switching from email to another field)
- Adding new roles with different access patterns
- Modifying project visibility rules
- Adding new features that need authorization

### How to Update Safely

**Step 1: Update Central Module**

Make changes in `/lib/auth/projectAuthorization.ts` ONLY:

```typescript
// Example: Adding new role
export function buildProjectAccessClause(
  userEmail: string | null,
  role: string,
  salesOffice?: string[],
  managedEmails?: string[]
): string {
  switch (role) {
    // ... existing cases ...

    case 'new_role':
      // Add authorization logic here
      return buildNewRoleClause(userEmail);

    default:
      return '{3.EQ.0}'; // Deny by default
  }
}
```

**Step 2: Test Thoroughly**

Test ALL these scenarios:
- [ ] User WITH QuickBase User ID can access projects
- [ ] User WITHOUT QuickBase User ID can access projects
- [ ] Project list shows only accessible projects
- [ ] Project detail page shows only accessible projects
- [ ] Dashboard metrics show only accessible projects
- [ ] Dashboard recent/urgent show only accessible projects
- [ ] Project notes/messages require project access
- [ ] Admins see all projects
- [ ] Office leaders see only their office's projects
- [ ] Reps see only their own projects

**Step 3: Use This Checklist**

When modifying authorization, check:

- [ ] Updated centralized module (`projectAuthorization.ts`)
- [ ] **DID NOT** add authorization logic in API routes
- [ ] **DID NOT** add authorization logic in QuickBase queries
- [ ] Updated this documentation
- [ ] Added tests for new scenarios
- [ ] Tested project list access
- [ ] Tested individual project access
- [ ] Tested dashboard access
- [ ] Tested with users at each role level

## Common Mistakes to Avoid

### ❌ DON'T: Duplicate Authorization Logic

```typescript
// ❌ BAD: Authorization logic in API route
export async function GET(req: Request) {
  const auth = await requireAuth();
  const role = auth.session.user.role;

  // ❌ DON'T do this - use centralized function!
  if (role === 'closer') {
    // filter by email...
  }
}
```

### ✅ DO: Use Centralized Functions

```typescript
// ✅ GOOD: Use centralized authorization
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { getUserEmail, getUserRole } from '@/lib/auth/userIdentity';

export async function GET(req: Request) {
  const auth = await requireAuth();
  const email = getUserEmail(auth.session);
  const role = getUserRole(auth.session);

  const whereClause = buildProjectAccessClause(email, role, offices, managedEmails);
  // Use whereClause in query
}
```

### ❌ DON'T: Use quickbaseUserId for Authorization

```typescript
// ❌ BAD: Using QuickBase User ID
const userId = session.user.quickbaseUserId; // May be null!
if (userId === project.closerId) { ... }
```

### ✅ DO: Use Email for Authorization

```typescript
// ✅ GOOD: Using email
const email = getUserEmail(session);
const closerEmail = project[518]?.value?.toLowerCase();
if (email === closerEmail) { ... }
```

## Debugging Authorization Issues

### Symptom: User sees wrong projects

**Check these in order:**

1. **Is the correct user ID being passed?**
   ```typescript
   // API route should use DATABASE user ID, not QuickBase User ID
   const { id } = auth.session.user;  // ✅ Correct
   const userId = id as string;
   ```

2. **Is email lookup working?**
   ```sql
   -- Check user has email in database
   SELECT id, email, role FROM users WHERE email = 'user@example.com';
   ```

3. **Is the WHERE clause correct?**
   ```typescript
   // Add logging to see generated clause
   console.log('[DEBUG] WHERE clause:', whereClause);
   // Should show email-based filtering like:
   // ({518}.EX.'user@example.com') OR ({331}.EX.'user@example.com')
   ```

4. **Is caching an issue?**
   ```bash
   # Clear project cache if needed
   # Cache keys use user ID, so logout/login refreshes cache
   ```

### Symptom: 403 Forbidden on project detail

**Check:**

1. User has access to project in list (proves filtering works)
2. Individual project auth uses same logic
3. Project email fields match user email

## Migration History

### Previous System (Deprecated)
- Used QuickBase User IDs for authorization
- Failed for users with multiple QB User IDs
- Failed for admin/ops users without QB accounts

### Current System (Active)
- Uses email addresses for authorization
- Works for users with 0, 1, or multiple QB User IDs
- Supports admin/ops users without QB accounts
- Single source of truth in projectAuthorization module

## Support

If you need to modify authorization and aren't sure how:

1. Read this document thoroughly
2. Review code in `/lib/auth/projectAuthorization.ts`
3. Check example implementations in API routes
4. Test changes with multiple user types
5. Ask for code review before deploying

---

**Remember**: Authorization is security-critical. When in doubt, ask for review!
