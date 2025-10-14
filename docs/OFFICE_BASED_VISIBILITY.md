# Office-Based Visibility Model

This document explains how office-based visibility works for managers and how it differs from user-based visibility for reps.

## Overview

The system uses two distinct visibility models:

- **User-based visibility**: Reps (closer/setter) see only their own projects (filtered by email)
- **Office-based visibility**: Managers (office_leader/area_director/divisional) see ALL projects in their assigned offices

This design ensures that managers can see team projects even if team members are inactive or haven't logged in, which is essential for project management and oversight.

## How Office Assignments Work

### Database Structure

Offices are assigned via the `office_assignments` table:
- Each row maps a `user_id` to an `office_name`
- Admins assign offices using the bulk assignment API or UI
- Assignments are fetched fresh from the database on every project query (no session caching)

### Assignment Process

1. Admin uses bulk assignment API: `POST /api/admin/office-assignments/bulk`
2. `office_assignments` table is updated with new assignments
3. `users.sales_office` array is updated to match
4. Manager's next project query fetches fresh offices from database
5. Manager immediately sees projects from new office (no logout required)

## Visibility Rules by Role

### super_admin / regional
- **Access**: See ALL projects (no filtering)
- **WHERE clause**: `{3.GT.0}` (record ID > 0)

### office_leader / area_director / divisional
- **Access**: See ALL projects in their assigned offices
- **WHERE clause**: `{2087.EX.'Office A'} OR {2087.EX.'Office B'}` (filters by SALES_OFFICE field)
- **Important**: Does NOT filter by `user.is_active` or user email
- **No offices assigned**: See NO projects (WHERE clause: `{3.EQ.0}`)

### team_lead
- **Access**: See projects for their managed users (user-based visibility)
- **WHERE clause**: `({518.EX.'user1@example.com'} OR {518.EX.'user2@example.com'}) OR ({331.EX.'user1@example.com'} OR {331.EX.'user2@example.com'})` (filters by CLOSER_EMAIL and SETTER_EMAIL)
- **Managed users**: Defined in the `user_hierarchies` table

### closer / setter
- **Access**: See only their own projects (user-based visibility)
- **WHERE clause**: `({518.EX.'myemail@example.com'}) OR ({331.EX.'myemail@example.com'})` (filters by their email)

## Key Implementation Details

### Database Tables

- **`office_assignments`**: Maps users to offices (user_id, office_name, access_level)
- **`user_hierarchies`**: Maps team leads to managed users (manager_id, user_id)
- **`users`**: Stores user data including `sales_office` array (kept in sync with office_assignments)

### Code Flow

1. User requests projects via `/api/projects`
2. API calls `getProjectsForUserList(userId, role)`
3. For office-based roles, function calls `getAssignedOffices(userId)` to fetch offices from database
4. Function calls `buildProjectAccessClause(userEmail, role, offices, managedEmails)` to build WHERE clause
5. WHERE clause is used in QuickBase query to filter projects

### Why We Don't Use Session

Previously, `salesOffice` was stored in the session JWT, which caused stale data issues:
- Newly assigned offices weren't visible until logout/login
- Session data could become outdated

Now, offices are always fetched fresh from the database:
- Ensures immediate visibility of newly assigned offices
- Single source of truth (database) instead of dual sources (session + database)

## Common Scenarios

### Scenario 1: Admin assigns new office to manager
1. Admin uses bulk assignment API
2. `office_assignments` table is updated
3. `users.sales_office` array is updated
4. Manager's next project query fetches fresh offices from database
5. Manager immediately sees projects from new office (no logout required)

### Scenario 2: Rep becomes inactive
1. Admin sets `user.is_active = false`
2. Rep can no longer log in
3. Rep's projects remain visible to their office manager
4. This allows managers to reassign or complete the rep's projects

### Scenario 3: Manager has no offices assigned
1. Manager logs in successfully
2. Manager's project query returns empty array
3. WHERE clause is `{3.EQ.0}` (no projects)
4. Admin should assign offices to fix this

## Troubleshooting

### Problem: Manager can't see expected projects
- **Check**: Does manager have offices assigned in `office_assignments` table?
- **Check**: Do the projects have the correct `SALES_OFFICE` value in QuickBase?
- **Check**: Are the office names spelled exactly the same (case-sensitive)?
- **Check**: Look at logs for `[OFFICE_ASSIGNMENT_RESOLUTION]` messages

### Problem: Manager sees projects from wrong office
- **Check**: Manager's office assignments in `office_assignments` table
- **Check**: Project's `SALES_OFFICE` field value in QuickBase
- **Check**: WHERE clause in logs to see what filter was applied

### Problem: Newly assigned office not visible
- **Check**: Was the assignment API call successful?
- **Check**: Is the office name in `office_assignments` table?
- **Check**: Is the office name in `users.sales_office` array?
- **Check**: Look at logs for `[PROJECT_QUERY] Fetched offices from database` message
- **If still not working**: Verify the API route is NOT passing `salesOffice` from session

## Related Files

- **`lib/auth/projectAuthorization.ts`**: Authorization logic and WHERE clause building
- **`lib/quickbase/queries.ts`**: Query functions and office fetching
- **`app/api/projects/route.ts`**: API endpoint for fetching projects
- **`app/api/admin/office-assignments/bulk/route.ts`**: API for assigning offices
- **`lib/db/migrations/006_user_hierarchies_schema.sql`**: Database schema

## Testing

Refer to integration tests:
- **`tests/integration/authorization.projectsList.spec.ts`**: Authorization logic tests
- **`tests/integration/officeAssignmentFlow.spec.ts`**: End-to-end office assignment flow
- **`tests/integration/officeVisibilityVerification.spec.ts`**: Office-based visibility verification

## Field Mappings

- **SALES_OFFICE**: Field 2087 (used for office-based filtering)
- **CLOSER_EMAIL**: Field 518 (used for user-based filtering)
- **SETTER_EMAIL**: Field 331 (used for user-based filtering)

## Security Considerations

- Office assignments are always fetched fresh from database (no caching)
- WHERE clauses are properly escaped to prevent SQL injection
- Email addresses are masked in logs for privacy compliance
- Authorization logic is centralized in `projectAuthorization.ts`
