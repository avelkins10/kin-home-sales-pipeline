# Office-Based Visibility Guide

## Overview

The Kin Home Sales Pipeline PWA implements a sophisticated **office-based visibility** system that allows managers to see ALL projects in their assigned offices, regardless of whether the closer/setter has an active account in the application. This design solves the challenge of managing thousands of historical sales reps while ensuring managers have complete visibility into their territories.

## How It Works

### Core Principle

**Office-based roles filter by `project.sales_office`, NOT by `user.is_active`**

When a manager with an office-based role logs in, the system queries QuickBase with a filter like:
```
{SALES_OFFICE}.EX.'Phoenix' OR {SALES_OFFICE}.EX.'Tucson'
```

This matches the project's office field, not the user's account status. The result: managers see ALL projects in their offices, including projects where the rep doesn't have an active account in the app.

### Office-Based Roles

| Role | Scope | Visibility |
|------|-------|------------|
| **Office Leader** | 1+ offices | ALL projects in assigned offices |
| **Area Director** | 3-5 offices | ALL projects in assigned offices |
| **Divisional Manager** | 5-10+ offices | ALL projects in assigned offices/regions |
| **Regional Manager** | All offices | ALL projects across all offices |

### User-Based Roles (for comparison)

| Role | Scope | Visibility |
|------|-------|------------|
| **Closer** | Own projects | Only projects where they are the closer |
| **Setter** | Own projects | Only projects where they are the setter |
| **Team Lead** | Managed users | Projects for their managed reps (both closer and setter) |

## Example Scenarios

### Scenario 1: Phoenix Office Manager
- **Situation**: Phoenix office has 50 active projects
  - 10 projects from closers with app accounts
  - 40 projects from closers without app accounts
- **Result**: Office leader sees all 50 projects
- **Why**: Query filters by `project.sales_office = 'Phoenix'`, not by user accounts

### Scenario 2: Area Director
- **Situation**: Area director manages Phoenix, Tucson, and Las Vegas offices
- **Result**: Sees all projects in all three offices, regardless of user accounts
- **Query**: `{SALES_OFFICE}.EX.'Phoenix' OR {SALES_OFFICE}.EX.'Tucson' OR {SALES_OFFICE}.EX.'Las Vegas'`

### Scenario 3: Historical Rep
- **Situation**: Closer from 2022 has no app account but has old projects
- **Result**: Their projects still show up for the office leader
- **Why**: Projects are filtered by office, not by whether the closer has an account

## Why This Design?

### The Problem
- Thousands of historical sales reps in QuickBase
- Most don't need app access (inactive, left company, etc.)
- Managers need visibility into ALL projects in their territory
- Creating accounts for everyone would clutter the system

### The Solution
- **Managers get accounts** (they need to log in)
- **Active closers/setters get accounts** (they need to track their projects)
- **Inactive closers/setters don't need accounts** (their projects are still visible to managers)
- **Office-based visibility** ensures managers see everything in their territory

## Implementation Details

### Database Schema
```sql
-- Office assignments for managers
CREATE TABLE office_assignments (
  id UUID PRIMARY KEY,
  user_id UUID REFERENCES users(id),
  office_name TEXT NOT NULL,
  access_level TEXT CHECK (access_level IN ('view', 'manage', 'admin')),
  assigned_at TIMESTAMP DEFAULT NOW()
);

-- User hierarchies for team leads
CREATE TABLE user_hierarchies (
  id UUID PRIMARY KEY,
  manager_id UUID REFERENCES users(id),
  user_id UUID REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW()
);
```

### Query Logic
```typescript
// Office-based roles see ALL projects in their offices
case 'office_leader':
case 'area_director':
case 'divisional':
  if (salesOffice && salesOffice.length > 0) {
    clause = buildOfficeClause(salesOffice); // Filters by project.sales_office
  }
  break;

// Team leads see projects for their managed users
case 'team_lead':
  if (managedUserIds && managedUserIds.length > 0) {
    const closerClause = buildClause(PROJECT_FIELDS.CLOSER_ID, managedUserIds);
    const setterClause = buildClause(PROJECT_FIELDS.SETTER_ID, managedUserIds);
    clause = `(${closerClause}) OR (${setterClause})`;
  }
  break;
```

## Smart User Provisioning

### The Challenge
- Thousands of historical reps in QuickBase
- Most don't need app access
- Need to avoid creating thousands of irrelevant accounts

### Recommended Approaches

#### 1. Invite-Based Provisioning (Recommended)
- Admin invites specific users by email
- User receives invite link, sets password, account created
- System automatically matches to QuickBase data
- **Benefits**: Only creates users who need access, no clutter

#### 2. QuickBase Lookup (Manual Creation)
- Admin searches QuickBase, selects user, creates account
- System auto-fills data from QuickBase
- Activity filter shows only users with recent projects
- **Benefits**: Admin control, no bulk sync needed

#### 3. Smart Sync (Activity-Based)
- Sync only users with projects in last N months
- Configurable activity threshold (3, 6, 12 months)
- Dry run mode to preview changes
- **Benefits**: Automated but filtered, avoids inactive users

## Who Needs an Account?

| User Type | Needs Account? | Reason |
|-----------|----------------|---------|
| **Managers** (office_leader, area_director, divisional, regional) | ✅ YES | They need to log in to see projects |
| **Active closers/setters** | ✅ YES | They need to track their projects |
| **Inactive closers/setters** | ❌ NO | Their projects are visible to managers via office-based visibility |
| **Historical reps** | ❌ NO | Their old projects are visible to managers, but they don't need access |

## FAQ

### Q: Why don't I see a closer's name in the user list?
**A**: They don't have an account, but their projects are still visible to you through office-based visibility.

### Q: How do I give someone access to multiple offices?
**A**: Assign them the area_director or divisional role and select multiple offices in their profile.

### Q: What if a closer becomes inactive?
**A**: Their account can be deactivated, but their projects remain visible to managers because visibility is based on project.sales_office, not user.is_active.

### Q: Can I see projects from users who left the company?
**A**: Yes, if the projects are in your assigned offices. Office-based visibility shows ALL projects in your territory, regardless of user status.

### Q: How do team leads work differently?
**A**: Team leads use user-based visibility - they see projects for specific users they manage, not all projects in offices.

## Best Practices

1. **Start with invite-based provisioning** for managers and active reps
2. **Use smart sync for initial bulk import** (6 month threshold)
3. **Don't create accounts for inactive/historical reps**
4. **Run activity update monthly** to keep data fresh
5. **Deactivate inactive users quarterly** to keep user list clean
6. **Review user list regularly** to ensure only active users have accounts

## Technical Notes

- Office-based roles filter by `project.sales_office` field in QuickBase
- User-based roles filter by `project.closer_id` or `project.setter_id` fields
- The `buildRoleClause` function in `lib/quickbase/queries.ts` handles the filtering logic
- Activity tracking uses `last_project_date` field to determine user activity
- Smart provisioning prevents creating thousands of irrelevant user accounts

This office-based visibility system ensures managers have complete visibility into their territories while keeping the user list clean and manageable.
