# User Provisioning Guide

## Overview

This guide explains how to provision users in the Kin Home Sales Pipeline PWA while avoiding the "thousands of users" problem. The system is designed to only create accounts for users who actually need access, while ensuring managers have complete visibility into their territories through office-based visibility.

## The Challenge

- **Thousands of historical reps** in QuickBase from past years
- **Most don't need app access** (inactive, left company, seasonal workers)
- **Managers need visibility** into ALL projects in their offices
- **Creating accounts for everyone** would clutter the system and create security risks

## The Solution: Smart User Provisioning

### Core Principle
**Only create accounts for users who need to log in. Managers see ALL projects in their offices regardless of user accounts.**

This is achieved through:
1. **Office-based visibility** - managers see projects by office, not by user accounts
2. **Activity-based filtering** - only sync users with recent project activity
3. **Invite-based provisioning** - manually invite specific users who need access

## Three Recommended Approaches

### 1. Invite-Based Provisioning (Recommended)

**How it works:**
- Admin invites specific users by email
- User receives invite link, sets password, account created
- System automatically matches to QuickBase data

**Benefits:**
- Only creates users who need access
- No clutter from inactive users
- Admin has full control
- User gets proper onboarding

**When to use:**
- For managers (office_leader, area_director, divisional, regional)
- For active closers/setters who need to track their projects
- For new hires who need immediate access

**Step-by-step:**
1. Go to Admin → Users → "Invite User"
2. Enter email, name, role, office(s)
3. System generates invite link
4. User receives email with link
5. User clicks link, sets password, account created
6. System auto-fills QuickBase data (phone, office, etc.)

### 2. QuickBase Lookup (Manual Creation)

**How it works:**
- Admin searches QuickBase for specific users
- System auto-fills data from QuickBase
- Admin creates account with pre-filled information

**Benefits:**
- Admin control over who gets accounts
- Auto-fill from QuickBase data
- Activity filter shows only users with recent projects
- No bulk sync needed

**When to use:**
- For specific users who need access
- When you want to review user data before creating account
- For users not found in recent activity sync

**Step-by-step:**
1. Go to Admin → Users → "Add User Manually"
2. Search QuickBase by name, email, or ID
3. Review user data and activity
4. Select user and create account
5. System creates account with QuickBase data

### 3. Smart Sync (Activity-Based)

**How it works:**
- Sync only users with projects in last N months
- Configurable activity threshold (3, 6, 12 months)
- Dry run mode to preview changes
- Batch processing with rate limiting

**Benefits:**
- Automated but filtered
- Avoids inactive users
- Configurable activity threshold
- Preview before applying changes

**When to use:**
- Initial setup with existing active users
- Periodic updates (monthly/quarterly)
- When you need to bulk import active users

**Step-by-step:**
1. Go to Admin → Users → "Smart Sync"
2. Choose activity timeframe (3, 6, 12 months)
3. Select role filter (closers, setters, both)
4. Run dry run to preview changes
5. Review results and apply if satisfied

## Who Needs an Account?

### ✅ YES - Create Account

| User Type | Reason | Method |
|-----------|--------|---------|
| **Managers** (office_leader, area_director, divisional, regional) | Need to log in to see projects | Invite-based |
| **Active closers/setters** | Need to track their projects | Invite-based or Smart Sync |
| **New hires** | Need immediate access | Invite-based |
| **Team leads** | Need to manage their reps | Invite-based |

### ❌ NO - Don't Create Account

| User Type | Reason | Alternative |
|-----------|--------|-------------|
| **Inactive closers/setters** | Don't need access | Projects visible to managers via office-based visibility |
| **Historical reps** | Left company or inactive | Projects visible to managers via office-based visibility |
| **Seasonal workers** | Not currently active | Projects visible to managers via office-based visibility |
| **Test users** | Not real users | N/A |

## Office-Based Visibility

### How It Works
Managers see ALL projects in their assigned offices, regardless of whether the closer/setter has an active account. This is because the system filters by `project.sales_office`, not by `user.is_active`.

### Example
- Phoenix office has 50 projects
- 10 projects from closers with app accounts
- 40 projects from closers without app accounts
- **Result**: Phoenix office leader sees all 50 projects

### Why This Matters
- You don't need to create accounts for every closer/setter
- Managers have complete visibility into their territory
- System stays clean with only active users
- Historical projects remain visible to managers

## Activity Filtering

### Activity Categories
- **Active (0-6 months)**: Users with projects in last 6 months
- **Inactive (6-12 months)**: Users with projects 6-12 months ago
- **Dormant (12+ months)**: Users with no projects in last 12 months

### Recommended Thresholds
- **Initial sync**: 6 months (catches most active users)
- **Monthly updates**: 3 months (keeps data fresh)
- **Quarterly cleanup**: 12 months (removes truly inactive users)

## User Lifecycle Management

### 1. Provisioning
- Use invite-based for managers and active reps
- Use smart sync for bulk import (6 month threshold)
- Don't create accounts for inactive users

### 2. Monitoring
- Run activity update monthly
- Review user list quarterly
- Monitor login patterns

### 3. Cleanup
- Deactivate users with no activity for 12+ months
- Remove test accounts
- Archive old invite tokens

### 4. Maintenance
- Update user data from QuickBase
- Sync office assignments
- Review hierarchy relationships

## Best Practices

### Do's
- ✅ Start with invite-based provisioning
- ✅ Use 6-month threshold for initial sync
- ✅ Run activity updates monthly
- ✅ Deactivate inactive users quarterly
- ✅ Review user list regularly
- ✅ Use office-based visibility for managers

### Don'ts
- ❌ Don't bulk sync all users from QuickBase
- ❌ Don't create accounts for inactive users
- ❌ Don't ignore activity thresholds
- ❌ Don't skip regular cleanup
- ❌ Don't create accounts for test users

## Troubleshooting

### Common Issues

**Q: User not found in QuickBase lookup**
- A: Check spelling, try different search terms, verify user exists in QuickBase

**Q: User has multiple QuickBase IDs**
- A: System handles comma-separated IDs automatically

**Q: User's email changed**
- A: Update email in QuickBase first, then sync or create new account

**Q: User needs access to multiple offices**
- A: Assign area_director or divisional role with multiple office assignments

**Q: Manager can't see projects**
- A: Check office assignments, verify role has office-based visibility

### Error Messages

**"Email already in use"**
- User already has an account, check existing users

**"Manager role is not appropriate"**
- Only certain roles can manage users (team_lead, office_leader, etc.)

**"User is already managed by someone else"**
- User already has a manager, remove existing relationship first

**"Invite has expired"**
- Invite tokens expire after 7 days, send new invite

## API Reference

### Endpoints

- `POST /api/admin/users/invite` - Send user invite
- `GET /api/admin/users/lookup` - Search QuickBase users
- `POST /api/admin/users/sync` - Smart sync active users
- `POST /api/admin/users/deactivate-inactive` - Deactivate inactive users
- `POST /api/admin/hierarchies` - Assign team lead relationships

### Scripts

- `npm run migrate:hierarchies` - Run database migration
- `npm run update:user-activity` - Update user activity data

## Security Considerations

- Invite tokens expire after 7 days
- Passwords must be at least 8 characters
- Only super_admin can bulk deactivate users
- All operations are logged for audit trail
- Rate limiting prevents API abuse

## Performance

- Smart sync processes users in batches of 10
- 2-second delay between batches to respect QuickBase API limits
- Activity updates run monthly to keep data fresh
- Office-based queries are optimized with proper indexes

This user provisioning strategy ensures you only create accounts for users who actually need access while maintaining complete visibility for managers through office-based filtering.
