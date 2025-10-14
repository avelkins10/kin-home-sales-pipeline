# Manager Setup Guide - Kin Home Sales Pipeline

## Overview

This guide provides step-by-step instructions for admins to set up managers and office assignments using the UI (not API/SQL). The system supports two types of managers:

### Manager Types

| Manager Type | Role | Visibility Method | Use Case |
|--------------|------|-------------------|----------|
| **User-Based** | team_lead | Manages specific users | Specialized teams, cross-office teams |
| **Office-Based** | office_leader, area_director, divisional | Manages offices/regions | Geographic management, office oversight |

### Prerequisites

- Admin access to the system
- Understanding of your organization structure
- List of offices and their names (must match QuickBase exactly)
- List of users who need manager roles

## Section 1: Setting Up Team Leads (User-Based Managers)

Team leads manage specific users and see projects from those users only.

### Step 1: Create the Team Lead User

1. **Navigate to Settings → Users tab**
2. **Click "Add User" button**
3. **Fill in the form**:
   - Name: Full name of the team lead
   - Email: Their email address (must match QuickBase user email)
   - Role: Select "Team Lead" from dropdown
   - Office: Their personal office (primary location)
4. **Click "Create User"**
5. **Verify**: User appears in users list with "Team Lead" role

### Step 2: Assign Reps to the Team Lead

#### Method 1: Drag and Drop (Recommended for 1-4 users)

1. **Navigate to Settings → Hierarchy tab**
2. **Find the team lead** in the tree view (use search if needed)
3. **Find a rep** (closer or setter) in the tree
4. **Drag the rep's card** onto the team lead's card
5. **Drop when you see the green border**
6. **Confirm the assignment** in the success toast
7. **Verify**: Team lead's card now shows "Manages X users"

#### Method 2: Bulk Assign (Recommended for 5+ users)

1. **Navigate to Settings → Hierarchy tab**
2. **Right-click the team lead's card**
3. **Select "Assign Users"** from context menu
4. **In the dialog**:
   - Select multiple reps using checkboxes
   - Review the selection
   - Click "Assign Users" button
5. **Verify**: Team lead's card shows correct user count

### Step 3: Verify the Assignment

1. **Expand the team lead's node** (click chevron icon)
2. **Verify all assigned reps** appear as children
3. **Check the "Manages X users" badge** shows correct count
4. **Test visibility**: Log in as the team lead and verify they see their team's projects

### Common Mistakes

- ❌ **Assigning office leaders to team leads**: Not allowed - role hierarchy violation
- ❌ **Forgetting to verify**: Always check the assignment worked
- ❌ **Not checking existing assignments**: User might already be managed by someone else

## Section 2: Setting Up Office Leaders (Office-Based Managers)

Office leaders manage offices and see all projects in those offices.

### Step 1: Create the Office Leader User

1. **Navigate to Settings → Users tab**
2. **Click "Add User" button**
3. **Fill in the form**:
   - Name: Full name of the office leader
   - Email: Their email address
   - Role: Select "Office Leader" from dropdown
   - Office: Their primary office
4. **Click "Create User"**

### Step 2: Assign Offices to the Office Leader

1. **Navigate to Settings → Offices tab**
2. **Click "Bulk Assign Offices" button**
3. **In the dialog**:
   - Select the office leader from the managers list (use checkbox)
   - Select one or more offices from the office dropdown
   - Choose access level: "Manage" (recommended for office leaders)
   - Review the preview: "You are about to assign X offices to Y managers"
   - Click "Assign Offices" button

### Step 3: Verify the Assignment

1. **Navigate to Settings → Hierarchy tab**
2. **Find the office leader** in the tree
3. **Click the office icon/badge** to expand office details
4. **Verify all assigned offices** appear with correct access levels
5. **Test visibility**: Log in as the office leader and verify they see projects from assigned offices

### Access Level Guidance

| Access Level | Permissions | Recommended For |
|--------------|-------------|------------------|
| **View** | Read-only access to office data | Rarely used, auditors |
| **Manage** | Can manage office operations, see all projects | Office Leaders |
| **Admin** | Full administrative access including user management | Area Directors and above |

## Section 3: Setting Up Area Directors and Divisional Managers

### Step 1: Create the Manager User

1. **Same as office leader** but select "Area Director" or "Divisional" role
2. **Area directors** typically manage 3-5 offices
3. **Divisional managers** typically manage 5-10+ offices

### Step 2: Assign Multiple Offices

1. **Use the same bulk assign offices process**
2. **Select multiple offices** (hold Cmd/Ctrl to multi-select)
3. **Choose access level**:
   - "Admin" for divisional managers
   - "Manage" for area directors

### Step 3: Verify Visibility

1. **Log in as the manager**
2. **Check dashboard** shows aggregated metrics from all assigned offices
3. **Verify project list** includes projects from all offices
4. **Use the ownership filter** to distinguish personal vs team projects

## Section 4: Nested Hierarchies (Team Leads Reporting to Office Leaders)

### When to Use

When team leads need oversight from office leaders.

### Setup Workflow

1. **Create team lead and assign reps** (Section 1)
2. **Create office leader and assign offices** (Section 2)
3. **Assign team lead to office leader**:
   - Navigate to Settings → Hierarchy tab
   - Drag the team lead's card onto the office leader's card
   - Verify the team lead appears as a child of the office leader

### Visibility Impact

- **Office leader sees**: ALL projects in assigned offices (office-based) + projects from team lead's managed users (user-based, inherited)
- **Team lead sees**: Only their managed users' projects

## Section 5: Bulk Operations for Large Teams

### Scenario: Assigning 20 Reps to a New Team Lead

1. **Navigate to Settings → Hierarchy tab**
2. **Click "Bulk Select" button** in toolbar
3. **Checkboxes appear** on all user cards
4. **Click each rep's card** to select them (or use "Select All Visible" after filtering)
5. **Click "Assign Selected" button** in the floating action bar
6. **Select the team lead** from the manager dropdown
7. **Review the preview** showing all selected users
8. **Click "Assign Users" button**
9. **Wait for success toast** (may take a few seconds for large batches)
10. **Verify all reps appear** under the team lead

### Performance Note

Bulk assignments of 50+ users may take 5-10 seconds. The UI will show a loading state.

## Section 6: Verification and Testing

### Database Check (Optional, for admins comfortable with SQL)

```sql
-- Check office assignments
SELECT * FROM office_assignments WHERE user_id = 'manager-id';

-- Check team lead assignments
SELECT * FROM user_hierarchies WHERE manager_id = 'team-lead-id';
```

### UI Verification

1. **Log in as the manager**
2. **Check dashboard** shows team metrics (not just personal)
3. **Verify project list** includes team projects
4. **Test ownership filter** (My Projects vs Team Projects)
5. **Check team activity feed** shows recent team updates

### Authorization Verification

1. **Log in as a rep** under the manager
2. **Verify rep sees** only their own projects
3. **Log in as the manager**
4. **Verify manager sees** rep's projects

## Section 7: Common Setup Scenarios

### Scenario A: New Office Opening

1. Create office in Settings → Offices tab
2. Create office leader user
3. Assign new office to office leader
4. Create team lead users for the office
5. Assign team leads to office leader (nested hierarchy)
6. Assign reps to team leads
7. Verify office leader sees all office projects

### Scenario B: Reorganizing Existing Team

1. Identify users to move
2. Remove existing assignments (right-click → Remove Assignment)
3. Assign to new manager (drag-and-drop or bulk assign)
4. Verify users appear under new manager
5. Notify affected users of the change

### Scenario C: Promoting a Rep to Team Lead

1. Update user's role: Settings → Users → Edit → Change role to "Team Lead"
2. Assign reps to the new team lead (Section 1)
3. If the team lead should report to an office leader, assign that relationship
4. Verify the team lead sees their managed users' projects

## Section 8: Best Practices

### Hierarchy Design

- **Keep team sizes manageable**: 5-10 direct reports for team leads
- **Use office-based visibility** for office leaders and above (simpler, more scalable)
- **Use user-based visibility** (team leads) for specialized teams or cross-office teams
- **Document your org structure** in a separate diagram for reference

### Office Assignments

- **Start with one office** per office leader, expand as needed
- **Use "Manage" access level** for most managers
- **Reserve "Admin" access** for divisional and above
- **Review office assignments quarterly** to ensure they're current

### Bulk Operations

- **Use bulk assign for 5+ users** (faster than drag-and-drop)
- **Use drag-and-drop for 1-4 users** (more visual)
- **Test with a small batch first** before bulk assigning 50 users
- **Export hierarchy before making large changes** (backup)

## Section 9: Related Documentation

For more details, see:

- **USER-HIERARCHY-GUIDE.md**: Technical details on hierarchy structure and API reference
- **OFFICE_BASED_VISIBILITY.md**: Deep dive on office-based visibility model
- **HIERARCHY-TROUBLESHOOTING.md**: Comprehensive troubleshooting guide
- **MANAGER-ONBOARDING-CHECKLIST.md**: Checklist for onboarding new managers
- **ROLE-HIERARCHY-REFERENCE.md**: Quick reference for roles and visibility rules
