# Role Hierarchy Reference - Kin Home Sales Pipeline

## Section 1: Role Hierarchy Chart

```
┌─────────────────────────────────────────────────────────────┐
│                      ROLE HIERARCHY                          │
│                                                              │
│  Super Admin (👑)                                            │
│  └── Regional Manager (🌍)                                   │
│      ├── Divisional Manager (🏢)                             │
│      │   └── Area Director (📊)                              │
│      │       └── Office Leader (🏪)                          │
│      │           └── Team Lead (👥)                          │
│      │               ├── Closer (🤝)                         │
│      │               └── Setter (📞)                         │
│      └── Coordinator (📋) [Special role, no hierarchy]       │
└─────────────────────────────────────────────────────────────┘
```

## Section 2: Management Capabilities Matrix

| Manager Role | Can Manage | Cannot Manage | Notes |
|--------------|------------|---------------|-------|
| **Super Admin** | Everyone | - | Full system access |
| **Regional** | Divisional, Area Director, Office Leader, Team Lead, Closer, Setter | Super Admin | Sees all projects |
| **Divisional** | Area Director, Office Leader, Team Lead, Closer, Setter | Regional, Super Admin | Manages large regions |
| **Area Director** | Office Leader, Team Lead, Closer, Setter | Divisional and above | Manages 3-5 offices |
| **Office Leader** | Team Lead, Closer, Setter | Area Director and above | Manages 1-2 offices |
| **Team Lead** | Closer, Setter | Office Leader and above | Manages 5-10 reps |
| **Closer** | - | Everyone | Cannot manage anyone |
| **Setter** | - | Everyone | Cannot manage anyone |
| **Coordinator** | - | Everyone | Special role, no management |

## Section 3: Visibility Rules Quick Reference

| Role | Visibility Type | What They See | Filter Method |
|------|----------------|---------------|---------------|
| **Super Admin** | Admin | ALL projects | No filter |
| **Regional** | Admin | ALL projects | No filter |
| **Divisional** | Office-based | Projects in assigned offices | SALES_OFFICE field |
| **Area Director** | Office-based | Projects in assigned offices | SALES_OFFICE field |
| **Office Leader** | Office-based | Projects in assigned offices | SALES_OFFICE field |
| **Team Lead** | User-based | Managed users' projects | CLOSER_EMAIL + SETTER_EMAIL |
| **Closer** | User-based | Own projects (as closer) | CLOSER_EMAIL |
| **Setter** | User-based | Own projects (as setter) | SETTER_EMAIL |
| **Coordinator** | None | No projects (by design) | Blocked |

## Section 4: Office-Based vs User-Based Visibility

| Aspect | Office-Based | User-Based |
|--------|--------------|------------|
| **Who uses it** | Office Leader, Area Director, Divisional, Regional | Closer, Setter, Team Lead |
| **Filter by** | Office location | User assignment |
| **Sees inactive users' projects** | ✅ Yes | ❌ No (team leads only see active managed users) |
| **Setup required** | Assign offices via office_assignments table | Assign users via user_hierarchies table |
| **Typical use case** | Managing an office or region | Managing a specific team |
| **Scalability** | High (one assignment per office) | Medium (one assignment per user) |

## Section 5: Access Level Reference

### For Office Assignments

| Access Level | Permissions | Recommended For |
|--------------|-------------|------------------|
| **View** | Read-only access to office data | Rarely used, auditors or observers |
| **Manage** | See all projects, manage operations | Office Leaders, Area Directors |
| **Admin** | Full access including user management | Divisional Managers, Regional Managers |

**Note**: Access level does NOT affect project visibility. All managers see all projects in assigned offices regardless of access level. Access level only affects what actions they can perform.

## Section 6: Common Setup Patterns

### Pattern 1: Single Office Leader
- **Role**: office_leader
- **Office assignments**: 1 office with "Manage" access
- **User assignments**: None (sees all office projects via office-based visibility)
- **Use case**: Managing a single office location

### Pattern 2: Area Director with Multiple Offices
- **Role**: area_director
- **Office assignments**: 3-5 offices with "Admin" access
- **User assignments**: None (sees all projects via office-based visibility)
- **Use case**: Managing a geographic area with multiple offices

### Pattern 3: Team Lead with Managed Reps
- **Role**: team_lead
- **Office assignments**: None (not needed for user-based visibility)
- **User assignments**: 5-10 closers/setters
- **Use case**: Managing a specialized team or cross-office team

### Pattern 4: Nested Hierarchy (Team Lead under Office Leader)
- **Office Leader**: office_leader role, assigned to office(s)
- **Team Lead**: team_lead role, assigned to office leader, manages reps
- **Reps**: closer/setter roles, assigned to team lead
- **Use case**: Office with multiple teams, each with a team lead

## Section 7: Field Reference

### QuickBase Fields Used for Filtering

| Field Name | Field ID | Usage | Data Type |
|------------|----------|-------|----------|
| SALES_OFFICE | 2087 | Office-based filtering | Text |
| CLOSER_EMAIL | 518 | User-based filtering (closer) | Email |
| SETTER_EMAIL | 331 | User-based filtering (setter) | Email |
| CLOSER_NAME | 517 | Display only | Text |
| SETTER_NAME | 330 | Display only | Text |

### Database Tables

| Table | Purpose | Key Columns |
|-------|---------|-------------|
| **users** | User accounts | id, email, role, office, sales_office, is_active |
| **user_hierarchies** | Team lead assignments | manager_id, user_id |
| **office_assignments** | Office-based manager assignments | user_id, office_name, access_level |

## Section 8: Quick Decision Tree

### "Which Role Should I Use?"

```
Does the person manage an office or region?
├─ Yes → Use office-based role
│  ├─ Manages 1 office → office_leader
│  ├─ Manages 3-5 offices → area_director
│  └─ Manages 5+ offices → divisional
└─ No → Does the person manage specific users?
   ├─ Yes → team_lead
   └─ No → closer or setter (rep role)
```

### "Should I Use Office Assignments or User Assignments?"

```
What type of manager?
├─ Office Leader, Area Director, Divisional → Use office assignments
│  └─ Assign offices via Settings → Offices → Bulk Assign Offices
└─ Team Lead → Use user assignments
   └─ Assign users via Settings → Hierarchy → Drag-and-drop or Bulk Assign
```

## Section 9: Keyboard Shortcuts Reference

### Hierarchy Tree View
- **⌘K** or **Ctrl+K**: Focus search
- **⌘E** or **Ctrl+E**: Expand all nodes
- **⌘⇧E** or **Ctrl+Shift+E**: Collapse all nodes
- **⌘B** or **Ctrl+B**: Toggle bulk selection mode
- **⌘R** or **Ctrl+R**: Refresh data

### General
- **Tab**: Navigate between elements
- **Enter**: Activate focused button
- **Esc**: Close dialogs
- **Space**: Toggle checkboxes

## Section 10: Related Documentation

### For Detailed Instructions
- **MANAGER-SETUP-GUIDE.md**: Step-by-step setup guide for admins
- **HIERARCHY-TROUBLESHOOTING.md**: Comprehensive troubleshooting
- **MANAGER-ONBOARDING-CHECKLIST.md**: Checklist for onboarding new managers

### For Technical Details
- **USER-HIERARCHY-GUIDE.md**: Hierarchy structure and API reference
- **OFFICE_BASED_VISIBILITY.md**: Office-based visibility deep dive

### For End Users
- **USER-ONBOARDING.md**: Guide for sales reps (not managers)
