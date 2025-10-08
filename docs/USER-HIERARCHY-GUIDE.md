# User Hierarchy Guide

## Overview

The Kin Home Sales Pipeline PWA supports a flexible hierarchical management structure that allows for both user-based and office-based visibility. This guide explains how to set up and manage user hierarchies, team lead assignments, and office access controls.

## Hierarchy Structure

### Role Hierarchy
```
Super Admin
├── Regional Manager (sees all projects)
├── Divisional Manager (sees assigned regions/offices)
├── Area Director (sees assigned offices)
├── Office Leader (sees assigned office)
└── Team Lead (sees managed users' projects)
    ├── Closer (sees own projects as closer)
    └── Setter (sees own projects as setter)
```

### Access Levels

| Role | Visibility Type | Scope | Description |
|------|----------------|-------|-------------|
| **Closer** | User-based | Own projects | Only projects where they are the closer |
| **Setter** | User-based | Own projects | Only projects where they are the setter |
| **Team Lead** | User-based | Managed users | Projects for their managed reps (both closer and setter) |
| **Office Leader** | Office-based | Assigned office(s) | ALL projects in assigned offices |
| **Area Director** | Office-based | Multiple offices | ALL projects in assigned offices (typically 3-5) |
| **Divisional Manager** | Office-based | Regions/divisions | ALL projects in assigned offices/regions (typically 5-10+) |
| **Regional Manager** | Office-based | All offices | ALL projects across all offices |
| **Super Admin** | Admin | Everything | All projects and admin features |

## User-Based vs Office-Based Visibility

### User-Based Visibility
**Who uses it:** Closers, Setters, Team Leads
**How it works:** Filters by user assignments in projects
**Query example:** `{CLOSER_ID}.EX.'user123' OR {SETTER_ID}.EX.'user123'`

**Team Lead Example:**
- Team Lead manages 5 closers and 3 setters
- Sees projects where any of their 8 managed users are assigned
- Query: `({CLOSER_ID}.EX.'user1' OR {CLOSER_ID}.EX.'user2' OR ...) OR ({SETTER_ID}.EX.'user1' OR {SETTER_ID}.EX.'user2' OR ...)`

### Office-Based Visibility
**Who uses it:** Office Leaders, Area Directors, Divisional Managers, Regional Managers
**How it works:** Filters by project's office location
**Query example:** `{SALES_OFFICE}.EX.'Phoenix' OR {SALES_OFFICE}.EX.'Tucson'`

**Area Director Example:**
- Area Director manages Phoenix, Tucson, and Las Vegas offices
- Sees ALL projects in those three offices
- Query: `{SALES_OFFICE}.EX.'Phoenix' OR {SALES_OFFICE}.EX.'Tucson' OR {SALES_OFFICE}.EX.'Las Vegas'`

## Setting Up Hierarchies

### 1. Create Team Leads

**Step 1: Create Team Lead User**
```typescript
// Create user with team_lead role
const teamLead = {
  name: "Sarah Johnson",
  email: "sarah.johnson@kinhome.com",
  role: "team_lead",
  // No office assignments needed for team leads
}
```

**Step 2: Assign Reps to Team Lead**
```typescript
// Assign closers and setters to team lead
const assignments = {
  managerId: "team-lead-uuid",
  userIds: [
    "closer-1-uuid",
    "closer-2-uuid", 
    "setter-1-uuid",
    "setter-2-uuid"
  ]
}
```

### 2. Create Office Leaders

**Step 1: Create Office Leader User**
```typescript
const officeLeader = {
  name: "Mike Rodriguez",
  email: "mike.rodriguez@kinhome.com", 
  role: "office_leader",
  office: "Phoenix"
}
```

**Step 2: Assign Office Access**
```typescript
const officeAccess = {
  userId: "office-leader-uuid",
  officeNames: ["Phoenix"],
  accessLevel: "manage"
}
```

### 3. Create Area Directors

**Step 1: Create Area Director User**
```typescript
const areaDirector = {
  name: "Jennifer Chen",
  email: "jennifer.chen@kinhome.com",
  role: "area_director"
}
```

**Step 2: Assign Multiple Offices**
```typescript
const officeAccess = {
  userId: "area-director-uuid", 
  officeNames: ["Phoenix", "Tucson", "Las Vegas"],
  accessLevel: "manage"
}
```

### 4. Create Divisional Managers

**Step 1: Create Divisional Manager User**
```typescript
const divisionalManager = {
  name: "Robert Kim",
  email: "robert.kim@kinhome.com",
  role: "divisional"
}
```

**Step 2: Assign Large Region**
```typescript
const officeAccess = {
  userId: "divisional-manager-uuid",
  officeNames: ["Phoenix", "Tucson", "Las Vegas", "Denver", "Salt Lake City", "Albuquerque"],
  accessLevel: "admin"
}
```

## Managing Complex Structures

### Matrix Organizations
Some organizations have users reporting to multiple managers:

```typescript
// User reports to both team lead and office leader
const userHierarchies = [
  {
    managerId: "team-lead-uuid",
    userId: "closer-uuid"
  },
  {
    managerId: "office-leader-uuid", 
    userId: "closer-uuid"
  }
]
```

### Nested Hierarchies
Team leads can report to office leaders:

```typescript
// Team lead reports to office leader
const teamLeadHierarchy = {
  managerId: "office-leader-uuid",
  userId: "team-lead-uuid"
}

// Team lead manages closers
const closerHierarchies = [
  {
    managerId: "team-lead-uuid",
    userId: "closer-1-uuid"
  },
  {
    managerId: "team-lead-uuid", 
    userId: "closer-2-uuid"
  }
]
```

## Using the Hierarchy Tree View

### Visual Organization
The hierarchy tree view shows your organizational structure:

```
📊 Kin Home Sales Pipeline
├── 👑 Super Admin (1)
├── 🌍 Regional Manager (1)
│   ├── 🏢 Divisional Manager - Southwest (1)
│   │   ├── 🏢 Area Director - Arizona (1)
│   │   │   ├── 🏢 Office Leader - Phoenix (1)
│   │   │   │   ├── 👥 Team Lead - Phoenix Closers (1)
│   │   │   │   │   ├── 👤 Closer (5)
│   │   │   │   │   └── 👤 Setter (3)
│   │   │   │   └── 👥 Team Lead - Phoenix Setters (1)
│   │   │   │       └── 👤 Setter (4)
│   │   │   └── 🏢 Office Leader - Tucson (1)
│   │   └── 🏢 Area Director - Nevada (1)
│   └── 🏢 Divisional Manager - Southeast (1)
```

### Interactive Features
- **Click user** to view details
- **Drag-and-drop** to reassign users
- **Right-click** for context menu
- **Search/filter** to find users
- **Expand/collapse** to show/hide teams

### Drag-and-Drop Logic
- ✅ Drag closers/setters to team leads
- ✅ Drag team leads to office leaders
- ✅ Drag office leaders to area directors
- ❌ Can't drop on invalid roles
- ❌ Can't drop on same level
- ❌ Can't create circular hierarchies

## API Reference

### Hierarchy Management

**Create Team Lead Assignment**
```typescript
POST /api/admin/hierarchies
{
  "managerId": "team-lead-uuid",
  "userIds": ["closer-1-uuid", "closer-2-uuid"]
}
```

**Remove Team Lead Assignment**
```typescript
DELETE /api/admin/hierarchies?managerId=team-lead-uuid&userId=closer-1-uuid
```

**Get User Hierarchies**
```typescript
GET /api/admin/hierarchies?managerId=team-lead-uuid
```

### Office Access Management

**Assign Office Access**
```typescript
POST /api/admin/users
{
  "name": "Area Director",
  "email": "director@kinhome.com",
  "role": "area_director",
  "officeAccess": [
    {
      "officeName": "Phoenix",
      "accessLevel": "manage"
    },
    {
      "officeName": "Tucson", 
      "accessLevel": "manage"
    }
  ]
}
```

## Best Practices

### Hierarchy Design
1. **Keep it simple** - avoid overly complex structures
2. **Use appropriate roles** - don't over-assign manager roles
3. **Limit team lead scope** - 5-10 direct reports maximum
4. **Document relationships** - keep hierarchy clear and documented

### Office Assignments
1. **Start small** - begin with single office assignments
2. **Expand gradually** - add offices as needed
3. **Use appropriate access levels** - view, manage, admin
4. **Review regularly** - ensure assignments are still valid

### User Management
1. **Assign managers during creation** - set up relationships early
2. **Update when roles change** - keep hierarchies current
3. **Remove inactive relationships** - clean up old assignments
4. **Monitor access patterns** - ensure users see appropriate data

## Troubleshooting

### Common Issues

**Q: Team lead can't see their managed users' projects**
- A: Check that user_hierarchies table has correct assignments
- A: Verify team lead role is set correctly
- A: Ensure managed users have projects in QuickBase

**Q: Office leader can't see projects in their office**
- A: Check office_assignments table for correct office assignments
- A: Verify office name matches exactly (case-sensitive)
- A: Ensure projects have correct sales_office value in QuickBase

**Q: User appears in multiple hierarchies**
- A: This is allowed for matrix organizations
- A: User will see projects from all their managers
- A: Consider if this is intentional or needs cleanup

**Q: Circular hierarchy detected**
- A: System prevents A manages B, B manages A
- A: Check for indirect circular relationships
- A: Restructure hierarchy to remove cycles

### Error Messages

**"Manager role is not appropriate"**
- Only certain roles can manage users
- Valid manager roles: team_lead, office_leader, area_director, divisional, regional, super_admin

**"User is already managed by someone else"**
- Each user can only have one direct manager
- Remove existing relationship before assigning new one

**"Cannot drop on invalid role"**
- Check drag-and-drop target is appropriate
- Team leads can't manage other team leads
- Closers can't manage anyone

**"Circular hierarchy detected"**
- System prevents A manages B, B manages A
- Restructure to remove circular relationships

## Performance Considerations

- **Hierarchy queries** are optimized with proper indexes
- **Office-based queries** use efficient QuickBase filters
- **User-based queries** batch multiple user IDs
- **Tree view** virtualizes large hierarchies for performance
- **Caching** reduces repeated database queries

## Security

- **Role-based access** controls who can manage hierarchies
- **Audit logging** tracks all hierarchy changes
- **Validation** prevents invalid assignments
- **Rate limiting** prevents abuse of hierarchy APIs

This hierarchy system provides flexible management structures while maintaining clear visibility rules and performance optimization.
