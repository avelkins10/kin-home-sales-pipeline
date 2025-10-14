# Hierarchy Troubleshooting Guide - Kin Home Sales Pipeline

## Quick Diagnostic Checklist

Use this flowchart to quickly identify the issue:

- [ ] **Can the user log in?** → If no, check `users.is_active` and password
- [ ] **Is the user a manager?** → If yes, check office assignments or managed users
- [ ] **Can they see any projects?** → If no, check role-based filtering
- [ ] **Can they see team projects?** → If no, check assignments
- [ ] **Are assignments showing in UI?** → If no, check database tables

## Section 1: Team Lead Issues

### Problem: Team Lead Can't See Managed Users' Projects

#### Diagnostic Steps

1. **Verify team lead role**:
   ```sql
   SELECT id, name, email, role FROM users WHERE email = 'teamlead@example.com';
   ```
   Expected: `role = 'team_lead'`

2. **Check user_hierarchies table**:
   ```sql
   SELECT h.*, u.name as user_name, u.email as user_email
   FROM user_hierarchies h
   JOIN users u ON h.user_id = u.id
   WHERE h.manager_id = 'team-lead-user-id';
   ```
   Expected: Rows for each managed user

3. **Verify managed users have projects**:
   - Log in to QuickBase
   - Search for projects where CLOSER_EMAIL or SETTER_EMAIL matches managed user's email
   - If no projects exist, that's why team lead sees nothing

4. **Check authorization logs**:
   - Look for `[PROJECT_AUTHORIZATION]` log entries
   - Verify `managedEmails` array is populated
   - Check WHERE clause includes managed user emails

#### Solutions

- **No hierarchies found**: Assign users to team lead via Settings → Hierarchy tab
- **Wrong manager_id**: Remove incorrect assignment and create correct one
- **Managed users have no projects**: This is expected - team lead will see projects when users get assigned to projects in QuickBase
- **Authorization not using managed emails**: Check `getManagedUserEmails()` function is being called (should see logs)

### Problem: Team Lead Sees Projects from Users They Don't Manage

#### Diagnostic Steps

1. **Check for duplicate hierarchies**:
   ```sql
   SELECT manager_id, user_id, COUNT(*) 
   FROM user_hierarchies 
   GROUP BY manager_id, user_id 
   HAVING COUNT(*) > 1;
   ```
   Expected: No rows (no duplicates)

2. **Check if user has multiple managers**:
   ```sql
   SELECT h.*, m.name as manager_name
   FROM user_hierarchies h
   JOIN users m ON h.manager_id = m.id
   WHERE h.user_id = 'user-id';
   ```
   Expected: One row per user (unless matrix org)

#### Solutions

- **Duplicate hierarchies**: Delete duplicates using DELETE query
- **Multiple managers**: This is allowed for matrix orgs, but verify it's intentional

## Section 2: Office Leader Issues

### Problem: Office Leader Can't See Projects in Their Office

#### Diagnostic Steps

1. **Verify office leader role**:
   ```sql
   SELECT id, name, email, role FROM users WHERE email = 'officeleader@example.com';
   ```
   Expected: `role = 'office_leader'` (or area_director, divisional)

2. **Check office_assignments table**:
   ```sql
   SELECT * FROM office_assignments WHERE user_id = 'office-leader-user-id';
   ```
   Expected: One or more rows with office names

3. **Check users.sales_office array**:
   ```sql
   SELECT id, name, sales_office FROM users WHERE id = 'office-leader-user-id';
   ```
   Expected: Array includes assigned office names

4. **Verify office names match exactly**:
   - Office in `office_assignments`: "Phoenix"
   - Office in QuickBase SALES_OFFICE field: "Phoenix"
   - Case-sensitive! "phoenix" ≠ "Phoenix"

5. **Check authorization logs**:
   - Look for `[OFFICE_ASSIGNMENT_RESOLUTION]` log entries
   - Verify offices array is populated
   - Check WHERE clause includes office names

#### Solutions

- **No office assignments**: Assign offices via Settings → Offices → Bulk Assign Offices
- **Office name mismatch**: Update office name in database or QuickBase to match exactly
- **sales_office array empty**: Run bulk assign offices again (it updates both tables)
- **Authorization not using offices**: Check `getAssignedOffices()` function is being called

### Problem: Office Leader Sees Projects from Wrong Office

#### Diagnostic Steps

1. **List all office assignments**:
   ```sql
   SELECT office_name, access_level FROM office_assignments WHERE user_id = 'office-leader-user-id';
   ```

2. **Check project's office in QuickBase**:
   - Open project in QuickBase
   - Check SALES_OFFICE field (field 2087)
   - Verify it matches one of the assigned offices

3. **Check WHERE clause in logs**:
   - Look for `[PROJECT_QUERY]` log entries
   - Verify WHERE clause includes correct office names
   - Example: `{2087.EX.'Phoenix'} OR {2087.EX.'Tucson'}`

#### Solutions

- **Seeing extra offices**: Remove unwanted office assignments
- **Not seeing expected office**: Add missing office assignment
- **Office name typo**: Fix office name in `office_assignments` table or QuickBase

## Section 3: Newly Assigned Office Not Visible

### Problem: Admin Assigns Office to Manager, But Manager Doesn't See Projects Immediately

#### Diagnostic Steps

1. **Verify assignment API succeeded**:
   - Check browser console for API response
   - Look for success toast: "Successfully assigned X office assignments"

2. **Check database was updated**:
   ```sql
   SELECT * FROM office_assignments 
   WHERE user_id = 'manager-id' 
   AND office_name = 'New Office'
   ORDER BY created_at DESC;
   ```
   Expected: Row exists with recent created_at timestamp

3. **Check users.sales_office array**:
   ```sql
   SELECT sales_office FROM users WHERE id = 'manager-id';
   ```
   Expected: Array includes 'New Office'

4. **Check if API is using stale session data**:
   - Look for `[PROJECT_QUERY] Using provided offices (not fetching from DB)` log
   - This indicates the API is passing salesOffice from session (BAD)
   - Should see: `[PROJECT_QUERY] Fetched offices from database` (GOOD)

#### Solutions

- **Assignment failed**: Check API error message, verify office exists in offices table
- **Database not updated**: Re-run bulk assign offices
- **API using session data**: This is a code bug - the API route should NOT pass salesOffice from session (see OFFICE_BASED_VISIBILITY.md for fix)
- **Manager needs to log out**: This should NOT be necessary - if it is, the session staleness bug exists

## Section 4: Circular Hierarchy Issues

### Problem: "Circular Hierarchy Detected" Error When Assigning Users

#### What It Means

The system prevents A manages B, B manages C, C manages A (creates a loop).

#### Diagnostic Steps

1. **Trace the hierarchy chain**:
   ```sql
   WITH RECURSIVE hierarchy_chain AS (
     SELECT user_id, manager_id, 1 as level
     FROM user_hierarchies
     WHERE user_id = 'user-id-being-assigned'
     
     UNION ALL
     
     SELECT h.user_id, h.manager_id, hc.level + 1
     FROM user_hierarchies h
     JOIN hierarchy_chain hc ON h.user_id = hc.manager_id
     WHERE hc.level < 10
   )
   SELECT * FROM hierarchy_chain;
   ```
   This shows the full chain from user to root

2. **Check if target manager appears in chain**:
   - If the manager you're trying to assign to appears in the chain, that's the circular dependency

#### Solutions

- **Direct circular**: A manages B, trying to assign A to B → Remove A manages B first, then assign B to A
- **Indirect circular**: A manages B, B manages C, trying to assign A to C → Restructure hierarchy to remove loop
- **Complex circular**: Use the SQL query above to trace the full chain and identify where the loop is

## Section 5: Drag-and-Drop Issues

### Problem: Can't Drop User on Manager (Red Border Appears)

#### Possible Causes and Solutions

**Cause 1: Role hierarchy violation**
- Example: Trying to drag office_leader onto team_lead
- Solution: Only drag users to managers with higher or equal role level
- Role hierarchy: super_admin > regional > divisional > area_director > office_leader > team_lead > closer/setter

**Cause 2: User already managed**
- Example: User is already assigned to a different manager
- Solution: Remove existing assignment first (right-click → Remove Assignment)

**Cause 3: Circular hierarchy**
- Example: Trying to drag A onto B when B is managed by A
- Solution: Restructure hierarchy to remove circular dependency

**Cause 4: Self-assignment**
- Example: Trying to drag user onto themselves
- Solution: Don't do that 😊

**Cause 5: Invalid manager role**
- Example: Trying to drag user onto a closer (closers can't manage anyone)
- Solution: Promote the target user to team_lead or higher first

## Section 6: Bulk Assignment Issues

### Problem: Bulk Assignment Fails with "Some Users Are Invalid"

#### Diagnostic Steps

1. **Check which users are invalid**:
   - API response includes list of invalid user IDs
   - Cross-reference with users table to find which users

2. **Common invalid reasons**:
   - User doesn't exist (deleted or ID typo)
   - User is super_admin (cannot be managed)
   - User is already managed by someone else

#### Solutions

- **User doesn't exist**: Remove from selection, verify user exists in database
- **User is super_admin**: Don't assign super_admins to anyone
- **User already managed**: Remove existing assignment first, or use "Replace Manager" feature (if implemented)

### Problem: Bulk Assignment Succeeds But Some Users Not Visible

#### Diagnostic Steps

1. **Check how many assignments were created**:
   ```sql
   SELECT COUNT(*) FROM user_hierarchies WHERE manager_id = 'manager-id';
   ```

2. **Compare with expected count**:
   - If count is less than expected, some assignments failed silently
   - Check API response for partial success message

#### Solutions

- **Partial success**: Re-run bulk assign for missing users
- **Transaction rolled back**: Check API logs for error, fix issue, retry

## Section 7: Office Assignment Issues

### Problem: Bulk Office Assignment Fails with "Some Offices Are Invalid"

#### Diagnostic Steps

1. **Check which offices are invalid**:
   - API response includes list of invalid office names

2. **Verify offices exist**:
   ```sql
   SELECT id, name FROM offices WHERE name IN ('Office A', 'Office B');
   ```

#### Solutions

- **Office doesn't exist**: Create office first in Settings → Offices tab
- **Office name typo**: Fix spelling to match offices table exactly
- **Case mismatch**: Ensure exact case match ("Phoenix" not "phoenix")

### Problem: Office Assignment Succeeds But Manager Sees No Projects

#### Diagnostic Steps

1. **Verify projects exist in that office**:
   - Log in to QuickBase
   - Filter projects by SALES_OFFICE = assigned office name
   - If no projects, that's why manager sees nothing

2. **Check office name in projects**:
   - Verify SALES_OFFICE field (2087) is populated
   - Verify spelling matches office_assignments table exactly

#### Solutions

- **No projects in office**: This is expected - manager will see projects when they're created
- **Office name mismatch**: Update SALES_OFFICE in QuickBase or office name in database
- **SALES_OFFICE field empty**: Update projects in QuickBase to include office

## Section 8: Performance Issues

### Problem: Hierarchy Tree View Is Slow with Large Teams (500+ Users)

#### Solutions

- Use search and filters to narrow results
- Collapse nodes you're not working with
- Use bulk operations instead of drag-and-drop for large batches
- Consider splitting large teams into smaller sub-teams

### Problem: Bulk Assignment Takes >30 Seconds

#### Diagnostic Steps

- Check how many assignments: 50 users × 10 offices = 500 assignments
- Check database performance logs

#### Solutions

- Break into smaller batches (25 users at a time)
- Assign offices in multiple operations
- Check database connection pool isn't exhausted

## Section 9: Data Sync Issues

### Problem: office_assignments Table and users.sales_office Array Are Out of Sync

#### Diagnostic Steps

1. **Compare the two sources**:
   ```sql
   SELECT 
     u.id,
     u.name,
     u.sales_office as array_offices,
     ARRAY_AGG(oa.office_name) as assigned_offices
   FROM users u
   LEFT JOIN office_assignments oa ON u.id = oa.user_id
   WHERE u.role IN ('office_leader', 'area_director', 'divisional')
   GROUP BY u.id, u.name, u.sales_office;
   ```

2. **Look for mismatches** where array_offices ≠ assigned_offices

#### Solutions

- **Out of sync**: Re-run bulk assign offices (it updates both)
- **Manual fix**: Update users.sales_office to match office_assignments:
   ```sql
   UPDATE users u
   SET sales_office = (
     SELECT ARRAY_AGG(office_name)
     FROM office_assignments
     WHERE user_id = u.id
   )
   WHERE id = 'user-id';
   ```

## Section 10: Authorization Issues

### Problem: Manager Sees Projects They Shouldn't See

#### Diagnostic Steps

1. Check their office assignments (should be limited)
2. Check their role (super_admin and regional see everything)
3. Check WHERE clause in logs

#### Solutions

- **Too many offices assigned**: Remove unwanted office assignments
- **Role too high**: Demote to appropriate role (e.g., regional → divisional)

### Problem: Manager Doesn't See Projects They Should See

#### Diagnostic Steps

1. Verify projects exist in QuickBase with correct SALES_OFFICE
2. Verify manager has office assigned
3. Check WHERE clause matches project's office

#### Solutions

- **Missing office assignment**: Assign the office
- **Project in wrong office**: Update SALES_OFFICE in QuickBase
- **Office name mismatch**: Fix spelling/case

## Section 11: UI Issues

### Problem: Drag-and-Drop Not Working

#### Solutions

- Ensure you're not in bulk selection mode (click "Bulk Select" to toggle off)
- Try using right-click → Assign Users instead
- Check browser console for JavaScript errors
- Try refreshing the page

### Problem: Search Not Finding Users

#### Solutions

- Search is case-insensitive, but check spelling
- Search looks in name AND email fields
- Clear other filters (role, office, activity) that might be hiding results
- Check if user exists in database

### Problem: Statistics Panel Shows Wrong Counts

#### Solutions

- Click "Refresh" button in toolbar to reload data
- Check if filters are active (statistics reflect filtered view)
- Verify database data is correct using SQL queries

## Section 12: Getting Help

### When to Escalate

- Database corruption or data loss
- API errors that persist after retry
- Authorization bugs (users seeing wrong data)
- Performance issues affecting multiple users

### What to Include in Support Request

- User ID and email of affected user
- Role of affected user
- Expected behavior vs actual behavior
- Screenshots of error messages
- Relevant log entries (search for user ID in logs)
- SQL query results from diagnostic steps

### Support Contacts

- Technical issues: [DEV_TEAM_EMAIL]
- Database issues: [DBA_EMAIL]
- Urgent production issues: [ON_CALL_PHONE]

## Section 13: Preventive Maintenance

### Weekly Checks

- Review unassigned users count (should be low)
- Check for inactive users with active assignments
- Verify largest team size is reasonable (<20)
- Review office assignments for accuracy

### Monthly Checks

- Audit user_hierarchies for orphaned records
- Verify office_assignments and users.sales_office are in sync
- Review authorization logs for errors
- Test login and project visibility for each role type

### Quarterly Checks

- Full hierarchy review with org chart export
- Remove inactive users from hierarchies
- Update office assignments for org changes
- Performance review (query times, API response times)

## Section 14: SQL Queries Reference

### List All Team Leads and Their Managed Users

```sql
SELECT 
  m.name as manager_name,
  m.email as manager_email,
  COUNT(h.user_id) as managed_count,
  STRING_AGG(u.name, ', ') as managed_users
FROM users m
LEFT JOIN user_hierarchies h ON m.id = h.manager_id
LEFT JOIN users u ON h.user_id = u.id
WHERE m.role = 'team_lead'
GROUP BY m.id, m.name, m.email
ORDER BY managed_count DESC;
```

### List All Office Leaders and Their Assigned Offices

```sql
SELECT 
  u.name,
  u.email,
  u.role,
  ARRAY_AGG(oa.office_name) as assigned_offices,
  ARRAY_AGG(oa.access_level) as access_levels
FROM users u
LEFT JOIN office_assignments oa ON u.id = oa.user_id
WHERE u.role IN ('office_leader', 'area_director', 'divisional')
GROUP BY u.id, u.name, u.email, u.role
ORDER BY u.role, u.name;
```

### Find Users Without a Manager

```sql
SELECT u.id, u.name, u.email, u.role
FROM users u
LEFT JOIN user_hierarchies h ON u.id = h.user_id
WHERE h.user_id IS NULL
AND u.role NOT IN ('super_admin', 'regional')
AND u.is_active = true
ORDER BY u.role, u.name;
```

### Find Circular Hierarchies (Should Return No Rows)

```sql
WITH RECURSIVE hierarchy_check AS (
  SELECT user_id, manager_id, ARRAY[user_id] as path
  FROM user_hierarchies
  
  UNION ALL
  
  SELECT h.user_id, h.manager_id, hc.path || h.user_id
  FROM user_hierarchies h
  JOIN hierarchy_check hc ON h.user_id = hc.manager_id
  WHERE NOT (h.user_id = ANY(hc.path))
)
SELECT * FROM hierarchy_check WHERE user_id = manager_id;
```

## Section 15: Related Documentation

- **MANAGER-SETUP-GUIDE.md**: Step-by-step setup instructions
- **OFFICE_BASED_VISIBILITY.md**: Technical details on office-based visibility
- **USER-HIERARCHY-GUIDE.md**: Hierarchy structure and API reference
- **SUPPORT-RUNBOOK.md**: Production support procedures
