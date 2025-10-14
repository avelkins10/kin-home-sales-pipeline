# Manager Onboarding Checklist - Kin Home Sales Pipeline

## Header Information

- **Manager Name**: _______________
- **Manager Email**: _______________
- **Manager Role**: _______________
- **Onboarding Date**: _______________
- **Admin Performing Setup**: _______________

---

## Section 1: Pre-Onboarding (Before Manager's First Day)

### Account Setup

- [ ] User account created in database (Settings → Users → Add User)
- [ ] Correct role assigned (team_lead, office_leader, area_director, divisional, regional)
- [ ] Personal office set (if applicable)
- [ ] Email address verified (no typos)
- [ ] Temporary password generated and securely shared
- [ ] Account marked as active (is_active = true)

### Office Assignments (for office-based managers only)

- [ ] Offices identified for assignment (list: _______________)
- [ ] Offices assigned via Bulk Assign Offices dialog
- [ ] Access level set appropriately (view/manage/admin)
- [ ] Office assignments verified in database (SQL query or UI check)
- [ ] Manager's sales_office array updated

### Team Assignments (for team leads only)

- [ ] Team members identified (list: _______________)
- [ ] Users assigned via Hierarchy tab (drag-and-drop or bulk assign)
- [ ] Assignments verified in user_hierarchies table
- [ ] Team lead's "Manages X users" badge shows correct count

### Nested Hierarchy (if applicable)

- [ ] Manager's manager identified (if any)
- [ ] Manager assigned to their manager in hierarchy
- [ ] Nested relationship verified in tree view

---

## Section 2: Access Verification

### Login Test

- [ ] Manager can log in with temporary password
- [ ] Password change prompt appears (if implemented)
- [ ] Manager successfully changes password
- [ ] Manager can log in with new password

### Dashboard Verification

- [ ] Dashboard loads without errors
- [ ] Team metrics visible (not just personal metrics)
- [ ] Performance metrics show team data
- [ ] Commission summary shows team commission
- [ ] Project buckets show team project counts
- [ ] Team activity feed shows recent team updates (managers only)
- [ ] Notifications feed working

### Project List Verification

- [ ] Project list loads without errors
- [ ] Manager sees team projects (not just their own)
- [ ] Ownership badges display correctly (Mine vs team member names)
- [ ] Ownership filter works (All / My Projects / Team Projects)
- [ ] Project buckets drill-down shows team member breakdown
- [ ] Click-through to team member's projects works

### Scope Toggle Verification (managers only)

- [ ] Scope toggle visible in dashboard header (My Performance / Team Performance)
- [ ] Switching to "My Performance" shows only manager's personal projects
- [ ] Switching to "Team Performance" shows team-wide metrics
- [ ] Commission comparison shows both personal and team tabs

---

## Section 3: Feature Training

### Dashboard Features

- [ ] Explained time range filter (Lifetime / This Month / This Week)
- [ ] Explained scope toggle (My Performance / Team Performance)
- [ ] Explained performance metrics (sold accounts, revenue, installs, retention)
- [ ] Explained commission breakdown (earned, lost, on hold, pending)
- [ ] Explained project buckets and drill-down by team member
- [ ] Explained team activity feed

### Project Management Features

- [ ] Explained ownership filter (All / My Projects / Team Projects)
- [ ] Explained ownership badges (Mine vs team member names)
- [ ] Explained how to filter by team member (click bucket drill-down)
- [ ] Explained how to view team member's projects

### Hierarchy Management (if manager has admin access)

- [ ] Explained how to view team in hierarchy tree
- [ ] Explained how to assign users (drag-and-drop or bulk)
- [ ] Explained how to remove assignments
- [ ] Explained office assignments (if applicable)

---

## Section 4: Manager Responsibilities

### Daily Tasks

- [ ] Review team activity feed for recent updates
- [ ] Check urgent alerts for team projects on hold >7 days
- [ ] Monitor team performance metrics
- [ ] Review team commission breakdown

### Weekly Tasks

- [ ] Review team project buckets (especially "Rep Attention")
- [ ] Check for unassigned projects in team's offices
- [ ] Review team retention rate
- [ ] Follow up on team projects on hold

### Monthly Tasks

- [ ] Review team performance trends (compare month-over-month)
- [ ] Identify top performers and struggling team members
- [ ] Review team commission totals
- [ ] Update office assignments if org changes

---

## Section 5: Common Questions

### Q: How do I see only my personal projects vs team projects?

**A**: Use the scope toggle in dashboard header (My Performance / Team Performance)
**A**: Use the ownership filter in project list (My Projects / Team Projects)

### Q: How do I see which team member owns a project?

**A**: Look for the ownership badge next to customer name (shows team member name)
**A**: Use project buckets drill-down to see projects grouped by team member

### Q: How do I assign a new rep to my team?

**A**: If you have admin access: Settings → Hierarchy → Drag rep onto your card
**A**: If you don't have admin access: Contact your admin to assign the rep

### Q: How do I see my team's commission breakdown?

**A**: Dashboard → Commission Summary card → Click "View by Team Member"
**A**: Expand to see each team member's earned, lost, on hold, and pending commission

### Q: What's the difference between "My Performance" and "Team Performance"?

**A**: My Performance = only projects where you are closer/setter
**A**: Team Performance = all projects in your scope (offices or managed users)

---

## Section 6: Troubleshooting for Managers

### Problem: I can't see my team's projects

**Solution**: Contact admin to verify office assignments or team lead assignments
**Solution**: Check if you're viewing "My Performance" instead of "Team Performance"
**Solution**: Verify your team members have projects in QuickBase

### Problem: Team member's projects not showing

**Solution**: Verify team member is assigned to you in hierarchy
**Solution**: Check if team member has projects in QuickBase
**Solution**: Verify team member's email matches CLOSER_EMAIL or SETTER_EMAIL in projects

### Problem: Commission breakdown doesn't match expected

**Solution**: Commission is calculated from COMMISSIONABLE_PPW × SYSTEM_SIZE_KW × 1000
**Solution**: Check if projects have commission data in QuickBase
**Solution**: Verify funding status (only funded projects show as "earned")

---

## Section 7: Sign-Off

### Admin Verification

- [ ] All checklist items completed
- [ ] Manager successfully logged in and viewed dashboard
- [ ] Manager can see team projects
- [ ] Manager understands key features
- [ ] Manager knows how to get help

### Manager Acknowledgment

- [ ] I have reviewed the dashboard and understand the key features
- [ ] I can see my team's projects and metrics
- [ ] I know how to switch between personal and team views
- [ ] I know how to get help if I have issues

### Signatures

- **Admin Name**: _______________ **Date**: _______________
- **Manager Name**: _______________ **Date**: _______________

---

## Section 8: Next Steps

### For the Manager

1. Review team performance metrics daily
2. Monitor team activity feed for recent updates
3. Check urgent alerts for team projects needing attention
4. Use commission breakdown to track team performance
5. Contact support if you encounter any issues

### For the Admin

1. File completed checklist for records
2. Monitor manager's first week for issues
3. Follow up after 1 week to ensure everything is working
4. Add manager to relevant Slack channels or email lists

---

## Related Documentation

- **MANAGER-SETUP-GUIDE.md**: Detailed setup instructions for admins
- **HIERARCHY-TROUBLESHOOTING.md**: Troubleshooting guide for common issues
- **USER-HIERARCHY-GUIDE.md**: Technical details on hierarchy structure
- **OFFICE_BASED_VISIBILITY.md**: Deep dive on office-based visibility model
