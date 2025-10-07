# Testing Checklist - Kin Home Sales Pipeline PWA

This comprehensive testing checklist guides the manual testing process for the application.

## Pre-Testing Setup
- Verify `npm install` has been run
- Confirm `.env.local` exists with all required variables
- Check that database migrations have been run
- Verify test users are seeded

## Test Scenarios

### 1. Development Server Startup
- [ ] Run `npm run dev`
- [ ] Server starts on port 3000 without errors
- [ ] No compilation errors in terminal
- [ ] No module resolution errors

### 2. Authentication Flow
- [ ] Navigate to http://localhost:3000
- [ ] Redirects to /login page
- [ ] Login form displays correctly
- [ ] Test login with `admin@kinhome.com` / `admin123`
- [ ] Session created successfully
- [ ] Redirects to dashboard after login
- [ ] User name displays in header

### 3. Dashboard Page
- [ ] Dashboard loads without errors
- [ ] Welcome message shows user name
- [ ] Role-specific title displays ("Super Admin Dashboard")
- [ ] Metrics cards load (4 cards: Installs This Week, Active Accounts, On Hold, Monthly Installs)
- [ ] Urgent Alerts section displays (or hidden if no urgent projects)
- [ ] Recent Projects section displays (or shows "No recent projects")
- [ ] Check browser console for API errors
- [ ] Check Network tab for failed requests

### 4. Projects List Page
- [ ] Navigate to /projects
- [ ] Page loads without errors
- [ ] Search bar displays
- [ ] Filter chips display (All, Active, On Hold, Install Ready, etc.)
- [ ] Projects table loads (or shows "No projects found")
- [ ] If projects load: verify traffic light pipeline displays
- [ ] If projects load: verify customer name, project ID, and metrics display
- [ ] Test search functionality
- [ ] Test filter chips (click different views)
- [ ] Check browser console for errors

### 5. Project Detail Page
- [ ] Click on a project from the list
- [ ] Project detail page loads
- [ ] Project header displays with customer name and project ID
- [ ] Customer Contact card displays
- [ ] System Specs card displays
- [ ] Project Timeline displays with 9 milestones
- [ ] Team Members card displays
- [ ] Adders card displays
- [ ] Hold Management card displays
- [ ] Check browser console for errors

### 6. Settings Page - Profile Tab
- [ ] Navigate to /settings
- [ ] Settings page loads
- [ ] Profile tab is active by default
- [ ] User information displays (name, email, role, office)
- [ ] Form fields are editable
- [ ] Check browser console for errors

### 7. Settings Page - Notifications Tab
- [ ] Click Notifications tab
- [ ] Notification settings form displays
- [ ] Toggle switches work
- [ ] Threshold inputs display
- [ ] Check browser console for errors

### 8. Settings Page - Admin Tabs (Super Admin Only)
- [ ] Users tab displays (super admin only)
- [ ] Offices tab displays (super admin only)
- [ ] System tab displays (super admin only)
- [ ] Audit Logs tab displays (super admin only)
- [ ] Check browser console for errors
- [ ] Note: These tabs may fail if migrations haven't been run

### 9. Test with Different User Roles
- [ ] Logout
- [ ] Login as `addison.r@kinhome.com` / `password` (closer)
- [ ] Verify projects load for Addison (should see 600 projects if IDs are correct)
- [ ] Verify dashboard metrics calculate correctly
- [ ] Verify settings page only shows Profile and Notifications tabs

### 10. Error Scenarios
- [ ] Test with invalid login credentials
- [ ] Test navigation to non-existent project ID
- [ ] Test offline mode (disconnect internet)
- [ ] Test with expired Quickbase token (if applicable)

## Expected Issues

### Issue: Projects Don't Load
**Possible Causes:**
- Quickbase token expired → Generate new token in Quickbase
- User ID mapping incorrect → Verify `quickbase_user_id` in database matches Quickbase CLOSER_ID field
- Network error → Check browser Network tab for failed API calls

### Issue: Settings Tabs Fail to Load
**Possible Causes:**
- Missing database tables → Run `npm run migrate:offices`, `npm run migrate:system`, `npm run migrate:audit`
- Database connection error → Check `DATABASE_URL` in `.env.local`

### Issue: Dashboard Metrics Show Zero
**Possible Causes:**
- No projects returned from Quickbase → Check user ID mapping
- Date filtering issue → Check browser console for calculation errors

### Issue: Authentication Fails
**Possible Causes:**
- Users not seeded → Run `npm run setup:seed`
- Database connection error → Check `DATABASE_URL`
- NextAuth configuration error → Verify `NEXTAUTH_SECRET` is set

## Documentation Requirements

For each issue found, document:
1. **What happened**: Exact error message or behavior
2. **Where it happened**: URL, component name, user action
3. **Browser console output**: Copy full error stack trace
4. **Network tab**: Note any failed API requests (status code, endpoint)
5. **Expected behavior**: What should have happened
6. **Reproduction steps**: How to trigger the issue again

## Success Criteria

✅ **Minimum Viable Test**:
- Dev server starts without errors
- Login works with at least one test user
- Dashboard loads without crashes
- Projects list page loads (even if empty)
- Settings page loads Profile tab

✅ **Full Success**:
- All test scenarios pass
- Projects load for Addison Richards
- All dashboard metrics calculate correctly
- All settings tabs load for super admin
- No console errors or warnings
