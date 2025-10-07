## QA Checklist

### Pre-Testing Setup
- [ ] Env vars configured, migrations applied, test users seeded, server running
- [ ] Database tables verified: users, sessions, project_cache, audit_logs, offices, system_settings
- [ ] Test user accounts available: closer@test.com, setter@test.com, admin@test.com
- [ ] Server accessible at http://localhost:3000 (or configured BASE_URL)

### Critical Path Testing

#### Authentication Flow
- [ ] **Login valid**: Use test@kinhome.com / testpassword → redirects to /dashboard
- [ ] **Login invalid**: Use invalid credentials → shows error message, stays on /login
- [ ] **Session persists**: Refresh page → remains logged in, no redirect to login
- [ ] **Logout works**: Click logout → redirects to /login, session cleared

#### Dashboard Testing
- [ ] **Loads successfully**: Page loads in <2 seconds, no console errors
- [ ] **Metrics display**: 4 metric cards visible with numeric values (not "Error" or "Loading")
  - Expected: Installs This Week, Active Projects, On Hold, Installs This Month
- [ ] **Alerts show**: Urgent alerts visible if projects on hold >7 days
- [ ] **Recent projects**: 5 most recent active projects displayed
- [ ] **Navigation works**: Click "View All Projects" → navigates to /projects

#### Projects List Testing
- [ ] **Loads successfully**: Project rows visible, no loading spinners stuck
- [ ] **Search works**: Type in search box → results filter in real-time
- [ ] **Filters work**: Click filter chips (Active, On Hold, etc.) → URL updates, results filter
- [ ] **Traffic lights correct**: Green/yellow/red indicators match project status
- [ ] **Navigation to detail**: Click project row → navigates to /projects/[id]

#### Project Detail Testing
- [ ] **Loads successfully**: All cards visible, no console errors
- [ ] **Customer Contact Card**: Name, phone, address displayed
- [ ] **Timeline status accurate**: Milestones show correct completion status
- [ ] **Hold management works**: Toggle hold status → updates immediately, shows in UI
- [ ] **System specs visible**: System size, price, PPW values displayed

#### Settings Testing
- [ ] **Profile edits**: Update name/phone → saves successfully, shows updated values
- [ ] **Password change**: New password works for login
- [ ] **Notifications tab**: Settings save, preferences persist
- [ ] **Admin tabs (super_admin only)**: Users, Offices, System tabs accessible

### Role-Based Access Testing
- [ ] **Closer role**: Only sees projects where closerId = their userId
- [ ] **Setter role**: Only sees projects where setterId = their userId  
- [ ] **Office leader**: Sees projects for their office + their own projects
- [ ] **Super admin**: Sees all projects, has access to admin settings

### Offline Functionality Testing
- [ ] **Service worker registers**: Check DevTools → Application → Service Workers
- [ ] **Offline indicator**: Toggle offline in DevTools → red indicator appears
- [ ] **Cached data**: Offline → project data still visible, no "network error"
- [ ] **Queued mutations**: Offline → hold updates queue, sync when online
- [ ] **Resync online**: Toggle online → queued changes sync, indicator disappears

### Performance Testing
- [ ] **Dashboard load**: <2 seconds from navigation to metrics visible
- [ ] **Projects list load**: <2 seconds from navigation to project rows visible
- [ ] **Project detail load**: <1.5 seconds from click to detail page loaded
- [ ] **No console errors**: Check DevTools Console → no red errors during normal usage
- [ ] **Memory usage**: No memory leaks during extended usage

### Security Testing
- [ ] **Unauthorized redirect**: Access /dashboard without login → redirects to /login
- [ ] **Admin 403**: Non-admin user accessing /settings → 403 or redirect
- [ ] **Rate limiting**: Rapid API calls → appropriate throttling (if implemented)
- [ ] **No sensitive console data**: Check console → no tokens, passwords, or sensitive data
- [ ] **HTTPS enforced**: Production only → secure cookies, no mixed content warnings

### Browser Compatibility Testing
- [ ] **Chrome (desktop)**: All functionality works, no console errors
- [ ] **Safari (iPad)**: Touch interactions work, responsive layout correct
- [ ] **Firefox (optional)**: Basic functionality works

### Production Smoke Testing
- [ ] **Run smoke tests**: `npm run test:smoke` → all tests pass
- [ ] **Production-only tests**: Set BASE_URL=https://domain.com → HTTPS/SW tests run
- [ ] **Screenshots captured**: Check test-results/ folder for documentation

### Sign-Off
- **Tester**: [Name]
- **Date**: [Date]
- **Environment**: [Local/Staging/Production]
- **Overall Status**: ✅ Pass / ❌ Fail
- **Blockers**: [List any issues preventing sign-off]


