# Support Runbook - Kin Home Sales Pipeline

**Purpose:** Troubleshooting guide and escalation paths for production support.

---

## Support Tiers

### Tier 1: Self-Service (User)
- **Response Time:** Immediate
- **Scope:** Common user issues, app usage questions
- **Resources:** USER-ONBOARDING.md, in-app help

### Tier 2: Manager/Team Lead
- **Response Time:** <2 hours during business hours
- **Scope:** Account issues, data discrepancies, training
- **Contact:** [MANAGER_EMAIL]

### Tier 3: Technical Support
- **Response Time:** <4 hours during business hours, <24 hours after hours
- **Scope:** App bugs, performance issues, integration problems
- **Contact:** [TECH_SUPPORT_EMAIL] or Slack #sales-pipeline-support

### Tier 4: Engineering (On-Call)
- **Response Time:** <1 hour for critical issues
- **Scope:** Production outages, security incidents, data loss
- **Contact:** [ONCALL_PHONE] (call for critical issues only)

---

## Common Issues & Solutions

### Issue: "I can't login"

**Symptoms:**
- Login form shows error message
- Redirects back to login after submitting
- "Invalid credentials" error

**Tier 1 Solutions:**
1. Verify email address is correct (use Kin Home email)
2. Check password (case-sensitive)
3. Try resetting password (contact manager)
4. Clear browser cache and try again
5. Try different browser (Safari vs Chrome)

**Tier 2 Solutions:**
1. Verify user account exists in database
2. Reset user password:
   ```bash
   # Connect to Neon database
   # Run: UPDATE users SET password_hash = [new_hash] WHERE email = '[user_email]'
   ```
3. Check user role is correct
4. Verify Quickbase user ID is mapped correctly

**Tier 3 Solutions:**
1. Check Vercel logs for authentication errors
2. Verify NextAuth configuration is correct
3. Verify database connection is working
4. Check for session cookie issues (HTTPS, secure flags)

**Escalate to Tier 4 if:** Multiple users can't login (system-wide auth failure)

---

### Issue: "My projects aren't showing"

**Symptoms:**
- Dashboard shows 0 projects
- Projects list is empty
- "No projects found" message

**Tier 1 Solutions:**
1. Check filter - might be on "On Hold" when you have no holds
2. Clear search bar (might have old search term)
3. Tap "All" filter chip to reset filters
4. Refresh the page (pull down to refresh on iPad)
5. Check if you're offline (yellow banner at top)

**Tier 2 Solutions:**
1. Verify user is assigned as closer/setter in Quickbase
2. Check user's Quickbase user ID is correct in database
3. Verify projects exist in Quickbase for this user
4. Check user role in database (closer, setter, etc.)

**Tier 3 Solutions:**
1. Check Vercel logs for Quickbase API errors
2. Verify Quickbase token is valid (not expired)
3. Test Quickbase query manually:
   ```bash
   # Run health check
   npm run setup:health
   ```
4. Check rate limiting (might be hitting 10 req/sec limit)
5. Verify query filters are correct in `getProjectsForUser()`

**Escalate to Tier 4 if:** No users can see projects (Quickbase API failure)

---

### Issue: "App says I'm offline but I have internet"

**Symptoms:**
- Yellow offline banner shows
- Can browse other websites fine
- App won't fetch new data

**Tier 1 Solutions:**
1. Close and reopen the app
2. Check iPad's internet connection (Settings → Wi-Fi)
3. Try switching between Wi-Fi and cellular
4. Refresh the page (pull down)
5. Restart iPad

**Tier 2 Solutions:**
1. Clear browser cache (Settings → Safari → Clear History and Website Data)
2. Uninstall and reinstall PWA
3. Try accessing app in Safari (not installed version)

**Tier 3 Solutions:**
1. Check if Quickbase API is down (test with curl)
2. Check Vercel deployment status
3. Verify service worker is registered correctly
4. Check browser console for errors
5. Test with different device/network

**Escalate to Tier 4 if:** Multiple users experiencing same issue (service outage)

---

### Issue: "My changes aren't syncing"

**Symptoms:**
- Placed hold but it's not in Quickbase
- Offline indicator shows "X pending updates"
- Changes made hours ago still not synced

**Tier 1 Solutions:**
1. Check if you're online (no yellow banner)
2. Wait 1-2 minutes for sync to complete
3. Refresh the page to trigger sync
4. Check for error toast notifications

**Tier 2 Solutions:**
1. Verify user has write permissions in Quickbase
2. Check if project exists in Quickbase
3. Verify hold update succeeded in Quickbase manually

**Tier 3 Solutions:**
1. Check Vercel logs for sync errors
2. Check Sentry for mutation failures
3. Verify Quickbase API is accepting updates
4. Check IndexedDB for stuck mutations:
   ```javascript
   // In browser console
   const db = await indexedDB.open('kin-solar-db');
   // Check pendingMutations store
   ```
5. Check retry count (max 3 retries)
6. Manually clear mutation queue if stuck

**Escalate to Tier 4 if:** Mutations failing for all users (API write failure)

---

### Issue: "App is slow"

**Symptoms:**
- Pages take >5 seconds to load
- Spinning loaders for extended time
- App feels sluggish

**Tier 1 Solutions:**
1. Check internet connection speed
2. Close other apps on iPad (free up memory)
3. Restart the app
4. Clear browser cache
5. Try on different network (Wi-Fi vs cellular)

**Tier 2 Solutions:**
1. Check if user has excessive number of projects (>500)
2. Verify filters are working (not loading all projects)
3. Check for large adder lists or notes

**Tier 3 Solutions:**
1. Check Vercel performance metrics
2. Check Quickbase API response times (should be <1 second)
3. Check Neon database performance
4. Review Vercel logs for slow queries
5. Check if rate limiting is causing delays
6. Run Lighthouse audit for performance issues

**Escalate to Tier 4 if:** Performance degradation affects all users (infrastructure issue)

---

### Issue: "Data doesn't match Quickbase"

**Symptoms:**
- Project status different in app vs Quickbase
- Missing projects
- Outdated information

**Tier 1 Solutions:**
1. Refresh the page (pull down on iPad)
2. Wait 30 seconds (cache refresh interval)
3. Check if you're viewing cached data (offline indicator)
4. Log out and log back in

**Tier 2 Solutions:**
1. Verify data is correct in Quickbase
2. Check when data was last updated in Quickbase
3. Verify field mappings are correct
4. Check if user has access to project in Quickbase

**Tier 3 Solutions:**
1. Check Quickbase query in `getProjectsForUser()`
2. Verify field IDs in `fieldIds.ts` match Quickbase
3. Check for Quickbase schema changes
4. Verify cache TTL is working (5 minutes)
5. Clear IndexedDB cache and refetch
6. Check TanStack Query cache status

**Escalate to Tier 4 if:** Systematic data mismatch (field mapping issue)

---

## Escalation Paths

### Critical Issues (Escalate Immediately)

**Production Outage:**
- App is completely down (500 errors, won't load)
- No users can login
- All API calls failing

**Action:**
1. Call engineering on-call: [ONCALL_PHONE]
2. Post in Slack: #incidents
3. Check Vercel status: https://vercel.com/status
4. Check Quickbase status: https://status.quickbase.com
5. Check Neon status: https://neon.tech/status

**Security Incident:**
- Quickbase token leaked
- Unauthorized access detected
- Data breach suspected

**Action:**
1. Call engineering on-call immediately
2. Rotate Quickbase token (see TOKEN-ROTATION.md)
3. Review access logs
4. Document incident
5. Notify security team

### High Priority Issues (Escalate Within 1 Hour)

**Data Sync Failures:**
- Multiple users reporting sync issues
- Holds not updating in Quickbase
- Offline queue not processing

**Action:**
1. Email tech support: [TECH_SUPPORT_EMAIL]
2. Post in Slack: #sales-pipeline-support
3. Provide: user email, project ID, timestamp, error message
4. Check Sentry for error patterns

**Performance Degradation:**
- App is slow for all users
- Timeouts on API calls
- Database connection issues

**Action:**
1. Email tech support
2. Check Vercel metrics
3. Check Neon database metrics
4. Provide: affected users, time started, symptoms

### Medium Priority Issues (Escalate Within 4 Hours)

**Individual User Issues:**
- One user can't login
- One user's projects not showing
- One user experiencing slowness

**Action:**
1. Try Tier 1 and Tier 2 solutions first
2. Email tech support if unresolved
3. Provide: user email, steps to reproduce, screenshots

**UI/UX Issues:**
- Layout broken on specific device
- Feature not working as expected
- Confusing interface element

**Action:**
1. Document issue with screenshots
2. Post in Slack: #sales-pipeline-support
3. Create GitHub issue for tracking

### Low Priority Issues (Escalate Within 24 Hours)

**Feature Requests:**
- User wants new functionality
- Suggestion for improvement

**Action:**
1. Document request
2. Post in Slack: #sales-pipeline-feedback
3. Product owner will prioritize

**Minor Bugs:**
- Typo in UI
- Incorrect label
- Non-critical visual issue

**Action:**
1. Create GitHub issue
2. Tag as "bug" and "low-priority"
3. Will be fixed in next release

---

## Monitoring & Alerts

**What to Monitor:**

**Daily:**
- [ ] Vercel deployment status (should be "Ready")
- [ ] Error rate in Sentry (should be <1%)
- [ ] Login success rate (should be >99%)
- [ ] Quickbase API errors in logs (should be minimal)

**Weekly:**
- [ ] User adoption rate (target: 90%+ daily active)
- [ ] Performance metrics (load times, API response times)
- [ ] Offline sync success rate (target: >95%)
- [ ] User feedback and issues

**Monthly:**
- [ ] Security audit (`npm run audit:prod`)
- [ ] Dependency updates
- [ ] Success metrics review (see SUCCESS-METRICS.md)
- [ ] User satisfaction survey

**Quarterly:**
- [ ] Quickbase token rotation (see TOKEN-ROTATION.md)
- [ ] Database backup verification
- [ ] Disaster recovery test
- [ ] Performance optimization review

---

## Incident Response Playbook

### Scenario 1: Quickbase API Outage

**Detection:**
- Multiple users report "can't load projects"
- Vercel logs show 401/403/500 errors from Quickbase
- Sentry shows spike in Quickbase API errors

**Response:**
1. Verify Quickbase status: https://status.quickbase.com
2. If Quickbase is down: Post announcement in Slack
3. Inform users: "Quickbase is experiencing issues. App will work with cached data (last 5 minutes). Updates will sync when Quickbase returns."
4. Monitor Quickbase status page
5. When Quickbase returns: Verify app recovers automatically
6. Check for any stuck mutations in queue

**Recovery Time:** Depends on Quickbase (typically <1 hour)

### Scenario 2: Database Connection Failure

**Detection:**
- Users can't login
- Vercel logs show database connection errors
- Sentry shows database timeout errors

**Response:**
1. Check Neon database status: https://console.neon.tech
2. Verify `DATABASE_URL` is correct in Vercel
3. Check Neon connection limits (may be exceeded)
4. Restart Neon database if needed
5. Verify database is in correct region
6. Check for long-running queries blocking connections

**Recovery Time:** <30 minutes

**Escalate to Tier 4 if:** Database is down or unresponsive

### Scenario 3: Deployment Failure

**Detection:**
- Vercel deployment shows "Failed" status
- Build logs show errors
- Production site shows old version

**Response:**
1. Check build logs in Vercel dashboard
2. Identify error (TypeScript, dependency, environment variable)
3. If quick fix: Push fix to `main` branch
4. If complex issue: Rollback to last good deployment (see DEPLOYMENT-RUNBOOK.md Step 7)
5. Verify rollback succeeds
6. Fix issue in separate branch
7. Test thoroughly before redeploying

**Recovery Time:** <15 minutes (rollback), varies for fix

### Scenario 4: Token Expiration

**Detection:**
- All users report "can't load projects"
- Vercel logs show 401 errors from Quickbase
- Error message: "Invalid token" or "Unauthorized"

**Response:**
1. Verify token expiration in Quickbase
2. Generate new token immediately (see TOKEN-ROTATION.md)
3. Update Vercel environment variable
4. Redeploy (or wait for auto-deploy if using CI)
5. Verify new token works
6. Post announcement: "Issue resolved. Please refresh your app."

**Recovery Time:** <10 minutes

**Prevention:** Set calendar reminder for token rotation every 90 days

### Scenario 5: Service Worker Issues

**Detection:**
- Users report "app won't update"
- Users see old version after deployment
- Offline functionality not working

**Response:**
1. Verify service worker is registered (check browser console)
2. Verify `sw.js` is accessible at `/sw.js`
3. Check service worker cache version
4. Instruct users to:
   - Close all app tabs/windows
   - Clear browser cache
   - Reopen app
   - Or: Uninstall and reinstall PWA
5. Verify new service worker activates

**Recovery Time:** <5 minutes per user

**Prevention:** Increment cache version in `sw.js` on each deployment

---

## Logging & Debugging

### Vercel Logs

**Access:**
1. Go to Vercel project → Logs
2. Filter by:
   - Time range (last hour, last day)
   - Log level (error, warning, info)
   - Source (function, edge, build)

**What to Look For:**
- Quickbase API errors (401, 403, 500)
- Database connection errors
- NextAuth errors
- Rate limiting warnings
- Slow query warnings (>2 seconds)

**Example Log Queries:**
- `[QB] POST /v1/records/query` - Quickbase requests
- `[ERROR]` - All errors
- `[SYNC]` - Offline sync events
- `Unauthorized` - Auth failures

### Sentry (If Enabled)

**Access:**
1. Go to https://sentry.io
2. Select "Kin Solar Pipeline" project
3. View Issues tab

**What to Look For:**
- Error frequency (spikes indicate problems)
- Error types (group by error message)
- Affected users (how many users hit this error)
- Stack traces (where error occurred)
- Breadcrumbs (what user was doing)

**Useful Filters:**
- `environment:production` - Production errors only
- `level:error` - Errors only (not warnings)
- `handled:no` - Unhandled errors (critical)

### Browser Console

**For User Issues:**
1. Ask user to open browser console:
   - Safari iPad: Settings → Safari → Advanced → Web Inspector
   - Chrome: F12 or Cmd+Option+I
2. Ask user to screenshot console errors
3. Look for:
   - Red errors (JavaScript errors)
   - Network errors (failed API calls)
   - Service worker errors

---

## Data Recovery

### Scenario: User Accidentally Updated Wrong Project

**Recovery:**
1. Identify project ID and timestamp of change
2. Check Quickbase audit log for previous value
3. Manually update in Quickbase to restore
4. Refresh app to see corrected data

**Prevention:** Add confirmation dialog for destructive actions (future enhancement)

### Scenario: Database Corruption

**Recovery:**
1. Identify affected tables/records
2. Restore from Neon backup:
   - Go to Neon dashboard → Backups
   - Select backup from before corruption
   - Restore to new database
   - Update `DATABASE_URL` in Vercel
   - Redeploy
3. Verify data integrity
4. Notify affected users

**Prevention:** Neon automatically backs up every 24 hours

---

## Contact Information

**Technical Support:**
- Email: [TECH_SUPPORT_EMAIL]
- Slack: #sales-pipeline-support
- Hours: Mon-Fri 8am-6pm PST

**Engineering On-Call:**
- Phone: [ONCALL_PHONE]
- Use for: Production outages, security incidents, data loss
- Hours: 24/7 for critical issues

**Product Owner:**
- Email: [PRODUCT_OWNER_EMAIL]
- Use for: Feature requests, prioritization, business questions

**Vendor Support:**
- Quickbase: https://help.quickbase.com (24/7)
- Vercel: https://vercel.com/support (24/7 for Pro plan)
- Neon: https://neon.tech/docs/introduction (docs + community)

---

**📋 Keep this runbook updated as new issues are discovered and resolved.**
