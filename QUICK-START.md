# Quick Start - Test the Application Now

This guide gets you testing the application in **5 minutes**.

## Prerequisites Check

✅ Node.js 18+ installed: `node --version`
✅ npm installed: `npm --version`
✅ `.env.local` file exists with Quickbase token and database URL

## Step 1: Start the Server (2 minutes)

```bash
# Install dependencies (if not done already)
npm install

# Start development server
npm run dev
```

**Expected Output**:
```
✓ Ready in 3.2s
○ Local: http://localhost:3000
```

**If you see errors**: Check the troubleshooting section below.

---

## Step 2: Test Login (1 minute)

1. Open browser: http://localhost:3000
2. You should be redirected to `/login`
3. Enter credentials:
   - **Email**: `admin@kinhome.com`
   - **Password**: `admin123`
4. Click "Sign In"

**Expected**: Redirect to dashboard with "Welcome back, Test Super Admin"

**If login fails**: See troubleshooting section.

---

## Step 3: Check Dashboard (1 minute)

You should see:
- ✅ Welcome message with your name
- ✅ 4 metric cards (Installs This Week, Active Accounts, On Hold, Monthly Installs)
- ✅ Recent Projects section (may be empty)
- ✅ Urgent Alerts section (may be hidden if no urgent projects)

**Open Browser Console** (F12 or Cmd+Option+I):
- Look for any red errors
- Check Network tab for failed API requests

---

## Step 4: Test Projects List (1 minute)

1. Click "Projects" in navigation (or go to http://localhost:3000/projects)
2. Check if projects load

**Expected Outcomes**:

### Scenario A: Projects Load ✅
- You see a list of projects with customer names
- Traffic light pipeline displays for each project
- Filter chips work (All, Active, On Hold, etc.)
- **This is GREAT!** Your Quickbase integration is working.

### Scenario B: "No projects found" ⚠️
- Empty state displays
- This could mean:
  - User ID mapping issue (most likely)
  - Quickbase token expired
  - No projects exist for this user in Quickbase

### Scenario C: Error Message ❌
- "Failed to load projects" displays
- Check browser console for error details
- Check Network tab for failed API request

---

## Step 5: Test Settings (30 seconds)

1. Click "Settings" in navigation
2. Verify tabs appear:
   - Profile ✅
   - Notifications ✅
   - Users ✅ (super admin only)
   - Offices ✅ (super admin only)
   - System ✅ (super admin only)
   - Audit Logs ✅ (super admin only)

3. Click each tab and check for errors

**Expected**: All tabs load without errors

**If tabs fail**: You may need to run additional database migrations (see troubleshooting).

---

## Quick Assessment

### ✅ Everything Works!
**Congratulations!** Your application is functional. Next steps:
1. Test with Addison Richards account: `addison.r@kinhome.com` / `password`
2. Verify projects load for Addison (should see ~600 projects)
3. Test all filter views and search functionality
4. Review `TESTING-CHECKLIST.md` for comprehensive testing
5. Proceed to deployment planning

### ⚠️ Partial Success
**Good progress!** Some features work, others don't. Common issues:

**Issue: Projects don't load**
- **Fix**: Test with Addison Richards account (has real Quickbase IDs)
- **Or**: Check if Quickbase token is valid
- **Or**: Review user ID mapping in database

**Issue: Settings tabs fail**
- **Fix**: Run additional migrations:
  ```bash
  npm run migrate:offices
  npm run migrate:system
  npm run migrate:audit
  ```

**Issue: Dashboard metrics show zero**
- **Cause**: No projects returned from Quickbase
- **Fix**: Resolve projects loading issue first

### ❌ Nothing Works
**Don't panic!** Let's troubleshoot systematically.

---

## Troubleshooting

### Server Won't Start

**Error**: `Cannot find module '@vercel/postgres'`
```bash
# Fix: Install dependencies
npm install
```

**Error**: `Port 3000 already in use`
```bash
# Fix: Kill existing process or use different port
kill -9 $(lsof -ti:3000)
# Or
PORT=3001 npm run dev
```

**Error**: Module resolution errors
```bash
# Fix: Clear cache and reinstall
rm -rf node_modules .next
npm install
npm run dev
```

---

### Login Fails

**Error**: "Invalid credentials"
```bash
# Fix: Re-seed users
npm run setup:seed
```

**Error**: Database connection error
```bash
# Fix: Check DATABASE_URL in .env.local
# Verify it includes -pooler suffix for Neon
# Example: postgresql://user:pass@host-pooler.region.aws.neon.tech/dbname?sslmode=require
```

**Error**: "NextAuth session not persisting"
```bash
# Fix: Verify NEXTAUTH_SECRET is set
# Generate new secret:
openssl rand -base64 32
# Add to .env.local as NEXTAUTH_SECRET
```

---

### Projects Don't Load

**Scenario**: Empty state or "No projects found"

**Step 1**: Check browser console for errors
- Look for 401 Unauthorized → Quickbase token expired
- Look for 403 Forbidden → Token lacks permissions
- Look for network errors → API endpoint issue

**Step 2**: Test with Addison Richards account
```bash
# Login as:
Email: addison.r@kinhome.com
Password: password
```
Addison has real Quickbase IDs and should see ~600 projects.

**Step 3**: Verify Quickbase token
```bash
# Test Quickbase connection
node scripts/fetch-addison-projects.js
```
If this returns projects, your token works!

**Step 4**: Check user ID mapping
```bash
# Verify user's quickbase_user_id in database
node scripts/verify-user.js
```

---

### Settings Tabs Fail

**Error**: "Failed to load" or database errors

**Fix**: Run missing migrations
```bash
npm run migrate:offices
npm run migrate:system
npm run migrate:audit
```

**Verify tables exist**:
```bash
# Check database for tables:
# - offices
# - system_settings
# - audit_logs
```

---

## Next Steps

### If Testing Successful ✅
1. Complete full testing using `TESTING-CHECKLIST.md`
2. Document results in `TESTING-RESULTS.md`
3. Review deployment runbook: `docs/DEPLOYMENT-RUNBOOK.md`
4. Set up Vercel project
5. Configure production environment variables
6. Deploy to staging
7. Run smoke tests
8. Deploy to production

### If Issues Found ⚠️
1. Document all issues in `TESTING-RESULTS.md`
2. Prioritize issues (Critical → Major → Minor)
3. Fix critical blockers first
4. Re-test after each fix
5. Iterate until all critical issues resolved

### If Blocked ❌
1. Document exact error messages
2. Check browser console output
3. Check Network tab for failed requests
4. Review relevant documentation:
   - `SETUP.md` - Setup instructions
   - `docs/QUICKBASE-USER-ID-MAPPING.md` - User ID issues
   - `docs/DEPLOYMENT-RUNBOOK.md` - Deployment guide
5. Ask for help with specific error details

---

## Test User Credentials

Use these credentials for testing different roles:

| Role | Email | Password | Notes |
|------|-------|----------|-------|
| Super Admin | admin@kinhome.com | admin123 | Full access to all features |
| Closer | addison.r@kinhome.com | password | Real user with ~600 projects |
| Closer | closer@kinhome.com | closer123 | Test user (may have no projects) |
| Setter | setter@kinhome.com | setter123 | Test user (may have no projects) |
| Office Leader | office@kinhome.com | office123 | Test user (may have no projects) |
| Regional | regional@kinhome.com | regional123 | Test user (may have no projects) |

**Recommendation**: Start with `admin@kinhome.com` for initial testing, then test with `addison.r@kinhome.com` to verify real data loading.

---

## Getting Help

If you're stuck:

1. **Check Documentation**:
   - `SETUP.md` - Detailed setup guide
   - `README.md` - Project overview
   - `docs/` folder - All technical documentation

2. **Review Logs**:
   - Terminal output from `npm run dev`
   - Browser console (F12)
   - Network tab in DevTools

3. **Run Health Check**:
   ```bash
   npm run setup:health
   ```
   This validates all integrations.

4. **Ask for Help**:
   - Provide exact error messages
   - Include browser console output
   - Share Network tab details
   - Describe steps to reproduce

---

## Success! What Now?

Once you've confirmed the application works:

1. ✅ **Document your findings** in `TESTING-RESULTS.md`
2. ✅ **Complete comprehensive testing** using `TESTING-CHECKLIST.md`
3. ✅ **Review deployment guide** in `docs/DEPLOYMENT-RUNBOOK.md`
4. ✅ **Set up Vercel** for production deployment
5. ✅ **Deploy to staging** first
6. ✅ **Run smoke tests** on staging
7. ✅ **Deploy to production** when ready

**Estimated Time to Production**: 2-4 hours (if no major issues found)

Good luck! 🚀
