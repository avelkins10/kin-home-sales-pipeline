# Phase 1 Setup Guide - Kin Home Sales Pipeline

This guide walks you through setting up the development environment for the Kin Home Sales Pipeline Dashboard. Follow these steps in order to ensure all integrations are properly configured.

## Prerequisites

- Node.js 18+ installed
- npm installed
- Access to Kin Home Quickbase account
- Neon PostgreSQL database credentials

## Setup Steps

### Step 1: Install Dependencies

```bash
npm install
```

This installs all required packages including Next.js, React, TanStack Query, NextAuth, bcryptjs, and @vercel/postgres.

### Step 2: Configure Environment Variables

#### 2.1 Create .env.local file:

```bash
cp env.example .env.local
```

#### 2.2 Generate Quickbase Token:

1. Login to Quickbase: https://kin.quickbase.com
2. Click your profile → My Preferences
3. Navigate to "My User Token" section
4. Click "Generate New Token"
5. Copy the token (you won't see it again!)
6. Paste into `.env.local` as `QUICKBASE_TOKEN`

#### 2.3 Get Neon Database URL:

1. Login to Neon: https://console.neon.tech
2. Select your project
3. Go to Dashboard → Connection Details
4. Copy the connection string (with pooler)
5. Paste into `.env.local` as `DATABASE_URL`

#### 2.4 Generate NextAuth Secret:

```bash
openssl rand -base64 32
```

Copy the output and paste into `.env.local` as `NEXTAUTH_SECRET`.

#### 2.5 Validate Environment:

```bash
npm run setup:env
```

This script checks that all variables are set correctly. Fix any ❌ errors before proceeding.

### Step 3: Initialize Database

#### 3.1 Run Database Setup:

```bash
npm run setup:db
```

This script:
- Enables the `pgcrypto` extension (required for UUID generation)
- Creates `users`, `sessions`, and `project_cache` tables
- Creates 5+ indexes for query performance
- Verifies schema creation including notification_settings table (if migration run)

**Expected Output:**
```
✅ Connected to database: neondb
✅ Enabled pgcrypto extension
✅ Migration complete
✅ Core tables verified: users, sessions, project_cache
✅ Notification settings table verified (if migration run)
✅ Verified indexes: 5+
```

**Troubleshooting:**
- **Connection refused:** Check `DATABASE_URL` is correct and Neon database is running
- **Permission denied:** Ensure database user has CREATE privileges
- **Tables already exist:** Safe to ignore if re-running setup

### Step 4: Investigate Quickbase User ID Mapping

**CRITICAL:** Before seeding users, you must discover the actual Quickbase user reference IDs.

#### Why This Matters
The application filters projects by user using Quickbase field 516 (CLOSER_ID). If the `quickbase_user_id` values in the database don't match the actual values in Quickbase, projects won't load for users.

#### Investigation Steps

1. **Discover all closer IDs in Quickbase:**
   ```bash
   node scripts/sync-closers-from-quickbase.js
   ```
   This will output a JSON array of all closers with their IDs, names, and emails.

2. **Map IDs to your users:**
   - Identify which Quickbase user IDs correspond to which application user emails
   - Create a mapping table (see `docs/QUICKBASE-USER-ID-MAPPING.md`)

3. **Update seed script:**
   - Edit `scripts/seed-users.js`
   - Replace placeholder `quickbase_user_id` values with real IDs from step 1
   - Pay special attention to Addison Richards (line 38)

4. **Verify IDs work:**
   ```bash
   node scripts/fetch-addison-projects.js
   ```
   If this returns projects, your IDs are correct!

5. **Seed database with correct IDs:**
   ```bash
   npm run setup:seed
   ```

#### Troubleshooting

**Problem:** `sync-closers-from-quickbase.js` returns no closers
- **Solution:** Check Quickbase credentials in `.env.local`
- **Solution:** Verify `QUICKBASE_TABLE_PROJECTS` is correct

**Problem:** `fetch-addison-projects.js` returns 0 projects
- **Solution:** The IDs in the script don't match Quickbase data
- **Solution:** Update the query with IDs from step 1 and try again

**Problem:** Projects still don't load after seeding
- **Solution:** Check browser console for API errors
- **Solution:** Verify the user's `quickbase_user_id` in the database matches Quickbase
- **Solution:** Run `node scripts/verify-user.js` to check user data

#### Quick Fix for Addison Only

If you just need to test with Addison Richards:

```bash
# 1. Update the script with Addison's real ID
# Edit scripts/update-addison-user.js line 44

# 2. Run the update
node scripts/update-addison-user.js

# 3. Test login
npm run dev
# Login as addison.r@kinhome.com
```

### Step 5: Seed Test Users

#### 5.1 Create Test Users:

```bash
npm run setup:seed
```

This creates 5 test users with bcrypt-hashed passwords:

| Role            | Email                  | Password    | Quickbase ID    |
|-----------------|------------------------|-------------|------------------|
| Closer          | closer@kinhome.com     | closer123   | closer-qb-001   |
| Setter          | setter@kinhome.com     | setter123   | setter-qb-001   |
| Office Leader   | office@kinhome.com     | office123   | office-qb-001   |
| Regional        | regional@kinhome.com   | regional123 | regional-qb-001 |
| Super Admin     | admin@kinhome.com      | admin123    | admin-qb-001    |

**⚠️ Security Note:** These are TEST credentials only. Never use in production!

**Troubleshooting:**
- **Duplicate key error:** Users already exist, safe to ignore
- **bcrypt error:** Ensure `bcryptjs` is installed: `npm install bcryptjs`

### Step 6: Run Settings Migration (Optional)

#### 6.1 Create Notification Settings Table:

```bash
npm run migrate:settings
```

This creates the `notification_settings` table with user preferences for:
- Email notifications (enabled by default)
- Urgent alerts (enabled by default) 
- Daily digest (disabled by default)
- Weekly summary (disabled by default)
- Threshold settings for hold alerts, age warnings, and install overdue

**Expected Output:**
```
✅ Settings migration completed
✅ Notification settings table created
✅ Default settings inserted for existing users
```

**Note:** This step is optional for basic functionality but required for notification features.

### Step 7: Run Health Check

#### 7.1 Validate All Integrations:

```bash
npm run setup:health
```

This comprehensive check validates:
- ✅ Environment variables are set correctly
- ✅ Database connection works
- ✅ Test users exist and passwords hash correctly
- ✅ Quickbase API is accessible with valid token
- ✅ Rate limiting is active (10 req/sec)
- ✅ Field constants are loaded (92 fields)

**Expected Output:**
```
========================================
🏥 HEALTH CHECK SUMMARY
========================================
✅ Environment Variables: OK
✅ Database Connection: OK
✅ NextAuth Authentication: OK
✅ Quickbase API: OK
✅ Field Constants: OK
========================================
🚀 System ready for development!
========================================
```

**Troubleshooting:**
- **Quickbase 401 error:** Token is invalid or expired, generate new token
- **Quickbase 403 error:** Token lacks permissions for table `br9kwm8na`
- **No test users found:** Run `npm run setup:seed` first

### Step 8: Start Development Server

```bash
npm run dev
```

Open http://localhost:3000 and login with any test user credentials.

## Step 9: Security Configuration (Optional)

### Enable Error Tracking with Sentry

Sentry provides error tracking and performance monitoring for production.

**8.1 Create Sentry Account:**

1. Sign up at https://sentry.io (free tier available)
2. Create new project: "Kin Solar Pipeline"
3. Select platform: "Next.js"
4. Copy the DSN (looks like: `https://abc123@o123.ingest.sentry.io/456`)

**8.2 Install Sentry Package:**

```bash
npm install @sentry/nextjs
```

**8.3 Configure Environment Variables:**

Add to `.env.local`:
```bash
SENTRY_DSN=your-server-dsn-here
NEXT_PUBLIC_SENTRY_DSN=your-client-dsn-here
```

**Note:** You can use the same DSN for both or create separate Sentry projects for client/server.

**8.4 Restart Development Server:**

```bash
npm run dev
```

Sentry will now capture errors automatically.

**8.5 Test Error Tracking:**

1. Trigger an error (e.g., invalid Quickbase query)
2. Check Sentry dashboard for captured error
3. Verify error includes context (user, project ID, etc.)

**Troubleshooting:**
- **Sentry not capturing errors:** Verify DSN is correct and app restarted
- **Too many errors:** Adjust sample rates in `sentry.client.config.ts`
- **Missing context:** Check that logger.ts is capturing errors correctly

### Security Audit

**8.6 Run Security Audit:**

```bash
npm run audit:prod
```

This checks production dependencies for known vulnerabilities.

**8.7 Review Results:**

- Critical/High: Must fix before production deployment
- Moderate: Review and document acceptable risks
- Low: Can be addressed in future updates

**8.8 Fix Vulnerabilities:**

```bash
npm run audit:fix
```

This automatically updates packages where possible.

**8.9 Document Findings:**

See `docs/SECURITY-READINESS.md` for detailed security documentation.

### Step 10: Set Up Managers and Office Assignments (Optional)

**Purpose**: Configure manager hierarchies and office assignments to enable team visibility and management features.

**When to do this**:
- After basic setup is complete (Steps 1-9)
- When you have multiple users and need team management
- When you have office leaders or area directors who need to see team projects

**Quick Setup**:

1. **Create manager users** (if not already created):
   ```bash
   # Use the UI: Settings → Users → Add User
   # Or use SQL:
   INSERT INTO users (name, email, role, office, is_active)
   VALUES ('Office Leader', 'leader@kinhome.com', 'office_leader', 'Phoenix', true);
   ```

2. **Assign offices to office-based managers**:
   - Navigate to Settings → Offices tab
   - Click "Bulk Assign Offices" button
   - Select managers and offices
   - Choose access level (use "Manage" for most cases)
   - Click "Assign Offices"
   - Verify: Check `office_assignments` table has rows

3. **Assign users to team leads**:
   - Navigate to Settings → Hierarchy tab
   - Drag reps onto team lead cards (or use bulk assign)
   - Verify: Team lead's card shows "Manages X users"

4. **Verify manager can see team projects**:
   - Log in as the manager
   - Check dashboard shows team metrics
   - Check project list includes team projects
   - Use ownership filter to distinguish personal vs team projects

**Detailed Instructions**:

For comprehensive step-by-step instructions, see:
- **docs/MANAGER-SETUP-GUIDE.md**: Complete setup walkthrough
- **docs/MANAGER-ONBOARDING-CHECKLIST.md**: Checklist for onboarding new managers
- **docs/HIERARCHY-TROUBLESHOOTING.md**: Troubleshooting guide

**Quick Verification**:

```bash
# Check office assignments
psql $DATABASE_URL -c "SELECT u.name, u.role, ARRAY_AGG(oa.office_name) as offices FROM users u LEFT JOIN office_assignments oa ON u.id = oa.user_id WHERE u.role IN ('office_leader', 'area_director', 'divisional') GROUP BY u.id, u.name, u.role;"

# Check team lead assignments
psql $DATABASE_URL -c "SELECT m.name as manager, COUNT(h.user_id) as managed_count FROM users m LEFT JOIN user_hierarchies h ON m.id = h.manager_id WHERE m.role = 'team_lead' GROUP BY m.id, m.name;"
```

**Expected Output**:
- Office leaders have 1+ offices assigned
- Team leads have 1+ managed users
- No errors in query results

**Troubleshooting**:
- **No office assignments**: Run bulk assign offices via UI
- **No team lead assignments**: Assign users via hierarchy tree view
- **Manager can't see team projects**: See docs/HIERARCHY-TROUBLESHOOTING.md

**Skip this step if**:
- You only have individual reps (no managers)
- You're just testing the app with one user
- You'll set up hierarchies later in production

---

## Quick Setup (All Steps)

Run all setup steps in sequence:

```bash
npm run setup:all
```

This executes: `setup:env` → `setup:db` → `setup:seed` → `setup:health`

**Note:** For notification features, also run:
```bash
npm run migrate:settings
```

## Common Issues

### Issue: "Cannot find module '@vercel/postgres'"
**Solution:** Run `npm install` to install dependencies

### Issue: "gen_random_uuid() does not exist"
**Solution:** Run `npm run setup:db` to enable pgcrypto extension

### Issue: "Invalid Quickbase token"
**Solution:** Generate fresh token at https://kin.quickbase.com → My Preferences → My User Token

### Issue: "Database connection timeout"
**Solution:** Check Neon database is running and `DATABASE_URL` includes `-pooler` suffix

### Issue: "NextAuth session not persisting"
**Solution:** Verify `NEXTAUTH_SECRET` is set and has length >=32 characters

### Issue: "Cookies not working in production"
**Solution:** Verify `NEXTAUTH_URL` is set to production HTTPS URL
- Solution: Verify hosting platform serves app over HTTPS
- Solution: Check browser DevTools → Application → Cookies for `__Secure-` prefix

### Issue: "Sentry errors not appearing"
**Solution:** Verify `SENTRY_DSN` is set correctly
- Solution: Verify `@sentry/nextjs` is installed
- Solution: Check Sentry dashboard project settings
- Solution: Trigger a test error to verify integration

## Next Steps

Once Phase 1 setup is complete:
1. Review `docs/TRAYCER-IMPLEMENTATION-BRIEF-ULTIMATE.md` for implementation details
2. Explore `lib/constants/fieldIds.ts` to understand the 92 Quickbase fields
3. Test Quickbase queries in `lib/quickbase/queries.ts`
4. Set up manager hierarchies and office assignments (see Step 10)
5. Review docs/MANAGER-SETUP-GUIDE.md for detailed manager setup instructions
6. Begin Phase 2: Dashboard & Project List implementation

## Additional Resources

- **Implementation Guide:** `docs/TRAYCER-IMPLEMENTATION-BRIEF-ULTIMATE.md`
- **Field Usage Analysis:** `docs/UNDERSTANDING-FIELD-USAGE.md`
- **Manager Setup Guide:** `docs/MANAGER-SETUP-GUIDE.md`
- **Hierarchy Troubleshooting:** `docs/HIERARCHY-TROUBLESHOOTING.md`
- **Manager Onboarding Checklist:** `docs/MANAGER-ONBOARDING-CHECKLIST.md`
- **Role Hierarchy Reference:** `docs/ROLE-HIERARCHY-REFERENCE.md`
- **Office-Based Visibility:** `docs/OFFICE_BASED_VISIBILITY.md`
- **Quickbase API Spec:** `QuickBase_RESTful_API_2025-08-28T17_29_31.942Z.json`
- **Main README:** `README.md`
