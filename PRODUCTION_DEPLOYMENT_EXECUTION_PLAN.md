# Arrivy Integration - Production Deployment Execution Plan

**Version:** 1.0  
**Last Updated:** October 29, 2025  
**Estimated Total Time:** 45-60 minutes  
**Status:** Ready for Production Deployment

---

## Table of Contents

1. [Pre-Deployment Checklist](#pre-deployment-checklist)
2. [Phase 1: Environment Variable Configuration](#phase-1-environment-variable-configuration-10-minutes)
3. [Phase 2: Database Migration Execution](#phase-2-database-migration-execution-15-minutes)
4. [Phase 3: Application Deployment](#phase-3-application-deployment-10-minutes)
5. [Phase 4: Arrivy External Configuration](#phase-4-arrivy-external-configuration-10-minutes)
6. [Phase 5: Initial Data Sync](#phase-5-initial-data-sync-5-minutes)
7. [Phase 6: Production Testing & Validation](#phase-6-production-testing--validation-10-minutes)
8. [Phase 7: Clean Up Test Data](#phase-7-clean-up-test-data-2-minutes)
9. [Phase 8: Post-Deployment Verification](#phase-8-post-deployment-verification-5-minutes)
10. [Deployment Success Criteria](#deployment-success-criteria)
11. [Rollback Plan](#rollback-plan)
12. [Post-Deployment Tasks](#post-deployment-tasks)
13. [Support & Troubleshooting](#support--troubleshooting)
14. [Deployment Completion Sign-Off](#deployment-completion-sign-off)
15. [Next Steps](#next-steps)

---

## Pre-Deployment Checklist

Before starting the deployment, verify all prerequisites are met:

### Code & Testing
- [ ] All local tests have passed
- [ ] Code reviewed and approved
- [ ] Git repository is clean (no uncommitted changes)
- [ ] All changes committed to main branch
- [ ] Local Arrivy integration tested successfully

### Infrastructure Access
- [ ] Vercel CLI installed and authenticated (`vercel whoami`)
- [ ] Production database connection string available
- [ ] Can connect to production database (`psql "$PROD_DB" -c "SELECT version();"`)
- [ ] Arrivy dashboard access confirmed (https://app.arrivy.com/)
- [ ] Arrivy API credentials available and tested

### Documentation
- [ ] Current production state documented for rollback reference
- [ ] Team notified of deployment window
- [ ] Post-deployment communication plan ready

### Environment Variables Ready
Ensure you have the following values ready:
- ARRIVY_AUTH_KEY: `0a27a7e3-e6b5`
- ARRIVY_AUTH_TOKEN: `5730gWxBjDzbQDEeFh3zrs`
- ARRIVY_COMPANY_NAME: `KIN Home`
- ARRIVY_WEBHOOK_SECRET: `GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=`
- ARRIVY_BASE_URL: `https://app.arrivy.com/api`
- ARRIVY_RATE_LIMIT: `30`

---

## Phase 1: Environment Variable Configuration (10 minutes)

### Objective
Set all 6 Arrivy environment variables in Vercel production environment before deploying code.

### Steps

#### 1.1 Authenticate with Vercel
```bash
vercel whoami
```
**Expected Output:** Your Vercel account email

If not authenticated:
```bash
vercel login
```

#### 1.2 Set Environment Variables
Execute each command, selecting "Production" environment when prompted:

```bash
# Set Auth Key
vercel env add ARRIVY_AUTH_KEY
# When prompted, paste: 0a27a7e3-e6b5
# Select: Production
# Add to all projects? No (current project only)

# Set Auth Token
vercel env add ARRIVY_AUTH_TOKEN
# When prompted, paste: 5730gWxBjDzbQDEeFh3zrs
# Select: Production

# Set Company Name
vercel env add ARRIVY_COMPANY_NAME
# When prompted, paste: KIN Home
# Select: Production

# Set Webhook Secret
vercel env add ARRIVY_WEBHOOK_SECRET
# When prompted, paste: GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=
# Select: Production

# Set Base URL
vercel env add ARRIVY_BASE_URL
# When prompted, paste: https://app.arrivy.com/api
# Select: Production

# Set Rate Limit
vercel env add ARRIVY_RATE_LIMIT
# When prompted, paste: 30
# Select: Production
```

#### 1.3 Verify Environment Variables
```bash
vercel env ls
```

**Expected Output:** Should show all 6 Arrivy variables with "Production" scope

### Verification Checklist
- [ ] All 6 environment variables set
- [ ] All variables scoped to "Production"
- [ ] No typos in variable names
- [ ] Values match credentials from `.env.local`

### Troubleshooting
- **Error: "Not authenticated"** → Run `vercel login`
- **Error: "No project linked"** → Run `vercel link` in project directory
- **Wrong value entered** → Use `vercel env rm VARIABLE_NAME` and re-add

---

## Phase 2: Database Migration Execution (15 minutes)

### Objective
Execute three database migrations in sequence to create Arrivy tables and modify existing schema.

### Prerequisites
```bash
# Set production database connection string
export PROD_DB="your-production-database-url"

# Test connection
psql "$PROD_DB" -c "SELECT version();"
```
**Expected Output:** PostgreSQL version number

### Migration 014: Create Core Arrivy Tables

#### 2.1 Execute Migration 014
```bash
psql "$PROD_DB" -f lib/db/migrations/014_create_arrivy_tables.sql
```

**Expected Output:**
```
BEGIN
CREATE TABLE (arrivy_tasks)
CREATE TABLE (arrivy_entities)
CREATE TABLE (arrivy_events)
CREATE TABLE (arrivy_task_status)
CREATE INDEX (13 indexes)
CREATE TRIGGER (2 triggers)
COMMIT
```

#### 2.2 Verify Migration 014
```sql
-- Check tables created
SELECT table_name 
FROM information_schema.tables 
WHERE table_name LIKE 'arrivy_%' 
ORDER BY table_name;
```
**Expected Output:** 4 tables (arrivy_entities, arrivy_events, arrivy_tasks, arrivy_task_status)

```sql
-- Check indexes
SELECT indexname 
FROM pg_indexes 
WHERE tablename LIKE 'arrivy_%' 
ORDER BY tablename, indexname;
```
**Expected Output:** 13 indexes across the 4 tables

```sql
-- Check triggers
SELECT trigger_name, event_object_table 
FROM information_schema.triggers 
WHERE trigger_name LIKE '%arrivy%';
```
**Expected Output:** 2 triggers (update_arrivy_tasks_updated_at, update_arrivy_events_updated_at)

### Migration 015: Create Join Table

#### 2.3 Execute Migration 015
```bash
psql "$PROD_DB" -f lib/db/migrations/015_create_arrivy_task_entities_join_table.sql
```

**Expected Output:**
```
BEGIN
CREATE TABLE (arrivy_task_entities)
CREATE INDEX (4 indexes)
Backfilled X task-entity relationships
COMMIT
```

#### 2.4 Verify Migration 015
```sql
-- Check join table created
SELECT table_name 
FROM information_schema.tables 
WHERE table_name = 'arrivy_task_entities';
```
**Expected Output:** 1 row (arrivy_task_entities)

```sql
-- Check backfill count
SELECT COUNT(*) as backfilled_relationships 
FROM arrivy_task_entities;
```
**Expected Output:** Count should match number in migration output (likely 0 on fresh deployment)

### Migration 016: Make QuickBase Fields Optional

#### 2.5 Execute Migration 016
```bash
psql "$PROD_DB" -f lib/db/migrations/016_make_quickbase_fields_optional.sql
```

**Expected Output:**
```
BEGIN
ALTER TABLE (quickbase_project_id nullable)
ALTER TABLE (quickbase_record_id nullable)
Cleaned up X placeholder records
Cleaned up Y zero-value records
COMMIT
```

#### 2.6 Verify Migration 016
```sql
-- Check columns are nullable
SELECT column_name, is_nullable 
FROM information_schema.columns 
WHERE table_name = 'arrivy_tasks' 
  AND column_name IN ('quickbase_project_id', 'quickbase_record_id');
```
**Expected Output:** Both columns should show `is_nullable = 'YES'`

```sql
-- Verify no placeholder data remains
SELECT COUNT(*) 
FROM arrivy_tasks 
WHERE quickbase_project_id LIKE 'ARRIVY-%' 
   OR quickbase_record_id = 0;
```
**Expected Output:** 0 rows

### Phase 2 Verification Checklist
- [ ] Migration 014: 4 tables created
- [ ] Migration 014: 13 indexes created
- [ ] Migration 014: 2 triggers created
- [ ] Migration 015: Join table created
- [ ] Migration 015: Data backfilled (if applicable)
- [ ] Migration 016: Fields made nullable
- [ ] Migration 016: Placeholder data cleaned
- [ ] No migration errors in output

### Troubleshooting
- **Error: "relation already exists"** → Migration already run; verify with queries above
- **Error: "permission denied"** → Check database user has CREATE TABLE permissions
- **Backfill count seems wrong** → Acceptable on fresh deployment; tasks will sync in Phase 5

---

## Phase 3: Application Deployment (10 minutes)

### Objective
Deploy the application to Vercel production with Arrivy integration code.

### Steps

#### 3.1 Verify Git Status
```bash
git status
```
**Expected Output:** "nothing to commit, working tree clean"

If there are uncommitted changes:
```bash
git add .
git commit -m "feat: Arrivy integration production deployment"
git push origin main
```

#### 3.2 Deploy to Production
```bash
vercel --prod
```

**Expected Output:**
```
🔍  Inspect: https://vercel.com/[your-project]/[deployment-id]
✅  Production: https://[your-domain].vercel.app [2-3 minutes]
```

**Note:** Deployment typically takes 2-3 minutes. Wait for "Production" URL to appear.

#### 3.3 Note Production URL
Save the production URL from output for Phase 4 webhook configuration.

Example: `https://rep-dashboard.vercel.app`

#### 3.4 Test Webhook Endpoint
```bash
curl https://[your-domain].vercel.app/api/operations/field-tracking/webhook
```

**Expected Output:**
```json
{"status":"ok"}
```

#### 3.5 Verify Dashboard Accessibility
Open in browser:
```
https://[your-domain].vercel.app/operations/field-tracking
```

**Expected Output:** Dashboard page loads (may show no tasks yet - this is normal)

### Phase 3 Verification Checklist
- [ ] Git repository is clean
- [ ] Deployment completed successfully
- [ ] Production URL noted and accessible
- [ ] Webhook endpoint responds with {"status":"ok"}
- [ ] Dashboard page loads without errors
- [ ] No console errors in browser developer tools

### Troubleshooting
- **Deployment fails** → Check Vercel logs: `vercel logs [deployment-url]`
- **Webhook endpoint 404** → Verify route exists in `app/api/operations/field-tracking/webhook/route.ts`
- **Dashboard 500 error** → Check environment variables are set correctly
- **Build errors** → Ensure all dependencies in package.json and code compiles locally

---

## Phase 4: Arrivy External Configuration (10 minutes)

### Objective
Configure webhook in Arrivy dashboard to send events to production application.

### Steps

#### 4.1 Log into Arrivy Dashboard
1. Navigate to: https://app.arrivy.com/
2. Log in with admin credentials
3. Verify you're in the correct company account (KIN Home)

#### 4.2 Navigate to Webhook Settings
1. Click **Settings** in left sidebar
2. Click **Integrations**
3. Click **Webhooks** tab

#### 4.3 Create New Webhook

**Configuration:**
- **Webhook URL:** `https://[your-domain].vercel.app/api/operations/field-tracking/webhook`
- **Webhook Secret:** `GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=`
- **Event Types:** Select all 8 events:
  - ✅ TASK_CREATED
  - ✅ TASK_STATUS
  - ✅ CREW_ASSIGNED
  - ✅ ARRIVING
  - ✅ LATE
  - ✅ NOSHOW
  - ✅ TASK_RATING
  - ✅ EXCEPTION

#### 4.4 Save and Test Webhook
1. Click **Save**
2. Click **Test Webhook** button
3. Select any event type (e.g., TASK_CREATED)
4. Click **Send Test**

#### 4.5 Verify Test Webhook Delivery
Check Vercel logs:
```bash
vercel logs --prod -n 10
```

**Expected Output:** Log entry showing test webhook received:
```
POST /api/operations/field-tracking/webhook 200
Webhook event: TASK_CREATED
```

#### 4.6 Verify or Create Field Crew Entities

**Option A: Manual Creation (5-10 minutes)**
1. Navigate to **Team** → **Entities**
2. For each field crew, create entity:
   - **Name:** [Crew Name]
   - **Type:** crew
   - **External ID:** [QuickBase crew ID if available]
   - **Status:** Active

**Option B: Sync Script (Recommended, 1 minute)**
```bash
npm run sync:arrivy:entities
```
**Expected Output:** "Synced X entities from Arrivy"

Verify entities in database:
```sql
SELECT id, name, type, external_id 
FROM arrivy_entities 
WHERE type = 'crew' 
ORDER BY name;
```

### Phase 4 Verification Checklist
- [ ] Webhook created in Arrivy dashboard
- [ ] Webhook URL matches production domain
- [ ] Webhook secret matches environment variable
- [ ] All 8 event types selected
- [ ] Test webhook sent successfully
- [ ] Test webhook appears in Vercel logs
- [ ] Field crew entities exist in Arrivy

### Troubleshooting
- **Test webhook fails** → Verify webhook URL is correct and HTTPS
- **401 error in logs** → Check webhook secret matches exactly
- **No logs appearing** → Ensure you're checking production logs, not preview
- **Entities not found** → Run sync script or create manually in Arrivy dashboard

---

## Phase 5: Initial Data Sync (5 minutes)

### Objective
Populate database with all existing tasks from Arrivy to establish baseline data.

### Steps

#### 5.1 Run Initial Sync
```bash
npm run sync:arrivy
```

**Expected Output:**
```
Starting Arrivy sync...
Fetching tasks from Arrivy API...
Processing batch 1 of 3 (100 tasks)...
Processing batch 2 of 3 (100 tasks)...
Processing batch 3 of 3 (47 tasks)...
✅ Sync complete: 247 tasks processed
   - Created: 247
   - Updated: 0
   - Failed: 0
Duration: 8.3 seconds
```

**Note:** Numbers will vary based on actual task count in Arrivy

#### 5.2 Monitor Progress
Watch for:
- Batch processing logs
- Task count
- Any error messages
- Total duration (should be < 30 seconds for < 1000 tasks)

#### 5.3 Verify Tasks in Database
```sql
-- Check total task count
SELECT COUNT(*) as total_tasks FROM arrivy_tasks;
```
**Expected Output:** Count should match sync output (e.g., 247)

```sql
-- Check sample tasks with details
SELECT 
  id,
  title,
  customer_first_name,
  customer_last_name,
  task_type,
  current_status,
  start_datetime
FROM arrivy_tasks 
ORDER BY start_datetime DESC 
LIMIT 5;
```
**Expected Output:** 5 most recent tasks with customer names and types populated

```sql
-- Check status distribution
SELECT current_status, COUNT(*) as count 
FROM arrivy_tasks 
GROUP BY current_status 
ORDER BY count DESC;
```
**Expected Output:** Distribution of tasks by status (e.g., COMPLETE: 180, ENROUTE: 35, etc.)

### Phase 5 Verification Checklist
- [ ] Sync script completed without errors
- [ ] Task count in database matches sync output
- [ ] Sample tasks have populated fields (customer names, types, dates)
- [ ] No duplicate tasks created (check by external_id)
- [ ] Task status distribution looks reasonable

### Troubleshooting
- **Sync script fails** → Check Arrivy credentials in environment variables
- **Rate limit errors** → Script should handle with exponential backoff; wait and retry
- **Duplicate tasks** → Delete and re-run sync after fixing issue
- **Missing customer data** → Normal for some tasks; verify a few have complete data
- **Sync takes > 1 minute** → Acceptable for > 500 tasks; monitor for completion

---

## Phase 6: Production Testing & Validation (10 minutes)

### Objective
Execute comprehensive end-to-end tests to validate all functionality in production environment.

### Test 1: Dashboard Access & Data Display

#### Steps:
1. Navigate to: `https://[your-domain].vercel.app/operations/field-tracking`
2. Verify page loads within 2 seconds
3. Check that tasks are displayed in list view
4. Verify filters are functional (status, type, date range)

#### Expected Results:
- ✅ Dashboard loads successfully
- ✅ Task cards display with customer names and status
- ✅ Filters update task list
- ✅ No console errors

### Test 2: Create Test Task in Arrivy

#### Steps:
1. Log into Arrivy dashboard
2. Create new task:
   - **Title:** "PROD TEST - Solar Installation"
   - **External ID:** `PROD-TEST-001`
   - **Customer:** "Test Customer"
   - **Start Date:** Today
   - **Crew:** Assign any field crew
   - **Status:** NEW
3. Save task

#### Expected Results:
- ✅ Task created successfully in Arrivy
- ✅ Task appears in Arrivy task list

### Test 3: Verify Webhook Delivery

#### Steps:
1. Wait 30 seconds for webhook delivery
2. Check Vercel logs:
```bash
vercel logs --prod -n 20 | grep "PROD-TEST-001"
```

3. Check database:
```sql
SELECT 
  id, 
  title, 
  external_id, 
  current_status,
  created_at 
FROM arrivy_tasks 
WHERE external_id = 'PROD-TEST-001';
```

#### Expected Results:
- ✅ Webhook log shows TASK_CREATED event received
- ✅ Task exists in database with correct details
- ✅ created_at timestamp is recent (< 1 minute ago)

### Test 4: Dashboard Real-Time Update

#### Steps:
1. Refresh dashboard in browser
2. Search or filter for "PROD TEST"
3. Verify test task appears

#### Expected Results:
- ✅ Test task visible in dashboard within 30 seconds
- ✅ Task card shows correct customer name and status
- ✅ Task card displays crew assignment

### Test 5: Task Status Update

#### Steps:
1. In Arrivy dashboard, update test task status to ENROUTE
2. Wait 30 seconds
3. Check Vercel logs:
```bash
vercel logs --prod -n 20 | grep "TASK_STATUS"
```

4. Refresh dashboard and verify status change

#### Expected Results:
- ✅ Webhook log shows TASK_STATUS event
- ✅ Database updated with new status
- ✅ Dashboard reflects status change (color/badge updated)

### Test 6: Customer Tracker URL

#### Steps:
1. Click on test task card in dashboard
2. Locate "Track This Job" or customer tracker URL
3. Click URL to open tracker page

#### Expected Results:
- ✅ Tracker URL opens successfully
- ✅ Tracker page shows task details
- ✅ Customer can see crew location/status (if GPS enabled)

### Test 7: Task Detail Modal

#### Steps:
1. Click on test task to open detail modal
2. Verify all sections display correctly:
   - Customer information
   - Task details
   - Crew assignment
   - Timeline/milestones
   - Activity feed

#### Expected Results:
- ✅ Modal opens without errors
- ✅ All data fields populated
- ✅ Timeline shows events
- ✅ Activity feed shows creation and status change

### Test 8: Activity Feed Events

#### Steps:
1. Navigate to activity feed tab
2. Filter by today's date
3. Verify test task events appear

#### Expected Results:
- ✅ TASK_CREATED event shows with timestamp
- ✅ TASK_STATUS event shows with status change
- ✅ Events are chronologically ordered
- ✅ Event details are accurate

### Test 9: Crew Performance Dashboard

#### Steps:
1. Navigate to: `https://[your-domain].vercel.app/operations/crew-performance`
2. Verify metrics load
3. Check that test task affects crew stats

#### Expected Results:
- ✅ Dashboard loads successfully
- ✅ Crew metrics display
- ✅ Assigned crew shows updated task count
- ✅ Charts render correctly

### Test 10: API Response Times

#### Steps:
1. Open browser developer tools (Network tab)
2. Navigate through dashboard pages
3. Check API response times for key endpoints:
   - `/api/operations/field-tracking/tasks`
   - `/api/operations/field-tracking/tasks/[id]`
   - `/api/operations/field-tracking/events`

#### Expected Results:
- ✅ All API calls complete in < 2 seconds
- ✅ No 500 errors
- ✅ No timeout errors
- ✅ Response payloads are correct

### Phase 6 Verification Checklist
- [ ] All 10 tests passed
- [ ] Dashboard displays tasks correctly
- [ ] Webhooks delivering in < 30 seconds
- [ ] Status updates reflected in real-time
- [ ] Task detail modal shows complete information
- [ ] Activity feed captures all events
- [ ] Crew performance metrics accurate
- [ ] API response times acceptable (< 2 seconds)
- [ ] No critical errors in logs
- [ ] Customer tracker URLs functional

### Troubleshooting
- **Webhook not received** → Check webhook URL in Arrivy, verify production logs
- **Task not in database** → Manually sync with `npm run sync:arrivy`
- **Dashboard not updating** → Clear browser cache, check API endpoint directly
- **Slow API responses** → Check database indexes, monitor Vercel function duration
- **Modal errors** → Check console for detailed error messages

---

## Phase 7: Clean Up Test Data (2 minutes)

### Objective
Remove test data created during validation to keep production database clean.

### Steps

#### 7.1 Delete Test Task from Arrivy
1. Log into Arrivy dashboard
2. Navigate to task list
3. Search for "PROD-TEST-001"
4. Open task and click **Delete**
5. Confirm deletion

#### 7.2 Delete Test Tasks from Database
```sql
-- Delete test task
DELETE FROM arrivy_tasks 
WHERE external_id LIKE 'PROD-TEST-%';
```

**Expected Output:** `DELETE X` (where X is number of test tasks, typically 1)

#### 7.3 Delete Related Test Events
```sql
-- Delete events for test tasks
DELETE FROM arrivy_events 
WHERE task_id NOT IN (SELECT id FROM arrivy_tasks);
```

**Expected Output:** `DELETE Y` (where Y is number of orphaned events)

#### 7.4 Verify Cleanup
```sql
-- Verify no test tasks remain
SELECT COUNT(*) 
FROM arrivy_tasks 
WHERE external_id LIKE 'PROD-TEST-%';
```
**Expected Output:** 0

```sql
-- Verify no orphaned events
SELECT COUNT(*) 
FROM arrivy_events 
WHERE task_id NOT IN (SELECT id FROM arrivy_tasks);
```
**Expected Output:** 0

### Phase 7 Verification Checklist
- [ ] Test task deleted from Arrivy
- [ ] Test task deleted from database
- [ ] Related events cleaned up
- [ ] No orphaned records remain
- [ ] Production data count is accurate

### Troubleshooting
- **Can't delete from Arrivy** → Mark as complete/archived instead
- **Foreign key constraint errors** → Delete related records first (events, then tasks)
- **Other test data found** → Repeat cleanup for any additional test records

---

## Phase 8: Post-Deployment Verification (5 minutes)

### Objective
Final comprehensive health check to ensure all systems are operating correctly.

### Steps

#### 8.1 Monitor Webhook Delivery
Watch Vercel logs for 2-3 minutes:
```bash
vercel logs --prod --follow
```

**Look for:**
- Incoming webhook events (should see occasional TASK_STATUS updates)
- No 500 errors
- No authentication failures
- Response times < 500ms for webhook endpoints

**Expected Pattern:**
```
POST /api/operations/field-tracking/webhook 200 - 234ms
Webhook event: TASK_STATUS
Task updated: [task-id]
```

#### 8.2 Check Database Table Counts
```sql
-- Verify all tables have data
SELECT 
  'arrivy_tasks' as table_name, 
  COUNT(*) as row_count 
FROM arrivy_tasks
UNION ALL
SELECT 'arrivy_entities', COUNT(*) FROM arrivy_entities
UNION ALL
SELECT 'arrivy_events', COUNT(*) FROM arrivy_events
UNION ALL
SELECT 'arrivy_task_status', COUNT(*) FROM arrivy_task_status
UNION ALL
SELECT 'arrivy_task_entities', COUNT(*) FROM arrivy_task_entities
ORDER BY table_name;
```

**Expected Output:**
- arrivy_tasks: > 0 (should match initial sync count)
- arrivy_entities: > 0 (should have crew members)
- arrivy_events: ≥ 0 (will grow over time)
- arrivy_task_status: ≥ 0 (historical status changes)
- arrivy_task_entities: ≥ 0 (task-crew relationships)

#### 8.3 Test API Response Times
```bash
# Test tasks endpoint
time curl -s "https://[your-domain].vercel.app/api/operations/field-tracking/tasks" > /dev/null
```
**Expected Output:** < 2 seconds

```bash
# Test events endpoint
time curl -s "https://[your-domain].vercel.app/api/operations/field-tracking/events" > /dev/null
```
**Expected Output:** < 2 seconds

#### 8.4 Review Error Logs
Check for any Arrivy-related errors:
```bash
vercel logs --prod -n 100 | grep -i "error\|exception\|failed"
```

**Expected Output:** No critical errors related to Arrivy integration

Acceptable errors:
- Occasional 429 (rate limit) with retry logs
- Missing optional fields warnings

Unacceptable errors:
- 500 errors on webhook endpoint
- Authentication failures
- Database connection errors
- Repeated failures without recovery

#### 8.5 Verify Dashboard Functionality
Final manual check:
1. Open dashboard: `https://[your-domain].vercel.app/operations/field-tracking`
2. Verify tasks load within 2 seconds
3. Open 2-3 task detail modals
4. Check activity feed shows recent events
5. Test all filters (status, type, date range)
6. Navigate to crew performance page
7. Verify all charts and metrics load

### Phase 8 Verification Checklist
- [ ] Webhook logs showing successful deliveries
- [ ] No authentication or 500 errors in logs
- [ ] All database tables populated
- [ ] API response times < 2 seconds
- [ ] No critical errors in last 10 minutes
- [ ] Dashboard fully functional
- [ ] Task detail modals working
- [ ] Activity feed displaying events
- [ ] Crew performance metrics accurate
- [ ] All filters and navigation working

### Final Health Check
Run this comprehensive query to verify data integrity:
```sql
-- System health check
SELECT 
  'Tasks' as metric,
  COUNT(*) as count,
  MIN(created_at) as earliest,
  MAX(created_at) as latest
FROM arrivy_tasks
UNION ALL
SELECT 
  'Events',
  COUNT(*),
  MIN(created_at),
  MAX(created_at)
FROM arrivy_events
UNION ALL
SELECT 
  'Entities',
  COUNT(*),
  MIN(created_at),
  MAX(created_at)
FROM arrivy_entities;
```

**Expected Output:** All counts > 0, recent timestamps for latest records

### Troubleshooting
- **High error rate** → Check specific error messages and consult troubleshooting section
- **Slow response times** → Monitor Vercel function duration, check database connection pool
- **Missing data** → Re-run sync script: `npm run sync:arrivy`
- **Webhook delivery issues** → Verify webhook still active in Arrivy dashboard

---

## Deployment Success Criteria

### Database Criteria (5 items)
- [ ] ✅ All 5 Arrivy tables created (tasks, entities, events, task_status, task_entities)
- [ ] ✅ All 17 indexes created and active
- [ ] ✅ 2 triggers active (updated_at for tasks and events)
- [ ] ✅ QuickBase fields (project_id, record_id) are nullable
- [ ] ✅ No placeholder or invalid data in tables

### Environment Criteria (2 items)
- [ ] ✅ All 6 Arrivy environment variables set in Vercel production
- [ ] ✅ Environment variables verified with `vercel env ls`

### Application Criteria (3 items)
- [ ] ✅ Application deployed to production successfully
- [ ] ✅ Webhook endpoint responds with 200 status
- [ ] ✅ Dashboard page accessible and loads without errors

### Arrivy Configuration Criteria (5 items)
- [ ] ✅ Webhook configured in Arrivy dashboard
- [ ] ✅ Webhook secret matches environment variable
- [ ] ✅ All 8 event types selected
- [ ] ✅ Test webhook delivered successfully
- [ ] ✅ Field crew entities synced or created

### Functionality Criteria (10 items)
- [ ] ✅ Initial sync completed (all existing tasks imported)
- [ ] ✅ Webhook events processing in real-time (< 30 second delay)
- [ ] ✅ Tasks display correctly in dashboard
- [ ] ✅ Task detail modal shows complete information
- [ ] ✅ Status updates reflect in UI within 30 seconds
- [ ] ✅ Activity feed captures and displays events
- [ ] ✅ Crew performance dashboard shows accurate metrics
- [ ] ✅ Customer tracker URLs functional
- [ ] ✅ All API endpoints respond in < 2 seconds
- [ ] ✅ No critical errors in production logs

**Overall Status:** [ ] All 25 criteria met - Deployment Successful

---

## Rollback Plan

If deployment fails or critical issues arise, follow this rollback procedure:

### Step 1: Disable Webhook (Immediate - 1 minute)

**Action:**
1. Log into Arrivy dashboard
2. Navigate to Settings → Integrations → Webhooks
3. Find production webhook
4. Click **Disable** or **Delete**
5. Confirm action

**Verification:**
```bash
# Should see no new webhook events
vercel logs --prod --follow
```

**Purpose:** Stop incoming webhook events immediately to prevent errors

### Step 2: Revert Application Deployment (2 minutes)

**Action:**
```bash
# List recent deployments
vercel ls

# Rollback to previous deployment
vercel rollback [previous-deployment-url]
```

**Verification:**
```bash
# Check current production deployment
vercel ls --prod
```

**Purpose:** Restore previous working version of application

### Step 3: Rollback Database Migrations (If Necessary - 5 minutes)

**⚠️ WARNING:** Only execute if migrations caused critical issues. Evaluate carefully.

#### Rollback Migration 016
```sql
BEGIN;

-- Restore NOT NULL constraints
ALTER TABLE arrivy_tasks 
ALTER COLUMN quickbase_project_id SET NOT NULL;

ALTER TABLE arrivy_tasks 
ALTER COLUMN quickbase_record_id SET NOT NULL;

COMMIT;
```

#### Rollback Migration 015
```sql
BEGIN;

-- Drop join table
DROP TABLE IF EXISTS arrivy_task_entities;

COMMIT;
```

#### Rollback Migration 014
```sql
BEGIN;

-- Drop all Arrivy tables (cascades to dependent objects)
DROP TABLE IF EXISTS arrivy_tasks CASCADE;
DROP TABLE IF EXISTS arrivy_entities CASCADE;
DROP TABLE IF EXISTS arrivy_events CASCADE;
DROP TABLE IF EXISTS arrivy_task_status CASCADE;

COMMIT;
```

**Verification:**
```sql
-- Verify tables removed
SELECT table_name 
FROM information_schema.tables 
WHERE table_name LIKE 'arrivy_%';
```
**Expected Output:** 0 rows (or 4 rows if only partial rollback)

### Step 4: Remove Environment Variables (Optional - 2 minutes)

**Action:**
```bash
vercel env rm ARRIVY_AUTH_KEY
vercel env rm ARRIVY_AUTH_TOKEN
vercel env rm ARRIVY_COMPANY_NAME
vercel env rm ARRIVY_WEBHOOK_SECRET
vercel env rm ARRIVY_BASE_URL
vercel env rm ARRIVY_RATE_LIMIT
```

**Purpose:** Clean up environment if completely reverting Arrivy integration

### Step 5: Verify Rollback Success (2 minutes)

#### Check Application
```bash
# Verify deployment rolled back
vercel ls --prod

# Check application loads
curl -I https://[your-domain].vercel.app
```

#### Check Database
```sql
-- If full rollback, should be empty
SELECT COUNT(*) FROM information_schema.tables 
WHERE table_name LIKE 'arrivy_%';

-- If partial rollback, check remaining tables
SELECT table_name FROM information_schema.tables 
WHERE table_name LIKE 'arrivy_%';
```

#### Check Logs
```bash
# Should see no new Arrivy webhook events
vercel logs --prod -n 20
```

### Step 6: Document Rollback (Critical)

Create incident report documenting:
- **Rollback Time:** [timestamp]
- **Reason:** [what failed]
- **Steps Taken:** [which rollback steps executed]
- **Current State:** [what's reverted, what remains]
- **Root Cause:** [identified issue]
- **Next Actions:** [plan to fix and redeploy]

### Rollback Decision Tree

**When to rollback:**
- ✅ Critical errors preventing application functionality
- ✅ Data corruption or loss detected
- ✅ Webhook flooding causing rate limit issues
- ✅ Security vulnerability discovered
- ✅ Complete integration failure

**When NOT to rollback:**
- ❌ Minor UI bugs (can be fixed with hotfix)
- ❌ Occasional webhook failures (expected, handled by retry logic)
- ❌ Slow response times (investigate performance optimization)
- ❌ Missing optional data (acceptable for integration)

### Post-Rollback Actions

1. **Notify Team:** Inform stakeholders of rollback and reason
2. **Investigate Root Cause:** Debug issues in development environment
3. **Fix Issues:** Address problems identified during deployment
4. **Re-test:** Thoroughly test fixes before redeployment
5. **Plan Redeployment:** Schedule new deployment with fixes
6. **Document Lessons:** Update deployment plan with lessons learned

---

## Post-Deployment Tasks

### Immediate Actions (Today)

#### 1. Monitor Production Logs (First 2 Hours)
```bash
# Keep logs open and watch for issues
vercel logs --prod --follow
```

**Watch for:**
- Webhook delivery failures
- API errors (500, 429, 401)
- Database connection issues
- Unusual traffic patterns

**Action:** Address any errors immediately

#### 2. Notify Operations Team
Send deployment notification email:

**To:** Operations team, field crews  
**Subject:** New Field Tracking System Now Live  
**Body:**
```
Hi team,

The new Arrivy-integrated field tracking system is now live!

What's New:
- Real-time task updates from Arrivy
- Enhanced crew performance tracking
- Live activity feed showing all field events
- Customer tracker URLs for easy sharing

Access: https://[your-domain].vercel.app/operations/field-tracking

Please report any issues to [support contact].

Training sessions scheduled for [dates].
```

#### 3. Update Internal Documentation
- [ ] Add production deployment date to project README
- [ ] Update team wiki with new dashboard URLs
- [ ] Document any deployment deviations from this plan
- [ ] Add production environment details to runbook

#### 4. Set Up Monitoring Alerts (Optional but Recommended)
If using monitoring tools:
- Configure alerts for webhook endpoint errors (> 5% error rate)
- Set up database query performance monitoring
- Create dashboard for Arrivy API rate limit usage
- Set alerts for API response times > 3 seconds

### Week 1 Actions

#### Daily Monitoring (Days 1-7)
**Morning Check (5 minutes):**
```bash
# Check webhook delivery rate
vercel logs --prod --since 24h | grep "webhook" | wc -l

# Check for errors
vercel logs --prod --since 24h | grep -i "error" | wc -l

# Check database task count growth
psql "$PROD_DB" -c "SELECT COUNT(*) FROM arrivy_tasks;"
```

**Action:** Document any trends or issues

**Evening Review (5 minutes):**
- Review error logs for patterns
- Check user feedback/support tickets
- Monitor database size growth
- Verify webhook delivery rate > 99%

#### Mid-Week Review (Day 3-4)
- [ ] Meet with operations team for initial feedback
- [ ] Review most common user workflows
- [ ] Identify any friction points in UI
- [ ] Collect feature requests for Phase 2
- [ ] Check database performance (query times)

#### End-of-Week Assessment (Day 7)
- [ ] Generate weekly metrics report:
  - Total tasks processed
  - Webhook delivery success rate
  - Average API response times
  - User engagement (daily active users)
  - Support ticket count
- [ ] Stakeholder meeting to review deployment success
- [ ] Prioritize any bug fixes or improvements
- [ ] Plan training sessions for following week

### Ongoing Maintenance

#### Monthly Tasks

**First Monday of Each Month:**
- [ ] Archive old events (> 90 days):
```sql
DELETE FROM arrivy_events 
WHERE created_at < NOW() - INTERVAL '90 days';
```
- [ ] Review and optimize slow database queries
- [ ] Update crew entities if team changes
- [ ] Review API rate limit usage trends

**Database Maintenance:**
```sql
-- Vacuum and analyze tables
VACUUM ANALYZE arrivy_tasks;
VACUUM ANALYZE arrivy_events;
VACUUM ANALYZE arrivy_entities;

-- Check index usage
SELECT 
  schemaname,
  tablename,
  indexname,
  idx_scan as scans
FROM pg_stat_user_indexes
WHERE tablename LIKE 'arrivy_%'
ORDER BY scans ASC;
```

**Action:** Drop unused indexes if scans = 0 after 30 days

#### Quarterly Tasks

**Every 3 Months:**
- [ ] Review database growth trends
- [ ] Assess need for additional indexes
- [ ] Conduct user satisfaction survey
- [ ] Review and update SLA thresholds
- [ ] Plan next phase features based on feedback
- [ ] Security audit of Arrivy integration
- [ ] Review and update documentation

#### As-Needed Tasks

**When Team Changes:**
- Update crew entities in Arrivy
- Sync entities: `npm run sync:arrivy:entities`
- Update notification preferences for new coordinators

**When Arrivy API Updates:**
- Review Arrivy changelog
- Test integration in development
- Update API client if needed
- Deploy updates following this plan

**When Performance Issues Arise:**
- Check database query performance
- Review API rate limit usage
- Optimize database indexes
- Consider caching strategies

---

## Support & Troubleshooting

### Common Issues and Solutions

#### Issue 1: Webhooks Not Being Received

**Symptoms:**
- Tasks created in Arrivy not appearing in dashboard
- Status updates not reflected
- No webhook logs in Vercel

**Diagnosis:**
```bash
# Check webhook configuration
curl https://[your-domain].vercel.app/api/operations/field-tracking/webhook

# Check recent logs
vercel logs --prod -n 50 | grep webhook
```

**Solutions:**

1. **Verify webhook URL in Arrivy:**
   - Must be HTTPS
   - Must match production domain exactly
   - No trailing slash

2. **Check webhook secret:**
```bash
# Verify environment variable
vercel env ls | grep WEBHOOK_SECRET
```

3. **Test webhook manually:**
   - Use Arrivy dashboard "Test Webhook" button
   - Check logs immediately after test

4. **Common causes:**
   - Webhook URL has typo
   - Webhook secret mismatch
   - Arrivy webhook disabled/deleted
   - Network/firewall blocking requests

#### Issue 2: Tasks Not Syncing from Arrivy

**Symptoms:**
- Dashboard shows no tasks
- Sync script completes but database empty
- Partial data synced

**Diagnosis:**
```bash
# Run sync with verbose logging
npm run sync:arrivy

# Check database
psql "$PROD_DB" -c "SELECT COUNT(*) FROM arrivy_tasks;"

# Check API credentials
curl -H "X-Auth-Key: [key]" -H "X-Auth-Token: [token]" \
  https://app.arrivy.com/api/tasks
```

**Solutions:**

1. **Verify API credentials:**
```bash
vercel env ls | grep ARRIVY_AUTH
```

2. **Check rate limiting:**
   - Arrivy API: 30 requests/second
   - Sync script should batch automatically
   - Wait 1 minute and retry if rate limited

3. **Verify entities exist:**
```sql
SELECT COUNT(*) FROM arrivy_entities WHERE type = 'crew';
```
   - If 0, run: `npm run sync:arrivy:entities`

4. **Check Arrivy API status:**
   - Visit https://status.arrivy.com/
   - Contact Arrivy support if API issues

#### Issue 3: Dashboard Shows No Data

**Symptoms:**
- Dashboard loads but shows "No tasks found"
- API endpoints return empty arrays
- Database has tasks but UI doesn't display them

**Diagnosis:**
```bash
# Test API directly
curl https://[your-domain].vercel.app/api/operations/field-tracking/tasks

# Check database
psql "$PROD_DB" -c "SELECT COUNT(*) FROM arrivy_tasks;"

# Check browser console for errors
# (Open Developer Tools → Console)
```

**Solutions:**

1. **Verify API endpoint:**
   - Should return JSON array of tasks
   - If 500 error, check Vercel logs for details

2. **Check database connection:**
```bash
vercel logs --prod -n 20 | grep "database"
```

3. **Clear browser cache:**
   - Hard refresh: Cmd+Shift+R (Mac) or Ctrl+Shift+R (Windows)
   - Clear all site data in browser settings

4. **Verify environment variables:**
   - Check `DATABASE_URL` is set correctly
   - Redeploy if variables changed: `vercel --prod`

#### Issue 4: Alerts Not Being Created

**Symptoms:**
- Tasks on hold but no alerts
- Late tasks not triggering notifications
- Email notifications not sent

**Diagnosis:**
```sql
-- Check if task should trigger alert
SELECT 
  id,
  title,
  current_status,
  scheduled_start_time,
  quickbase_project_id
FROM arrivy_tasks
WHERE [condition that should trigger alert];

-- Check if alert exists
SELECT * FROM pc_notifications 
WHERE quickbase_project_id = '[project-id]';
```

**Solutions:**

1. **Verify QuickBase association:**
   - Task must have `quickbase_project_id` populated
   - Check `pc_projects` table for matching project

2. **Check coordinator email:**
```sql
SELECT coordinator_email 
FROM pc_projects 
WHERE quickbase_project_id = '[project-id]';
```
   - Must have valid email address

3. **Verify notification preferences:**
```sql
SELECT * FROM pc_notification_preferences 
WHERE user_email = '[coordinator-email]';
```
   - Check `alert_late_crew` and `alert_on_hold` are enabled

4. **Test alert creation manually:**
   - Update task to trigger condition
   - Check alert creation logic in codebase
   - Review logs for alert creation attempts

#### Issue 5: Slow API Response Times

**Symptoms:**
- Dashboard takes > 5 seconds to load
- Task detail modal slow to open
- API endpoints timing out

**Diagnosis:**
```bash
# Test API response time
time curl "https://[your-domain].vercel.app/api/operations/field-tracking/tasks"

# Check Vercel function duration
vercel logs --prod -n 50 | grep "Duration"

# Check database query performance
psql "$PROD_DB" -c "
  SELECT query, mean_exec_time, calls 
  FROM pg_stat_statements 
  WHERE query LIKE '%arrivy_%' 
  ORDER BY mean_exec_time DESC 
  LIMIT 10;
"
```

**Solutions:**

1. **Check database indexes:**
```sql
-- Verify indexes exist
SELECT indexname FROM pg_indexes 
WHERE tablename LIKE 'arrivy_%';
```

2. **Optimize slow queries:**
   - Add indexes for frequently filtered columns
   - Use EXPLAIN ANALYZE to identify bottlenecks

3. **Increase database connection pool:**
   - Check `DATABASE_URL` connection settings
   - Consider upgrading Neon plan if needed

4. **Implement caching:**
   - Add React Query caching with longer staleTime
   - Consider edge caching for static data

5. **Monitor Vercel function limits:**
   - Free tier: 10-second timeout
   - Pro tier: 60-second timeout
   - Optimize or upgrade if hitting limits

### Debugging Commands

#### Check Webhook Delivery
```bash
# Recent webhook events
vercel logs --prod -n 100 | grep "webhook"

# Webhook errors only
vercel logs --prod -n 100 | grep "webhook" | grep "error"

# Count webhooks in last hour
vercel logs --prod --since 1h | grep "POST /api/operations/field-tracking/webhook" | wc -l
```

#### Check Database State
```sql
-- System health check
SELECT 
  'Tasks' as table_name, 
  COUNT(*) as count,
  pg_size_pretty(pg_total_relation_size('arrivy_tasks')) as size
FROM arrivy_tasks
UNION ALL
SELECT 'Entities', COUNT(*), 
  pg_size_pretty(pg_total_relation_size('arrivy_entities'))
FROM arrivy_entities
UNION ALL
SELECT 'Events', COUNT(*),
  pg_size_pretty(pg_total_relation_size('arrivy_events'))
FROM arrivy_events;

-- Recent tasks
SELECT id, title, current_status, created_at 
FROM arrivy_tasks 
ORDER BY created_at DESC 
LIMIT 10;

-- Recent events
SELECT event_type, task_id, created_at 
FROM arrivy_events 
ORDER BY created_at DESC 
LIMIT 10;
```

#### Check API Endpoints
```bash
# Test tasks endpoint
curl -s "https://[your-domain].vercel.app/api/operations/field-tracking/tasks" | jq '.data | length'

# Test specific task
curl -s "https://[your-domain].vercel.app/api/operations/field-tracking/tasks/[id]" | jq '.'

# Test events endpoint
curl -s "https://[your-domain].vercel.app/api/operations/field-tracking/events" | jq '.data | length'

# Test webhook health
curl "https://[your-domain].vercel.app/api/operations/field-tracking/webhook"
```

### Getting Help

#### Internal Resources
- **Deployment Guide:** `ARRIVY_DEPLOYMENT_GUIDE.md` - Comprehensive deployment documentation
- **Testing Checklist:** `ARRIVY_TESTING_CHECKLIST.md` - Detailed test scenarios
- **Sync Guide:** `ARRIVY_SYNC_GUIDE.md` - Data synchronization procedures
- **Crew Performance Guide:** `CREW_PERFORMANCE_GUIDE.md` - Dashboard usage

#### External Resources
- **Arrivy API Documentation:** https://app.arrivy.com/developer-portal/api
- **Arrivy Dashboard:** https://app.arrivy.com/
- **Arrivy Status Page:** https://status.arrivy.com/
- **Arrivy Support:** support@arrivy.com

#### Escalation Path
1. **Level 1:** Check this troubleshooting guide
2. **Level 2:** Review Vercel and database logs
3. **Level 3:** Contact technical lead with diagnostic output
4. **Level 4:** Open support ticket with Arrivy if API issues
5. **Level 5:** Consider rollback if critical production impact

---

## Deployment Completion Sign-Off

### Deployment Details

**Deployment Date:** ___________________  
**Deployment Time (Start):** ___________________  
**Deployment Time (End):** ___________________  
**Total Duration:** ___________________  

**Deployed By:** ___________________  
**Title:** ___________________  
**Email:** ___________________  

### Deployment Summary

**Git Commit Hash:** ___________________  
**Vercel Deployment URL:** ___________________  
**Database Migration Version:** 016  

**Tasks Synced:** ___________________  
**Entities Synced:** ___________________  
**Initial Event Count:** ___________________  

### Issues Encountered

**Were any issues encountered during deployment?** [ ] Yes [ ] No

If yes, describe:
```
___________________________________________________________________________
___________________________________________________________________________
___________________________________________________________________________
```

**Were issues resolved?** [ ] Yes [ ] No  
**Resolution details:**
```
___________________________________________________________________________
___________________________________________________________________________
___________________________________________________________________________
```

### Success Criteria Verification

**All 25 success criteria met?** [ ] Yes [ ] No  

If no, list criteria not met and justification:
```
___________________________________________________________________________
___________________________________________________________________________
___________________________________________________________________________
```

### Sign-Off Approvals

**Technical Lead:**  
Name: ___________________  
Signature: ___________________  
Date: ___________________  

**Operations Manager:**  
Name: ___________________  
Signature: ___________________  
Date: ___________________  

**Quality Assurance:**  
Name: ___________________  
Signature: ___________________  
Date: ___________________  

### Post-Deployment Notes

**Any observations or recommendations:**
```
___________________________________________________________________________
___________________________________________________________________________
___________________________________________________________________________
___________________________________________________________________________
```

**Follow-up actions required:**
- [ ] ___________________________________________________________________________
- [ ] ___________________________________________________________________________
- [ ] ___________________________________________________________________________

---

## Next Steps

### Immediate Next Steps (This Week)

1. **Monitor Closely**
   - Watch logs daily for first week
   - Track webhook delivery rate (target > 99%)
   - Monitor API response times (target < 2 seconds)
   - Gather user feedback proactively

2. **User Training**
   - Schedule training sessions with operations team
   - Create quick reference guides
   - Record video tutorials for common workflows
   - Set up feedback channels

3. **Optimization**
   - Identify slow database queries
   - Optimize API endpoints based on usage patterns
   - Add additional indexes if needed
   - Fine-tune notification rules based on feedback

### Phase 2 Planning (Weeks 2-4)

Based on user feedback, prioritize Phase 2 enhancements:

#### Potential Features:
- **Map View:** Visual representation of crew locations and tasks
- **Advanced Analytics:** Deeper insights into crew performance and efficiency
- **Enhanced QuickBase Integration:** Bidirectional sync, automatic task creation
- **Mobile Optimization:** Native mobile app or enhanced PWA
- **Automated Reporting:** Scheduled reports sent to managers
- **Smart Notifications:** AI-driven alerts for anomalies and issues
- **Task Assignment:** Allow coordinators to assign/reassign crews
- **Customer Communication:** In-app messaging with customers

#### Planning Process:
1. **Week 2:** Collect and prioritize feature requests
2. **Week 3:** Technical feasibility assessment
3. **Week 4:** Create detailed specifications for Phase 2
4. **Week 5+:** Begin Phase 2 development

### Long-Term Vision (Months 2-6)

**Month 2:**
- Stabilize Phase 1 features
- Implement highest-priority Phase 2 features
- Expand user base to additional regions

**Month 3:**
- Launch mobile applications (iOS/Android)
- Integrate with additional external systems (accounting, CRM)
- Implement advanced analytics dashboards

**Month 4-6:**
- AI/ML features (predictive ETAs, intelligent scheduling)
- Customer self-service portal
- Integration with additional operations tools
- Scale to support 100+ crews

### Success Metrics

**Track these KPIs monthly:**
- **Adoption Rate:** % of operations team using dashboard daily
- **Task Completion Rate:** % of tasks completed on time
- **Response Time:** Average time to respond to on-hold tasks
- **User Satisfaction:** NPS score from operations team
- **System Reliability:** Uptime % and webhook delivery rate
- **Performance:** API response times and database query speeds

**Target Goals (3 months post-deployment):**
- 90%+ daily active users
- 95%+ on-time task completion
- 50% reduction in hold resolution time
- NPS score > 50
- 99.9%+ system uptime
- < 1 second average API response time

---

## Appendix

### Reference Documents

All deployment-related documentation:

- **ARRIVY_DEPLOYMENT_GUIDE.md** - Comprehensive deployment guide with detailed explanations
- **ARRIVY_PRODUCTION_DEPLOYMENT.md** - Concise production-specific commands
- **ARRIVY_TESTING_CHECKLIST.md** - Complete testing scenarios and validation procedures
- **ARRIVY_SYNC_GUIDE.md** - Data synchronization operations and troubleshooting
- **CREW_PERFORMANCE_GUIDE.md** - Crew performance dashboard usage and features
- **ENV_LOCAL_SETUP_INSTRUCTIONS.md** - Local environment configuration
- **ARRIVY_QUICK_START.md** - Quick reference for common operations

### Migration Files

Database migration SQL files (execute in order):

1. **lib/db/migrations/014_create_arrivy_tables.sql** - Creates 4 core tables, 13 indexes, 2 triggers
2. **lib/db/migrations/015_create_arrivy_task_entities_join_table.sql** - Creates join table, 4 indexes, backfills data
3. **lib/db/migrations/016_make_quickbase_fields_optional.sql** - Makes QuickBase fields nullable, cleans placeholder data

### Environment Variables Reference

Complete list of Arrivy-related environment variables:

```bash
# Arrivy API Authentication
ARRIVY_AUTH_KEY=0a27a7e3-e6b5
ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs

# Arrivy Configuration
ARRIVY_COMPANY_NAME=KIN Home
ARRIVY_BASE_URL=https://app.arrivy.com/api
ARRIVY_RATE_LIMIT=30

# Webhook Security
ARRIVY_WEBHOOK_SECRET=GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=
```

### API Endpoints Reference

All Arrivy-related API endpoints:

- **GET /api/operations/field-tracking/tasks** - List all tasks
- **GET /api/operations/field-tracking/tasks/[id]** - Get task details
- **POST /api/operations/field-tracking/webhook** - Receive Arrivy webhooks
- **GET /api/operations/field-tracking/events** - List all events
- **GET /api/operations/crew-performance** - Get crew performance metrics

### Database Schema Reference

Core Arrivy tables:

```sql
-- Tasks
arrivy_tasks (
  id, external_id, title, description, 
  customer_first_name, customer_last_name, customer_phone, customer_email,
  task_type, source, current_status, scheduled_start_time, scheduled_end_time,
  actual_start_time, actual_end_time, customer_address_line1, customer_address_line2,
  customer_city, customer_state, customer_zipcode, customer_country,
  customer_exact_location, customer_mobile_number, url_safe_id,
  quickbase_project_id (nullable), quickbase_record_id (nullable),
  assigned_entity_ids, extra_fields, notes,
  created_at, updated_at
)

-- Entities (Crews)
arrivy_entities (
  id, external_id, name, type, email, phone,
  image_path, details, extra_fields, 
  created_at, updated_at
)

-- Events
arrivy_events (
  id, task_id (FK), external_id, event_type, 
  notes, source, time, reporter_id, reporter_name,
  created_at, updated_at
)

-- Status History
arrivy_task_status (
  id, task_id (FK), external_id, 
  title, type, time, source, reporter_id, reporter_name,
  color, created_at, updated_at
)

-- Task-Entity Join
arrivy_task_entities (
  task_id (FK), entity_id (FK),
  created_at, updated_at
)
```

### Contact Information

**Technical Support:**
- Technical Lead: [name] - [email]
- DevOps: [name] - [email]
- Database Admin: [name] - [email]

**Business Stakeholders:**
- Operations Manager: [name] - [email]
- Regional Manager: [name] - [email]
- Project Manager: [name] - [email]

**External Support:**
- Arrivy Support: support@arrivy.com
- Vercel Support: https://vercel.com/support

---

**END OF DEPLOYMENT EXECUTION PLAN**

*This document is a living guide. Update it with lessons learned from each deployment.*



