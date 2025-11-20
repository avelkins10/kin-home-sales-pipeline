# Arrivy Integration - Complete Deployment Guide for Claude Code

**Purpose:** Step-by-step instructions for deploying the Arrivy integration to production  
**Date:** October 29, 2025  
**Estimated Time:** 45-60 minutes  
**Status:** Ready for Deployment

---

## 📋 Pre-Deployment Checklist

Before starting, verify:
- [ ] You have access to Vercel CLI (`vercel whoami`)
- [ ] You have production database connection string
- [ ] You have Arrivy API credentials
- [ ] Git repository is clean (or ready to commit)
- [ ] You're in the project root: `/Users/austinelkins/Rep_Dashboard`

---

## 🚀 DEPLOYMENT SEQUENCE

Follow these steps **in exact order**. Do not skip steps.

---

## PHASE 1: Commit Code Changes (5 minutes)

### What Changed
The following verification comments have been implemented:
1. ✅ Migration 016 renamed to 018 (no conflict)
2. ✅ ARRIVING event doesn't create notifications
3. ✅ All queries refactored to use JOIN instead of UNNEST
4. ✅ Dotenv loading is conditional and production-safe
5. ✅ Migration 018 header comment fixed

### Files Modified (Summary)
- `lib/integrations/arrivy/service.ts` - ARRIVING notification fix
- `lib/db/arrivy.ts` - Query refactoring (14 patterns)
- `scripts/sync-arrivy-tasks.ts` - Conditional dotenv loading
- `lib/db/migrations/018_drop_assigned_entity_ids_array.sql` - Header comment fix
- Multiple documentation files updated

### Step 1.1: Check Git Status
```bash
cd /Users/austinelkins/Rep_Dashboard
git status
```

**Expected:** Should show modified files from the verification comments implementation.

### Step 1.2: Stage All Changes
```bash
git add .
```

### Step 1.3: Commit Changes
```bash
git commit -m "feat: implement verification comments and query refactoring

- Rename migration 016 to 018 to avoid numbering conflict
- Fix ARRIVING event to not create notifications (status only)
- Refactor all queries to use JOIN instead of UNNEST/ANY operations
- Make dotenv loading conditional and production-safe
- Update migration 018 header comment to match filename
- Update all documentation references to migration 018

Changes prepare codebase for join table migration deployment.
All queries now use proper JOINs on arrivy_task_entities.
Safe to run migration 015 after deployment.
Migration 018 should only run after 24-48h verification."
```

### Step 1.4: Push to Remote
```bash
git push origin main
```

**Expected:** Push successful, GitHub updated.

---

## PHASE 2: Environment Variables (10 minutes)

### Verify Required Variables

You need these 6 Arrivy environment variables in Vercel production:

```bash
ARRIVY_AUTH_KEY=0a27a7e3-e6b5
ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs
ARRIVY_COMPANY_NAME=KIN Home
ARRIVY_WEBHOOK_SECRET=GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=
ARRIVY_BASE_URL=https://app.arrivy.com/api
ARRIVY_RATE_LIMIT=30
```

### Step 2.1: Authenticate with Vercel
```bash
vercel whoami
```

**If not authenticated:**
```bash
vercel login
```

### Step 2.2: Check Existing Environment Variables
```bash
vercel env ls
```

### Step 2.3: Add Missing Variables

**Only add if not already set in production:**

```bash
# Add Arrivy Auth Key
vercel env add ARRIVY_AUTH_KEY
# When prompted:
# - Value: 0a27a7e3-e6b5
# - Environment: Production
# - Scope: Current project

# Add Arrivy Auth Token
vercel env add ARRIVY_AUTH_TOKEN
# Value: 5730gWxBjDzbQDEeFh3zrs
# Environment: Production

# Add Company Name
vercel env add ARRIVY_COMPANY_NAME
# Value: KIN Home
# Environment: Production

# Add Webhook Secret
vercel env add ARRIVY_WEBHOOK_SECRET
# Value: GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=
# Environment: Production

# Add Base URL
vercel env add ARRIVY_BASE_URL
# Value: https://app.arrivy.com/api
# Environment: Production

# Add Rate Limit
vercel env add ARRIVY_RATE_LIMIT
# Value: 30
# Environment: Production
```

### Step 2.4: Verify All Variables Set
```bash
vercel env ls | grep ARRIVY
```

**Expected:** Should see all 6 ARRIVY variables listed with "Production" scope.

---

## PHASE 3: Deploy Application (10 minutes)

### Step 3.1: Deploy to Production
```bash
vercel --prod
```

**Expected Output:**
```
🔍  Inspect: https://vercel.com/[your-project]/[deployment-id]
✅  Production: https://[your-domain].vercel.app [2-3 minutes]
```

**Wait for deployment to complete.** Note the production URL.

### Step 3.2: Verify Application Loads
```bash
# Replace with your actual domain
curl -I https://[your-domain].vercel.app
```

**Expected:** `200 OK` response.

### Step 3.3: Test Webhook Endpoint
```bash
curl https://[your-domain].vercel.app/api/operations/field-tracking/webhook
```

**Expected:** `{"status":"ok"}`

### Step 3.4: Check Deployment Logs
```bash
vercel logs --prod -n 20
```

**Expected:** No errors, application started successfully.

---

## PHASE 4: Database Migration 015 (15 minutes)

### ⚠️ IMPORTANT: This Creates the Join Table

This migration:
- Creates `arrivy_task_entities` table
- Creates 4 indexes
- Backfills data from existing `assigned_entity_ids` arrays
- Keeps `assigned_entity_ids` column for safety

### Step 4.1: Set Database Connection
```bash
# Set your production database URL
export DATABASE_URL="your-production-database-url-here"
```

**Or get from Vercel:**
```bash
vercel env pull .env.production
# Then:
export DATABASE_URL=$(grep DATABASE_URL .env.production | cut -d '=' -f2-)
```

### Step 4.2: Test Database Connection
```bash
psql "$DATABASE_URL" -c "SELECT version();"
```

**Expected:** PostgreSQL version displayed.

### Step 4.3: Run Migration 015
```bash
psql "$DATABASE_URL" -f lib/db/migrations/015_create_arrivy_task_entities_join_table.sql
```

**Expected Output:**
```
BEGIN
CREATE TABLE
CREATE TABLE
CREATE TABLE
CREATE TABLE
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE INDEX
CREATE TRIGGER
CREATE TRIGGER
COMMENT ON TABLE arrivy_tasks IS ...
(4 more comments)
INSERT 0 X  (X = number of existing task-entity relationships backfilled)
COMMIT
```

### Step 4.4: Verify Migration Success
```sql
psql "$DATABASE_URL" << 'EOF'
-- Check tables created
SELECT table_name 
FROM information_schema.tables 
WHERE table_name LIKE 'arrivy_%' 
ORDER BY table_name;

-- Expected: arrivy_entities, arrivy_events, arrivy_task_entities, arrivy_tasks, arrivy_task_status

-- Check join table has data
SELECT COUNT(*) as join_table_rows FROM arrivy_task_entities;

-- Check indexes created
SELECT COUNT(*) as index_count 
FROM pg_indexes 
WHERE tablename LIKE 'arrivy_%';
-- Expected: 17 indexes

-- Check triggers created
SELECT COUNT(*) as trigger_count 
FROM information_schema.triggers 
WHERE trigger_name LIKE '%arrivy%';
-- Expected: 2 triggers
EOF
```

**Expected:** 
- 5 tables (arrivy_tasks, arrivy_entities, arrivy_events, arrivy_task_status, arrivy_task_entities)
- 17 indexes
- 2 triggers
- Join table has rows (matching assignments)

---

## PHASE 5: Arrivy External Configuration (10 minutes)

### Step 5.1: Log into Arrivy Dashboard
```
URL: https://app.arrivy.com/
Credentials: (use your Arrivy admin credentials)
```

### Step 5.2: Configure Webhook

1. Navigate to: **Settings** → **Integrations** → **Webhooks**
2. Click **"Create New Webhook"** or edit existing
3. Configure:
   - **Webhook URL:** `https://[your-domain].vercel.app/api/operations/field-tracking/webhook`
   - **Webhook Secret:** `GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=`
   - **Event Types:** Select ALL:
     - ✅ TASK_CREATED
     - ✅ TASK_STATUS
     - ✅ CREW_ASSIGNED
     - ✅ ARRIVING
     - ✅ LATE
     - ✅ NOSHOW
     - ✅ TASK_RATING
     - ✅ EXCEPTION

4. Click **"Save"**

### Step 5.3: Test Webhook Delivery
1. In Arrivy dashboard, click **"Test Webhook"**
2. Select event type: **TASK_CREATED**
3. Click **"Send Test"**

### Step 5.4: Verify Webhook Received
```bash
vercel logs --prod -n 20 | grep webhook
```

**Expected:** Should see log entries like:
```
POST /api/operations/field-tracking/webhook 200
Webhook event: TASK_CREATED
```

### Step 5.5: Sync Entities (Crew Members)

**Option A: Via Script**
```bash
npm run sync:arrivy:entities
```

**Option B: Verify in Database**
```sql
psql "$DATABASE_URL" -c "
SELECT id, name, type 
FROM arrivy_entities 
WHERE type = 'crew' 
ORDER BY name 
LIMIT 10;
"
```

**Expected:** Should see crew members listed.

---

## PHASE 6: Initial Data Sync (5 minutes)

### Step 6.1: Run Initial Sync
```bash
npm run sync:arrivy
```

**Expected Output:**
```
🚀 Starting Arrivy sync...
✅ Database connection verified
✅ Arrivy API connection verified
📅 Fetching tasks in 8 batches...

📦 Processing batch 1/8
   Found 100 tasks in this batch
   📈 Processed 10 tasks...
   📈 Processed 20 tasks...
   ...

📊 SYNC SUMMARY
================================================================
Timestamp: 2025-10-29T...
Total tasks processed: 247
Tasks created: 247
Tasks updated: 0
Success rate: 100%
Errors: 0

⏱️  Execution time: 0m 12s

✅ Sync completed
```

### Step 6.2: Verify Data in Database
```sql
psql "$DATABASE_URL" << 'EOF'
-- Check total tasks
SELECT COUNT(*) as total_tasks FROM arrivy_tasks;

-- Check sample tasks
SELECT 
  arrivy_task_id,
  customer_name,
  task_type,
  current_status,
  scheduled_start
FROM arrivy_tasks 
ORDER BY created_at DESC 
LIMIT 5;

-- Check join table populated
SELECT COUNT(*) as task_entity_relationships 
FROM arrivy_task_entities;

-- Check status distribution
SELECT current_status, COUNT(*) as count 
FROM arrivy_tasks 
GROUP BY current_status 
ORDER BY count DESC;
EOF
```

**Expected:**
- Total tasks > 0
- Sample tasks show customer names and details
- Join table has relationships
- Status distribution looks reasonable

---

## PHASE 7: Production Testing (10 minutes)

### Test 1: Dashboard Access
```bash
# Open in browser (replace with your domain)
open https://[your-domain].vercel.app/operations/field-tracking
```

**Verify:**
- ✅ Page loads within 2 seconds
- ✅ Tasks are displayed
- ✅ Filters work (status, type, date)
- ✅ No console errors

### Test 2: Task Detail Modal
**In the dashboard:**
1. Click on any task card
2. Detail modal should open
3. Verify:
   - ✅ Customer information displays
   - ✅ Crew assignments show (using JOIN table)
   - ✅ Timeline/milestones display
   - ✅ Activity feed shows events

### Test 3: Crew Performance Dashboard
```bash
open https://[your-domain].vercel.app/operations/crew-performance
```

**Verify:**
- ✅ Page loads
- ✅ Crew metrics display
- ✅ Charts render
- ✅ Data looks accurate

### Test 4: Activity Feed
```bash
open https://[your-domain].vercel.app/operations/field-tracking
```

**Navigate to Activity Feed tab:**
- ✅ Events display chronologically
- ✅ Filters work (event type, crew member, date)
- ✅ ARRIVING events show (but didn't create notifications)

### Test 5: API Response Times
**In browser dev tools (Network tab):**
```bash
# Navigate through dashboard and check:
# /api/operations/field-tracking/tasks - Should be < 2 seconds
# /api/operations/field-tracking/tasks/[id] - Should be < 2 seconds
# /api/operations/field-tracking/events - Should be < 2 seconds
```

**Expected:** All API calls complete in < 2 seconds.

### Test 6: Check Logs for Errors
```bash
vercel logs --prod -n 100 | grep -i "error\|exception"
```

**Expected:** No critical errors related to Arrivy integration.

### Test 7: Create Test Task (Optional)
**In Arrivy dashboard:**
1. Create new task:
   - Title: "PROD-TEST-001 - Verification Test"
   - Customer: "Test Customer"
   - Assign crew
   - Status: NEW

2. Wait 30 seconds

3. Check appears in app:
```bash
curl "https://[your-domain].vercel.app/api/operations/field-tracking/tasks?search=PROD-TEST-001"
```

4. **Clean up test task:**
```sql
psql "$DATABASE_URL" -c "
DELETE FROM arrivy_tasks 
WHERE quickbase_project_id LIKE 'PROD-TEST-%';
"
```

---

## PHASE 8: Post-Deployment Verification (5 minutes)

### Step 8.1: Monitor Webhook Delivery
```bash
# Watch logs for 2-3 minutes
vercel logs --prod --follow
```

**Look for:**
- ✅ Webhook events being received
- ✅ No 500 errors
- ✅ No authentication failures
- ✅ Response times < 500ms

**Press Ctrl+C to stop when satisfied.**

### Step 8.2: Check Database Health
```sql
psql "$DATABASE_URL" << 'EOF'
-- System health check
SELECT 
  'Tasks' as metric,
  COUNT(*) as count,
  MIN(created_at) as earliest,
  MAX(created_at) as latest,
  pg_size_pretty(pg_total_relation_size('arrivy_tasks')) as size
FROM arrivy_tasks
UNION ALL
SELECT 
  'Entities',
  COUNT(*),
  MIN(created_at),
  MAX(created_at),
  pg_size_pretty(pg_total_relation_size('arrivy_entities'))
FROM arrivy_entities
UNION ALL
SELECT 
  'Events',
  COUNT(*),
  MIN(created_at),
  MAX(created_at),
  pg_size_pretty(pg_total_relation_size('arrivy_events'))
FROM arrivy_events
UNION ALL
SELECT 
  'Task-Entities',
  COUNT(*),
  MIN(created_at),
  MAX(created_at),
  pg_size_pretty(pg_total_relation_size('arrivy_task_entities'))
FROM arrivy_task_entities;
EOF
```

**Expected:** All tables have data, recent timestamps, reasonable sizes.

### Step 8.3: Verify Query Performance
```sql
psql "$DATABASE_URL" << 'EOF'
-- Test JOIN query performance (should use indexes)
EXPLAIN ANALYZE
SELECT 
  t.*,
  ARRAY_AGG(e.name) as entity_names
FROM arrivy_tasks t
LEFT JOIN arrivy_task_entities te ON te.arrivy_task_id = t.arrivy_task_id
LEFT JOIN arrivy_entities e ON e.arrivy_entity_id = te.arrivy_entity_id
GROUP BY t.id
LIMIT 10;
EOF
```

**Look for:**
- Execution time < 100ms
- Index scans (not seq scans)
- "Index Scan using idx_arrivy_task_entities_task"

### Step 8.4: Final Smoke Test
```bash
# Test all main endpoints
curl -s https://[your-domain].vercel.app/api/operations/field-tracking/tasks | jq '.data | length'
# Expected: Number of tasks

curl -s https://[your-domain].vercel.app/api/operations/field-tracking/events | jq '.data | length'
# Expected: Number of events

curl -s https://[your-domain].vercel.app/api/operations/field-tracking/webhook
# Expected: {"status":"ok"}
```

---

## 🎉 DEPLOYMENT COMPLETE!

### Success Criteria Checklist

Verify all are ✅ before marking complete:

#### Code & Deployment
- [ ] Code committed and pushed to GitHub
- [ ] Application deployed to Vercel production
- [ ] All environment variables set correctly
- [ ] Deployment shows no errors

#### Database
- [ ] Migration 015 executed successfully
- [ ] 5 tables exist (arrivy_tasks, arrivy_entities, arrivy_events, arrivy_task_status, arrivy_task_entities)
- [ ] 17 indexes created
- [ ] 2 triggers created
- [ ] Join table populated with data

#### Arrivy Configuration
- [ ] Webhook configured in Arrivy dashboard
- [ ] All 8 event types selected
- [ ] Test webhook delivered successfully
- [ ] Webhook secret matches environment variable

#### Functionality
- [ ] Initial sync completed (tasks imported)
- [ ] Dashboard loads and displays tasks
- [ ] Task detail shows crew assignments
- [ ] Activity feed displays events
- [ ] Crew performance dashboard works
- [ ] API response times < 2 seconds
- [ ] No errors in production logs

#### Performance
- [ ] Queries use JOIN operations (not UNNEST)
- [ ] Indexes being utilized
- [ ] Dashboard loads in < 2 seconds
- [ ] No N+1 query patterns

---

## 📊 Post-Deployment Monitoring

### Immediate (Next 2 Hours)

**Monitor logs:**
```bash
vercel logs --prod --follow
```

**Watch for:**
- Webhook delivery success rate (should be > 99%)
- API response times (should be < 2 seconds)
- Any error patterns
- Database connection issues

### Daily (Next Week)

**Run daily health check:**
```bash
# Check webhook delivery rate
vercel logs --prod --since 24h | grep "webhook" | wc -l

# Check for errors
vercel logs --prod --since 24h | grep -i "error" | wc -l

# Check database growth
psql "$DATABASE_URL" -c "
SELECT 
  COUNT(*) as total_tasks,
  COUNT(CASE WHEN created_at >= NOW() - INTERVAL '24 hours' THEN 1 END) as new_tasks_24h
FROM arrivy_tasks;
"
```

### After 24-48 Hours: Run Migration 018 (Drop Array Column)

**⚠️ ONLY after verifying everything works perfectly:**

```bash
# Create backup first
pg_dump "$DATABASE_URL" > backup_before_array_drop_$(date +%Y%m%d).sql

# Run migration 018
psql "$DATABASE_URL" -f lib/db/migrations/018_drop_assigned_entity_ids_array.sql

# Verify column dropped
psql "$DATABASE_URL" -c "\d arrivy_tasks"
# Should NOT see assigned_entity_ids column

# Test application still works
curl https://[your-domain].vercel.app/operations/field-tracking
# Should load normally
```

---

## 🔥 ROLLBACK PLAN

### If Issues Occur BEFORE Migration 018

**1. Revert Vercel Deployment:**
```bash
vercel rollback
```

**2. Repopulate Array from Join Table (if needed):**
```sql
psql "$DATABASE_URL" << 'EOF'
UPDATE arrivy_tasks t
SET assigned_entity_ids = (
  SELECT ARRAY_AGG(arrivy_entity_id)
  FROM arrivy_task_entities
  WHERE arrivy_task_id = t.arrivy_task_id
);
EOF
```

**3. Revert Code Changes:**
```bash
git revert HEAD
git push origin main
vercel --prod
```

### If Issues Occur AFTER Migration 018

**1. Restore from Backup:**
```bash
psql "$DATABASE_URL" < backup_before_array_drop_20251029.sql
```

**Or recreate column:**
```sql
psql "$DATABASE_URL" << 'EOF'
ALTER TABLE arrivy_tasks ADD COLUMN assigned_entity_ids BIGINT[];

UPDATE arrivy_tasks t
SET assigned_entity_ids = (
  SELECT ARRAY_AGG(arrivy_entity_id)
  FROM arrivy_task_entities
  WHERE arrivy_task_id = t.arrivy_task_id
);
EOF
```

**2. Revert deployment and code as above.**

---

## 📞 Support & Troubleshooting

### Common Issues

**Issue: Webhook not receiving events**
```bash
# Check webhook URL in Arrivy (must be HTTPS, exact match)
# Verify webhook secret matches
# Test endpoint directly:
curl https://[your-domain].vercel.app/api/operations/field-tracking/webhook
# Should return: {"status":"ok"}
```

**Issue: Dashboard shows no tasks**
```bash
# Check database has tasks:
psql "$DATABASE_URL" -c "SELECT COUNT(*) FROM arrivy_tasks;"

# Re-run sync if needed:
npm run sync:arrivy
```

**Issue: Slow query performance**
```bash
# Check indexes exist:
psql "$DATABASE_URL" -c "
SELECT indexname FROM pg_indexes 
WHERE tablename = 'arrivy_task_entities';
"
# Should see 4 indexes
```

**Issue: Environment variables not loading**
```bash
# Re-pull env vars:
vercel env pull
# Or check manually:
vercel env ls
```

### Debug Commands

```bash
# Check deployment status
vercel ls --prod

# View recent logs
vercel logs --prod -n 100

# Check specific endpoint
curl -v https://[your-domain].vercel.app/api/operations/field-tracking/tasks

# Test database connection
psql "$DATABASE_URL" -c "SELECT NOW();"

# Check table sizes
psql "$DATABASE_URL" -c "
SELECT 
  schemaname,
  tablename,
  pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) AS size
FROM pg_tables
WHERE tablename LIKE 'arrivy_%'
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;
"
```

---

## 📚 Reference Documentation

- **Complete Deployment Guide:** `ARRIVY_DEPLOYMENT_GUIDE.md`
- **Production Deployment:** `ARRIVY_PRODUCTION_DEPLOYMENT.md`
- **Testing Checklist:** `ARRIVY_TESTING_CHECKLIST.md`
- **Verification Comments:** `VERIFICATION_COMMENTS_IMPLEMENTED.md`
- **Migration Details:** `JOIN_TABLE_MIGRATION_IMPLEMENTED.md`
- **Crew Performance:** `CREW_PERFORMANCE_GUIDE.md`
- **Sync Guide:** `ARRIVY_SYNC_GUIDE.md`

---

## ✅ Deployment Sign-Off

**Deployed By:** _________________  
**Date:** _________________  
**Time:** _________________  
**Deployment URL:** _________________  
**Database:** _________________  

**Verified By:** _________________  
**Date:** _________________  

**Issues Encountered:** 
- None / [describe]

**Status:** ✅ Complete / ⚠️ Issues / ❌ Failed

---

**END OF DEPLOYMENT GUIDE**

This guide is complete and ready for execution by Claude Code or any deployment agent.







