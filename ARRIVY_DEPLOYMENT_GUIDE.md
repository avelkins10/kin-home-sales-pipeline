# Arrivy Integration Deployment Guide

## Overview
This guide walks through deploying the Arrivy field operations tracking integration to production. The Arrivy integration enables real-time field crew tracking, customer notifications, and operations visibility for survey, installation, inspection, and service appointments.

## Prerequisites
- Arrivy account with API credentials
  - Auth Key: `0a27a7e3-e6b5`
  - Auth Token: `5730gWxBjDzbQDEeFh3zrs`
- PostgreSQL database access (Vercel Postgres or compatible)
- Production deployment environment (Vercel recommended)
- Access to Arrivy dashboard for webhook configuration
- OpenSSL installed for generating webhook secret
- QuickBase integration already configured

## Architecture Overview

```
QuickBase → Sync Service → PostgreSQL ← API Endpoints → Dashboard UI
                ↓                         ↑
              Arrivy API              Webhook
                                    (Real-time updates)
```

### Data Flow
1. **Task Creation**: Sales team schedules appointment in QuickBase
2. **Sync to Arrivy**: Background service creates task in Arrivy with customer details
3. **Field Crew Assignment**: Operations coordinator assigns crew via dashboard
4. **Real-time Updates**: Arrivy sends webhooks for status changes (ENROUTE, STARTED, COMPLETE)
5. **Dashboard Updates**: UI polls every 30 seconds and processes webhook events
6. **Customer Experience**: Customers receive tracker link to see crew location and ETA

---

## Phase 1: Environment Configuration (15 minutes)

### Step 1.1: Create .env.local
The `.env.local` file has been created with Arrivy credentials pre-filled. You need to:

1. **Set your company name:**
```bash
# Edit .env.local and replace:
ARRIVY_COMPANY_NAME=your_company_name
# With your actual company name as it appears in Arrivy
# This is used for tracker URLs: https://app.arrivy.com/live/track/{company_name}/{url_safe_id}
```

2. **Generate webhook secret:**
```bash
openssl rand -base64 32
```
Copy the output and replace `ARRIVY_WEBHOOK_SECRET` in `.env.local`.

3. **Verify other required variables are set:**
   - `DATABASE_URL` - Your PostgreSQL connection string
   - `NEXTAUTH_SECRET` - Authentication secret
   - `NEXTAUTH_URL` - Your deployment URL
   - `QUICKBASE_TOKEN` - QuickBase API token
   - `QUICKBASE_REALM` - QuickBase realm (kin.quickbase.com)

### Step 1.2: Environment Variables Reference
```bash
# Already configured in .env.local:
ARRIVY_AUTH_KEY=0a27a7e3-e6b5
ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs
ARRIVY_BASE_URL=https://app.arrivy.com/api
ARRIVY_RATE_LIMIT=30

# You must configure:
ARRIVY_COMPANY_NAME=your_company_name        # TODO
ARRIVY_WEBHOOK_SECRET=generated_secret_here  # TODO
```

### Step 1.3: Verify Configuration
```bash
# Test that environment variables are loaded
npm run dev
# Check logs for "Arrivy client initialized" message
```

---

## Phase 2: Database Migration (10 minutes)

### Step 2.1: Execute Migration 014

**For local development:**
```bash
psql $DATABASE_URL -f lib/db/migrations/014_create_arrivy_tables.sql
```

**For Vercel Postgres (production):**
```bash
# Option A: Using Vercel CLI
vercel env pull .env.production.local
psql "$(grep DATABASE_URL .env.production.local | cut -d '=' -f2-)" -f lib/db/migrations/014_create_arrivy_tables.sql

# Option B: Direct connection
psql "postgresql://username:password@host/database?sslmode=require" -f lib/db/migrations/014_create_arrivy_tables.sql
```

**Expected output:**
```
BEGIN
CREATE FUNCTION
CREATE TABLE
CREATE TABLE
CREATE TABLE
CREATE TABLE
CREATE INDEX (x8)
CREATE TRIGGER (x2)
COMMENT (x8)
COMMIT
```

### Step 2.2: Verify Tables Created
```sql
SELECT table_name 
FROM information_schema.tables 
WHERE table_schema = 'public' AND table_name LIKE 'arrivy_%'
ORDER BY table_name;
```

**Expected result:**
```
    table_name     
-------------------
 arrivy_entities
 arrivy_events
 arrivy_task_status
 arrivy_tasks
(4 rows)
```

### Step 2.3: Verify Indexes
```sql
SELECT tablename, indexname 
FROM pg_indexes 
WHERE tablename LIKE 'arrivy_%'
ORDER BY tablename, indexname;
```

**Expected result:** 8 indexes total
- `idx_arrivy_tasks_qb_project`
- `idx_arrivy_tasks_status`
- `idx_arrivy_tasks_scheduled`
- `idx_arrivy_tasks_type`
- `idx_arrivy_tasks_synced`
- `idx_arrivy_entities_email`
- `idx_arrivy_entities_qb_user`
- `idx_arrivy_events_task`
- `idx_arrivy_events_type`
- `idx_arrivy_events_time`
- `idx_arrivy_task_status_task`
- `idx_arrivy_task_status_type`

### Step 2.2: Execute Migration 016 (Make QuickBase Fields Optional)

**For local development:**
```bash
psql $DATABASE_URL -f lib/db/migrations/016_make_quickbase_fields_optional.sql
```

**For Vercel Postgres (production):**
```bash
# Option A: Using Vercel CLI
vercel env pull .env.production.local
psql "$(grep DATABASE_URL .env.production.local | cut -d '=' -f2-)" -f lib/db/migrations/016_make_quickbase_fields_optional.sql

# Option B: Direct connection
psql "postgresql://username:password@host/database?sslmode=require" -f lib/db/migrations/016_make_quickbase_fields_optional.sql
```

**Verification:**
```bash
psql $DATABASE_URL -c "SELECT column_name, is_nullable FROM information_schema.columns WHERE table_name = 'arrivy_tasks' AND column_name IN ('quickbase_project_id', 'quickbase_record_id');"
```

**Expected:** Both columns show `is_nullable = YES`

### Step 2.4: Test Database Access
```sql
-- Verify tables are accessible
SELECT COUNT(*) FROM arrivy_tasks;
SELECT COUNT(*) FROM arrivy_entities;
SELECT COUNT(*) FROM arrivy_events;
SELECT COUNT(*) FROM arrivy_task_status;
```

**Expected result:** All queries return `0` (tables are empty but accessible)

### Step 2.5: Rollback Plan (if needed)

**Rollback Migration 016:**
```sql
-- Check for tasks with null QuickBase IDs before rollback
SELECT COUNT(*) FROM arrivy_tasks WHERE quickbase_project_id IS NULL;

-- If rollback is needed and there are null values, you must either:
-- Option 1: Delete those tasks
DELETE FROM arrivy_tasks WHERE quickbase_project_id IS NULL;

-- Option 2: Set placeholder values before rollback
UPDATE arrivy_tasks SET quickbase_project_id = 'ARRIVY-' || arrivy_task_id WHERE quickbase_project_id IS NULL;
UPDATE arrivy_tasks SET quickbase_record_id = 0 WHERE quickbase_record_id IS NULL;

-- Then rollback:
ALTER TABLE arrivy_tasks ALTER COLUMN quickbase_project_id SET NOT NULL;
ALTER TABLE arrivy_tasks ALTER COLUMN quickbase_record_id SET NOT NULL;
```

**Rollback Migration 014:**
If migration 014 fails or needs to be reversed:
```sql
BEGIN;
DROP TABLE IF EXISTS arrivy_task_status CASCADE;
DROP TABLE IF EXISTS arrivy_events CASCADE;
DROP TABLE IF EXISTS arrivy_entities CASCADE;
DROP TABLE IF EXISTS arrivy_tasks CASCADE;
COMMIT;
```

---

## Phase 3: Complete Task Detail Endpoint (Already Done!)

✅ The GET handler in `app/api/operations/field-tracking/tasks/[id]/route.ts` has been enhanced to:
- Fetch task details from `arrivy_tasks` table
- Fetch status history from `arrivy_task_status` table
- Fetch events from `arrivy_events` table
- Map entity IDs to entity names from `arrivy_entities` table

### Step 3.1: Test Endpoint Locally
```bash
# Start development server
npm run dev

# Test endpoint (after creating a test task)
curl http://localhost:3000/api/operations/field-tracking/tasks/TEST-001 \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN"
```

**Expected response:**
```json
{
  "task": {
    "id": 1,
    "arrivy_task_id": 123456789,
    "url_safe_id": "abc123",
    "quickbase_project_id": "TEST-001",
    "customer_name": "John Doe",
    "tracker_url": "https://app.arrivy.com/live/track/company/abc123",
    "entity_names": ["John Smith", "Jane Doe"]
  },
  "statusHistory": [...],
  "events": [...]
}
```

---

## Phase 4: Arrivy External Configuration (20 minutes)

### Step 4.1: Configure Webhook in Arrivy Dashboard

1. **Log into Arrivy:**
   - Navigate to https://app.arrivy.com/
   - Use your Arrivy admin credentials

2. **Access Webhook Settings:**
   - Go to **Settings** → **Integrations** → **Webhooks**
   - Click **Add New Webhook**

3. **Configure Webhook:**
   ```
   Name: Kin Home Sales Pipeline
   URL: https://your-domain.com/api/webhooks/arrivy
   Secret: [Paste your ARRIVY_WEBHOOK_SECRET from .env.local]
   Method: POST
   Events (select all):
     ☑ TASK_CREATED (automatically syncs new tasks to dashboard)
     ☑ TASK_STATUS (tracks progress: ENROUTE, STARTED, COMPLETE)
     ☑ CREW_ASSIGNED (updates crew assignments)
     ☑ ARRIVING (crew approaching customer location)
     ☑ LATE (automatic delay detection)
     ☑ NOSHOW (customer not available)
     ☑ TASK_RATING (customer feedback)
     ☑ EXCEPTION (issues reported by crew)
   ```

4. **Test Webhook Delivery:**
   - Click **Send Test Event**
   - Check your application logs for webhook processing
   - Expected log: `[Arrivy Webhook] Received event: TEST`

5. **Verify Webhook Configuration:**
```bash
# Check webhook endpoint is accessible
curl https://your-domain.com/api/webhooks/arrivy
```
**Expected response:**
```json
{"status":"ok","service":"arrivy-webhook"}
```

### Step 4.2: Create Field Crew Entities

You need to create an Arrivy entity for each field coordinator. There are two options:

#### Option A: Via Arrivy Dashboard (Recommended for initial setup)

1. **Navigate to Team Management:**
   - Go to **Team** → **Entities**
   - Click **Add Entity**

2. **For each coordinator, enter:**
   ```
   Name: John Smith
   Email: john.smith@kinhome.com
   Phone: +1-555-123-4567
   Type: CREW
   ```

3. **Save and note the Entity ID** (shown in URL or entity details)

4. **Link to QuickBase:**
   - In your application, entities will be auto-synced on first task assignment
   - Or manually sync using the service function

#### Option B: Via API (Automated sync)

Use the sync service to create entities from QuickBase coordinators:

```typescript
// In your sync script or admin panel
import { syncEntityFromQuickBase } from '@/lib/integrations/arrivy/service';

// For each coordinator in QuickBase
await syncEntityFromQuickBase({
  quickbase_user_id: 'QB_USER_ID',
  name: 'John Smith',
  email: 'john.smith@kinhome.com',
  phone: '+1-555-123-4567',
  entity_type: 'CREW'
});
```

### Step 4.3: Verify Entities in Database
```sql
SELECT 
  arrivy_entity_id,
  name,
  email,
  entity_type,
  quickbase_user_id
FROM arrivy_entities
ORDER BY name;
```

**Expected result:** List of all field coordinators with their Arrivy entity IDs

### Step 4.4: Test Task Creation

Create a test task to verify the full flow:

```bash
curl -X POST https://your-domain.com/api/operations/field-tracking/tasks \
  -H "Content-Type: application/json" \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN" \
  -d '{
    "projectId": "TEST-001",
    "recordId": 99999,
    "taskType": "survey",
    "customerName": "Test Customer",
    "customerPhone": "+1-555-987-6543",
    "customerEmail": "test@example.com",
    "customerAddress": "123 Main St, City, ST 12345",
    "scheduledStart": "2025-10-29T10:00:00Z",
    "scheduledEnd": "2025-10-29T12:00:00Z"
  }'
```

**Expected response:**
```json
{
  "success": true,
  "task": {
    "id": 123456789,
    "url_safe_id": "abc123",
    "tracker_url": "https://app.arrivy.com/live/track/company/abc123"
  }
}
```

---

## Phase 5: Production Deployment (15 minutes)

### Step 5.1: Set Environment Variables in Vercel

Using Vercel CLI:
```bash
# Add Arrivy credentials to production environment
vercel env add ARRIVY_AUTH_KEY production
# When prompted, enter: 0a27a7e3-e6b5

vercel env add ARRIVY_AUTH_TOKEN production
# When prompted, enter: 5730gWxBjDzbQDEeFh3zrs

vercel env add ARRIVY_COMPANY_NAME production
# When prompted, enter your company name

vercel env add ARRIVY_WEBHOOK_SECRET production
# When prompted, paste the generated webhook secret

vercel env add ARRIVY_BASE_URL production
# When prompted, enter: https://app.arrivy.com/api

vercel env add ARRIVY_RATE_LIMIT production
# When prompted, enter: 30
```

Or via Vercel Dashboard:
1. Go to https://vercel.com/your-project/settings/environment-variables
2. Add each variable manually
3. Set environment to **Production**

### Step 5.2: Run Production Migration

If not done in Phase 2:
```bash
# Get production database URL
vercel env pull .env.production.local
export DATABASE_URL=$(grep DATABASE_URL .env.production.local | cut -d '=' -f2-)

# Run migration
psql "$DATABASE_URL" -f lib/db/migrations/014_create_arrivy_tables.sql
```

### Step 5.3: Deploy Application

```bash
# Deploy to production
vercel --prod

# Or using Git (if auto-deploy enabled)
git add .
git commit -m "Deploy Arrivy integration"
git push origin main
```

### Step 5.4: Verify Deployment

1. **Check deployment status:**
```bash
vercel ls
```

2. **Test webhook endpoint:**
```bash
curl https://your-production-domain.com/api/webhooks/arrivy
```
**Expected:** `{"status":"ok","service":"arrivy-webhook"}`

3. **Test dashboard endpoint:**
```bash
curl https://your-production-domain.com/api/operations/field-tracking/dashboard \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN"
```
**Expected:** Dashboard data with metrics and tasks

4. **Check deployment logs:**
```bash
vercel logs
```
Look for: `[Arrivy] Client initialized successfully`

---

## Phase 6: Testing & Validation (30 minutes)

### Test 1: Create Test Task
```bash
curl -X POST https://your-domain.com/api/operations/field-tracking/tasks \
  -H "Content-Type: application/json" \
  -H "Cookie: next-auth.session-token=YOUR_SESSION_TOKEN" \
  -d '{
    "projectId": "TEST-001",
    "recordId": 99999,
    "taskType": "survey",
    "customerName": "Test Customer",
    "customerPhone": "+1-555-987-6543",
    "customerEmail": "test@example.com",
    "customerAddress": "123 Main St, City, ST 12345",
    "scheduledStart": "2025-10-29T10:00:00Z",
    "scheduledEnd": "2025-10-29T12:00:00Z"
  }'
```

**Verification:**
- ✅ Response includes `arrivy_task_id` and `tracker_url`
- ✅ Task appears in Arrivy dashboard
- ✅ Customer receives notification (if configured)

### Test 2: Verify Dashboard Display
1. Navigate to `https://your-domain.com/operations/scheduling`
2. **Verify metrics cards show:**
   - Total tasks count
   - Active tasks (ENROUTE, STARTED)
   - Completed today
   - Overdue tasks

3. **Verify task card displays:**
   - Customer name
   - Project ID
   - Task type badge
   - Status badge
   - Scheduled time
   - Tracker URL button
   - Assigned crew names

4. **Test interactions:**
   - Click task card → Detail modal opens
   - Click "Copy Tracker URL" → URL copied to clipboard
   - Click "Open Tracker" → Opens Arrivy tracker in new tab

### Test 2.5: Verify TASK_CREATED Webhook
1. Create a new task in Arrivy dashboard
2. Check application logs for webhook processing:
   ```bash
   vercel logs --filter "TASK_CREATED" --follow
   ```
3. Verify task appears in database:
   ```sql
   SELECT customer_name, task_type, tracker_url 
   FROM arrivy_tasks 
   ORDER BY created_at DESC 
   LIMIT 1;
   ```
4. Verify task appears in dashboard within 30 seconds (auto-refresh)
5. Check tracker URL is accessible

### Test 3: Webhook Processing
1. **Open task in Arrivy dashboard**
2. **Update task status:**
   - Change status to "ENROUTE"
   - Add a note
3. **Verify webhook processing:**
```bash
# Check application logs
vercel logs --follow

# Look for:
[Arrivy Webhook] Processing event: TASK_STATUS
[Arrivy Webhook] Task 123456789 status updated to: ENROUTE
```

4. **Verify database updated:**
```sql
SELECT * FROM arrivy_events 
WHERE arrivy_task_id = 123456789 
ORDER BY event_time DESC LIMIT 5;

SELECT * FROM arrivy_task_status
WHERE arrivy_task_id = 123456789
ORDER BY reported_at DESC LIMIT 5;
```

5. **Verify dashboard updates:**
   - Wait 30 seconds (polling interval)
   - Refresh dashboard
   - Status badge should show "ENROUTE"
   - Activity feed should show status change event

### Test 4: Tracker URL Customer Experience
1. **Copy tracker URL from task card**
   - Example: `https://app.arrivy.com/live/track/company/abc123`

2. **Open tracker in incognito/private window** (simulate customer)

3. **Verify tracker displays:**
   - Company logo and branding
   - Scheduled appointment time
   - Crew member names and photos (if configured)
   - Live crew location on map
   - Estimated arrival time (ETA)
   - Ability to message crew
   - Status updates timeline

4. **Test customer interactions:**
   - Send a message to crew
   - Verify crew receives notification in Arrivy mobile app
   - Verify message appears in events feed

### Test 5: Detail Modal
1. **Click any task card** in dashboard
2. **Verify modal displays:**
   - Full task details (customer name, address, phone, email)
   - Project ID and record ID
   - Task type and scheduled time range
   - Current status with color-coded badge
   - Assigned crew members with names
   - Tracker URL with copy and open buttons
   - Status history timeline
   - Activity feed with all events

3. **Test modal actions:**
   - Copy tracker URL
   - Open tracker in new tab
   - Close modal (click outside or X button)

### Test 6: Error Handling
1. **Test rate limiting:**
```bash
# Send 35 requests rapidly (exceeds 30/min limit)
for i in {1..35}; do
  curl https://your-domain.com/api/operations/field-tracking/dashboard &
done
```
**Expected:** Some requests return 429 with exponential backoff

2. **Test invalid task ID:**
```bash
curl https://your-domain.com/api/operations/field-tracking/tasks/INVALID-999
```
**Expected:** 404 Not Found

3. **Test unauthorized access:**
```bash
curl https://your-domain.com/api/operations/field-tracking/dashboard
```
**Expected:** 401 Unauthorized

### Test 7: Real-World Scenario
Simulate a complete appointment lifecycle:

1. **Create task** (QuickBase or API)
2. **Verify task synced** to Arrivy
3. **Assign crew** in dashboard
4. **Verify crew receives** Arrivy mobile app notification
5. **Crew marks ENROUTE** in mobile app
6. **Verify webhook** processed and dashboard updated
7. **Customer opens tracker** to see crew location
8. **Crew arrives** → Marks STARTED
9. **Crew completes** → Marks COMPLETE
10. **Verify completion** synced back to QuickBase (if configured)

**Expected result:** All steps complete successfully with real-time updates throughout.

---

## Phase 7: Alert System Configuration (10 minutes)

### Step 7.1: Verify Notification Types
Confirm new Arrivy notification types are included in the system:
- arrivy_task_late
- arrivy_task_noshow
- arrivy_task_exception
- arrivy_task_cancelled

```bash
# Check database for notification types
psql $DATABASE_URL -c "SELECT DISTINCT type FROM notifications WHERE source = 'arrivy' ORDER BY type;"
```

### Step 7.2: Configure Alert Preferences
1. Navigate to **Operations → Settings**
2. Review notification preferences section
3. Verify Arrivy alert types are listed under "Field Alerts"
4. Configure email preferences:
   - Enable/disable email notifications
   - Set email frequency (immediate, daily, weekly)
   - Configure quiet hours if desired
5. Select which alert types to receive
6. Click **Save Changes**

### Step 7.3: Test Alert Creation
1. **Create test task** in Arrivy with QuickBase external_id
2. **Update task status to LATE** in Arrivy dashboard
3. **Verify webhook processes event:**
```bash
# Check logs for webhook processing
tail -f /var/log/app.log | grep "Arrivy"
```

4. **Check notification created in database:**
```sql
SELECT id, type, title, source, priority, created_at 
FROM notifications 
WHERE source = 'arrivy' 
ORDER BY created_at DESC 
LIMIT 5;
```

5. **Verify email sent** (if preferences enabled):
   - Check email inbox for alert
   - Verify subject line and content
   - Verify tracker URLs are clickable

6. **Check dashboard bell** shows unread count
7. **Navigate to Operations → Alerts** to view notification

### Step 7.4: Test Alert Preferences
1. **Navigate to Settings** and disable "Field Task Late" notification type
2. **Trigger LATE event** in Arrivy
3. **Verify notification NOT created** (or created but email not sent)
4. **Re-enable preference** and test again
5. **Verify notification and email are sent**

### Step 7.5: Test Quiet Hours
1. **Set quiet hours** to current time ± 1 hour
2. **Trigger NOSHOW event**
3. **Verify:**
   - Notification is created in database
   - Email is NOT sent
   - Logs show "quiet hours" skip message
4. **Set quiet hours** outside current time
5. **Trigger EXCEPTION event**
6. **Verify both notification and email are sent**

### Step 7.6: Test Alerts Page
1. Navigate to **Operations → Alerts**
2. **Verify page loads** with all notifications
3. **Test filters:**
   - Filter by "Unread" → shows only unread
   - Filter by source "Arrivy" → shows only field alerts
   - Filter by type "Task Late" → shows only LATE events
4. **Test search** by customer name
5. **Click alert card** → marks as read and navigates
6. **Test "Mark All Read"** button
7. **Test "Load More"** pagination if applicable

### Step 7.7: Test Email Templates
Trigger each event type and verify email rendering:
- **LATE:** Red alert, urgency notice, "Contact customer immediately" action
- **NOSHOW:** Red alert, urgency notice, "Attempt to reach customer" action
- **EXCEPTION:** Orange alert, "Review crew report" action
- **CANCELLED:** Gray alert, "Verify cancellation reason" action

**Verify:**
- Subject lines are descriptive
- Customer name and task type displayed
- Tracker URLs are clickable
- Recommended actions are clear
- Styling is consistent and mobile-friendly

### Success Criteria
- ✅ Arrivy notification types added to system
- ✅ Alert preferences configurable in settings
- ✅ Critical events create notifications automatically
- ✅ Email alerts sent based on user preferences
- ✅ Notifications appear in dashboard bell
- ✅ Alerts page displays all notifications with filtering
- ✅ Quiet hours respected for email delivery
- ✅ Coordinator email lookup works for linked tasks
- ✅ Tasks without QuickBase link handled gracefully
- ✅ Duplicate alerts prevented
- ✅ Email templates render correctly

---

## Phase 8: Crew Performance Dashboard (Optional - 5 minutes)

### Step 8.1: Verify Crew Entities Synced
Ensure crew members are synced to the database:
```bash
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_entities;"
# Expected: At least 1 crew member
```

If no entities exist, sync from Arrivy:
```bash
npm run sync:arrivy:entities
```

### Step 8.2: Access Crew Performance Dashboard
1. Navigate to Operations → Crew Performance
2. Verify page loads without errors
3. Check metrics cards display data
4. Test time range filters

### Step 8.3: Test Performance Metrics
1. Verify tasks completed counts are accurate
2. Check completion time calculations
3. Confirm on-time percentage matches expectations
4. Validate customer ratings display correctly

### Step 8.4: Test CSV Export
1. Click "Export to CSV" button
2. Verify CSV downloads with crew metrics
3. Open CSV and check data accuracy
4. Confirm all columns are present

### Success Criteria
- ✅ Crew performance dashboard accessible
- ✅ Metrics calculated correctly from Arrivy data
- ✅ Comparison charts display crew vs team averages
- ✅ Top performers and support needed cards populated
- ✅ Performance table sortable and filterable
- ✅ CSV export generates valid file
- ✅ Dashboard auto-refreshes every 60 seconds

---

## Troubleshooting

### Alerts Not Being Created

**Symptoms:**
- Webhook processes successfully but no notification created
- Coordinator not receiving alerts

**Possible Causes & Solutions:**

1. **Task has no QuickBase association**
```bash
# Check task record
psql $DATABASE_URL -c "SELECT arrivy_task_id, quickbase_project_id, quickbase_record_id FROM arrivy_tasks WHERE arrivy_task_id = YOUR_TASK_ID;"
```
**Solution:** Ensure task was created with valid QuickBase external_id

2. **Coordinator email not in QuickBase**
```bash
# Check project coordinator field
# This requires QuickBase API access
```
**Solution:** Update PROJECT_COORDINATOR_EMAIL field in QuickBase

3. **User preferences block notification**
```bash
# Check user preferences
psql $DATABASE_URL -c "SELECT notification_types, email_enabled FROM pc_notification_preferences WHERE user_id = 'coordinator@example.com';"
```
**Solution:** Update preferences to include Arrivy notification types

4. **Notification creation failed**
```bash
# Check application logs
grep "Failed to create notification" /var/log/app.log
```
**Solution:** Review error logs for specific failure reason

### Emails Not Being Sent

**Symptoms:**
- Notification created but email not delivered
- Email enabled in preferences

**Possible Causes & Solutions:**

1. **EMAIL_ENABLED not true**
```bash
echo $EMAIL_ENABLED
```
**Solution:** Set `EMAIL_ENABLED=true` in environment

2. **Notification type not in user's enabled types**
```bash
# Check preferences
psql $DATABASE_URL -c "SELECT notification_types FROM pc_notification_preferences WHERE user_id = 'coordinator@example.com';"
```
**Solution:** Add missing notification type to array

3. **Quiet hours active**
```bash
# Check quiet hours settings
psql $DATABASE_URL -c "SELECT quiet_hours_start, quiet_hours_end FROM pc_notification_preferences WHERE user_id = 'coordinator@example.com';"
```
**Solution:** Wait until outside quiet hours or adjust settings

4. **Email configuration invalid**
```bash
# Check environment variables
echo $EMAIL_HOST
echo $EMAIL_PORT
echo $EMAIL_FROM
```
**Solution:** Verify all email environment variables are set correctly

### Crew Performance Shows No Data

**Symptoms:**
- Crew performance dashboard shows "No crew members found"
- Metrics show zero values
- Comparison charts empty

**Possible Causes & Solutions:**

1. **No crew entities synced**
```bash
# Check entities table
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_entities;"
```
**Solution:** Run entity sync command: `npm run sync:arrivy:entities`

2. **Tasks have no assigned entities**
```bash
# Check task assignments
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_tasks WHERE assigned_entity_ids IS NOT NULL AND array_length(assigned_entity_ids, 1) > 0;"
```
**Solution:** Ensure tasks are assigned to crew members in Arrivy

3. **No completed tasks in time range**
```bash
# Check for completed tasks
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_task_status WHERE status_type = 'COMPLETE' AND reported_at >= NOW() - INTERVAL '30 days';"
```
**Solution:** Select longer time range or wait for tasks to complete

4. **API endpoint error**
```bash
# Check logs
vercel logs --since 1h | grep crew-performance
```
**Solution:** Review error logs and check database connectivity

---

## Original Troubleshooting

### Issue: Webhook Not Receiving Events

**Symptoms:**
- Arrivy shows webhook delivery failures
- Events not appearing in activity feed
- Status changes not reflecting in dashboard

**Solutions:**
1. **Verify webhook URL is accessible:**
```bash
curl https://your-domain.com/api/webhooks/arrivy
```
Should return: `{"status":"ok","service":"arrivy-webhook"}`

2. **Check webhook secret matches:**
```bash
# In .env.local
grep ARRIVY_WEBHOOK_SECRET .env.local

# Compare with Arrivy dashboard webhook secret
```

3. **Review webhook logs in Arrivy:**
   - Go to Settings → Integrations → Webhooks
   - Click on webhook → View Delivery Log
   - Check response codes and error messages

4. **Check application logs:**
```bash
vercel logs --filter "Arrivy Webhook"
```
Look for signature verification errors or parsing errors.

5. **Test webhook locally with ngrok:**
```bash
# Start local server
npm run dev

# In another terminal
ngrok http 3000

# Update Arrivy webhook URL to ngrok URL
# Trigger test event
```

### Issue: Tasks Not Syncing to Arrivy

**Symptoms:**
- Task creation returns error
- Tasks appear in QuickBase but not Arrivy
- Dashboard shows no tasks

**Solutions:**
1. **Verify Arrivy credentials:**
```bash
# Test API connection
curl -X GET https://app.arrivy.com/api/tasks \
  -H "X-Auth-Key: 0a27a7e3-e6b5" \
  -H "X-Auth-Token: 5730gWxBjDzbQDEeFh3zrs"
```
Should return task list (may be empty).

2. **Check rate limiting:**
```bash
# Review logs for rate limit errors
grep "rate limit" vercel.log
```
Solution: Implement request queuing or reduce sync frequency.

3. **Verify database tables exist:**
```sql
\dt arrivy_*
```

4. **Check for API errors:**
```bash
vercel logs --filter "Failed to create Arrivy task"
```

5. **Test task creation directly:**
```bash
curl -X POST https://your-domain.com/api/operations/field-tracking/tasks \
  -H "Content-Type: application/json" \
  -d '{"projectId":"TEST","recordId":1,"taskType":"survey",...}'
```

### Issue: Dashboard Shows No Data

**Symptoms:**
- Dashboard loads but shows 0 tasks
- Metrics cards show all zeros
- Activity feed is empty

**Solutions:**
1. **Verify migration ran successfully:**
```sql
SELECT COUNT(*) FROM arrivy_tasks;
```

2. **Check user role permissions:**
```sql
-- Verify user role in session
SELECT role FROM users WHERE email = 'user@example.com';
```
Ensure user has one of: `operations_coordinator`, `operations_manager`, `office_leader`, `regional`, `super_admin`

3. **Test API endpoint directly:**
```bash
curl https://your-domain.com/api/operations/field-tracking/dashboard \
  -H "Cookie: next-auth.session-token=YOUR_TOKEN"
```

4. **Check browser console for errors:**
   - Open DevTools (F12)
   - Check Console tab for API errors
   - Check Network tab for failed requests

5. **Verify tasks exist in database:**
```sql
SELECT * FROM arrivy_tasks ORDER BY created_at DESC LIMIT 10;
```

### Issue: Rate Limit Exceeded

**Symptoms:**
- API returns 429 errors
- Logs show "Rate limit exceeded"
- Slow dashboard performance

**Solutions:**
1. **Check current rate limit:**
```bash
grep ARRIVY_RATE_LIMIT .env.local
```
Default: 30 requests per minute

2. **Implement request batching:**
   - Group multiple task updates into single request
   - Use bulk API endpoints where available

3. **Adjust polling frequency:**
   - Increase dashboard polling interval from 30s to 60s
   - Reduce number of entities synced per batch

4. **Monitor rate limit usage:**
```bash
# Check Arrivy API response headers
curl -I https://app.arrivy.com/api/tasks \
  -H "X-Auth-Key: ..." \
  -H "X-Auth-Token: ..."
```
Look for `X-RateLimit-Remaining` header.

### Issue: New Tasks Not Appearing in Dashboard

**Symptoms:**
- Tasks created in Arrivy don't appear in dashboard
- No TASK_CREATED webhooks in logs
- Manual refresh doesn't show new tasks

**Solutions:**
1. **Verify TASK_CREATED webhook is enabled:**
   - Log into Arrivy dashboard
   - Go to Settings → Integrations → Webhooks
   - Verify TASK_CREATED event is checked

2. **Check webhook delivery logs in Arrivy:**
   - Settings → Integrations → Webhooks → View Delivery Log
   - Look for failed TASK_CREATED deliveries
   - Check error messages

3. **Review application logs for processing errors:**
```bash
vercel logs --filter "TASK_CREATED" --filter "error"
```

4. **Confirm task was created with valid customer information:**
   - Tasks without customer name may not sync properly
   - Check task has scheduled date/time

5. **Test webhook endpoint manually with sample payload:**
   - Use the test payload from Arrivy documentation
   - Send test webhook and verify processing

### Issue: Tracker URLs Not Working

**Symptoms:**
- Clicking "Open Tracker" shows 404
- Tracker URL format looks incorrect
- Customers report tracker not loading

**Solutions:**
1. **Verify company name is set correctly:**
```bash
grep ARRIVY_COMPANY_NAME .env.local
```
Must match company name in Arrivy account settings.

2. **Check tracker URL format:**
Expected: `https://app.arrivy.com/live/track/{company_name}/{url_safe_id}`

3. **Test tracker URL manually:**
   - Copy URL from database
   - Open in incognito window
   - Check for redirects or errors

4. **Verify task has `url_safe_id`:**
```sql
SELECT url_safe_id, tracker_url FROM arrivy_tasks WHERE quickbase_project_id = 'TEST-001';
```

5. **Check Arrivy task visibility settings:**
   - Task must have "visible to customer" enabled
   - Task must not be in draft status

---

## Rollback Plan

If deployment fails or needs to be reversed:

### Step 1: Disable Webhook
1. Log into Arrivy dashboard
2. Go to Settings → Integrations → Webhooks
3. Toggle webhook to **Disabled**
4. This stops new events from being sent

### Step 2: Revert Environment Variables
```bash
# Remove Arrivy variables from Vercel
vercel env rm ARRIVY_AUTH_KEY production
vercel env rm ARRIVY_AUTH_TOKEN production
vercel env rm ARRIVY_WEBHOOK_SECRET production
vercel env rm ARRIVY_COMPANY_NAME production
```

### Step 3: Rollback Database Migration
```sql
BEGIN;

-- Drop tables in reverse order (respects foreign keys)
DROP TABLE IF EXISTS arrivy_task_status CASCADE;
DROP TABLE IF EXISTS arrivy_events CASCADE;
DROP TABLE IF EXISTS arrivy_entities CASCADE;
DROP TABLE IF EXISTS arrivy_tasks CASCADE;

-- Optional: Drop function if no longer needed
DROP FUNCTION IF EXISTS update_updated_at_column();

COMMIT;
```

### Step 4: Remove Code (Optional)
If rolling back completely:
```bash
git revert HEAD  # Revert last commit
git push origin main
vercel --prod  # Redeploy without Arrivy integration
```

### Step 5: Verify Rollback
1. Verify dashboard still loads (without field tracking section)
2. Check no Arrivy-related errors in logs
3. Verify QuickBase sync still works
4. Test other features to ensure no regressions

---

## Post-Deployment

### Monitoring

1. **Set up monitoring alerts** for critical errors:
```javascript
// In monitoring service (e.g., Sentry, DataDog)
const alerts = [
  { name: 'Arrivy Webhook Failures', threshold: 5, window: '5m' },
  { name: 'Arrivy API Rate Limit', threshold: 10, window: '1h' },
  { name: 'Task Sync Failures', threshold: 3, window: '15m' },
];
```

2. **Monitor webhook delivery success rate:**
   - Check Arrivy webhook dashboard daily
   - Target: >99% success rate
   - Alert if <95% over 24 hours

3. **Track API usage:**
```sql
-- Monitor task creation rate
SELECT 
  DATE_TRUNC('hour', created_at) as hour,
  COUNT(*) as tasks_created
FROM arrivy_tasks
WHERE created_at > NOW() - INTERVAL '24 hours'
GROUP BY hour
ORDER BY hour DESC;
```

4. **Monitor performance:**
```sql
-- Slow queries (if query logging enabled)
SELECT query, mean_exec_time, calls
FROM pg_stat_statements
WHERE query LIKE '%arrivy%'
ORDER BY mean_exec_time DESC
LIMIT 10;
```

### Maintenance

1. **Clean up old events periodically:**
```sql
-- Delete events older than 90 days
DELETE FROM arrivy_events
WHERE event_time < NOW() - INTERVAL '90 days';
```

2. **Archive completed tasks:**
```sql
-- Create archive table
CREATE TABLE arrivy_tasks_archive AS 
SELECT * FROM arrivy_tasks WHERE 1=0;

-- Move completed tasks older than 90 days
INSERT INTO arrivy_tasks_archive
SELECT * FROM arrivy_tasks
WHERE current_status = 'COMPLETE'
  AND updated_at < NOW() - INTERVAL '90 days';

DELETE FROM arrivy_tasks
WHERE current_status = 'COMPLETE'
  AND updated_at < NOW() - INTERVAL '90 days';
```

3. **Update entity information as crew changes:**
```sql
-- Update entity details
UPDATE arrivy_entities
SET 
  name = 'New Name',
  phone = '+1-555-NEW-PHONE',
  email = 'newemail@example.com',
  updated_at = NOW()
WHERE arrivy_entity_id = 123456;
```

4. **Review and optimize indexes:**
```sql
-- Check index usage
SELECT 
  schemaname, tablename, indexname, idx_scan
FROM pg_stat_user_indexes
WHERE tablename LIKE 'arrivy_%'
ORDER BY idx_scan ASC;

-- Drop unused indexes if needed
-- DROP INDEX IF EXISTS idx_name;
```

### Training

1. **Operations Coordinators:**
   - How to create field tasks
   - How to assign crews
   - How to monitor task progress
   - How to handle exceptions (late arrivals, no-shows)
   - How to share tracker URLs with customers

2. **Field Crews:**
   - How to use Arrivy mobile app
   - How to update task status (ENROUTE, STARTED, COMPLETE)
   - How to message customers
   - How to report issues or exceptions

3. **Customer Service:**
   - How to share tracker links with customers
   - How to troubleshoot tracker issues
   - How to escalate field issues

### Documentation

Create internal documentation for:
1. **Task Creation Process:**
   - When to create field tasks
   - Required fields and optional fields
   - Task type selection guidelines

2. **Crew Assignment Guidelines:**
   - How to assign appropriate crew for task type
   - Capacity planning considerations
   - Scheduling best practices

3. **Customer Communication:**
   - When to send tracker links
   - How to set customer expectations
   - Handling customer questions about crew location

---

## Success Criteria

✅ **Technical Success:**
- [ ] All environment variables configured
- [ ] Migration 014 executed successfully
- [ ] Migration 016 executed successfully
- [ ] Existing tasks with QuickBase IDs preserved
- [ ] New Arrivy-only tasks can be created with null QuickBase fields
- [ ] Webhook configured and receiving events
- [ ] Field crew entities created and synced
- [ ] Test task created and visible in dashboard
- [ ] Webhook events processed correctly
- [ ] Dashboard updates in real-time (30s polling)
- [ ] Tracker URLs accessible to customers
- [ ] Detail modal displays full information
- [ ] Task detail modal displays customer ratings (if available)
- [ ] Task detail modal shows crew contact information with phone/email
- [ ] Task detail modal displays attachment count and file names
- [ ] "View Live Tracker" button is prominent and functional
- [ ] Duration metrics calculated correctly for completed tasks
- [ ] Status timeline displays in chronological order with visual indicators
- [ ] No errors in application logs for 24 hours

✅ **User Success:**
- [ ] Operations coordinators can create tasks in <30 seconds
- [ ] Field crews receive notifications within 1 minute
- [ ] Customers can track crew location in real-time
- [ ] Status updates reflect in dashboard within 30 seconds
- [ ] 90%+ user satisfaction with tracker experience

✅ **Business Success:**
- [ ] Reduction in "Where is my crew?" customer calls
- [ ] Improved on-time arrival rate
- [ ] Better crew utilization and scheduling efficiency
- [ ] Increased customer satisfaction scores
- [ ] Reduced appointment no-shows

---

## Enhanced Task Detail Modal Testing

### Test 6: Enhanced Task Detail Modal

#### 6.1 Test Crew Contacts Display
1. Open task detail modal for task with assigned crew
2. Navigate to "Overview" tab
3. **Verify:**
   - Crew members display with avatars (initials)
   - Phone numbers and emails shown
   - Call and SMS buttons functional
   - "No crew assigned" message shown for unassigned tasks

#### 6.2 Test Attachments Display
1. In Arrivy mobile app, upload photo to a task
2. Report status update with attachment (e.g., STARTED with photo)
3. Wait for webhook to process
4. Open task detail modal
5. Navigate to "Photos" tab
6. **Verify:**
   - Attachment count badge shows on tab
   - Photo filename and uploader displayed
   - Upload timestamp formatted correctly
   - "View in Arrivy" button opens tracker in new tab
   - Empty state message for tasks with no photos

#### 6.3 Test Customer Ratings
1. Get tracker URL from a completed task
2. Open tracker URL in incognito browser
3. Leave 5-star rating with feedback comment
4. Wait for TASK_RATING webhook (check logs)
5. Open task detail modal
6. Navigate to "Feedback" tab
7. **Verify:**
   - Rating count badge shows on tab
   - 5 filled stars displayed correctly
   - Customer feedback text shown
   - Rating timestamp formatted
   - Empty state message for tasks with no ratings

**Verify in database:**
```bash
psql $DATABASE_URL -c "SELECT event_type, message, extra_fields->>'rating' as rating FROM arrivy_events WHERE event_type = 'TASK_RATING' ORDER BY event_time DESC LIMIT 1;"
```

#### 6.4 Test Duration Metrics
1. Create task scheduled for 2 hours (e.g., 10:00 AM - 12:00 PM)
2. At 10:05 AM, report STARTED status
3. At 11:30 AM, report COMPLETE status
4. Open task detail modal
5. Navigate to "Overview" tab, scroll to Schedule & Duration
6. **Verify:**
   - Scheduled Duration: 2h 0m
   - Actual Duration: 1h 25m
   - Start Status: "Started 5m late" (red, with TrendingDown icon)
   - Completed metrics show in header

#### 6.5 Test Visual Timeline
1. Create task with multiple status updates (ASSIGNED → ENROUTE → STARTED → COMPLETE)
2. Open task detail modal
3. Navigate to "Timeline" tab
4. **Verify:**
   - Statuses in chronological order (oldest to newest, top to bottom)
   - Vertical connecting line between status items
   - Status icons: CheckCircle2 (green) for COMPLETE, filled Circle for STARTED/ENROUTE
   - Reporter avatars with initials
   - Attachment indicators ("Has attachments") on statuses with photos
   - Timestamps formatted correctly

#### 6.6 Test Crew Contact Actions
1. Assign task to crew member with phone and email
2. Open task detail modal
3. Navigate to "Overview" tab
4. Locate crew contact card
5. **Verify:**
   - Phone icon button initiates call (`tel:` link)
   - SMS icon button opens SMS dialog (`sms:` link)
   - Avatar shows initials correctly
   - Entity type badge displayed (if available)

#### 6.7 Test Tracker Button Prominence
1. Open any task detail modal
2. **Verify:**
   - "View Live Tracker" button is at the top (hero section)
   - Large button with primary variant (blue)
   - "Copy Link" button adjacent
   - Both buttons functional
   - "View Live Tracker" opens in new tab
   - "Copy Link" shows success toast

#### 6.8 Test Empty States
1. **Task with no attachments:**
   - Photos tab shows "No photos uploaded yet"
2. **Task with no ratings:**
   - Feedback tab shows "No customer feedback yet"
3. **Task with no crew:**
   - Crew section shows "No crew assigned"
4. **Task with no status history:**
   - Timeline tab shows "No status updates yet"

---

### Troubleshooting Enhanced Modal Features

**Attachments Not Showing:**
- Verify crew uploaded photos via Arrivy mobile app
- Check `has_attachments` flag in database: `SELECT has_attachments FROM arrivy_task_status WHERE arrivy_task_id = X;`
- Test Arrivy API: Review response from `getTaskStatuses()` for files array
- Confirm files metadata is fetched in API endpoint

**Ratings Not Appearing:**
- Verify TASK_RATING webhook is enabled in Arrivy dashboard
- Check `arrivy_events` table: `SELECT * FROM arrivy_events WHERE event_type = 'TASK_RATING';`
- Confirm rating value is in `extra_fields` JSONB column
- Test rating via Arrivy tracker URL to generate sample event

**Crew Contacts Missing:**
- Verify entities exist: `SELECT * FROM arrivy_entities WHERE arrivy_entity_id = ANY(ARRAY[...]);`
- Check phone and email fields are populated
- Confirm entity IDs in task match entities in database

**Duration Metrics Incorrect:**
- Verify STARTED and COMPLETE status timestamps
- Check scheduled_start and scheduled_end are set on task
- Confirm calculation logic in `calculateTaskDurationMetrics()`

---

## Next Steps

### Phase 2 Enhancements (Future Roadmap)

1. **Map View:**
   - Integrate mapping library (Mapbox or Google Maps)
   - Show all active tasks on map with crew locations
   - Filter by task type, status, crew
   - Real-time location updates

2. **Advanced Analytics:**
   - Average task duration by type
   - Crew performance metrics
   - On-time arrival percentage
   - Customer satisfaction trends
   - Route optimization suggestions

3. **Smart Scheduling:**
   - AI-powered crew assignment
   - Route optimization for multiple tasks
   - Automatic rescheduling on delays
   - Capacity forecasting

4. **Enhanced Customer Experience:**
   - SMS notifications with tracker links
   - Two-way messaging with crew
   - Pre-appointment surveys
   - Post-appointment feedback collection

5. **Integration Enhancements:**
   - Sync task completion back to QuickBase
   - Trigger QuickBase workflows on status changes
   - Automatic follow-up task creation
   - Integration with CRM for customer history

---

## Support

### Internal Support
- **Development Team:** For API errors, webhook issues, database problems
- **Operations Team:** For workflow questions, crew assignment issues
- **IT Support:** For access issues, permission problems

### External Support

**Arrivy Support:**
- Documentation: https://app.arrivy.com/developer-portal
- Email: support@arrivy.com
- Response time: 24-48 hours

**Common Arrivy Support Topics:**
- API rate limit increases
- Webhook delivery troubleshooting
- Custom branding for tracker pages
- Mobile app configuration

### Logging and Debugging

**Check application logs:**
```bash
# All Arrivy-related logs
vercel logs --filter "Arrivy"

# Webhook processing logs
vercel logs --filter "Arrivy Webhook"

# Task sync logs
vercel logs --filter "Arrivy task"

# Error logs only
vercel logs --filter "error" --filter "arrivy"
```

**Database queries for debugging:**
```sql
-- Recent task activity
SELECT 
  t.quickbase_project_id,
  t.customer_name,
  t.current_status,
  COUNT(e.id) as event_count,
  MAX(e.event_time) as last_event
FROM arrivy_tasks t
LEFT JOIN arrivy_events e ON e.arrivy_task_id = t.arrivy_task_id
WHERE t.created_at > NOW() - INTERVAL '7 days'
GROUP BY t.id
ORDER BY t.created_at DESC;

-- Webhook event summary
SELECT 
  event_type,
  COUNT(*) as count,
  MAX(event_time) as last_received
FROM arrivy_events
WHERE event_time > NOW() - INTERVAL '24 hours'
GROUP BY event_type
ORDER BY count DESC;
```

---

## Appendix

### A. Environment Variables Reference

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `ARRIVY_AUTH_KEY` | Yes | - | Arrivy API authentication key |
| `ARRIVY_AUTH_TOKEN` | Yes | - | Arrivy API authentication token |
| `ARRIVY_COMPANY_NAME` | Yes | - | Company name for tracker URLs |
| `ARRIVY_WEBHOOK_SECRET` | Yes | - | Secret for webhook signature verification |
| `ARRIVY_BASE_URL` | No | `https://app.arrivy.com/api` | Arrivy API base URL |
| `ARRIVY_RATE_LIMIT` | No | `30` | Max requests per minute |

### B. Database Schema Reference

**Tables:**
- `arrivy_tasks` - Field tasks synced from QuickBase
- `arrivy_entities` - Field crew members and technicians
- `arrivy_events` - Webhook events for audit trail
- `arrivy_task_status` - Task status update history

**Key Relationships:**
- `arrivy_events.arrivy_task_id` → `arrivy_tasks.arrivy_task_id`
- `arrivy_task_status.arrivy_task_id` → `arrivy_tasks.arrivy_task_id`
- `arrivy_tasks.assigned_entity_ids[]` → `arrivy_entities.arrivy_entity_id`

### C. API Endpoints Reference

| Endpoint | Method | Auth | Purpose |
|----------|--------|------|---------|
| `/api/operations/field-tracking/dashboard` | GET | Required | Dashboard data and metrics |
| `/api/operations/field-tracking/tasks` | GET | Required | List all tasks |
| `/api/operations/field-tracking/tasks` | POST | Required | Create new task |
| `/api/operations/field-tracking/tasks/[id]` | GET | Required | Get task details |
| `/api/operations/field-tracking/tasks/[id]` | PUT | Required | Update task |
| `/api/operations/field-tracking/tasks/[id]` | DELETE | Required | Delete task |
| `/api/operations/field-tracking/entities` | GET | Required | List crew entities |
| `/api/operations/field-tracking/events` | GET | Required | Activity feed |
| `/api/webhooks/arrivy` | POST | Webhook secret | Receive Arrivy events |

### D. Webhook Event Types

| Event Type | Description | Triggers |
|------------|-------------|----------|
| `TASK_CREATED` | New task created | Task creation in Arrivy |
| `TASK_STATUS` | Status changed | Crew updates status in mobile app |
| `CREW_ASSIGNED` | Crew assigned | Coordinator assigns crew |
| `ARRIVING` | Crew arriving soon | GPS proximity trigger (5 min ETA) |
| `LATE` | Crew running late | Estimated arrival > scheduled time |
| `NOSHOW` | Customer not available | Crew reports no-show |
| `TASK_RATING` | Customer rated task | Customer submits rating |
| `EXCEPTION` | Exception occurred | Error or issue reported |

### E. Task Type Definitions

| Task Type | Description | Typical Duration | Required Fields |
|-----------|-------------|------------------|-----------------|
| `survey` | Site survey | 1-2 hours | Customer name, address, phone |
| `install` | Solar installation | 4-8 hours | Customer name, address, system size |
| `inspection` | Post-install inspection | 1 hour | Customer name, address, install date |
| `service` | Service call | 1-2 hours | Customer name, address, issue description |
| `other` | Other appointment | Varies | Customer name |

### F. Status Definitions

| Status | Display Name | Color | Description |
|--------|--------------|-------|-------------|
| `NOTSTARTED` | Not Started | Gray | Task created but crew not assigned |
| `ASSIGNED` | Assigned | Blue | Crew assigned but not en route |
| `ENROUTE` | En Route | Yellow | Crew traveling to customer |
| `STARTED` | In Progress | Green | Crew on site working |
| `COMPLETE` | Complete | Green | Task finished successfully |
| `CANCELLED` | Cancelled | Red | Task cancelled |
| `LATE` | Late | Red | Crew running behind schedule |
| `EXCEPTION` | Exception | Red | Issue or problem occurred |

---

## Changelog

### Version 1.0.0 (2025-10-28)
- Initial deployment guide created
- Covers all 6 deployment phases
- Includes troubleshooting and rollback procedures
- Added post-deployment monitoring and maintenance

---

**Document maintained by:** Development Team  
**Last updated:** October 28, 2025  
**Next review:** December 1, 2025

