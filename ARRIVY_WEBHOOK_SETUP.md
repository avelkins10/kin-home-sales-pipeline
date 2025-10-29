# Arrivy Webhook Setup & Verification Guide

This guide explains how to configure Arr ivy webhooks and verify they're working correctly.

## Webhook Endpoint

**Production URL:** `https://your-domain.vercel.app/api/webhooks/arrivy`

**What it does:**
- Receives real-time events from Arrivy (task status changes, crew assignments, exceptions)
- Stores events in `arrivy_events` table
- Updates `arrivy_tasks` table with current status
- Creates notifications for critical events (LATE, NOSHOW, EXCEPTION)
- Triggers email alerts based on user preferences

## Configure Webhooks in Arrivy

### 1. Log into Arrivy Dashboard
Visit https://app.arrivy.com and log in as admin

### 2. Navigate to Webhooks
Go to **Settings → Webhooks** (or **Developer → Webhooks**)

### 3. Create Webhook
Click **Add Webhook** and configure:

| Field | Value |
|-------|-------|
| **URL** | `https://your-production-domain.vercel.app/api/webhooks/arrivy` |
| **Secret** | Copy from `ARRIVY_WEBHOOK_SECRET` env var |
| **Events** | Select all or specific events (see below) |
| **Active** | ✅ Enabled |

### 4. Events to Subscribe To

**Critical Events (Required):**
- `TASK_STATUS` - Status changes (STARTED, COMPLETE, etc.)
- `TASK_CREATED` - New task creation
- `LATE` - Crew running late
- `NOSHOW` - Crew no-show
- `ARRIVING` - Crew en route
- `CREW_ASSIGNED` - Crew assignment changes
- `TASK_RATING` - Customer ratings

**Optional Events:**
- `CUSTOMER_NOTE` - Customer feedback/notes
- `TASK_UPDATED` - Task field changes
- `TASK_DELETED` - Task cancellations

## Environment Variables

Ensure these are set in Vercel:

```bash
# Arrivy API Credentials
ARRIVY_AUTH_KEY=your_auth_key
ARRIVY_AUTH_TOKEN=your_auth_token

# Webhook Security (optional but recommended)
ARRIVY_WEBHOOK_SECRET=your_secret_here

# Email Notifications
SENDGRID_API_KEY=your_sendgrid_key
SENDGRID_FROM_EMAIL=noreply@yourdomain.com
```

## Testing Webhooks

### Test 1: Health Check
```bash
curl https://your-domain.vercel.app/api/webhooks/arrivy

# Expected response:
{
  "status": "ok",
  "service": "arrivy-webhook",
  "timestamp": "2025-10-29T..."
}
```

### Test 2: Simulate Webhook (requires secret)
```bash
curl -X POST https://your-domain.vercel.app/api/webhooks/arrivy \
  -H "Content-Type: application/json" \
  -H "x-arrivy-signature: your_signature_here" \
  -d '{
    "EVENT_ID": 12345,
    "EVENT_TYPE": "TASK_STATUS",
    "EVENT_SUB_TYPE": "STARTED",
    "EVENT_TIME": "2025-10-29T12:00:00Z",
    "OBJECT_ID": 67890,
    "REPORTER_ID": 123,
    "REPORTER_NAME": "John Doe",
    "TITLE": "Task Started",
    "MESSAGE": "Task has been started",
    "OBJECT_FIELDS": {},
    "EXTRA_FIELDS": {}
  }'
```

### Test 3: Trigger Real Webhook
1. Create a test task in Arrivy
2. Change its status (NOT_STARTED → STARTED)
3. Check Vercel logs for webhook receipt:
   ```bash
   vercel logs --follow
   ```
4. Verify event appears in database:
   ```sql
   SELECT * FROM arrivy_events ORDER BY event_time DESC LIMIT 10;
   ```

## Webhook Flow

```
Arrivy Event Occurs (e.g., crew marks task as STARTED)
        ↓
Arrivy sends HTTP POST to /api/webhooks/arrivy
        ↓
Signature verification (if ARRIVY_WEBHOOK_SECRET set)
        ↓
Parse webhook payload
        ↓
Store event in arrivy_events table (dedupe by EVENT_ID)
        ↓
Process event based on type:
  - TASK_STATUS → Update arrivy_tasks.current_status
  - TASK_CREATED → Fetch full task, store in arrivy_tasks
  - LATE/NOSHOW → Create notification + email alert
  - CREW_ASSIGNED → Update task.assigned_entity_ids
        ↓
Return 200 OK to Arrivy
```

## Troubleshooting

### Webhooks Not Arriving

1. **Check Arrivy webhook configuration:**
   - URL is correct and uses HTTPS
   - Webhook is enabled/active
   - Events are selected

2. **Check Vercel logs:**
   ```bash
   vercel logs --follow
   ```
   Look for `[Arrivy Webhook]` log messages

3. **Verify endpoint is accessible:**
   ```bash
   curl https://your-domain.vercel.app/api/webhooks/arrivy
   ```
   Should return 200 OK with `{"status":"ok"}`

4. **Check for signature mismatch:**
   If `ARRIVY_WEBHOOK_SECRET` is set, signature must match
   Temporarily disable by removing env var to test

### Duplicate Events

- Arrivy may retry webhooks on 5xx errors
- Our handler dedupe by `EVENT_ID` - duplicates are silently ignored
- Check `arrivy_events` table for duplicate `event_id` values

### Missing Events in Database

1. **Check Vercel function logs** for errors during processing
2. **Verify database connection** - check `POSTGRES_URL` env var
3. **Check for rate limiting** - Arrivy may batch/throttle webhooks
4. **Look for 5xx responses** - causes Arrivy to retry

## Webhook + Cron Sync Strategy

**Webhooks provide:**
- Real-time updates (instant notification of status changes)
- Low latency (events arrive within seconds)
- Event-driven architecture

**Cron sync provides:**
- Safety net for missed webhooks
- Backfill of historical data
- Status history bulk sync
- Attachment metadata caching

**Recommended Configuration:**
- Webhooks: Enabled for all events ✅
- Cron sync: Every 6 hours (syncs last 7 days) ✅
- Both work together for complete coverage

## Monitoring Webhooks

### Check Webhook Delivery in Arrivy
1. Go to Arrivy **Settings → Webhooks**
2. Click on your webhook
3. View **Recent Deliveries** tab
4. Check for 200 OK responses

### Check Database for Recent Events
```sql
SELECT
  event_type,
  event_sub_type,
  COUNT(*) as count,
  MAX(event_time) as latest_event
FROM arrivy_events
WHERE event_time > NOW() - INTERVAL '24 hours'
GROUP BY event_type, event_sub_type
ORDER BY latest_event DESC;
```

### Check Notifications Created
```sql
SELECT
  notification_type,
  COUNT(*) as count,
  MAX(created_at) as latest
FROM pc_notifications
WHERE notification_type LIKE 'arrivy_%'
  AND created_at > NOW() - INTERVAL '24 hours'
GROUP BY notification_type;
```

## Next Steps

1. ✅ Configure webhook in Arrivy dashboard
2. ✅ Add `ARRIVY_WEBHOOK_SECRET` to Vercel env vars
3. ✅ Test webhook with real Arrivy event
4. ✅ Monitor Vercel logs for webhook receipts
5. ✅ Verify events appear in `arrivy_events` table
6. ✅ Verify notifications created for LATE/NOSHOW events
7. ✅ Run initial backfill sync via cron endpoint
8. ✅ Verify cron runs every 6 hours automatically
