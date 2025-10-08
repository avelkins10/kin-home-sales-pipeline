# Production Deployment Checklist

Complete checklist for deploying the notification system to production.

## ✅ Step 1: Environment Variables (COMPLETE)

All required environment variables have been added to Vercel:

- ✅ `QUICKBASE_WEBHOOK_SECRET` - For webhook signature verification
- ✅ `NEXT_PUBLIC_FRONT_CHAT_ID` - Front Chat widget ID
- ✅ `FRONT_CHAT_SECRET` - Front Chat verification secret

## ✅ Step 2: Deploy to Production (COMPLETE)

Latest deployment: https://kin-home-sales-pipeline-bbfrkdnn0-avelkins10s-projects.vercel.app

All code changes have been pushed and deployed:
- Commit 672952d: Phase 1 bug fixes
- Commit 054906e: Phase 2A QuickBase webhooks
- Commit b8c2bff: Phase 2B messaging backend
- Commit b5c2101: Phase 2B messaging UI

## 📋 Step 3: Run Database Migrations

### Option A: Via Admin API Endpoints (Recommended)

1. Log in to your production app as a super_admin user
2. Run migrations using curl or browser:

```bash
# Run notifications migration
curl -X POST https://your-domain.vercel.app/api/admin/migrate/notifications \
  -H "Cookie: your-session-cookie"

# Run messages migration
curl -X POST https://your-domain.vercel.app/api/admin/migrate/messages \
  -H "Cookie: your-session-cookie"
```

**OR** Use browser console while logged in as admin:
```javascript
// Run notifications migration
await fetch('/api/admin/migrate/notifications', { method: 'POST' })
  .then(r => r.json())
  .then(console.log);

// Run messages migration
await fetch('/api/admin/migrate/messages', { method: 'POST' })
  .then(r => r.json())
  .then(console.log);
```

### Option B: Direct Database Access

If you have direct access to the Neon database:

```bash
# Connect to production database
psql $DATABASE_URL

# Run migrations manually
\i scripts/migrations/001-create-notifications.sql
\i scripts/migrations/002-create-project-messages.sql
```

### Verify Migrations

Check that tables exist:
```sql
SELECT table_name FROM information_schema.tables
WHERE table_name IN ('notifications', 'project_messages');
```

## 📋 Step 4: Configure QuickBase Webhook

Follow the detailed guide in `docs/QUICKBASE_WEBHOOK_SETUP.md`

### Quick Setup Steps:

1. **Navigate to QuickBase Notes Table** (`bsb6bqt3b`)

2. **Go to Settings → Webhooks → Create New Webhook**

3. **Configure Basic Settings:**
   - **Name:** Rep Dashboard - New Rep Visible Note
   - **Trigger:** Record Created
   - **Filter:** `{141.EX.'Rep Visible'}`

4. **Configure Endpoint:**
   - **URL:** `https://your-domain.vercel.app/api/webhooks/quickbase/notes`
   - **Method:** POST
   - **Content Type:** application/json

5. **Configure Security:**
   - **Signing Method:** HMAC-SHA256
   - **Secret:** `7084855fa2409d7522bf4ddf0d4faeaa47829ae5d6da887687c7c6fa67b4da53`
   - **Header:** `x-quickbase-signature`

6. **Configure Payload:**
```json
{
  "recordid": "{{record_id}}",
  "fieldChanges": {
    "3": {"value": "{{3}}"},
    "6": {"value": "{{6}}"},
    "7": {"value": "{{7}}"},
    "8": {"value": "{{8}}"},
    "9": {"value": "{{9}}"},
    "13": {"value": "{{13}}"},
    "141": {"value": "{{141}}"}
  }
}
```

7. **Save and Enable** the webhook

## 📋 Step 5: Test the System

### Test 1: QuickBase Note Notifications

1. Log in to QuickBase
2. Navigate to a project in the Projects table
3. Create a new note:
   - **Category:** Sales (critical priority)
   - **Note Content:** "Test notification - please verify receipt"
   - **Rep Visible:** ✅ Checked
4. Save the note
5. Verify:
   - Webhook executes successfully in QuickBase logs
   - Check your Rep Dashboard notification bell
   - Notification should appear for all team members

### Test 2: Internal Project Messages

1. Log in to Rep Dashboard
2. Navigate to any project details page
3. Scroll to "Team Messages" section
4. Click "New Message"
5. Type a test message: "Testing the new messaging system!"
6. Click "Send Message"
7. Verify:
   - Toast notification appears with recipient count
   - Message appears in the list
   - Other team members receive notifications

### Test 3: Notification Bell Behavior

1. Create a test notification (via QB note or message)
2. Verify bell shows red badge with unread count
3. Click bell to open dropdown
4. Verify notification appears with correct:
   - Icon and color
   - Title and message preview
   - Timestamp
   - Priority badge
5. Click notification to navigate to project
6. Verify notification is marked as read
7. Verify badge count decreases

### Test 4: Permission Checking

Test with different user roles:

1. **Closer:** Should see notifications for their projects only
2. **Setter:** Should see notifications for their projects only
3. **Office Leader:** Should see all projects in their office
4. **Admin:** Should see all notifications

## 🔍 Monitoring & Troubleshooting

### Check Vercel Logs

```bash
vercel logs --prod
```

Look for:
- `[WEBHOOK] Received QuickBase note webhook`
- `[WEBHOOK] Creating notifications for recipients`
- `[INFO] [API] POST /api/webhooks/quickbase/notes`
- Any error messages

### Check QuickBase Webhook Logs

1. Go to QuickBase → Settings → Webhooks
2. Click on your webhook
3. View "Execution History"
4. Verify:
   - Successful executions (200/201 status)
   - No signature verification failures (401)
   - No payload errors (400)

### Common Issues

**Webhook not firing:**
- Verify "Rep Visible" checkbox is checked
- Check webhook filter: `{141.EX.'Rep Visible'}`
- Ensure webhook is enabled

**Signature verification fails:**
- Verify `QUICKBASE_WEBHOOK_SECRET` matches in both QuickBase and Vercel
- Check header name is `x-quickbase-signature`
- Ensure signing method is HMAC-SHA256

**No notifications created:**
- Check that project has team assignments (closer, setter, etc.)
- Verify users exist in database
- Check server logs for errors

**Notifications not appearing:**
- Clear browser cache
- Check network tab for API call failures
- Verify user is logged in
- Check notification bell React Query cache

## 🎉 Success Criteria

System is fully deployed when:

- ✅ All environment variables configured in Vercel
- ✅ Latest code deployed to production
- ✅ Database migrations completed successfully
- ✅ QuickBase webhook configured and active
- ✅ Test notifications appear in dashboard
- ✅ Test messages send successfully
- ✅ Notifications marked as read when viewed
- ✅ Webhook logs show successful executions

## 📞 Support

If you encounter issues:

1. Check Vercel deployment logs
2. Check QuickBase webhook execution history
3. Check browser console for errors
4. Verify database tables exist
5. Test with different user roles

For detailed troubleshooting, see:
- `docs/QUICKBASE_WEBHOOK_SETUP.md`
- `docs/NOTIFICATIONS_SYSTEM.md`
