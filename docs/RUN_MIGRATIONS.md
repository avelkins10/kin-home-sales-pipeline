# Run Production Migrations - Quick Guide

## 🚀 How to Run Migrations in Production

### Method 1: Browser Console (Easiest)

1. Log in to your production app as **super_admin** user
2. Open browser DevTools (F12)
3. Go to Console tab
4. Run these commands one at a time:

```javascript
// Run notifications migration
await fetch('/api/admin/migrate/notifications', { method: 'POST' })
  .then(r => r.json())
  .then(result => {
    console.log('✅ Notifications migration:', result);
  });

// Run messages migration
await fetch('/api/admin/migrate/messages', { method: 'POST' })
  .then(r => r.json())
  .then(result => {
    console.log('✅ Messages migration:', result);
  });
```

Expected output:
```json
{
  "success": true,
  "message": "Notifications migration completed",
  "tableCreated": true
}
```

### Method 2: Using curl (if you have session cookie)

```bash
# Get your session cookie from browser DevTools → Application → Cookies
SESSION_COOKIE="your-session-cookie-here"
DOMAIN="https://kin-home-sales-pipeline-qywqzixjm-avelkins10s-projects.vercel.app"

# Run notifications migration
curl -X POST "$DOMAIN/api/admin/migrate/notifications" \
  -H "Cookie: $SESSION_COOKIE"

# Run messages migration
curl -X POST "$DOMAIN/api/admin/migrate/messages" \
  -H "Cookie: $SESSION_COOKIE"
```

## ✅ Verify Migrations

After running both migrations, verify they worked:

```javascript
// Check if tables exist
await fetch('/api/projects/1001/messages')
  .then(r => r.json())
  .then(console.log);

await fetch('/api/notifications')
  .then(r => r.json())
  .then(console.log);
```

Both should return data (or empty arrays) instead of errors.

## 🔒 Security Notes

- Only **super_admin** users can run migrations
- Migrations can be run multiple times safely (DROP TABLE IF EXISTS)
- Existing data will be preserved if tables already exist
- Migrations are logged for audit purposes

## ⚠️ Troubleshooting

**403 Forbidden:**
- Make sure you're logged in as a super_admin user
- Check your user role in the users table

**500 Internal Server Error:**
- Check Vercel logs: `vercel logs --prod`
- Verify DATABASE_URL environment variable is set
- Check database connection is working

**Already ran migrations but getting errors:**
- Migrations are idempotent (safe to run multiple times)
- Try running them again - they'll drop and recreate tables

## 📝 What Gets Created

### Notifications Table
- Stores all notification types (QB notes, messages, alerts)
- Indexed for fast queries by user, project, read status
- Auto-updating timestamps

### Project Messages Table
- Stores internal team messages
- Soft delete for audit trail
- Indexed for fast project lookups

## 🎯 Next Steps After Migrations

1. ✅ Run migrations (you're here!)
2. 📋 Configure QuickBase webhook (see QUICKBASE_WEBHOOK_SETUP.md)
3. 🧪 Test system end-to-end
4. 🎉 Go live!
