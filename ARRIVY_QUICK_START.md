# Arrivy Integration - Quick Start Guide

## ⚡ 5-Minute Setup

This is the fastest path from code complete to working integration.

### Prerequisites
- [ ] Arrivy credentials (Auth Key: `0a27a7e3-e6b5`, Auth Token: `5730gWxBjDzbQDEeFh3zrs`)
- [ ] Database access with PostgreSQL
- [ ] Your Arrivy company name

---

## 🚀 Quick Setup Commands

```bash
# 1. Create environment file (30 seconds)
cd /Users/austinelkins/Rep_Dashboard
cp env.example .env.local

# 2. Generate webhook secret (10 seconds)
echo "ARRIVY_WEBHOOK_SECRET=$(openssl rand -base64 32)" >> .env.local

# 3. Edit .env.local and update these lines:
# Find the Arrivy section and set:
# ARRIVY_AUTH_KEY=0a27a7e3-e6b5 (already correct)
# ARRIVY_AUTH_TOKEN=5730gWxBjDzbQDEeFh3zrs (already correct)
# ARRIVY_COMPANY_NAME=your_company_name_from_arrivy_dashboard
# ARRIVY_WEBHOOK_SECRET=... (auto-generated above)

# 4. Run database migration (30 seconds)
psql $DATABASE_URL -f lib/db/migrations/014_create_arrivy_tables.sql

# 5. Verify migration (10 seconds)
psql $DATABASE_URL -c "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_name LIKE 'arrivy_%';"

# 6. Start development server (10 seconds)
npm run dev

# 7. Check logs for successful initialization
# Expected: "[Arrivy] Client initialized successfully"
```

**Total time: ~2 minutes**

---

## 📝 Environment File Quick Edit

Open `.env.local` and find the Arrivy section (around line 105). Update these two values:

```bash
# Line 113 - Replace with your company name
ARRIVY_COMPANY_NAME=your_company_name

# Line 118 - Should already be set by command above
# If not, run: openssl rand -base64 32
ARRIVY_WEBHOOK_SECRET=generated_secret_here
```

**How to find your company name:**
1. Log into https://app.arrivy.com/
2. Go to Settings → Company Profile
3. Look for "Company Name" or "URL Slug"
4. Copy that exact name (case-sensitive)

---

## ✅ Verification Checklist

Run these commands to verify everything is working:

```bash
# 1. Verify .env.local exists
ls -la .env.local
# Expected: .env.local file exists

# 2. Verify Arrivy variables are set
grep "ARRIVY_" .env.local
# Expected: 6 lines with ARRIVY_ variables

# 3. Verify database tables exist
psql $DATABASE_URL -c "\dt arrivy_*"
# Expected: 4 tables (arrivy_tasks, arrivy_entities, arrivy_events, arrivy_task_status)

# 4. Verify indexes exist
psql $DATABASE_URL -c "SELECT indexname FROM pg_indexes WHERE tablename LIKE 'arrivy_%';"
# Expected: 12 indexes

# 5. Test API endpoint
curl http://localhost:3000/api/webhooks/arrivy
# Expected: {"status":"ok","service":"arrivy-webhook"}
```

---

## 🧪 Quick Test

Create a test task to verify the full integration:

```bash
# Replace YOUR_SESSION_TOKEN with your actual session token
# (Get from browser dev tools → Application → Cookies → next-auth.session-token)

curl -X POST http://localhost:3000/api/operations/field-tracking/tasks \
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

**Then verify:**
1. Task appears at http://localhost:3000/operations/scheduling
2. Task exists in Arrivy dashboard at https://app.arrivy.com/tasks
3. Database has task: `psql $DATABASE_URL -c "SELECT * FROM arrivy_tasks;"`

---

## 🔗 Configure Arrivy Webhook (5 minutes)

1. **Log into Arrivy:** https://app.arrivy.com/
2. **Navigate:** Settings → Integrations → Webhooks
3. **Click:** Add New Webhook
4. **Configure:**
   - Name: `Kin Home Sales Pipeline`
   - URL: `http://localhost:3000/api/webhooks/arrivy` (for dev) or `https://your-domain.com/api/webhooks/arrivy` (for prod)
   - Secret: Copy from `.env.local` `ARRIVY_WEBHOOK_SECRET` value
   - Method: `POST`
   - Events: Select all:
     - ☑ TASK_CREATED
     - ☑ TASK_STATUS
     - ☑ CREW_ASSIGNED
     - ☑ ARRIVING
     - ☑ LATE
     - ☑ NOSHOW
     - ☑ TASK_RATING
     - ☑ EXCEPTION
5. **Test:** Click "Send Test Event"
6. **Verify:** Check application logs for webhook processing

---

## 👥 Create Field Crew Entities (5 minutes)

**Option A: Via Arrivy Dashboard (Recommended)**
1. Go to Team → Entities
2. Click "Add Entity"
3. For each coordinator:
   - Name: John Smith
   - Email: john.smith@kinhome.com
   - Phone: +1-555-123-4567
   - Type: CREW
4. Save

**Option B: Via API**
```typescript
// In admin panel or script
import { syncEntityFromQuickBase } from '@/lib/integrations/arrivy/service';

await syncEntityFromQuickBase({
  quickbase_user_id: 'QB_USER_ID',
  name: 'John Smith',
  email: 'john.smith@kinhome.com',
  phone: '+1-555-123-4567',
  entity_type: 'CREW'
});
```

**Verify:**
```bash
psql $DATABASE_URL -c "SELECT * FROM arrivy_entities;"
```

---

## 🚀 Deploy to Production (10 minutes)

```bash
# 1. Set environment variables in Vercel
vercel env add ARRIVY_AUTH_KEY production
# Enter: 0a27a7e3-e6b5

vercel env add ARRIVY_AUTH_TOKEN production
# Enter: 5730gWxBjDzbQDEeFh3zrs

vercel env add ARRIVY_COMPANY_NAME production
# Enter: your_company_name

vercel env add ARRIVY_WEBHOOK_SECRET production
# Enter: (same secret from .env.local)

vercel env add ARRIVY_BASE_URL production
# Enter: https://app.arrivy.com/api

vercel env add ARRIVY_RATE_LIMIT production
# Enter: 30

# 2. Run production migration
vercel env pull .env.production.local
export PROD_DB=$(grep DATABASE_URL .env.production.local | cut -d '=' -f2-)
psql "$PROD_DB" -f lib/db/migrations/014_create_arrivy_tables.sql

# 3. Deploy
vercel --prod

# 4. Verify
curl https://your-domain.com/api/webhooks/arrivy
# Expected: {"status":"ok","service":"arrivy-webhook"}

# 5. Update Arrivy webhook URL to production URL
# Go to Arrivy dashboard and update webhook URL to:
# https://your-domain.com/api/webhooks/arrivy
```

---

## 🎯 Success Indicators

After setup, you should see:

✅ **Environment:**
- `.env.local` exists with 6 ARRIVY_ variables
- No errors when starting dev server
- Logs show "Arrivy client initialized"

✅ **Database:**
- 4 tables created (arrivy_tasks, arrivy_entities, arrivy_events, arrivy_task_status)
- 12 indexes created
- No migration errors

✅ **API:**
- Webhook endpoint returns `{"status":"ok"}`
- Test task creation succeeds
- Task appears in database and Arrivy dashboard

✅ **UI:**
- Dashboard loads at /operations/scheduling
- Test task appears in task list
- Task card shows tracker URL button
- Detail modal opens and displays task info

---

## ❌ Common Issues & Quick Fixes

### Issue: "Arrivy not configured"
```bash
# Fix: Verify .env.local exists and has ARRIVY_ variables
grep "ARRIVY_" .env.local

# If missing, copy from env.example
cp env.example .env.local
# Then edit and add credentials
```

### Issue: "Database tables not found"
```bash
# Fix: Run migration
psql $DATABASE_URL -f lib/db/migrations/014_create_arrivy_tables.sql

# Verify tables exist
psql $DATABASE_URL -c "\dt arrivy_*"
```

### Issue: "Webhook signature verification failed"
```bash
# Fix: Ensure secret matches in both places
grep ARRIVY_WEBHOOK_SECRET .env.local
# Compare with secret in Arrivy webhook settings
# If different, regenerate and update both
```

### Issue: "Tracker URL shows 404"
```bash
# Fix: Verify company name is correct
grep ARRIVY_COMPANY_NAME .env.local
# Should match exactly (case-sensitive) with Arrivy dashboard
# Settings → Company Profile → Company Name
```

### Issue: "Rate limit exceeded"
```bash
# Fix: Reduce polling frequency or batch requests
# Check rate limit in .env.local
grep ARRIVY_RATE_LIMIT .env.local
# Default is 30 requests per minute (Arrivy's limit)
```

---

## 📚 Full Documentation

For complete details, troubleshooting, and advanced configuration:

- **Full deployment guide:** `ARRIVY_DEPLOYMENT_GUIDE.md`
- **Environment setup:** `ARRIVY_ENV_SETUP.md`
- **Implementation summary:** `ARRIVY_IMPLEMENTATION_SUMMARY.md`

---

## 🆘 Need Help?

1. **Check logs:**
```bash
# Development
npm run dev  # Check console output

# Production
vercel logs --filter "Arrivy"
```

2. **Test API directly:**
```bash
# Test Arrivy API credentials
curl -X GET https://app.arrivy.com/api/tasks \
  -H "X-Auth-Key: 0a27a7e3-e6b5" \
  -H "X-Auth-Token: 5730gWxBjDzbQDEeFh3zrs"
```

3. **Check database:**
```bash
# Verify data exists
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_tasks;"
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_entities;"
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_events;"
```

4. **Review documentation:**
- Start with `ARRIVY_QUICK_START.md` (this file)
- Deep dive with `ARRIVY_DEPLOYMENT_GUIDE.md`
- Environment issues: `ARRIVY_ENV_SETUP.md`

---

## ⏱️ Quick Start Timeline

| Step | Duration | Total |
|------|----------|-------|
| Environment setup | 2 min | 2 min |
| Database migration | 1 min | 3 min |
| Verification | 1 min | 4 min |
| Webhook config | 5 min | 9 min |
| Create entities | 5 min | 14 min |
| Test locally | 5 min | 19 min |
| Deploy to prod | 10 min | 29 min |
| Final testing | 10 min | 39 min |

**Total: ~40 minutes from start to production** 🚀

---

**Ready to start?** Copy and paste the commands from the "Quick Setup Commands" section above!

