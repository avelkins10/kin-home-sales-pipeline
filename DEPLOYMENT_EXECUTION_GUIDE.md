# Arrivy Integration - Deployment Execution Guide

**Status:** Ready for Deployment (95% Complete)  
**Estimated Time:** 50 minutes  
**Last Updated:** October 28, 2025

---

## 📋 Quick Overview

All code is implemented and ready. This guide focuses on **configuration and deployment** only.

**What's Complete:**
- ✅ Backend API and database layer
- ✅ Frontend dashboard and components
- ✅ Webhook processing with signature verification
- ✅ Environment variables configured in `.env.local`
- ✅ Database migration file ready

**What's Needed:**
- ⚠️ Execute database migration
- ⚠️ Configure Arrivy webhook
- ⚠️ Create field crew entities
- ⚠️ Deploy to production
- ⚠️ Test end-to-end

---

## 🚀 Execution Steps

### Phase 1: Database Setup (5 minutes)

#### Step 1.1: Execute Migration (Local)
```bash
cd /Users/austinelkins/Rep_Dashboard
psql $DATABASE_URL -f lib/db/migrations/014_create_arrivy_tables.sql
```

**Expected Output:**
```
BEGIN
CREATE FUNCTION
CREATE TABLE
CREATE TABLE
CREATE TABLE
CREATE TABLE
CREATE INDEX (x13)
CREATE TRIGGER (x2)
COMMENT (x12)
COMMIT
```

#### Step 1.2: Verify Tables Created
```bash
psql $DATABASE_URL -c "SELECT table_name FROM information_schema.tables WHERE table_schema = 'public' AND table_name LIKE 'arrivy_%' ORDER BY table_name;"
```

**Expected Output:**
```
    table_name     
-------------------
 arrivy_entities
 arrivy_events
 arrivy_task_status
 arrivy_tasks
(4 rows)
```

#### Step 1.3: Verify Indexes
```bash
psql $DATABASE_URL -c "SELECT COUNT(*) FROM pg_indexes WHERE tablename LIKE 'arrivy_%';"
```

**Expected Output:**
```
 count 
-------
    13
(1 row)
```

#### Step 1.4: Test Query
```bash
psql $DATABASE_URL -c "SELECT COUNT(*) FROM arrivy_tasks;"
```

**Expected Output:**
```
 count 
-------
     0
(1 row)
```

✅ **Phase 1 Complete** - Database ready for use

---

### Phase 2: External Service Configuration (10 minutes)

#### Step 2.1: Configure Arrivy Webhook

Follow **[ARRIVY_EXTERNAL_CONFIGURATION.md](ARRIVY_EXTERNAL_CONFIGURATION.md) - Part 1**

**Quick Steps:**
1. Log into https://app.arrivy.com/
2. Go to Settings → Integrations → Webhooks
3. Click "Add New Webhook"
4. Configure:
   - Name: `KIN Home Operations Dashboard`
   - URL: `http://localhost:3000/api/webhooks/arrivy` (for local testing)
   - Secret: `GUTbkjl47DoOMaRsSSpDo/GKJcdrK+PP536UHEFE5ws=`
   - Events: Select ALL (TASK_CREATED, TASK_STATUS, CREW_ASSIGNED, ARRIVING, LATE, NOSHOW, TASK_RATING, EXCEPTION)
5. Save and test

**Verify:**
```bash
curl http://localhost:3000/api/webhooks/arrivy
# Expected: {"status":"ok","service":"arrivy-webhook"}
```

#### Step 2.2: Create Field Crew Entities

Follow **[ARRIVY_EXTERNAL_CONFIGURATION.md](ARRIVY_EXTERNAL_CONFIGURATION.md) - Part 2**

**Quick Steps:**
1. In Arrivy, go to Team → Entities
2. Click "Add Entity"
3. Fill in details for each crew member:
   - Name: Full name
   - Email: Work email
   - Phone: +1-555-XXX-XXXX
   - Type: CREW
4. Save
5. Repeat for all crew members

**Verify:**
```bash
# After creating entities and syncing
psql $DATABASE_URL -c "SELECT name, email FROM arrivy_entities ORDER BY name;"
```

✅ **Phase 2 Complete** - External services configured

---

### Phase 3: Local Testing & Validation (10 minutes)

#### Step 3.1: Start Development Server
```bash
npm run dev
```

**Expected:** Server starts on http://localhost:3000

#### Step 3.2: Run Full Test Suite

Follow **[ARRIVY_TESTING_CHECKLIST.md](ARRIVY_TESTING_CHECKLIST.md) - Local Testing**

**Critical Tests:**
1. ✅ Webhook endpoint responds
2. ✅ Dashboard loads
3. ✅ Database tables accessible
4. ✅ Arrivy API connection works
5. ✅ Test task created and synced
6. ✅ Webhook events received
7. ✅ Status updates propagate

**Quick Validation:**
```bash
# Test webhook
curl http://localhost:3000/api/webhooks/arrivy

# Access dashboard
open http://localhost:3000/operations/scheduling

# Test API
curl http://localhost:3000/api/operations/field-tracking/dashboard \
  -H "Cookie: next-auth.session-token=YOUR_TOKEN"
```

✅ **Phase 3 Complete** - Local environment validated

---

### Phase 4: Production Deployment (15 minutes)

Follow **[ARRIVY_PRODUCTION_DEPLOYMENT.md](ARRIVY_PRODUCTION_DEPLOYMENT.md)** for complete steps.

#### Step 4.1: Set Vercel Environment Variables
```bash
vercel link  # If not already linked
vercel env add ARRIVY_AUTH_KEY production
vercel env add ARRIVY_AUTH_TOKEN production
vercel env add ARRIVY_COMPANY_NAME production
vercel env add ARRIVY_WEBHOOK_SECRET production
vercel env add ARRIVY_BASE_URL production
vercel env add ARRIVY_RATE_LIMIT production
```

#### Step 4.2: Run Production Migration
```bash
vercel env pull .env.production.local
export PROD_DB=$(grep DATABASE_URL .env.production.local | cut -d '=' -f2- | tr -d '"')
psql "$PROD_DB" -f lib/db/migrations/014_create_arrivy_tables.sql
```

#### Step 4.3: Deploy Application
```bash
vercel --prod
```

#### Step 4.4: Update Arrivy Webhook URL
1. Log into https://app.arrivy.com/
2. Go to Settings → Integrations → Webhooks
3. Edit webhook
4. Update URL to: `https://your-domain.vercel.app/api/webhooks/arrivy`
5. Save and test

✅ **Phase 4 Complete** - Production deployed

---

### Phase 5: Production Testing (10 minutes)

Follow **[ARRIVY_TESTING_CHECKLIST.md](ARRIVY_TESTING_CHECKLIST.md) - Production Testing**

#### Step 5.1: Access Production Dashboard
```bash
open https://your-domain.vercel.app/operations/scheduling
```

#### Step 5.2: Create Test Task
1. Log into Arrivy
2. Create task with External ID: `PROD-TEST-001`
3. Assign crew member
4. Save

#### Step 5.3: Verify Webhook Delivery
```bash
vercel logs --filter "Arrivy" --follow
```

#### Step 5.4: Verify Database
```bash
psql "$PROD_DB" -c "SELECT customer_name FROM arrivy_tasks ORDER BY created_at DESC LIMIT 5;"
```

#### Step 5.5: Test Status Update
1. Update task status in Arrivy to "ENROUTE"
2. Wait 30 seconds
3. Refresh production dashboard
4. Verify status updated

✅ **Phase 5 Complete** - Production validated

---

## ✅ Final Checklist

- [ ] Database migration executed (local)
- [ ] Database migration executed (production)
- [ ] Arrivy webhook configured
- [ ] Field crew entities created
- [ ] Local testing passed (15 tests)
- [ ] Environment variables set in Vercel
- [ ] Application deployed to production
- [ ] Production webhook URL updated
- [ ] Production testing passed (5 tests)
- [ ] No errors in production logs

---

## 📊 Verification Commands

### Local Environment
```bash
# Database
psql $DATABASE_URL -c "\dt arrivy_*"

# Webhook
curl http://localhost:3000/api/webhooks/arrivy

# Dashboard
open http://localhost:3000/operations/scheduling
```

### Production Environment
```bash
# Database
psql "$PROD_DB" -c "\dt arrivy_*"

# Webhook
curl https://your-domain.vercel.app/api/webhooks/arrivy

# Dashboard
open https://your-domain.vercel.app/operations/scheduling

# Logs
vercel logs --filter "Arrivy"
```

---

## 🆘 Troubleshooting

### Issue: Migration Fails
```bash
# Check connection
psql $DATABASE_URL -c "SELECT version();"

# Check for existing tables
psql $DATABASE_URL -c "\dt arrivy_*"

# If tables exist, drop and retry
psql $DATABASE_URL -c "BEGIN; DROP TABLE IF EXISTS arrivy_task_status CASCADE; DROP TABLE IF EXISTS arrivy_events CASCADE; DROP TABLE IF EXISTS arrivy_entities CASCADE; DROP TABLE IF EXISTS arrivy_tasks CASCADE; COMMIT;"
```

### Issue: Webhook Not Receiving Events
```bash
# Verify endpoint
curl http://localhost:3000/api/webhooks/arrivy

# Check webhook secret matches
grep ARRIVY_WEBHOOK_SECRET .env.local

# For local testing, use ngrok
ngrok http 3000
# Update Arrivy webhook URL to ngrok HTTPS URL
```

### Issue: Dashboard Shows No Data
```bash
# Check database
psql $DATABASE_URL -c "SELECT * FROM arrivy_tasks;"

# Check API
curl http://localhost:3000/api/operations/field-tracking/dashboard \
  -H "Cookie: next-auth.session-token=YOUR_TOKEN"

# Check logs
npm run dev  # Look for errors
```

---

## 📚 Reference Documentation

| Document | Purpose |
|----------|---------|
| **[ARRIVY_EXTERNAL_CONFIGURATION.md](ARRIVY_EXTERNAL_CONFIGURATION.md)** | Webhook and entity setup in Arrivy dashboard |
| **[ARRIVY_TESTING_CHECKLIST.md](ARRIVY_TESTING_CHECKLIST.md)** | Complete test suite (local + production) |
| **[ARRIVY_PRODUCTION_DEPLOYMENT.md](ARRIVY_PRODUCTION_DEPLOYMENT.md)** | Detailed production deployment steps |
| **[ARRIVY_DEPLOYMENT_GUIDE.md](ARRIVY_DEPLOYMENT_GUIDE.md)** | Comprehensive guide with troubleshooting |
| **[ARRIVY_QUICK_START.md](ARRIVY_QUICK_START.md)** | Quick reference and commands |

---

## 🎯 Success Criteria

### Technical Success
- ✅ All database tables created
- ✅ All indexes created
- ✅ Webhook endpoint responding
- ✅ Dashboard loading correctly
- ✅ API endpoints functional
- ✅ Real-time updates working
- ✅ No errors in logs for 24 hours

### User Success
- ✅ Operations team can access dashboard
- ✅ Tasks sync from QuickBase to Arrivy
- ✅ Crew can update status via mobile
- ✅ Customers can track via URL
- ✅ Status updates reflect in dashboard <30s
- ✅ Team trained on new workflow

---

## 📞 Support

### During Deployment
- Check application logs: `npm run dev` or `vercel logs`
- Database issues: Verify connection string
- Webhook issues: Use ngrok for local testing
- API errors: Check browser console

### Post-Deployment
- Monitor webhook delivery rate (should be >99%)
- Check error logs daily for first week
- Gather team feedback
- Adjust polling intervals if needed

---

## 🎉 Ready to Deploy!

**Estimated Total Time:** 50 minutes

1. **Database:** 5 minutes
2. **Configuration:** 10 minutes
3. **Local Testing:** 10 minutes
4. **Production:** 15 minutes
5. **Validation:** 10 minutes

Start with **Phase 1** above and work through each phase sequentially.

Good luck! 🚀

