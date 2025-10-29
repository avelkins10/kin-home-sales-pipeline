# Arrivy Integration - Production Deployment Steps

Execute these steps after local testing is complete.

## Prerequisites
- ✅ All local tests passed
- ✅ Vercel account with project access
- ✅ Vercel CLI installed (`npm i -g vercel`)
- ✅ Production database accessible

---

## Step 1: Set Environment Variables in Vercel (5 min)

```bash
cd /Users/austinelkins/Rep_Dashboard

# Link to Vercel project
vercel link

# Add environment variables
vercel env add ARRIVY_AUTH_KEY production
# Enter: <your_arrivy_auth_key_from_dashboard>

vercel env add ARRIVY_AUTH_TOKEN production
# Enter: <your_arrivy_auth_token_from_dashboard>

vercel env add ARRIVY_COMPANY_NAME production
# Enter: <your_company_name>

vercel env add ARRIVY_WEBHOOK_SECRET production
# Enter: <your_generated_webhook_secret>
# Generate with: openssl rand -base64 32

vercel env add ARRIVY_BASE_URL production
# Enter: https://app.arrivy.com/api

vercel env add ARRIVY_RATE_LIMIT production
# Enter: 30
```

**Verify:**
```bash
vercel env ls
# Expected: All 6 ARRIVY_ variables listed
```

---

## Step 2: Run Production Database Migration (3 min)

```bash
# Pull production environment
vercel env pull .env.production.local

# Extract database URL
export PROD_DB=$(grep DATABASE_URL .env.production.local | cut -d '=' -f2- | tr -d '"')

# Test connection
psql "$PROD_DB" -c "SELECT version();"

# Run migration
psql "$PROD_DB" -f lib/db/migrations/014_create_arrivy_tables.sql
```

**Verify:**
```bash
psql "$PROD_DB" -c "SELECT table_name FROM information_schema.tables WHERE table_name LIKE 'arrivy_%';"
# Expected: 4 tables
```

---

## Step 3: Deploy to Production (5 min)

```bash
# Ensure changes are committed
git status

# Deploy
vercel --prod

# Wait for deployment to complete
# Note the production URL
```

**Verify:**
```bash
curl https://your-domain.vercel.app/api/webhooks/arrivy
# Expected: {"status":"ok"}
```

---

## Step 4: Update Arrivy Webhook URL (3 min)

1. Log into https://app.arrivy.com/
2. Go to Settings → Integrations → Webhooks
3. Edit your webhook
4. Update URL to: `https://your-domain.vercel.app/api/webhooks/arrivy`
5. Save
6. Click **Test** to verify

---

## Step 5: Production Testing (5 min)

### Test 1: Access Dashboard
```bash
open https://your-domain.vercel.app/operations/scheduling
```

### Test 2: Create Test Task
- Create task in Arrivy with External ID: PROD-TEST-001
- Verify webhook delivered

### Test 3: Verify Database
```bash
psql "$PROD_DB" -c "SELECT customer_name FROM arrivy_tasks ORDER BY created_at DESC LIMIT 5;"
```

### Test 4: Test Status Update
- Update task status in Arrivy
- Verify dashboard updates

### Test 5: Monitor Logs
```bash
vercel logs --filter "Arrivy" --follow
```

---

## Step 6: Clean Up Test Data (Optional)

```bash
psql "$PROD_DB" -c "DELETE FROM arrivy_tasks WHERE quickbase_project_id LIKE 'TEST-%' OR quickbase_project_id LIKE 'PROD-TEST-%';"
```

---

## Deployment Checklist

- [ ] Environment variables set in Vercel
- [ ] Production migration executed
- [ ] Application deployed
- [ ] Webhook URL updated
- [ ] Production dashboard accessible
- [ ] Test task created and visible
- [ ] Webhooks processing correctly
- [ ] Status updates working
- [ ] No errors in logs
- [ ] Operations team trained

---

## Rollback Plan

If deployment fails:

```bash
# Revert deployment
vercel rollback

# Disable webhook in Arrivy dashboard

# Rollback database (if needed)
psql "$PROD_DB" -c "BEGIN; DROP TABLE IF EXISTS arrivy_task_status CASCADE; DROP TABLE IF EXISTS arrivy_events CASCADE; DROP TABLE IF EXISTS arrivy_entities CASCADE; DROP TABLE IF EXISTS arrivy_tasks CASCADE; COMMIT;"
```

---

## Post-Deployment

### Monitor (Week 1)
- Check webhook delivery rate daily
- Review logs for errors
- Gather team feedback

### Ongoing Maintenance
- Archive old events monthly
- Update crew entities as needed
- Monitor API rate limit usage

---

## Success Metrics

1. **Webhook Delivery:** >99% success rate
2. **Dashboard Load Time:** <2 seconds
3. **Team Adoption:** Daily active users
4. **Customer Satisfaction:** Tracker URL feedback

---

## Congratulations! 🎉

Your Arrivy integration is now live in production!

For support, refer to:
- `ARRIVY_DEPLOYMENT_GUIDE.md` - Full guide
- `ARRIVY_QUICK_START.md` - Quick reference
- Vercel logs - Real-time monitoring

