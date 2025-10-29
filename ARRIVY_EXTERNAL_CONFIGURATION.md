# Arrivy External Configuration Guide

This guide covers configuration steps that must be performed in the Arrivy web dashboard.

## Part 1: Configure Webhook Endpoint

### Step 1: Access Arrivy Dashboard
- Navigate to: https://app.arrivy.com/
- Log in with your Arrivy account credentials

### Step 2: Navigate to Webhook Settings
- Click **Settings** (gear icon) in the left sidebar
- Select **Integrations** from the settings menu
- Click **Webhooks** tab
- Click **Add New Webhook** or **Create Webhook** button

### Step 3: Configure Webhook Details

**Basic Settings:**
- **Name:** `KIN Home Operations Dashboard`
- **Status:** Active/Enabled
- **Description:** "Real-time field operations tracking for KIN Home dashboard"

**Endpoint Configuration:**
- **URL (for local testing):** `http://localhost:3000/api/webhooks/arrivy`
  - Note: Use ngrok for local testing if Arrivy can't reach localhost
- **URL (for production):** `https://your-production-domain.vercel.app/api/webhooks/arrivy`
  - Must use HTTPS (Arrivy requires secure webhooks)
- **Method:** `POST`
- **Content-Type:** `application/json`

**Authentication:**
- **Secret/Signing Key:** Paste from `.env.local` → `ARRIVY_WEBHOOK_SECRET`
  - Value: `<your_webhook_secret_from_env_local>`
  - ⚠️ **SECURITY:** Never commit this secret to version control
  - Must match exactly with your environment variable
  - Generate with: `openssl rand -base64 32`
  - **Note:** Webhook endpoint accepts signatures in both hex and base64 formats
  - Signature header: `x-arrivy-signature` (optional `sha256=` prefix supported)

**Event Selection:**
Select ALL of the following event types:
- ☑ TASK_CREATED
- ☑ TASK_STATUS
- ☑ CREW_ASSIGNED
- ☑ ARRIVING
- ☑ LATE
- ☑ NOSHOW
- ☑ TASK_RATING
- ☑ EXCEPTION

### Step 4: Test Webhook
- Click **Test** or **Send Test Event** button
- Check your application logs for the test event
- Expected log: `[Arrivy Webhook] Received event`

### Step 5: Verify Configuration
```bash
curl https://your-domain.com/api/webhooks/arrivy
# Expected: {"status":"ok","service":"arrivy-webhook"}
```

---

## Part 2: Create Field Crew Entities

### Why This Is Needed
Arrivy tasks require assigned entities (crew members). You must create these before assigning tasks.

### Option A: Via Arrivy Dashboard (Recommended)

1. **Navigate to Team Management:**
   - Click **Team** in the left sidebar
   - Select **Entities** or **Team Members**

2. **Add New Entity:**
   - Click **Add Entity** or **Create New** button

3. **Fill in Entity Details:**
   
   **Required Fields:**
   - **Name:** Full name (e.g., "John Smith")
   - **Email:** Work email (e.g., "john.smith@kinhome.com")
   - **Phone:** Mobile with country code (e.g., "+1-555-123-4567")
   - **Type:** Select "CREW" or "TECHNICIAN"
   
   **Optional Fields:**
   - **Image:** Profile photo
   - **Color:** For calendar/map visualization
   - **Skills:** Add relevant skills
   - **Permissions:** Set access levels

4. **Save Entity**

5. **Repeat for All Crew Members**

### Example Entities:

```
Entity 1:
- Name: John Smith
- Email: john.smith@kinhome.com
- Phone: +1-555-123-4567
- Type: CREW
- Skills: Solar Installation, Site Survey

Entity 2:
- Name: Sarah Johnson
- Email: sarah.johnson@kinhome.com
- Phone: +1-555-234-5678
- Type: CREW
- Skills: Solar Installation, Electrical Work
```

### Option B: Via API (For Bulk Creation)

Use the `ArrivyClient.createEntity()` method in `lib/integrations/arrivy/client.ts` or the `syncEntityFromQuickBase()` function in `lib/integrations/arrivy/service.ts`.

### Verification

**Via Arrivy Dashboard:**
- Go to Team → Entities
- Verify all crew members are listed

**Via Database:**
```bash
psql $DATABASE_URL -c "SELECT * FROM arrivy_entities ORDER BY created_at DESC;"
```

**Via API:**
```bash
curl http://localhost:3000/api/operations/field-tracking/entities
```

---

## Troubleshooting

**Webhook Not Receiving Events:**
- Verify URL is accessible from internet
- Check webhook secret matches in both places
- Ensure HTTPS is used in production
- Review Arrivy webhook delivery logs

**Signature Verification Failing:**
- Confirm webhook secret is exactly the same
- Check for extra spaces or newlines
- Verify secret is base64-encoded string

**For Local Testing with ngrok:**
```bash
ngrok http 3000
# Use the HTTPS URL as webhook URL
```

---

## Security Considerations

- Webhook secret provides authentication - keep it secure
- Signature verification prevents unauthorized webhook calls
- HTTPS ensures encrypted transmission
- Webhook endpoint has rate limiting and error handling built-in
- Idempotency checks prevent duplicate event processing

