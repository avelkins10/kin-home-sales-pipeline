# RepCard Webhook Setup Guide

## Overview

Webhooks provide **near-instant updates** (seconds instead of 5 minutes) when appointments or customers are created/updated in RepCard. This is much better than polling!

**Benefits:**
- ✅ **Real-time updates**: Appointments appear in your app within seconds
- ✅ **More efficient**: Only syncs when data actually changes
- ✅ **Lower API usage**: No constant polling
- ✅ **Better user experience**: Instant feedback

---

## Webhook Endpoint

**Production URL:** `https://kineticsales.app/api/webhooks/repcard`
**Alternative:** `https://kin-home-sales-pipeline.vercel.app/api/webhooks/repcard`

**What it does:**
- Receives real-time events from RepCard when appointments/customers are created/updated
- Triggers immediate sync of the affected data
- Auto-links appointments to customers
- Updates metrics automatically via database triggers

---

## Setup in RepCard

### Step 1: Access RepCard Settings

1. Log into RepCard dashboard
2. Navigate to **Settings** → **Integrations** or **Developer** → **Webhooks**
3. Click **Add Webhook** or **Create Webhook**

### Step 2: Configure Webhook

**Basic Settings:**
- **Name:** `KIN Home Dashboard - Real-time Sync`
- **Status:** Active/Enabled
- **Description:** Real-time sync for appointments and customers

**Endpoint Configuration:**
- **URL:** `https://kineticsales.app/api/webhooks/repcard` (production)
  - Or: `https://kin-home-sales-pipeline.vercel.app/api/webhooks/repcard`
  - Must use HTTPS
- **Method:** `POST`
- **Content-Type:** `application/json`

**Authentication:**
- **API Key Header:** `x-api-key` or `Authorization`
- **Value:** Your `REPCARD_API_KEY` (same as used for API calls)
- This verifies webhooks are from RepCard

**Events to Subscribe To:**

**CRITICAL (Must Have):**
- ☑ **Appointment Set** - When a setter creates an appointment (MOST IMPORTANT - this is what you asked about!)
- ☑ **New Contact** - When a new lead/customer is created (needed so appointments can link to customers)
- ☑ **Door knocked** - When a setter knocks a door (creates a lead) - same as New Contact, but RepCard may send both

**IMPORTANT (Recommended):**
- ☑ **Appointment Update** - When appointment details change (time, location, etc.)
- ☑ **Appointment Outcome** - When appointment disposition changes (closed, no-show, cancelled, etc.)
- ☑ **Update Contact** - When customer info changes (ensures data stays current)

**OPTIONAL (Nice to Have):**
- ☐ **Status Changed** - When customer/appointment status changes (useful but not critical)
- ☐ **Contact Type Changed** - When customer type changes (rare, can skip)
- ☐ **Contact Removed** - When a contact is deleted (rare, can skip)

**SKIP (Not Needed):**
- ☐ **New User** / **Update User** / **Remove User** - User sync happens via cron, not critical for real-time
- ☐ **Closer Update** - Less critical, handled by cron

### Step 3: Test Webhook

1. Click **Test Webhook** or **Send Test Event** in RepCard
2. Check Vercel logs:
   ```bash
   vercel logs --follow
   ```
3. Look for: `[RepCard Webhook] Received event`

---

## Environment Variables

Ensure these are set in Vercel:

```bash
# RepCard API Credentials (same as used for sync)
REPCARD_API_KEY=your_api_key_here
```

**Note:** The webhook uses the same API key for authentication. If RepCard supports webhook-specific secrets, we can add that later.

---

## Webhook Flow

```
Setter creates appointment in RepCard
        ↓
RepCard sends HTTP POST to /api/webhooks/repcard
        ↓
Verify API key authentication
        ↓
Parse webhook payload
        ↓
Determine event type (appointment/customer)
        ↓
Trigger incremental sync for that entity
        ↓
Auto-link appointments to customers
        ↓
Database triggers recalculate metrics automatically
        ↓
Return 200 OK to RepCard
        ↓
Frontend polls every 30 seconds → sees new data!
```

**Total latency: ~30-60 seconds** (just frontend polling delay)

---

## Webhook Payload Format

RepCard may send webhooks in different formats. The endpoint handles:

**RepCard Webhook Payload Format:**
RepCard sends webhooks with a `trigger_event` field. Example:

```json
{
  "trigger_event": "Appointment Set",
  "appointment_id": 12345,
  "contact_id": 67890,
  "user_id": 111,
  "timestamp": "2026-01-22T15:30:00Z",
  "data": { ... }
}
```

**Supported Trigger Events:**
- `Appointment Set` - New appointment created
- `Appointment Update` - Appointment modified
- `Appointment Outcome` - Appointment disposition changed
- `New Contact` - New customer/lead created
- `Update Contact` - Customer info updated
- `Door knocked` - Setter knocked a door
- `Status Changed` - Customer/appointment status changed
- `Contact Type Changed` - Customer type changed

The endpoint automatically maps these events to the correct sync operations.

---

## Testing

### Test 1: Health Check
```bash
curl https://kineticsales.app/api/webhooks/repcard

# Expected:
{
  "status": "ok",
  "service": "repcard-webhook",
  "timestamp": "2026-01-22T..."
}
```

### Test 2: Simulate Webhook
```bash
curl -X POST https://kineticsales.app/api/webhooks/repcard \
  -H "Content-Type: application/json" \
  -H "x-api-key: YOUR_REPCARD_API_KEY" \
  -d '{
    "trigger_event": "Appointment Set",
    "appointment_id": 12345,
    "contact_id": 67890,
    "user_id": 111,
    "timestamp": "2026-01-22T15:30:00Z"
  }'
```

### Test 3: Real Webhook
1. Create a test appointment in RepCard
2. Check Vercel logs for webhook receipt
3. Verify appointment appears in database within 30-60 seconds

---

## Troubleshooting

### Webhooks Not Arriving

1. **Check RepCard webhook configuration:**
   - URL is correct and uses HTTPS
   - Webhook is enabled/active
   - Events are selected
   - API key is correct

2. **Check Vercel logs:**
   ```bash
   vercel logs --follow
   ```
   Look for `[RepCard Webhook]` messages

3. **Verify endpoint is accessible:**
   ```bash
   curl https://kineticsales.app/api/webhooks/repcard
   ```

### Webhooks Arriving But Not Processing

1. **Check authentication:**
   - Verify `REPCARD_API_KEY` matches what's in RepCard
   - Check logs for "Invalid API key" errors

2. **Check payload format:**
   - Look at webhook payload in logs
   - Verify it has `object_type` or `resource` field

3. **Check sync results:**
   - Look for sync completion messages in logs
   - Verify appointments are being linked

---

## Fallback: Cron Sync Still Runs

**Important:** The 5-minute cron sync still runs as a **safety net**:
- Catches any missed webhooks
- Handles bulk updates
- Ensures data consistency

Webhooks provide **real-time updates**, but cron ensures **nothing is missed**.

---

## Comparison: Webhooks vs Polling

| Feature | Webhooks | Polling (Current) |
|---------|----------|-------------------|
| **Latency** | ~30-60 seconds | 2.5-5.5 minutes |
| **API Calls** | Only on changes | Every 5 minutes |
| **Efficiency** | High | Medium |
| **Reliability** | Requires webhook setup | Always works |
| **Cost** | Lower | Higher |

**Recommendation:** Use webhooks for real-time updates, keep cron as backup.

---

## Next Steps

1. ✅ Set up webhook in RepCard dashboard
2. ✅ Test webhook with a test appointment
3. ✅ Monitor logs to verify it's working
4. ✅ Optional: Reduce frontend polling to 60 seconds (since webhooks handle real-time)

Once webhooks are working, appointments will appear in your app within **30-60 seconds** instead of 2.5-5.5 minutes!
