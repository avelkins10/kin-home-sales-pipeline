# RepCard Webhook Flow - Complete Data Journey

## 🔄 Complete Flow: From RepCard to Your Screen

```
┌─────────────────────────────────────────────────────────────────┐
│ 1. SETTER CREATES APPOINTMENT IN REPCARD                        │
│    (RepCard web interface)                                      │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 2. REPCARD SENDS WEBHOOK                                        │
│    POST https://kineticsales.app/api/webhooks/repcard           │
│    Payload: { trigger_event: "Appointment Set", ... }          │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 3. WEBHOOK ENDPOINT PROCESSES                                    │
│    /api/webhooks/repcard/route.ts                               │
│    - Validates payload                                          │
│    - Calls syncAppointments({ incremental: true })              │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 4. SYNC SERVICE FETCHES FROM REPCARD API                        │
│    lib/repcard/sync-service.ts                                  │
│    - Calls RepCard API to get latest appointments               │
│    - Fetches appointment data                                    │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 5. DATA STORED IN DATABASE                                      │
│    PostgreSQL Tables:                                           │
│    - repcard_appointments (appointment data)                    │
│    - repcard_customers (customer data)                          │
│    - repcard_customer_attachments (power bills)                  │
│    - repcard_appointment_attachments (attachments)               │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 6. DATABASE TRIGGERS AUTO-CALCULATE METRICS                      │
│    Migration 032: Event-Driven Metrics                           │
│    - calculate_is_within_48_hours()                             │
│    - calculate_has_power_bill()                                 │
│    - Updates is_within_48_hours and has_power_bill columns      │
│    - Logs to repcard_metric_audit table                         │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 7. AUTO-LINK APPOINTMENTS TO CUSTOMERS                          │
│    Webhook also runs:                                           │
│    UPDATE repcard_appointments SET customer_id = ...            │
│    (Links appointments to customers automatically)               │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 8. FRONTEND POLLS FOR DATA                                      │
│    components/analytics/RepCardOptimizedDashboard.tsx            │
│    - Calls /api/repcard/unified-dashboard                       │
│    - Polls every 30 seconds (refetchInterval: 30000)            │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 9. API READS FROM DATABASE                                      │
│    /api/repcard/unified-dashboard/route.ts                      │
│    - Queries repcard_appointments table                         │
│    - Queries repcard_customers table                            │
│    - Calculates metrics (48h speed, power bill rate, etc.)       │
│    - Returns JSON response                                      │
└────────────────────────────┬────────────────────────────────────┘
                             │
                             ▼
┌─────────────────────────────────────────────────────────────────┐
│ 10. FRONTEND DISPLAYS DATA                                      │
│     RepCardOptimizedDashboard component                         │
│     - Shows appointments in leaderboards                         │
│     - Displays quality metrics (48h speed, power bill rate)      │
│     - Updates automatically every 30 seconds                    │
└─────────────────────────────────────────────────────────────────┘
```

---

## 📊 Where Data is Stored

### Database Tables:
1. **`repcard_appointments`** - All appointment data
   - `repcard_appointment_id` - RepCard's ID
   - `repcard_customer_id` - Links to customer
   - `customer_id` - Internal UUID link
   - `scheduled_at` - When appointment is scheduled
   - `is_within_48_hours` - Calculated by trigger
   - `has_power_bill` - Calculated by trigger
   - `setter_user_id` - Who set the appointment
   - `closer_user_id` - Who runs the appointment

2. **`repcard_customers`** - All customer/lead data
   - `repcard_customer_id` - RepCard's ID
   - `created_at` - When customer was created (for 48h calculation)
   - `setter_user_id` - Who created the lead

3. **`repcard_customer_attachments`** - Power bills on customers
4. **`repcard_appointment_attachments`** - Attachments on appointments
5. **`repcard_metric_audit`** - Logs all metric calculations

---

## 🔍 How to Verify It's Working

### 1. Check Webhook Logs
```bash
vercel logs --follow
```
Look for:
- `[RepCard Webhook] Received event`
- `[RepCard Webhook] Processing appointment webhook`
- `[RepCard Webhook] Appointment sync completed`

### 2. Check Database
```sql
-- See recent appointments
SELECT 
  repcard_appointment_id,
  scheduled_at,
  is_within_48_hours,
  has_power_bill,
  updated_at
FROM repcard_appointments
ORDER BY updated_at DESC
LIMIT 10;

-- See webhook-triggered syncs
SELECT 
  entity_type,
  status,
  records_fetched,
  records_updated,
  started_at
FROM repcard_sync_log
WHERE entity_type IN ('appointments', 'customers')
ORDER BY started_at DESC
LIMIT 10;
```

### 3. Check Frontend
- Go to Analytics → RepCard dashboard
- Create a test appointment in RepCard
- Wait 30-60 seconds
- Check if it appears in the dashboard

### 4. Check Metrics
- Go to Settings → RepCard → Overview tab
- Look at "Quality Metrics (Last 30 Days)"
- Should show updated percentages

---

## ✅ Everything is Already Configured!

**Yes, everything is already set up:**

1. ✅ **Webhook endpoint** - `/api/webhooks/repcard` (receives events)
2. ✅ **Sync service** - `lib/repcard/sync-service.ts` (fetches from RepCard API)
3. ✅ **Database storage** - Tables exist and are being used
4. ✅ **Auto-linking** - Webhook automatically links appointments to customers
5. ✅ **Metric calculation** - Database triggers auto-calculate metrics
6. ✅ **Frontend API** - `/api/repcard/unified-dashboard` (reads from database)
7. ✅ **Frontend display** - `RepCardOptimizedDashboard` component (displays data)
8. ✅ **Auto-refresh** - Frontend polls every 30 seconds

**The complete pipeline is ready!** When a webhook fires:
- Data flows: RepCard → Webhook → Sync → Database → API → Frontend
- Everything updates automatically
- No manual steps needed

---

## 🧪 Test It

1. **Create a test appointment in RepCard**
2. **Check Vercel logs** (should see webhook received)
3. **Wait 30-60 seconds**
4. **Check your dashboard** (appointment should appear)

The webhook test showing "200 Ok" means the endpoint is working. Once you save the webhook, it will start processing real events automatically!
