# Run Migrations in Production

## Quick Options

### Option 1: Via Admin Dashboard (Easiest)
1. Log in to production as `super_admin`
2. Navigate to: `/admin/repcard-sync` or similar admin page
3. Look for "Run Migrations" button
4. Click to run migrations 034 and 035

### Option 2: Via API Endpoint (cURL)
```bash
# Replace with your production URL
PROD_URL="https://your-production-url.vercel.app"

# Run migrations 034 and 035
curl -X POST "$PROD_URL/api/admin/run-migrations-034-035" \
  -H "Cookie: your-session-cookie" \
  -H "Content-Type: application/json"
```

### Option 3: Via Browser Console (If logged in)
1. Open production site
2. Open browser console (F12)
3. Run:
```javascript
fetch('/api/admin/run-migrations-034-035', {
  method: 'POST',
  credentials: 'include'
})
.then(r => r.json())
.then(console.log)
```

### Option 4: Check Status First
```bash
# Check if migrations are already applied
curl "$PROD_URL/api/admin/run-migrations-034-035"
```

## What the Migrations Do

**Migration 034**: Fixes metric audit foreign key constraint
- Adds `repcard_appointment_id` column
- Updates trigger functions
- Fixes FK constraint violations

**Migration 035**: Adds useful webhook fields
- Adds 7 fields to `repcard_appointments` (appointment_link, remind_at, contact_source, etc.)
- Adds 3 fields to `repcard_customers` (contact_source, latitude, longitude)
- Creates indexes for performance

## Verification

After running, verify with:
```bash
GET $PROD_URL/api/admin/run-migrations-034-035
```

Should return:
```json
{
  "status": {
    "migration034": true,
    "migration035": true
  }
}
```
