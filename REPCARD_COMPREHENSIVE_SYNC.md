# RepCard Comprehensive Sync System

## Overview

This system pulls **ALL available data** from RepCard and stores it in the database for fast analytics and reporting. Previously, we were only syncing customers, appointments, and status logs. Now we sync everything:

### Data Types Synced

1. **Users/Reps** (`repcard_users`)
   - User profiles, roles, offices, teams
   - Profile images, ratings, bios
   - First door knock and appointment dates

2. **Offices** (`repcard_offices`)
   - Office names, addresses, locations
   - Company associations

3. **Customers** (`repcard_customers`) ✅ Already syncing
   - Leads/door knocks
   - Contact information
   - Status tracking

4. **Appointments** (`repcard_appointments`) ✅ Already syncing
   - Scheduled appointments
   - Setter and closer attribution
   - Disposition tracking

5. **Status Logs** (`repcard_status_logs`) ✅ Already syncing
   - Customer status changes
   - Historical tracking

6. **Customer Attachments** (`repcard_customer_attachments`) 🆕 NEW
   - Power bills
   - Documents
   - Images
   - Linked to customers

7. **Appointment Attachments** (`repcard_appointment_attachments`) 🆕 NEW
   - Documents attached to appointments
   - Linked to appointments and customers

---

## Setup

### 1. Run Database Migration

First, create the new tables:

```bash
# Run the migration
psql $DATABASE_URL -f lib/db/migrations/014_repcard_comprehensive_tables.sql

# Or if using a migration runner
npm run migrate
```

### 2. Set Environment Variables

Ensure `REPCARD_API_KEY` is set:

```bash
# Local development
export REPCARD_API_KEY="your-api-key-here"

# Production (Vercel)
vercel env add REPCARD_API_KEY production
```

---

## Usage

### Option 1: Command Line Script (Recommended)

```bash
# Full sync of everything
npx tsx scripts/run-comprehensive-repcard-sync.ts

# Incremental sync (only new/updated records)
npx tsx scripts/run-comprehensive-repcard-sync.ts --incremental

# Sync specific date range
npx tsx scripts/run-comprehensive-repcard-sync.ts --start-date 2025-01-01 --end-date 2025-12-31

# Skip attachments (faster sync)
npx tsx scripts/run-comprehensive-repcard-sync.ts --skip-attachments

# Sync only specific entities
npx tsx scripts/run-comprehensive-repcard-sync.ts --skip-status-logs --skip-attachments

# See all options
npx tsx scripts/run-comprehensive-repcard-sync.ts --help
```

### Option 2: API Endpoint

```bash
# Full sync
curl -X POST "https://your-domain.com/api/admin/repcard/comprehensive-sync" \
  -H "Cookie: your-session-cookie"

# Incremental sync
curl -X POST "https://your-domain.com/api/admin/repcard/comprehensive-sync?incremental=true" \
  -H "Cookie: your-session-cookie"

# Date range
curl -X POST "https://your-domain.com/api/admin/repcard/comprehensive-sync?startDate=2025-01-01&endDate=2025-12-31" \
  -H "Cookie: your-session-cookie"

# Skip attachments
curl -X POST "https://your-domain.com/api/admin/repcard/comprehensive-sync?skipCustomerAttachments=true&skipAppointmentAttachments=true" \
  -H "Cookie: your-session-cookie"

# Check sync status
curl -X GET "https://your-domain.com/api/admin/repcard/comprehensive-sync" \
  -H "Cookie: your-session-cookie"
```

**Note:** API endpoint requires `super_admin` role.

---

## Sync Order

The sync runs in a specific order to maintain data integrity:

1. **Users** - Needed for attribution
2. **Offices** - Needed for office data
3. **Customers** - Required for foreign keys
4. **Appointments** - Depends on customers
5. **Status Logs** - Depends on customers
6. **Customer Attachments** - Depends on customers
7. **Appointment Attachments** - Depends on appointments

---

## Database Schema

### New Tables

#### `repcard_users`
Stores RepCard user/rep data for enrichment and reference.

```sql
SELECT * FROM repcard_users LIMIT 10;
```

Key fields:
- `repcard_user_id` - RepCard user ID (links to `users.repcard_user_id`)
- `email` - User email (for matching)
- `office_id`, `office_name` - Office information
- `status` - 1 = active, 0 = inactive
- `raw_data` - Full RepCard API response (JSONB)

#### `repcard_offices`
Stores RepCard office data.

```sql
SELECT * FROM repcard_offices;
```

#### `repcard_customer_attachments`
Stores attachments linked to customers (power bills, documents).

```sql
SELECT 
  c.name as customer_name,
  ca.file_name,
  ca.attachment_type,
  ca.file_url,
  ca.created_at
FROM repcard_customer_attachments ca
JOIN repcard_customers c ON c.id = ca.customer_id
ORDER BY ca.created_at DESC
LIMIT 10;
```

#### `repcard_appointment_attachments`
Stores attachments linked to appointments.

```sql
SELECT 
  c.name as customer_name,
  aa.file_name,
  aa.attachment_type,
  aa.file_url
FROM repcard_appointment_attachments aa
JOIN repcard_appointments a ON a.id = aa.appointment_id
JOIN repcard_customers c ON c.id = aa.customer_id
ORDER BY aa.created_at DESC
LIMIT 10;
```

---

## Querying Synced Data

### Get All RepCard Users

```sql
SELECT 
  repcard_user_id,
  first_name || ' ' || last_name as name,
  email,
  office_name,
  role,
  status
FROM repcard_users
WHERE status = 1
ORDER BY last_name;
```

### Get Customers with Attachments

```sql
SELECT 
  c.name,
  c.email,
  COUNT(ca.id) as attachment_count,
  MAX(ca.created_at) as latest_attachment
FROM repcard_customers c
LEFT JOIN repcard_customer_attachments ca ON ca.customer_id = c.id
GROUP BY c.id, c.name, c.email
HAVING COUNT(ca.id) > 0
ORDER BY attachment_count DESC;
```

### Get Appointments with Attachments

```sql
SELECT 
  c.name as customer_name,
  a.scheduled_at,
  a.disposition,
  COUNT(aa.id) as attachment_count
FROM repcard_appointments a
JOIN repcard_customers c ON c.id = a.customer_id
LEFT JOIN repcard_appointment_attachments aa ON aa.appointment_id = a.id
GROUP BY a.id, c.name, a.scheduled_at, a.disposition
HAVING COUNT(aa.id) > 0
ORDER BY a.scheduled_at DESC;
```

### Get RepCard Office Mappings

```sql
SELECT 
  ro.repcard_office_id,
  ro.name as repcard_office_name,
  o.id as dashboard_office_id,
  o.name as dashboard_office_name,
  o.quickbase_office_id
FROM repcard_offices ro
LEFT JOIN offices o ON o.name = ro.name;
```

---

## Sync Logs

All syncs are logged in `repcard_sync_log`:

```sql
SELECT 
  entity_type,
  sync_type,
  started_at,
  completed_at,
  status,
  records_fetched,
  records_inserted,
  records_updated,
  records_failed
FROM repcard_sync_log
WHERE entity_type IN ('users', 'offices', 'customer_attachments', 'appointment_attachments')
ORDER BY started_at DESC
LIMIT 20;
```

---

## Performance Considerations

### Full Sync
- **Initial sync**: Can take 10-30 minutes depending on data volume
- **Recommended**: Run during off-hours or as a one-time setup

### Incremental Sync
- **Daily incremental**: Usually < 5 minutes
- **Recommended**: Run daily via cron job

### Attachments
- **Large files**: Attachments can be large, sync may be slower
- **Skip option**: Use `--skip-attachments` for faster syncs if attachments aren't needed immediately

---

## Scheduling

### Daily Incremental Sync

Add to Vercel Cron or your scheduler:

```json
{
  "crons": [
    {
      "path": "/api/cron/repcard-comprehensive-sync",
      "schedule": "0 2 * * *"
    }
  ]
}
```

Create `/app/api/cron/repcard-comprehensive-sync/route.ts`:

```typescript
import { NextRequest, NextResponse } from 'next/server';
import { runComprehensiveSync } from '@/lib/repcard/comprehensive-sync';

export async function GET(request: NextRequest) {
  // Verify cron secret
  const authHeader = request.headers.get('authorization');
  if (authHeader !== `Bearer ${process.env.CRON_SECRET}`) {
    return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
  }

  try {
    const results = await runComprehensiveSync({ incremental: true });
    return NextResponse.json({ success: true, results });
  } catch (error) {
    return NextResponse.json(
      { error: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    );
  }
}
```

---

## Error Handling

The sync handles errors gracefully:
- **Per-entity errors**: One entity failing doesn't stop others
- **Pagination errors**: Failed pages are logged but sync continues
- **Missing data**: Foreign key lookups handle missing relationships

Check sync logs for detailed error information:

```sql
SELECT 
  entity_type,
  status,
  error_message,
  started_at,
  completed_at
FROM repcard_sync_log
WHERE status = 'failed'
ORDER BY started_at DESC;
```

---

## Next Steps

Now that all RepCard data is synced, you can:

1. **Build analytics dashboards** using synced data (no API calls needed)
2. **Create reports** on attachments, user performance, office metrics
3. **Build UI components** that query local database instead of API
4. **Set up alerts** based on sync status
5. **Track trends** over time using historical data

---

## Troubleshooting

### Sync Failing
1. Check `REPCARD_API_KEY` is set correctly
2. Verify API key has access to all endpoints
3. Check sync logs for specific error messages
4. Test API connectivity: `npx tsx scripts/diagnose-repcard.ts`

### Missing Data
1. Check if date range filters are excluding data
2. Verify foreign key relationships (customers before appointments)
3. Check sync logs for failed records

### Slow Sync
1. Use incremental sync for daily updates
2. Skip attachments if not needed immediately
3. Check rate limits (RepCard API: 100 requests/period)

---

## Files Created

- `lib/db/migrations/014_repcard_comprehensive_tables.sql` - Database schema
- `lib/repcard/comprehensive-sync.ts` - Sync service
- `app/api/admin/repcard/comprehensive-sync/route.ts` - API endpoint
- `scripts/run-comprehensive-repcard-sync.ts` - CLI script

---

## Summary

You now have a comprehensive RepCard sync system that pulls **everything** from RepCard:
- ✅ Users/Reps
- ✅ Offices  
- ✅ Customers
- ✅ Appointments
- ✅ Status Logs
- ✅ Customer Attachments
- ✅ Appointment Attachments

All data is stored locally for fast queries and analytics. Decide how to display it based on your needs!

