# Arrivy Task Sync Guide

This guide explains how to sync existing tasks from Arrivy to your local database.

## Overview

The `sync-arrivy-tasks.ts` script fetches all tasks from Arrivy and populates your local PostgreSQL database. This is typically run once during initial setup, then periodically to catch any tasks created directly in Arrivy.

## Sync Methods

There are two ways tasks are synced from Arrivy to your database:

### 1. Real-Time Webhook Sync (Automatic)

When a task is created in Arrivy, a TASK_CREATED webhook is automatically sent to your application. The webhook handler:
- Receives the event notification
- Fetches full task details from Arrivy API
- Stores the task in your database
- Generates the customer tracker URL

**Advantages:**
- Instant synchronization (within seconds)
- No manual intervention required
- Keeps dashboard always up-to-date

**Limitations:**
- Only syncs tasks created AFTER webhook is configured
- Requires webhook endpoint to be accessible (HTTPS)
- Depends on Arrivy webhook delivery reliability

### 2. Manual Sync Script (Batch)

The `sync-arrivy-tasks.ts` script fetches tasks in bulk from Arrivy API:
- Queries tasks by date range
- Processes in batches to respect rate limits
- Stores or updates tasks in database

**Use Cases:**
- Initial setup (sync historical tasks)
- Backfill missed tasks (if webhook was down)
- Periodic reconciliation (verify data consistency)
- Recovery from webhook delivery failures

**When to Use:**
- **Initial Setup:** Run once to import all existing tasks
- **Daily:** Sync recent tasks (last 7 days) to catch any missed webhooks
- **Weekly:** Full sync to ensure data consistency
- **After Issues:** If webhook endpoint was unavailable

### Recommended Workflow

1. **Initial Setup:**
   ```bash
   # Sync all historical tasks
   npm run sync:arrivy
   ```

2. **Configure Webhook:**
   - Set up TASK_CREATED webhook in Arrivy dashboard
   - New tasks will sync automatically

3. **Periodic Reconciliation:**
   ```bash
   # Daily: Sync last 7 days (catches missed webhooks)
   npm run sync:arrivy -- --start-date=$(date -v-7d +%Y-%m-%d)
   ```

4. **Monitor:**
   - Check webhook delivery logs in Arrivy dashboard
   - Review application logs for processing errors
   - Compare task counts between Arrivy and database

**Note:** The sync script uses `upsertArrivyTask()` which safely handles tasks that already exist (from webhooks), so running both methods is safe and recommended for data consistency.

## Prerequisites

- ✅ Database migration `014_create_arrivy_tables.sql` executed
- ✅ Environment variables configured (ARRIVY_AUTH_KEY, ARRIVY_AUTH_TOKEN)
- ✅ Arrivy account with API access
- ✅ Network access to app.arrivy.com

## Quick Start

```bash
# Full sync of all tasks
npm run sync:arrivy

# Preview without making changes
npm run sync:arrivy:dry-run

# Sync with detailed logging
npm run sync:arrivy:verbose
```

## Command Options

### Basic Commands

| Command | Description |
|---------|-------------|
| `npm run sync:arrivy` | Full sync of all Arrivy tasks |
| `npm run sync:arrivy:dry-run` | Preview changes without updating database |
| `npm run sync:arrivy:verbose` | Detailed logging for debugging |
| `npm run sync:arrivy:entities` | Sync entities (crew) before tasks |
| `npm run sync:arrivy:recent` | Sync only last 30 days (incremental) |

### Advanced Options

```bash
# Custom date range
npm run sync:arrivy -- --start-date=2024-01-01 --end-date=2024-12-31

# Test with limited tasks
npm run sync:arrivy -- --limit=10 --verbose

# Sync entities and tasks together
npm run sync:arrivy -- --sync-entities --verbose

# Incremental sync (last 7 days)
npm run sync:arrivy -- --start-date=$(date -v-7d +%Y-%m-%d)
```

## How It Works

### 1. Entity Synchronization (Optional)

If `--sync-entities` flag is provided:
- Fetches all entities (crew members) from Arrivy
- Stores them in `arrivy_entities` table
- Required before syncing tasks if entities don't exist

### 2. Task Fetching

- Fetches tasks from Arrivy in date range batches (3-month chunks)
- Default range: Last 2 years from today
- Can be customized with `--start-date` and `--end-date`
- **Adaptive sub-chunking**: Automatically splits date ranges if pagination limits are detected (≥1000 tasks per query)
- Deduplicates tasks by `arrivy_task_id` to handle overlapping date boundaries

### 3. Data Mapping

Each Arrivy task is mapped to the database schema:

| Arrivy Field | Database Field | Notes |
|--------------|----------------|-------|
| `id` | `arrivy_task_id` | Primary identifier |
| `url_safe_id` | `url_safe_id` | For tracker URLs |
| `external_id` | `quickbase_project_id` | Or null if not linked to QuickBase |
| `customer_name` | `customer_name` | |
| `customer_phone` | `customer_phone` | |
| `customer_email` | `customer_email` | |
| `customer_address_line_1` + city + state | `customer_address` | Combined |
| `start_datetime` | `scheduled_start` | Parsed to timestamp |
| `end_datetime` | `scheduled_end` | Parsed to timestamp |
| `entity_ids` | `assigned_entity_ids` | Array of crew IDs |
| `status` | `current_status` | Task status |
| Generated | `tracker_url` | Customer-facing URL |
| `template_id` | `template_id` | Template reference |
| `extra_fields` | `extra_fields` | JSON metadata |

### 4. Upsert Logic

- Uses `upsertArrivyTask()` which performs INSERT or UPDATE
- Based on `arrivy_task_id` (unique constraint)
- Safe to run multiple times (idempotent)
- **Smart QuickBase ID handling**: Preserves existing QuickBase associations during updates
- Tasks originating in Arrivy have null QuickBase fields
- Tasks synced from QuickBase have `external_id` populated

### 5. Progress Tracking

- Shows progress every 10 tasks
- Displays batch progress ("Processing batch 2/8...")
- Estimates time remaining
- Provides detailed summary at completion

## QuickBase Field Handling

Tasks originating in Arrivy have `quickbase_project_id` and `quickbase_record_id` set to `null`. Tasks synced from QuickBase will have these fields populated with actual values. The sync script automatically detects and preserves existing QuickBase associations.

### Linking Tasks to QuickBase

Tasks can be linked to QuickBase projects later through the future QuickBase integration phase. For manual linking if needed:

```sql
UPDATE arrivy_tasks 
SET quickbase_project_id = 'PROJECT-123', 
    quickbase_record_id = 456 
WHERE arrivy_task_id = 789;
```

## Performance

### Rate Limiting

- Arrivy API limit: 30 requests per minute
- Client automatically handles rate limiting
- Tasks are processed sequentially to respect limits

### Batch Processing

- Tasks are fetched in 3-month date range batches
- Prevents memory issues with large datasets
- Reduces API timeout risk
- **Adaptive sub-chunking**: Automatically splits ranges when ≥1000 tasks detected
- Recursive splitting continues until all tasks retrieved or minimum range (1 hour) reached
- Date boundaries set to end-of-day (23:59:59.999) to prevent gaps

### Typical Execution Times

| Task Count | Estimated Time |
|------------|----------------|
| 100 tasks | ~2 minutes |
| 500 tasks | ~8 minutes |
| 1,000 tasks | ~15 minutes |
| 5,000 tasks | ~75 minutes |

## Error Handling

### Retry Logic

- 3 retry attempts with exponential backoff (1s, 2s, 4s)
- Retries on: network errors, timeouts, 5xx errors, rate limits
- Continues processing remaining tasks on individual failures

### Error Reporting

- Detailed error messages in summary
- Error rate warnings if > 5%
- Suggestions for resolution

## Common Issues

### "Arrivy client not configured"

**Cause:** Missing environment variables

**Solution:**
```bash
# Check .env.local has:
ARRIVY_AUTH_KEY=your_key
ARRIVY_AUTH_TOKEN=your_token
```

### "Database connection failed"

**Cause:** Invalid DATABASE_URL or database not accessible

**Solution:**
```bash
# Test connection
psql $DATABASE_URL -c "SELECT 1;"
```

### "Rate limit exceeded"

**Cause:** Too many requests to Arrivy API

**Solution:** The client handles this automatically with backoff. If persistent, reduce batch size or add delays.

### "Foreign key constraint violation"

**Cause:** Task references entity that doesn't exist in database

**Solution:**
```bash
# Sync entities first
npm run sync:arrivy -- --sync-entities
```

### "Tasks not linking to QuickBase"

**Cause:** Task created in Arrivy without external_id

**Solution:** This is expected behavior; use manual linking or future integration feature

## Best Practices

### Initial Setup

1. Run dry-run first to preview:
   ```bash
   npm run sync:arrivy:dry-run
   ```

2. Sync entities before tasks:
   ```bash
   npm run sync:arrivy:entities
   ```

3. Test with limited tasks:
   ```bash
   npm run sync:arrivy -- --limit=10 --verbose
   ```

4. Run full sync:
   ```bash
   npm run sync:arrivy
   ```

### Ongoing Maintenance

- **Daily:** Sync recent tasks (last 7 days)
  ```bash
  npm run sync:arrivy -- --start-date=$(date -v-7d +%Y-%m-%d)
  ```

- **Weekly:** Full sync to catch any missed tasks
  ```bash
  npm run sync:arrivy
  ```

- **After Arrivy changes:** Re-sync affected date range
  ```bash
  npm run sync:arrivy -- --start-date=2024-10-01 --end-date=2024-10-31
  ```

### Monitoring

- Check sync summary for error rate
- Monitor execution time trends
- Review error details for patterns
- Verify task counts match Arrivy dashboard

## Verification

### Check Synced Tasks

```sql
-- Count synced tasks
SELECT COUNT(*) FROM arrivy_tasks;

-- Recent tasks
SELECT 
  customer_name,
  task_type,
  current_status,
  scheduled_start,
  synced_at
FROM arrivy_tasks
ORDER BY synced_at DESC
LIMIT 10;

-- Tasks by status
SELECT 
  current_status,
  COUNT(*) as count
FROM arrivy_tasks
GROUP BY current_status
ORDER BY count DESC;
```

### Compare with Arrivy

1. Log into https://app.arrivy.com/
2. Go to Tasks view
3. Compare total count with database count
4. Spot-check a few tasks for data accuracy

## Troubleshooting

### Enable Verbose Logging

```bash
npm run sync:arrivy:verbose
```

This shows:
- Each task being processed
- API request/response details
- Retry attempts
- Database operations

### Check Logs

The script logs to console. Redirect to file for analysis:

```bash
npm run sync:arrivy 2>&1 | tee sync-log.txt
```

### Manual Verification

Test individual task sync:

```typescript
import { arrivyClient } from '@/lib/integrations/arrivy/client';
import { upsertArrivyTask } from '@/lib/db/arrivy';

const task = await arrivyClient.getTask(12345);
console.log('Task from Arrivy:', task);

const result = await upsertArrivyTask(mappedTaskData);
console.log('Upserted task:', result);
```

## Next Steps

After successful sync:

1. ✅ Verify task count matches Arrivy
2. ✅ Check dashboard displays tasks correctly
3. ✅ Test webhook integration
4. ✅ Configure scheduled syncs (cron job or GitHub Actions)
5. ✅ Set up monitoring alerts for sync failures

## Support

For issues:
- Check this guide first
- Review error messages in sync summary
- Enable verbose logging for details
- Check Arrivy API status: https://status.arrivy.com/
- Contact Arrivy support for API issues

