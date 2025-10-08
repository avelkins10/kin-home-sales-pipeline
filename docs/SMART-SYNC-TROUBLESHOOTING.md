# Smart Sync Troubleshooting Guide

## Overview
This guide helps diagnose and fix common issues with the Smart Sync feature for ingesting users from QuickBase.

## Common Issues

### Issue 1: "Column 'is_active' does not exist"
**Symptom**: Sync fails with SQL error about missing `is_active` column
**Cause**: Migration 006 hasn't been run or failed partway through
**Solution**: 
1. Run the migration: `npm run migrate:hierarchies`
2. Verify the column exists: `SELECT column_name FROM information_schema.columns WHERE table_name='users' AND column_name='is_active';`
3. If missing, manually add: `ALTER TABLE users ADD COLUMN is_active BOOLEAN DEFAULT true;`

### Issue 2: "Sync is already running"
**Symptom**: 429 error saying sync is already running
**Cause**: Previous sync didn't release advisory lock (crashed or timed out)
**Solution**:
1. Check for stuck locks: `SELECT * FROM pg_locks WHERE locktype = 'advisory';`
2. Release manually: `SELECT pg_advisory_unlock_all();`
3. Wait 1 minute and try again

### Issue 3: "No users found in QuickBase"
**Symptom**: Sync returns 0 users even though projects exist
**Cause**: Date filter too restrictive or QuickBase field IDs incorrect
**Solution**:
1. Increase `monthsBack` parameter (try 12 or 24 months)
2. Verify QuickBase field IDs in `lib/constants/fieldIds.ts`:
   - CLOSER_ID (516)
   - CLOSER_NAME (517)
   - CLOSER_EMAIL (356)
   - CLOSER_PHONE (357)
   - SETTER_ID (518)
   - SETTER_NAME (519)
   - SETTER_EMAIL (358)
   - SETTER_PHONE (359)
   - SALES_DATE (7)
3. Check QuickBase API credentials are valid

### Issue 4: "Notification settings insert failed"
**Symptom**: Users created but notification_settings records missing
**Cause**: Schema mismatch or constraint violation
**Solution**:
1. Verify notification_settings table schema matches migration 002
2. Check for orphaned records: `SELECT user_id FROM notification_settings WHERE user_id NOT IN (SELECT id FROM users);`
3. Manually insert missing records: `INSERT INTO notification_settings (user_id) SELECT id FROM users WHERE id NOT IN (SELECT user_id FROM notification_settings);`

### Issue 5: "Duplicate key violation on email"
**Symptom**: Sync fails with unique constraint violation on users.email
**Cause**: User with same email already exists
**Solution**:
1. Check for duplicates: `SELECT email, COUNT(*) FROM users GROUP BY email HAVING COUNT(*) > 1;`
2. Merge duplicate users or update QuickBase data
3. Use dry run mode to preview conflicts before syncing

## Performance Issues

### Slow Sync (>30 seconds)
**Cause**: Large number of users or slow QuickBase API
**Solution**:
1. Reduce `monthsBack` to sync fewer users
2. Use role filter to sync closers and setters separately
3. Check QuickBase API rate limits
4. Verify database indexes exist: `\d users` in psql

### High Memory Usage
**Cause**: Syncing thousands of users at once
**Solution**:
1. Use activity-based filtering (6 months recommended)
2. Sync in batches by role (closers first, then setters)
3. Increase Node.js memory limit: `NODE_OPTIONS=--max-old-space-size=4096`

## Debugging Tips

### Enable Verbose Logging
Set `NODE_ENV=development` to see detailed logs:
- QuickBase API requests and responses
- SQL queries and results
- User matching logic
- Error stack traces

### Check Sync Logs
Query the sync_logs table to see history:
```sql
SELECT 
  created_at,
  results->>'created' as created,
  results->>'updated' as updated,
  results->>'skipped' as skipped,
  results->>'inactiveSkipped' as inactive_skipped
FROM sync_logs 
WHERE sync_type = 'user_sync'
ORDER BY created_at DESC
LIMIT 10;
```

### Test QuickBase Connection
Use the QuickBase lookup API to test connectivity:
```bash
curl -X GET 'http://localhost:3000/api/admin/users/lookup?search=test' \
  -H 'Cookie: next-auth.session-token=YOUR_TOKEN'
```

### Verify User Data
Check what data would be synced with dry run:
```bash
curl -X POST 'http://localhost:3000/api/admin/users/sync' \
  -H 'Content-Type: application/json' \
  -H 'Cookie: next-auth.session-token=YOUR_TOKEN' \
  -d '{"dryRun": true, "monthsBack": 6}'
```

## Best Practices

1. **Always use dry run first** to preview changes
2. **Start with 6 months** activity threshold to avoid thousands of users
3. **Monitor sync logs** to track success/failure rates
4. **Run activity updates monthly** to keep last_project_date current
5. **Deactivate inactive users quarterly** to keep user list clean
6. **Use invite-based provisioning** for managers instead of bulk sync

## Getting Help

If issues persist:
1. Check application logs: `vercel logs` or `docker logs`
2. Review audit logs: `SELECT * FROM audit_logs WHERE action LIKE 'sync%' ORDER BY timestamp DESC LIMIT 20;`
3. Contact support with:
   - Error message and stack trace
   - Sync parameters used (monthsBack, role, dryRun)
   - Last successful sync timestamp
   - Number of users in database vs QuickBase
