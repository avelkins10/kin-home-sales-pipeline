# Database Migration Instructions

## Overview

After running `npm run setup:all`, three additional database migrations need to be executed to enable all Settings functionality. These migrations create the required tables for the Settings tabs: Offices, System, and Audit Logs.

## Prerequisites

Before running these migrations, ensure:

- ✅ `npm install` has been completed
- ✅ `npm run setup:all` has completed successfully  
- ✅ `DATABASE_URL` is properly set in `.env.local`
- ✅ Neon PostgreSQL database is accessible

## Migration Execution Results

### Migration 003 - Offices Schema

**Command:**
```bash
npm run migrate:offices
```

**Purpose:** Creates `offices` table for multi-office support and adds user management columns.

**Actual Execution Results:**
```
🚀 Starting offices migration...
📝 Executing migration SQL...
✅ Added last_login_at column to users table
✅ Added region column to users table
✅ Created offices table
✅ Created indexes (5 total)
🔍 Verifying schema...
✅ Offices migration complete
📊 Schema verification passed
```

**What it does:**
- Creates `offices` table with office information
- Adds `last_login_at`, `region`, `is_active` columns to `users` table
- Creates 5 performance indexes

**Required for:** OfficesTab in Settings

### Migration 004 - System Settings

**Command:**
```bash
npm run migrate:system
```

**Purpose:** Creates `system_settings` table with default configuration values.

**Actual Execution Results:**
```
Running system settings migration...
✅ Executed migration SQL
✅ Created system_settings table
✅ Inserted default system settings
✅ Created JSONB index
✅ System settings migration complete
```

**What it does:**
- Creates `system_settings` table with JSONB configuration storage
- Inserts default system configuration (SLA thresholds, hold reasons, date format, timezone)
- Creates JSONB index for efficient querying

**Required for:** SystemTab in Settings

### Migration 005 - Audit Logs

**Command:**
```bash
npm run migrate:audit
```

**Purpose:** Creates `audit_logs` table for compliance tracking and activity monitoring.

**Actual Execution Results:**
```
🔄 Starting audit logs migration...
📄 Read migration file: /Users/austinelkins/Rep Dashboard/lib/db/migrations/005_audit_logs_schema.sql
🚀 Executing migration...
✅ Created audit_logs table
✅ Created indexes (6 total)
🔍 Verifying schema...
✅ audit_logs table exists
✅ All indexes created (6 total)
🎉 Audit logs migration complete!
📊 Summary:
   - audit_logs table created
   - 6 indexes created for efficient filtering
   - Ready for audit logging
✅ Migration script completed successfully
```

**What it does:**
- Creates `audit_logs` table for tracking user actions
- Creates 6 indexes for efficient filtering by timestamp, user, action, and resource

**Required for:** AuditLogsTab in Settings

**Note:** Initial execution failed with `sql.unsafe is not a function` error, which was fixed by updating the script to use `sql.query()` instead.

## Verification Results

After running all migrations, the database schema was verified:

### Table Verification Query

```sql
SELECT table_name FROM information_schema.tables 
WHERE table_schema = 'public' 
AND table_name IN ('users', 'sessions', 'project_cache', 'notification_settings', 'offices', 'system_settings', 'audit_logs')
ORDER BY table_name;
```

**Actual Result:** ✅ All 7 required tables exist:
- audit_logs
- notification_settings
- offices
- project_cache
- sessions
- system_settings
- users

### Index Count Verification

```sql
SELECT COUNT(*) FROM pg_indexes WHERE schemaname = 'public';
```

**Actual Result:** ✅ 28 indexes created (exceeds expected 20+)

## Migration Summary

| Migration | Command | Status | Tables Created | Columns Added | Indexes Created | Required For |
|-----------|---------|--------|----------------|---------------|-----------------|--------------|
| 003 | `npm run migrate:offices` | ✅ Success | offices | last_login_at, region, is_active (users) | 5 | OfficesTab |
| 004 | `npm run migrate:system` | ✅ Success | system_settings | - | 1 | SystemTab |
| 005 | `npm run migrate:audit` | ✅ Success* | audit_logs | - | 6 | AuditLogsTab |

*Note: Migration 005 initially failed due to `sql.unsafe is not a function` error, which was resolved by updating the script to use `sql.query()` instead.

## Troubleshooting

### Issues Encountered and Resolved

**"sql.unsafe is not a function" error (Migration 005)**
- ❌ **Error encountered:** `TypeError: sql.unsafe is not a function`
- ✅ **Resolution:** Updated `scripts/run-audit-migration.js` to use `sql.query()` instead of `sql.unsafe()`
- **Root cause:** Incompatible API usage with current `@vercel/postgres` version
- **Fix applied:** Changed line 38 from `await sql.unsafe(migrationSQL)` to `await sql.query(migrationSQL)`

### Common Issues

**"Table already exists" error**
- ✅ **Safe to ignore** - Migrations are idempotent and can be run multiple times
- The migration will skip existing tables and continue

**"DATABASE_URL not set" error**
- Check `.env.local` file has the correct database URL
- Ensure the URL format is: `postgresql://username:password@host:port/database`

**Connection timeout**
- Verify Neon database is running and accessible
- Check network connectivity to Neon servers
- Ensure database credentials are correct

**Permission denied**
- Ensure database user has CREATE TABLE privileges
- Contact database administrator if using restricted access

**Module resolution errors**
- Run `npm install` to ensure all dependencies are installed
- Check that all migration scripts exist in `scripts/` directory

## Next Steps

✅ **All migrations completed successfully!** The database now has all required tables and indexes.

1. **Start Development Server**
   ```bash
   npm run dev
   ```

2. **Test Settings Page**
   - Navigate to `/settings` in your browser
   - Verify all 6 tabs load without errors:
     - Profile Tab
     - Notifications Tab
     - Users Tab
     - Offices Tab
     - System Tab
     - Audit Logs Tab

3. **Test Core Functionality**
   - Test user login/logout
   - Verify dashboard loads correctly
   - Check projects list functionality
   - Test project detail views

4. **Proceed to Testing**
   - Run integration tests: `npm run test:integration`
   - Run unit tests: `npm run test:unit`
   - Test user flows end-to-end

## Support

If you encounter issues not covered in this troubleshooting guide:

1. Check the console output for specific error messages
2. Verify your database connection with: `npm run health-check`
3. Review the migration SQL files in `lib/db/migrations/`
4. Check the migration scripts in `scripts/` directory

All migrations are designed to be safe and idempotent - they can be run multiple times without causing issues.