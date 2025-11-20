-- Complete Migration 018: Normalize Remaining RepCard User IDs to INTEGER
-- Purpose: Finish the partial migration - convert remaining TEXT user IDs to INTEGER
-- Date: 2025-11-20
--
-- This completes migration 018 by running Steps 1, 2, and 4 that were missed.
-- Step 3 (users.repcard_user_id) already completed successfully.

BEGIN;

-- Step 1: Fix repcard_customers.setter_user_id
-- Convert TEXT to INTEGER (handles both numeric strings and integers)
ALTER TABLE repcard_customers
  ALTER COLUMN setter_user_id TYPE INTEGER 
  USING CASE 
    WHEN setter_user_id ~ '^[0-9]+$' THEN setter_user_id::integer
    ELSE NULL
  END;

-- Recreate index with INTEGER type
DROP INDEX IF EXISTS idx_repcard_customers_setter;
CREATE INDEX IF NOT EXISTS idx_repcard_customers_setter 
  ON repcard_customers(setter_user_id);

-- Step 2: Fix repcard_appointments.setter_user_id and closer_user_id
ALTER TABLE repcard_appointments
  ALTER COLUMN setter_user_id TYPE INTEGER 
  USING CASE 
    WHEN setter_user_id ~ '^[0-9]+$' THEN setter_user_id::integer
    ELSE NULL
  END,
  ALTER COLUMN closer_user_id TYPE INTEGER 
  USING CASE 
    WHEN closer_user_id ~ '^[0-9]+$' THEN closer_user_id::integer
    ELSE NULL
  END;

-- Recreate indexes with INTEGER type
DROP INDEX IF EXISTS idx_repcard_appointments_setter;
DROP INDEX IF EXISTS idx_repcard_appointments_closer;
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter 
  ON repcard_appointments(setter_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_closer 
  ON repcard_appointments(closer_user_id);

-- Step 4: Fix repcard_status_logs.changed_by_user_id (if table exists)
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'repcard_status_logs') THEN
    ALTER TABLE repcard_status_logs
      ALTER COLUMN changed_by_user_id TYPE INTEGER 
      USING CASE 
        WHEN changed_by_user_id ~ '^[0-9]+$' THEN changed_by_user_id::integer
        ELSE NULL
      END;

    DROP INDEX IF EXISTS idx_repcard_status_logs_changed_by;
    CREATE INDEX IF NOT EXISTS idx_repcard_status_logs_changed_by 
      ON repcard_status_logs(changed_by_user_id);
  END IF;
END $$;

-- Step 5: Add composite indexes for common query patterns
-- These indexes will significantly improve leaderboard query performance

-- For date-based queries (common in leaderboards)
CREATE INDEX IF NOT EXISTS idx_repcard_customers_setter_created_date 
  ON repcard_customers(setter_user_id, DATE(created_at));

CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter_completed_date
  ON repcard_appointments(setter_user_id, DATE(completed_at));

CREATE INDEX IF NOT EXISTS idx_repcard_appointments_closer_completed_date
  ON repcard_appointments(closer_user_id, DATE(completed_at));

-- For office filtering with active users
DO $$
BEGIN
  IF EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'repcard_users') THEN
    CREATE INDEX IF NOT EXISTS idx_repcard_users_office_status
      ON repcard_users(office_id, status) WHERE status = 1;
  END IF;
END $$;

-- Step 6: Update composite indexes that include user IDs
DROP INDEX IF EXISTS idx_repcard_customers_setter_created;
CREATE INDEX IF NOT EXISTS idx_repcard_customers_setter_created 
  ON repcard_customers(setter_user_id, created_at);

DROP INDEX IF EXISTS idx_repcard_appointments_setter_created;
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter_created 
  ON repcard_appointments(setter_user_id, created_at);

DROP INDEX IF EXISTS idx_repcard_appointments_closer_scheduled;
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_closer_scheduled 
  ON repcard_appointments(closer_user_id, scheduled_at);

COMMIT;

-- Add comments explaining the change
COMMENT ON COLUMN repcard_customers.setter_user_id IS 'RepCard user ID (INTEGER) - normalized for performance';
COMMENT ON COLUMN repcard_appointments.setter_user_id IS 'RepCard user ID (INTEGER) - normalized for performance';
COMMENT ON COLUMN repcard_appointments.closer_user_id IS 'RepCard user ID (INTEGER) - normalized for performance';


