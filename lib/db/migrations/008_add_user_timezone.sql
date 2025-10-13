-- migrations/008_add_user_timezone.sql
-- Add timezone field to users table for personalized date/time display

BEGIN;

-- Add timezone column to users table
-- Default to America/New_York (ET) as most common for solar installation business
ALTER TABLE users
ADD COLUMN IF NOT EXISTS timezone TEXT DEFAULT 'America/New_York';

-- Create index for timezone lookups
CREATE INDEX IF NOT EXISTS idx_users_timezone ON users(timezone);

-- Add comment for documentation
COMMENT ON COLUMN users.timezone IS 'User timezone for date/time display (IANA timezone identifier)';

COMMIT;
