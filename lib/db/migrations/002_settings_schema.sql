BEGIN;

-- Add phone column to users table
ALTER TABLE users ADD COLUMN IF NOT EXISTS phone TEXT;

-- Create notification_settings table
CREATE TABLE IF NOT EXISTS notification_settings (
  user_id TEXT PRIMARY KEY REFERENCES users(id) ON DELETE CASCADE,
  email_enabled BOOLEAN DEFAULT true,
  urgent_alerts BOOLEAN DEFAULT true,
  daily_digest BOOLEAN DEFAULT false,
  weekly_summary BOOLEAN DEFAULT false,
  hold_threshold INTEGER DEFAULT 7,
  age_warning_threshold INTEGER DEFAULT 90,
  install_overdue_threshold INTEGER DEFAULT 14,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Create index for notification_settings
CREATE INDEX IF NOT EXISTS idx_notification_settings_user 
  ON notification_settings(user_id);

-- Insert default notification settings for existing users
INSERT INTO notification_settings (user_id)
SELECT id FROM users
WHERE id NOT IN (SELECT user_id FROM notification_settings)
ON CONFLICT (user_id) DO NOTHING;

COMMIT;
