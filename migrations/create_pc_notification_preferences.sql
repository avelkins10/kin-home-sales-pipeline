-- Create PC notification preferences table
CREATE TABLE IF NOT EXISTS pc_notification_preferences (
  id SERIAL PRIMARY KEY,
  user_id VARCHAR(255) NOT NULL UNIQUE,
  email_enabled BOOLEAN DEFAULT false,
  email_frequency VARCHAR(50) DEFAULT 'immediate',
  notification_types JSONB DEFAULT '[]'::jsonb,
  quiet_hours_start TIME DEFAULT '22:00:00',
  quiet_hours_end TIME DEFAULT '08:00:00',
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Create index for fast lookups
CREATE INDEX IF NOT EXISTS idx_pc_notification_preferences_user_id ON pc_notification_preferences(user_id);

-- Add trigger to update updated_at
CREATE OR REPLACE FUNCTION update_pc_notification_preferences_updated_at()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_update_pc_notification_preferences_updated_at
  BEFORE UPDATE ON pc_notification_preferences
  FOR EACH ROW
  EXECUTE FUNCTION update_pc_notification_preferences_updated_at();
