-- Migration: Create Notifications System
-- Created: 2025-01-08
-- Purpose: Unified notifications for Quickbase notes, internal messages, and system alerts

-- Create notifications table
CREATE TABLE IF NOT EXISTS notifications (
  id SERIAL PRIMARY KEY,

  -- User and Project associations
  user_id VARCHAR(255) NOT NULL,
  project_id INTEGER NOT NULL,

  -- Type and Priority
  type VARCHAR(50) NOT NULL,              -- 'quickbase_note', 'internal_message', 'system_alert'
  priority VARCHAR(20) DEFAULT 'normal',  -- 'critical', 'normal', 'info'
  source VARCHAR(50) NOT NULL,            -- 'quickbase', 'internal', 'system'

  -- Content
  title VARCHAR(255) NOT NULL,
  message TEXT,
  metadata JSONB DEFAULT '{}',

  -- Sender information (optional - null for system notifications)
  sender_id VARCHAR(255),
  sender_name VARCHAR(255),
  sender_role VARCHAR(50),

  -- Display properties
  icon VARCHAR(50) DEFAULT 'bell',
  color VARCHAR(20) DEFAULT 'blue',
  action_url VARCHAR(500),

  -- Read status
  is_read BOOLEAN DEFAULT FALSE,
  read_at TIMESTAMPTZ,

  -- Timestamps
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX IF NOT EXISTS idx_notifications_user_unread
  ON notifications(user_id, is_read, priority DESC, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_notifications_project
  ON notifications(project_id, created_at DESC);

CREATE INDEX IF NOT EXISTS idx_notifications_user_project
  ON notifications(user_id, project_id, is_read);

CREATE INDEX IF NOT EXISTS idx_notifications_type
  ON notifications(type, created_at DESC);

-- Create updated_at trigger function
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ language 'plpgsql';

-- Create trigger for updated_at
CREATE TRIGGER update_notifications_updated_at
  BEFORE UPDATE ON notifications
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

-- Comments for documentation
COMMENT ON TABLE notifications IS 'Unified notification system for Quickbase notes, internal messages, and system alerts';
COMMENT ON COLUMN notifications.type IS 'Notification type: quickbase_note, internal_message, system_alert';
COMMENT ON COLUMN notifications.priority IS 'Display priority: critical (red), normal (blue), info (gray)';
COMMENT ON COLUMN notifications.source IS 'Source system: quickbase, internal, system';
COMMENT ON COLUMN notifications.metadata IS 'Flexible JSON storage for type-specific data';
