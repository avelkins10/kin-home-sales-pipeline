-- Project Messages Table Migration
-- Creates table for internal project messaging between team members

-- Drop existing table if re-running migration
DROP TABLE IF EXISTS project_messages CASCADE;

-- Create project_messages table
CREATE TABLE project_messages (
  -- Primary key
  id SERIAL PRIMARY KEY,

  -- Project and User associations
  project_id INTEGER NOT NULL,
  sender_id VARCHAR(255) NOT NULL,       -- User email or ID
  sender_name VARCHAR(255) NOT NULL,
  sender_role VARCHAR(50) NOT NULL,      -- closer, setter, coordinator, office_leader, admin

  -- Message content
  message TEXT NOT NULL,

  -- Message metadata
  is_system_message BOOLEAN DEFAULT FALSE, -- True for automated system messages
  metadata JSONB DEFAULT '{}'::jsonb,    -- Additional data (attachments, mentions, etc.)

  -- Soft delete (for audit trail)
  is_deleted BOOLEAN DEFAULT FALSE,
  deleted_at TIMESTAMP WITH TIME ZONE,
  deleted_by VARCHAR(255),

  -- Timestamps
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
);

-- Indexes for performance
CREATE INDEX idx_project_messages_project ON project_messages(project_id, created_at DESC);
CREATE INDEX idx_project_messages_sender ON project_messages(sender_id);
CREATE INDEX idx_project_messages_created ON project_messages(created_at DESC);
CREATE INDEX idx_project_messages_not_deleted ON project_messages(is_deleted) WHERE is_deleted = FALSE;

-- Auto-update updated_at timestamp
CREATE TRIGGER update_project_messages_updated_at
  BEFORE UPDATE ON project_messages
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

-- Comments for documentation
COMMENT ON TABLE project_messages IS 'Internal messaging system for project collaboration';
COMMENT ON COLUMN project_messages.project_id IS 'QuickBase project record ID';
COMMENT ON COLUMN project_messages.sender_id IS 'User email or QB ID of message sender';
COMMENT ON COLUMN project_messages.is_system_message IS 'True for automated messages (status changes, etc.)';
COMMENT ON COLUMN project_messages.is_deleted IS 'Soft delete flag - messages never truly deleted for audit trail';
COMMENT ON COLUMN project_messages.metadata IS 'Additional message data (file attachments, @mentions, etc.)';

-- Grant permissions (adjust role name as needed)
-- GRANT SELECT, INSERT, UPDATE ON project_messages TO your_app_user;
-- GRANT USAGE, SELECT ON SEQUENCE project_messages_id_seq TO your_app_user;
