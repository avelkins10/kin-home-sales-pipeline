-- Migration: User Hierarchies, Manager Roles, and Activity Tracking
-- This migration adds support for team lead hierarchies, multiple manager types,
-- invite system, and activity tracking to prevent thousands of users problem.

-- Create pgcrypto extension for gen_random_uuid() function
CREATE EXTENSION IF NOT EXISTS pgcrypto;

-- Add new manager role types to users table
ALTER TABLE users DROP CONSTRAINT IF EXISTS users_role_check;
ALTER TABLE users ADD CONSTRAINT users_role_check 
  CHECK (role IN (
    'closer', 
    'setter', 
    'team_lead', 
    'office_leader', 
    'area_director', 
    'divisional', 
    'regional', 
    'super_admin'
  ));

-- Add activity tracking fields to users table
ALTER TABLE users ADD COLUMN IF NOT EXISTS last_project_date TIMESTAMP;
ALTER TABLE users ADD COLUMN IF NOT EXISTS invited_at TIMESTAMP;
ALTER TABLE users ADD COLUMN IF NOT EXISTS invite_token TEXT UNIQUE;
ALTER TABLE users ADD COLUMN IF NOT EXISTS invite_accepted_at TIMESTAMP;

-- Create user_hierarchies table for team lead relationships
CREATE TABLE IF NOT EXISTS user_hierarchies (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  manager_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(manager_id, user_id)
);

-- Create office_assignments table for manager office access
CREATE TABLE IF NOT EXISTS office_assignments (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  office_name TEXT NOT NULL,
  access_level TEXT NOT NULL CHECK (access_level IN ('view', 'manage', 'admin')),
  assigned_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(user_id, office_name)
);

-- Create sync_logs table for tracking sync operations
CREATE TABLE IF NOT EXISTS sync_logs (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  sync_type TEXT NOT NULL,
  results JSONB,
  created_by TEXT NOT NULL,
  created_at TIMESTAMP DEFAULT NOW()
);

-- Create notification_settings table for user notification preferences
CREATE TABLE IF NOT EXISTS notification_settings (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id UUID NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  email_notifications BOOLEAN DEFAULT true,
  push_notifications BOOLEAN DEFAULT true,
  project_updates BOOLEAN DEFAULT true,
  hold_alerts BOOLEAN DEFAULT true,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(user_id)
);

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_user_hierarchies_manager_id ON user_hierarchies(manager_id);
CREATE INDEX IF NOT EXISTS idx_user_hierarchies_user_id ON user_hierarchies(user_id);
CREATE INDEX IF NOT EXISTS idx_users_last_project_date ON users(last_project_date);
CREATE INDEX IF NOT EXISTS idx_users_invite_token ON users(invite_token);
CREATE INDEX IF NOT EXISTS idx_office_assignments_user_id ON office_assignments(user_id);
CREATE INDEX IF NOT EXISTS idx_office_assignments_office_name ON office_assignments(office_name);
CREATE INDEX IF NOT EXISTS idx_sync_logs_sync_type ON sync_logs(sync_type);
CREATE INDEX IF NOT EXISTS idx_sync_logs_created_at ON sync_logs(created_at);
CREATE INDEX IF NOT EXISTS idx_notification_settings_user_id ON notification_settings(user_id);

-- Add comments to document office-based visibility
COMMENT ON TABLE user_hierarchies IS 'Team lead relationships - user-based visibility where team leads see projects for their managed users';
COMMENT ON TABLE office_assignments IS 'Office access for managers - office-based visibility where managers see ALL projects in their assigned offices regardless of user accounts';
COMMENT ON TABLE sync_logs IS 'Log of sync operations for tracking and debugging user synchronization from QuickBase';
COMMENT ON TABLE notification_settings IS 'User notification preferences for email, push, and project update notifications';
COMMENT ON COLUMN users.last_project_date IS 'Date of user''s most recent project - used for activity-based filtering to avoid creating thousands of inactive users';
COMMENT ON COLUMN users.invite_token IS 'Token for invite-based user provisioning - recommended approach to avoid bulk user creation';

-- Create function to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  RETURN NEW;
END;
$$ language 'plpgsql';

-- Add triggers for updated_at
CREATE TRIGGER update_user_hierarchies_updated_at 
  BEFORE UPDATE ON user_hierarchies 
  FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

CREATE TRIGGER update_notification_settings_updated_at 
  BEFORE UPDATE ON notification_settings 
  FOR EACH ROW EXECUTE FUNCTION update_updated_at_column();

-- Create migrations_log table if it doesn't exist
CREATE TABLE IF NOT EXISTS migrations_log (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  migration_name TEXT NOT NULL UNIQUE,
  executed_at TIMESTAMP DEFAULT NOW(),
  executed_by TEXT DEFAULT 'system'
);

-- Log this migration
INSERT INTO migrations_log (migration_name) 
VALUES ('006_user_hierarchies_schema') 
ON CONFLICT (migration_name) DO NOTHING;
