-- Migration: Add external system IDs to users table
-- Purpose: Support master user table with IDs from RepCard, Enerflo, and future systems
-- Date: 2025-01-25

-- Add external system ID columns
ALTER TABLE users
ADD COLUMN IF NOT EXISTS repcard_user_id TEXT,
ADD COLUMN IF NOT EXISTS enerflo_user_id TEXT,
ADD COLUMN IF NOT EXISTS external_ids JSONB DEFAULT '{}'::jsonb,
ADD COLUMN IF NOT EXISTS last_synced_at TIMESTAMP,
ADD COLUMN IF NOT EXISTS sync_confidence DECIMAL(3,2);

-- Add indexes for performance
CREATE INDEX IF NOT EXISTS idx_users_repcard_user_id ON users(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_users_enerflo_user_id ON users(enerflo_user_id);
CREATE INDEX IF NOT EXISTS idx_users_external_ids ON users USING GIN(external_ids);

-- Add comments for documentation
COMMENT ON COLUMN users.repcard_user_id IS 'RepCard user ID for linking to RepCard system';
COMMENT ON COLUMN users.enerflo_user_id IS 'Enerflo user ID for linking to Enerflo system';
COMMENT ON COLUMN users.external_ids IS 'JSONB object storing IDs from additional systems (e.g., {"salesforce": "abc123"})';
COMMENT ON COLUMN users.last_synced_at IS 'Timestamp of last sync with external systems';
COMMENT ON COLUMN users.sync_confidence IS 'Match confidence score 0.0-1.0 for external ID matches';

-- Create audit log table for user sync operations
CREATE TABLE IF NOT EXISTS user_sync_log (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  user_id TEXT REFERENCES users(id) ON DELETE CASCADE,
  source_system TEXT NOT NULL,
  external_id TEXT NOT NULL,
  match_method TEXT NOT NULL,
  confidence DECIMAL(3,2),
  synced_by TEXT REFERENCES users(id),
  synced_at TIMESTAMP DEFAULT NOW(),
  notes TEXT
);

-- Add indexes for audit log
CREATE INDEX IF NOT EXISTS idx_user_sync_log_user_id ON user_sync_log(user_id);
CREATE INDEX IF NOT EXISTS idx_user_sync_log_source_system ON user_sync_log(source_system);
CREATE INDEX IF NOT EXISTS idx_user_sync_log_synced_at ON user_sync_log(synced_at DESC);

-- Add comments
COMMENT ON TABLE user_sync_log IS 'Audit trail for user sync operations across external systems';
COMMENT ON COLUMN user_sync_log.source_system IS 'External system name (repcard, enerflo, quickbase, etc.)';
COMMENT ON COLUMN user_sync_log.match_method IS 'How the match was made (email, name, manual, quickbase_closer_id)';
COMMENT ON COLUMN user_sync_log.confidence IS 'Match confidence score 0.0-1.0';
COMMENT ON COLUMN user_sync_log.synced_by IS 'Admin user who approved the sync (NULL for automated)';
