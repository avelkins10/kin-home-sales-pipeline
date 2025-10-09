-- Migration: 007_milestone_configurations_schema.sql
-- Purpose: Add milestone_configurations table for customizable milestone tracking
--
-- This migration adds the ability to override default milestone configurations
-- stored in milestones.json with custom configurations per organization.

BEGIN;

-- 1. Create milestone_configurations table
CREATE TABLE IF NOT EXISTS milestone_configurations (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  milestone_id TEXT NOT NULL UNIQUE,
  configuration JSONB NOT NULL,
  is_active BOOLEAN DEFAULT TRUE,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  created_by TEXT REFERENCES users(id) ON DELETE SET NULL,
  updated_by TEXT REFERENCES users(id) ON DELETE SET NULL,
  version INTEGER DEFAULT 1,
  notes TEXT
);

-- 2. Create milestone_configuration_history for version tracking
CREATE TABLE IF NOT EXISTS milestone_configuration_history (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  milestone_id TEXT NOT NULL,
  configuration JSONB NOT NULL,
  version INTEGER NOT NULL,
  changed_at TIMESTAMP DEFAULT NOW(),
  changed_by TEXT REFERENCES users(id) ON DELETE SET NULL,
  change_description TEXT,
  FOREIGN KEY (milestone_id) REFERENCES milestone_configurations(milestone_id) ON DELETE CASCADE
);

-- 3. Create indexes
CREATE INDEX IF NOT EXISTS idx_milestone_configurations_milestone_id
  ON milestone_configurations(milestone_id);
CREATE INDEX IF NOT EXISTS idx_milestone_configurations_active
  ON milestone_configurations(is_active);
CREATE INDEX IF NOT EXISTS idx_milestone_configuration_history_milestone
  ON milestone_configuration_history(milestone_id, version DESC);

-- 4. Create trigger to update updated_at timestamp
CREATE OR REPLACE FUNCTION update_milestone_configuration_timestamp()
RETURNS TRIGGER AS $$
BEGIN
  NEW.updated_at = NOW();
  NEW.version = OLD.version + 1;

  -- Archive old version to history
  INSERT INTO milestone_configuration_history
    (milestone_id, configuration, version, changed_by, change_description)
  VALUES
    (OLD.milestone_id, OLD.configuration, OLD.version, NEW.updated_by, 'Configuration updated');

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER trigger_update_milestone_configuration
  BEFORE UPDATE ON milestone_configurations
  FOR EACH ROW
  EXECUTE FUNCTION update_milestone_configuration_timestamp();

COMMIT;
