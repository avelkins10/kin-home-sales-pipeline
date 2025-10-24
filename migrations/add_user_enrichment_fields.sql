-- Migration: Add user enrichment fields for self-enriching database
-- Purpose: Enable automatic enrichment from Contacts, Projects, RepCard API
-- Date: 2025-01-25

-- Add columns for external system integration
ALTER TABLE users
ADD COLUMN IF NOT EXISTS quickbase_contact_id TEXT UNIQUE,
ADD COLUMN IF NOT EXISTS sequifi_user_id TEXT,
ADD COLUMN IF NOT EXISTS num_closer_projects INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS num_setter_projects INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS office TEXT,
ADD COLUMN IF NOT EXISTS team TEXT,
ADD COLUMN IF NOT EXISTS is_rookie BOOLEAN DEFAULT false,
ADD COLUMN IF NOT EXISTS is_setter BOOLEAN DEFAULT false,
ADD COLUMN IF NOT EXISTS profile_image_url TEXT,
ADD COLUMN IF NOT EXISTS last_synced_from_contacts_at TIMESTAMP,
ADD COLUMN IF NOT EXISTS last_synced_from_repcard_at TIMESTAMP;

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_users_quickbase_contact_id ON users(quickbase_contact_id);
CREATE INDEX IF NOT EXISTS idx_users_sequifi_user_id ON users(sequifi_user_id);
CREATE INDEX IF NOT EXISTS idx_users_office ON users(office);
CREATE INDEX IF NOT EXISTS idx_users_team ON users(team);
CREATE INDEX IF NOT EXISTS idx_users_num_closer_projects ON users(num_closer_projects DESC);
CREATE INDEX IF NOT EXISTS idx_users_num_setter_projects ON users(num_setter_projects DESC);

-- Comments for documentation
COMMENT ON COLUMN users.quickbase_contact_id IS 'QuickBase Contacts table Record ID (field 3) - stable link, never changes';
COMMENT ON COLUMN users.sequifi_user_id IS 'Sequifi user ID for linking to Sequifi system';
COMMENT ON COLUMN users.num_closer_projects IS 'Total projects as closer (from Contacts field 114)';
COMMENT ON COLUMN users.num_setter_projects IS 'Total projects as setter (from Contacts field 278)';
COMMENT ON COLUMN users.office IS 'Office name from RepCard (Contacts field 253) or local assignment';
COMMENT ON COLUMN users.team IS 'Team name from RepCard (Contacts field 255)';
COMMENT ON COLUMN users.is_rookie IS 'Rookie status from RepCard (Contacts field 256)';
COMMENT ON COLUMN users.is_setter IS 'Setter role flag';
COMMENT ON COLUMN users.profile_image_url IS 'RepCard profile image URL (Contacts field 261)';
COMMENT ON COLUMN users.last_synced_from_contacts_at IS 'Last time data was enriched from QuickBase Contacts table';
COMMENT ON COLUMN users.last_synced_from_repcard_at IS 'Last time data was enriched from RepCard API';
