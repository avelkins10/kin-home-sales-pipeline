-- migrations/016_repcard_complete_data.sql
-- Complete RepCard data sync - adds missing endpoints
-- Customer Notes, Status Definitions, Calendars, Custom Fields, Leaderboard Snapshots, Teams

BEGIN;

-- Customer Notes
CREATE TABLE IF NOT EXISTS repcard_customer_notes (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_note_id TEXT UNIQUE NOT NULL, -- RepCard uses MongoDB ObjectId as string
  customer_id TEXT REFERENCES repcard_customers(id) ON DELETE CASCADE,
  repcard_customer_id INTEGER NOT NULL,
  user_id TEXT REFERENCES users(id),
  repcard_user_id INTEGER NOT NULL,
  note TEXT NOT NULL,
  created_at TIMESTAMP NOT NULL,
  updated_at TIMESTAMP NOT NULL,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_repcard_notes_repcard_id ON repcard_customer_notes(repcard_note_id);
CREATE INDEX IF NOT EXISTS idx_repcard_notes_customer ON repcard_customer_notes(repcard_customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_notes_repcard_customer ON repcard_customer_notes(repcard_customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_notes_user ON repcard_customer_notes(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_notes_repcard_user ON repcard_customer_notes(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_notes_created_at ON repcard_customer_notes(created_at);

-- Customer Status Definitions
CREATE TABLE IF NOT EXISTS repcard_customer_statuses (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_status_id INTEGER UNIQUE NOT NULL,
  status_name TEXT NOT NULL,
  short_name TEXT,
  type INTEGER,
  colour TEXT,
  icon_name TEXT,
  icon_url TEXT,
  status_order INTEGER,
  is_global BOOLEAN DEFAULT FALSE,
  is_base_status BOOLEAN DEFAULT FALSE,
  company_id INTEGER,
  user_id INTEGER, -- Creator user ID
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_repcard_statuses_repcard_id ON repcard_customer_statuses(repcard_status_id);
CREATE INDEX IF NOT EXISTS idx_repcard_statuses_company ON repcard_customer_statuses(company_id);
CREATE INDEX IF NOT EXISTS idx_repcard_statuses_order ON repcard_customer_statuses(status_order);
CREATE INDEX IF NOT EXISTS idx_repcard_statuses_type ON repcard_customer_statuses(type);

-- Calendars
CREATE TABLE IF NOT EXISTS repcard_calendars (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_calendar_id INTEGER UNIQUE NOT NULL,
  name TEXT NOT NULL,
  company_id INTEGER NOT NULL,
  status TEXT DEFAULT 'active',
  setters INTEGER[], -- Array of RepCard user IDs
  closers INTEGER[], -- Array of RepCard user IDs
  dispatchers INTEGER[], -- Array of RepCard user IDs
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_repcard_calendars_repcard_id ON repcard_calendars(repcard_calendar_id);
CREATE INDEX IF NOT EXISTS idx_repcard_calendars_company ON repcard_calendars(company_id);
CREATE INDEX IF NOT EXISTS idx_repcard_calendars_status ON repcard_calendars(status);

-- Custom Fields
CREATE TABLE IF NOT EXISTS repcard_custom_fields (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_field_id INTEGER UNIQUE NOT NULL,
  entity_type TEXT NOT NULL, -- 'lead', 'customer', 'recruit', 'other'
  internal_name TEXT NOT NULL,
  display_name TEXT NOT NULL,
  field_type TEXT NOT NULL, -- 'number', 'text', 'date', etc.
  data_type TEXT NOT NULL,
  option_values JSONB, -- Array of options for select fields
  company_id INTEGER,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_repcard_fields_repcard_id ON repcard_custom_fields(repcard_field_id);
CREATE INDEX IF NOT EXISTS idx_repcard_fields_entity_type ON repcard_custom_fields(entity_type);
CREATE INDEX IF NOT EXISTS idx_repcard_fields_company ON repcard_custom_fields(company_id);
CREATE INDEX IF NOT EXISTS idx_repcard_fields_internal_name ON repcard_custom_fields(internal_name);

-- Teams (from Offices)
CREATE TABLE IF NOT EXISTS repcard_teams (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_team_id INTEGER UNIQUE NOT NULL,
  team_name TEXT NOT NULL,
  office_id INTEGER NOT NULL,
  repcard_office_id INTEGER,
  team_logo TEXT,
  company_id INTEGER,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

CREATE INDEX IF NOT EXISTS idx_repcard_teams_repcard_id ON repcard_teams(repcard_team_id);
CREATE INDEX IF NOT EXISTS idx_repcard_teams_office ON repcard_teams(office_id);
CREATE INDEX IF NOT EXISTS idx_repcard_teams_repcard_office ON repcard_teams(repcard_office_id);

-- Add team fields to repcard_users if not exists
ALTER TABLE repcard_users ADD COLUMN IF NOT EXISTS team_id INTEGER;
ALTER TABLE repcard_users ADD COLUMN IF NOT EXISTS team_name TEXT;

CREATE INDEX IF NOT EXISTS idx_repcard_users_team ON repcard_users(team_id);

-- Leaderboard Snapshots (store historical leaderboard data)
-- Note: Table may already exist from migration 014, so we'll alter it if needed
-- Add new columns if table exists (safe to re-run)
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS company_id INTEGER;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS leaderboard_name TEXT;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS leaderboard_id TEXT;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS user_id TEXT REFERENCES users(id);
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS customer_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS door_knocks INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS lead_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS appointment_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS avg_door_knocks_per_day NUMERIC DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS avg_distance_per_knocks NUMERIC DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS avg_rating NUMERIC DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS video_viewed INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS review_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS referral_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS engagement_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS card_sent_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS recruit_count INTEGER DEFAULT 0;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS item_rank INTEGER;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS office_name TEXT;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS team_id INTEGER;
ALTER TABLE repcard_leaderboard_snapshots ADD COLUMN IF NOT EXISTS team_name TEXT;

-- Update existing rows to have leaderboard_name if null (default to 'custom' for existing rows)
UPDATE repcard_leaderboard_snapshots SET leaderboard_name = COALESCE(leaderboard_name, 'custom') WHERE leaderboard_name IS NULL;

-- Drop old unique constraint if it exists and create new one
DROP INDEX IF EXISTS idx_repcard_leaderboard_snapshots_composite;
CREATE UNIQUE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_unique ON repcard_leaderboard_snapshots(snapshot_date, COALESCE(leaderboard_name, 'custom'), repcard_user_id);

CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_date ON repcard_leaderboard_snapshots(snapshot_date);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_leaderboard ON repcard_leaderboard_snapshots(leaderboard_name);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_user ON repcard_leaderboard_snapshots(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_repcard_user ON repcard_leaderboard_snapshots(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_company ON repcard_leaderboard_snapshots(company_id);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_rank ON repcard_leaderboard_snapshots(item_rank);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_composite ON repcard_leaderboard_snapshots(snapshot_date, leaderboard_name, repcard_user_id);

COMMIT;

