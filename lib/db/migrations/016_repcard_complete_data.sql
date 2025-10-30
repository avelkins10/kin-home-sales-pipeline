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
CREATE TABLE IF NOT EXISTS repcard_leaderboard_snapshots (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  snapshot_date DATE NOT NULL,
  leaderboard_name TEXT NOT NULL,
  leaderboard_id TEXT NOT NULL, -- RepCard leaderboard _id (MongoDB ObjectId)
  company_id INTEGER NOT NULL,
  user_id TEXT REFERENCES users(id),
  repcard_user_id INTEGER NOT NULL,
  -- Metrics from D2D Leaderboard
  customer_count INTEGER DEFAULT 0,
  door_knocks INTEGER DEFAULT 0,
  lead_count INTEGER DEFAULT 0,
  appointment_count INTEGER DEFAULT 0,
  avg_door_knocks_per_day NUMERIC DEFAULT 0,
  avg_distance_per_knocks NUMERIC DEFAULT 0,
  -- Metrics from Overview Leaderboard
  avg_rating NUMERIC DEFAULT 0,
  video_viewed INTEGER DEFAULT 0,
  -- Metrics from Engagements Leaderboard
  review_count INTEGER DEFAULT 0,
  referral_count INTEGER DEFAULT 0,
  engagement_count INTEGER DEFAULT 0,
  card_sent_count INTEGER DEFAULT 0,
  -- Metrics from Recruiting Leaderboard
  recruit_count INTEGER DEFAULT 0,
  -- Ranking
  item_rank INTEGER,
  office_id INTEGER,
  office_name TEXT,
  team_id INTEGER,
  team_name TEXT,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(snapshot_date, leaderboard_name, repcard_user_id)
);

CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_date ON repcard_leaderboard_snapshots(snapshot_date);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_leaderboard ON repcard_leaderboard_snapshots(leaderboard_name);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_user ON repcard_leaderboard_snapshots(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_repcard_user ON repcard_leaderboard_snapshots(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_company ON repcard_leaderboard_snapshots(company_id);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_rank ON repcard_leaderboard_snapshots(item_rank);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_snapshots_composite ON repcard_leaderboard_snapshots(snapshot_date, leaderboard_name, repcard_user_id);

COMMIT;

