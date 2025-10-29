-- migrations/014_repcard_comprehensive_tables.sql
-- Comprehensive RepCard data sync tables
-- Adds support for syncing users, offices, attachments, and leaderboard snapshots

BEGIN;

-- RepCard Users Sync Table
-- Stores RepCard user data for enrichment and reference
CREATE TABLE IF NOT EXISTS repcard_users (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_user_id INTEGER UNIQUE NOT NULL,
  company_id INTEGER NOT NULL,
  office_id INTEGER,
  first_name TEXT,
  last_name TEXT,
  email TEXT,
  phone TEXT,
  username TEXT,
  role TEXT,
  status INTEGER DEFAULT 1, -- 1 = active, 0 = inactive
  office_name TEXT,
  team TEXT,
  job_title TEXT,
  profile_image TEXT,
  rating TEXT,
  bio TEXT,
  badge_id TEXT,
  qr_code TEXT,
  first_verified_door_knock TIMESTAMP,
  first_appointment TIMESTAMP,
  created_at TIMESTAMP,
  updated_at TIMESTAMP,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Offices Sync Table
-- Stores RepCard office data for reference and mapping
CREATE TABLE IF NOT EXISTS repcard_offices (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_office_id INTEGER UNIQUE NOT NULL,
  company_id INTEGER NOT NULL,
  name TEXT NOT NULL,
  address TEXT,
  city TEXT,
  state TEXT,
  zip_code TEXT,
  created_at TIMESTAMP,
  updated_at TIMESTAMP,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Customer Attachments
-- Stores attachments (power bills, documents) linked to customers
CREATE TABLE IF NOT EXISTS repcard_customer_attachments (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_attachment_id INTEGER UNIQUE NOT NULL,
  customer_id TEXT REFERENCES repcard_customers(id) ON DELETE CASCADE,
  repcard_customer_id INTEGER, -- For easier lookups
  attachment_type TEXT, -- 'power_bill', 'document', 'image', etc.
  file_name TEXT,
  file_url TEXT,
  file_size INTEGER,
  uploaded_by_user_id INTEGER, -- RepCard user ID
  created_at TIMESTAMP,
  updated_at TIMESTAMP,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Appointment Attachments
-- Stores attachments linked to specific appointments
CREATE TABLE IF NOT EXISTS repcard_appointment_attachments (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_attachment_id INTEGER UNIQUE NOT NULL,
  appointment_id TEXT REFERENCES repcard_appointments(id) ON DELETE CASCADE,
  repcard_appointment_id INTEGER, -- For easier lookups
  customer_id TEXT REFERENCES repcard_customers(id) ON DELETE CASCADE,
  repcard_customer_id INTEGER,
  attachment_type TEXT,
  file_name TEXT,
  file_url TEXT,
  file_size INTEGER,
  uploaded_by_user_id INTEGER, -- RepCard user ID
  created_at TIMESTAMP,
  updated_at TIMESTAMP,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Leaderboard Snapshots (Optional - for historical tracking)
-- Stores point-in-time leaderboard data for trend analysis
CREATE TABLE IF NOT EXISTS repcard_leaderboard_snapshots (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  snapshot_date DATE NOT NULL,
  metric TEXT NOT NULL, -- 'doors_knocked', 'appointments_set', 'sales_closed', 'revenue', 'quality_score'
  role TEXT, -- 'setter', 'closer', 'all'
  office_id INTEGER,
  repcard_user_id INTEGER NOT NULL,
  rank INTEGER NOT NULL,
  metric_value NUMERIC,
  trend TEXT, -- 'up', 'down', 'same', 'new'
  period_start_date DATE,
  period_end_date DATE,
  raw_data JSONB,
  created_at TIMESTAMP DEFAULT NOW(),
  UNIQUE(snapshot_date, metric, role, office_id, repcard_user_id)
);

-- Indexes for repcard_users
CREATE INDEX IF NOT EXISTS idx_repcard_users_repcard_id ON repcard_users(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_users_email ON repcard_users(email);
CREATE INDEX IF NOT EXISTS idx_repcard_users_office_id ON repcard_users(office_id);
CREATE INDEX IF NOT EXISTS idx_repcard_users_status ON repcard_users(status);
CREATE INDEX IF NOT EXISTS idx_repcard_users_updated_at ON repcard_users(updated_at);

-- Indexes for repcard_offices
CREATE INDEX IF NOT EXISTS idx_repcard_offices_repcard_id ON repcard_offices(repcard_office_id);
CREATE INDEX IF NOT EXISTS idx_repcard_offices_company_id ON repcard_offices(company_id);
CREATE INDEX IF NOT EXISTS idx_repcard_offices_name ON repcard_offices(name);

-- Indexes for repcard_customer_attachments
CREATE INDEX IF NOT EXISTS idx_repcard_customer_attachments_repcard_id ON repcard_customer_attachments(repcard_attachment_id);
CREATE INDEX IF NOT EXISTS idx_repcard_customer_attachments_customer ON repcard_customer_attachments(customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_customer_attachments_repcard_customer ON repcard_customer_attachments(repcard_customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_customer_attachments_type ON repcard_customer_attachments(attachment_type);
CREATE INDEX IF NOT EXISTS idx_repcard_customer_attachments_created_at ON repcard_customer_attachments(created_at);

-- Indexes for repcard_appointment_attachments
CREATE INDEX IF NOT EXISTS idx_repcard_appointment_attachments_repcard_id ON repcard_appointment_attachments(repcard_attachment_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointment_attachments_appointment ON repcard_appointment_attachments(appointment_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointment_attachments_repcard_appointment ON repcard_appointment_attachments(repcard_appointment_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointment_attachments_customer ON repcard_appointment_attachments(customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointment_attachments_type ON repcard_appointment_attachments(attachment_type);

-- Indexes for repcard_leaderboard_snapshots
CREATE INDEX IF NOT EXISTS idx_repcard_leaderboard_snapshots_date ON repcard_leaderboard_snapshots(snapshot_date);
CREATE INDEX IF NOT EXISTS idx_repcard_leaderboard_snapshots_metric ON repcard_leaderboard_snapshots(metric);
CREATE INDEX IF NOT EXISTS idx_repcard_leaderboard_snapshots_role ON repcard_leaderboard_snapshots(role);
CREATE INDEX IF NOT EXISTS idx_repcard_leaderboard_snapshots_user ON repcard_leaderboard_snapshots(repcard_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_leaderboard_snapshots_rank ON repcard_leaderboard_snapshots(rank);
CREATE INDEX IF NOT EXISTS idx_repcard_leaderboard_snapshots_composite ON repcard_leaderboard_snapshots(snapshot_date, metric, role);

-- Update sync_log to support new entity types
ALTER TABLE repcard_sync_log DROP CONSTRAINT IF EXISTS repcard_sync_log_entity_type_check;
-- Note: In PostgreSQL, we can't easily modify CHECK constraints, so we'll validate in application code

COMMIT;

