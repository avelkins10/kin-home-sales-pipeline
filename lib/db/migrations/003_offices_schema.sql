BEGIN;

-- Add last_login_at column to users table
ALTER TABLE users ADD COLUMN IF NOT EXISTS last_login_at TIMESTAMP;

-- Add region column to users table
ALTER TABLE users ADD COLUMN IF NOT EXISTS region TEXT;

-- Add is_active column to users table (if not exists)
ALTER TABLE users ADD COLUMN IF NOT EXISTS is_active BOOLEAN DEFAULT true;

-- Create offices table
CREATE TABLE IF NOT EXISTS offices (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  name TEXT UNIQUE NOT NULL,
  region TEXT NOT NULL CHECK (region IN ('southwest', 'southeast', 'midwest', 'northeast', 'west')),
  leader_id TEXT REFERENCES users(id) ON DELETE SET NULL,
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Create indexes
CREATE INDEX IF NOT EXISTS idx_offices_region ON offices(region);
CREATE INDEX IF NOT EXISTS idx_offices_leader ON offices(leader_id);
CREATE INDEX IF NOT EXISTS idx_users_office ON users(sales_office);
CREATE INDEX IF NOT EXISTS idx_users_region ON users(region);
CREATE INDEX IF NOT EXISTS idx_users_active ON users(is_active);

COMMIT;
