BEGIN;

-- Create offices table
CREATE TABLE IF NOT EXISTS offices (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  name TEXT UNIQUE NOT NULL,
  region TEXT,
  leader_id TEXT,
  is_active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMP NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP NOT NULL DEFAULT NOW()
);

-- Add columns to users table if they don't exist
ALTER TABLE users ADD COLUMN IF NOT EXISTS last_login_at TIMESTAMP;
ALTER TABLE users ADD COLUMN IF NOT EXISTS region TEXT;

-- Add foreign key constraint for office leader
ALTER TABLE offices ADD CONSTRAINT IF NOT EXISTS fk_offices_leader 
  FOREIGN KEY (leader_id) REFERENCES users(id) ON DELETE SET NULL;

-- Add check constraint for region enum
ALTER TABLE offices ADD CONSTRAINT IF NOT EXISTS check_offices_region 
  CHECK (region IN ('southwest', 'southeast', 'midwest', 'northeast', 'west'));

-- Create performance indexes
CREATE INDEX IF NOT EXISTS idx_offices_region ON offices(region);
CREATE INDEX IF NOT EXISTS idx_offices_leader ON offices(leader_id);
CREATE INDEX IF NOT EXISTS idx_users_office ON users USING GIN (sales_office);
CREATE INDEX IF NOT EXISTS idx_users_region ON users(region);
CREATE INDEX IF NOT EXISTS idx_users_active ON users(is_active) WHERE is_active = true;

COMMIT;
