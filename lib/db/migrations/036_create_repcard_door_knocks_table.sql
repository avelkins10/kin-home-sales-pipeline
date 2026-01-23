-- migrations/036_create_repcard_door_knocks_table.sql
-- Create table for door knock events from RepCard webhooks
-- Doors knocked are tracked separately from customers (a door knock can happen before a customer is created)

BEGIN;

-- RepCard Door Knocks Table
-- Stores door knock events from webhooks
CREATE TABLE IF NOT EXISTS repcard_door_knocks (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_door_knock_id TEXT UNIQUE, -- RepCard event ID if available
  setter_user_id INTEGER NOT NULL, -- RepCard user ID who knocked
  repcard_customer_id INTEGER, -- Customer/contact ID if available (may be null if door knock happens before customer creation)
  customer_id TEXT REFERENCES repcard_customers(id) ON DELETE SET NULL, -- Link to our customer table if exists
  office_id INTEGER, -- Office ID
  door_knocked_at TIMESTAMPTZ NOT NULL, -- When the door was knocked
  status TEXT, -- Status from door knock (e.g., "Not Interested", "Not Home", etc.)
  contact_distance NUMERIC(10, 2), -- Distance from setter location
  latitude NUMERIC(10, 8),
  longitude NUMERIC(11, 8),
  verified BOOLEAN DEFAULT FALSE, -- Whether this is a verified door knock
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  raw_data JSONB,
  synced_at TIMESTAMPTZ DEFAULT NOW()
);

-- Indexes for performance
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_setter ON repcard_door_knocks(setter_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_customer ON repcard_door_knocks(repcard_customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_door_knocked_at ON repcard_door_knocks(door_knocked_at);
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_office ON repcard_door_knocks(office_id);
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_verified ON repcard_door_knocks(verified) WHERE verified = TRUE;
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_date_range ON repcard_door_knocks(setter_user_id, door_knocked_at);

-- Comments
COMMENT ON TABLE repcard_door_knocks IS 'Stores door knock events from RepCard webhooks. Doors knocked are tracked separately from customers.';
COMMENT ON COLUMN repcard_door_knocks.setter_user_id IS 'RepCard user ID of the setter who knocked the door';
COMMENT ON COLUMN repcard_door_knocks.repcard_customer_id IS 'RepCard customer/contact ID if door knock is associated with a customer';
COMMENT ON COLUMN repcard_door_knocks.door_knocked_at IS 'Timestamp when the door was knocked (in RepCard timezone, stored as UTC)';
COMMENT ON COLUMN repcard_door_knocks.verified IS 'Whether this is a verified door knock (from verifiedDoorKnocks array)';

COMMIT;
