-- Migration: Make repcard_users.company_id nullable
-- Reason: RepCard API /users/minimal endpoint doesn't return companyId
-- We'll backfill company_id from offices after offices are synced

BEGIN;

-- Make company_id nullable
ALTER TABLE repcard_users 
  ALTER COLUMN company_id DROP NOT NULL;

-- Add comment explaining why it's nullable
COMMENT ON COLUMN repcard_users.company_id IS 'Company ID from RepCard. May be NULL initially - backfilled from repcard_offices after offices sync.';

COMMIT;

