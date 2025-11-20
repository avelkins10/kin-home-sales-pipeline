-- Migration: Make repcard_offices.company_id nullable
-- Reason: RepCard API /offices endpoint may not always return companyId
-- We'll handle NULL values gracefully

BEGIN;

-- Make company_id nullable
ALTER TABLE repcard_offices 
  ALTER COLUMN company_id DROP NOT NULL;

-- Add comment explaining why it's nullable
COMMENT ON COLUMN repcard_offices.company_id IS 'Company ID from RepCard. May be NULL if not provided by API.';

COMMIT;

