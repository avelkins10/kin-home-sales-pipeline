BEGIN;

-- Add quickbase_office_id column to offices table
-- This stores the QuickBase Record ID from Field 810 for stable ID-based filtering
-- Nullable during migration, can be made NOT NULL after backfilling
ALTER TABLE offices ADD COLUMN IF NOT EXISTS quickbase_office_id INTEGER;

-- Create index for fast lookups when filtering projects by office ID
CREATE INDEX IF NOT EXISTS idx_offices_quickbase_id ON offices(quickbase_office_id);

-- Add unique constraint to ensure one-to-one mapping with QuickBase offices
-- Using a partial index to allow NULL values during migration
CREATE UNIQUE INDEX IF NOT EXISTS idx_offices_quickbase_id_unique
  ON offices(quickbase_office_id)
  WHERE quickbase_office_id IS NOT NULL;

-- Add comment to document the purpose of this column
COMMENT ON COLUMN offices.quickbase_office_id IS 'QuickBase Record ID from Field 810 - stable ID that does not change when office names are updated';

COMMIT;
