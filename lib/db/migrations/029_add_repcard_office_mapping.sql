-- Migration 029: Add Office Foreign Keys and Mapping Table
-- This enables proper office filtering and attribution
-- Maps RepCard office IDs to QuickBase office IDs for cross-system consistency

-- Create office mapping table
CREATE TABLE IF NOT EXISTS repcard_office_mappings (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_office_id INTEGER NOT NULL REFERENCES repcard_offices(repcard_office_id) ON DELETE CASCADE,
  quickbase_office_id INTEGER NOT NULL REFERENCES offices(quickbase_office_id) ON DELETE CASCADE,
  repcard_office_name TEXT,
  quickbase_office_name TEXT,
  confidence_score DECIMAL(3,2) DEFAULT 1.00, -- 1.00 = exact match, <1.00 = fuzzy match
  mapping_method TEXT CHECK (mapping_method IN ('exact_name', 'fuzzy_name', 'manual', 'id_match')),
  created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
  created_by TEXT,
  UNIQUE(repcard_office_id, quickbase_office_id)
);

-- Add indexes for fast lookups in both directions
CREATE INDEX IF NOT EXISTS idx_repcard_office_mappings_repcard_id
ON repcard_office_mappings(repcard_office_id);

CREATE INDEX IF NOT EXISTS idx_repcard_office_mappings_quickbase_id
ON repcard_office_mappings(quickbase_office_id);

-- Clean up invalid office_id references before adding foreign keys
-- Set office_id to NULL if it references a non-existent office
UPDATE repcard_customers
SET office_id = NULL
WHERE office_id IS NOT NULL
  AND NOT EXISTS (
    SELECT 1 FROM repcard_offices
    WHERE repcard_office_id = repcard_customers.office_id
  );

UPDATE repcard_appointments
SET office_id = NULL
WHERE office_id IS NOT NULL
  AND NOT EXISTS (
    SELECT 1 FROM repcard_offices
    WHERE repcard_office_id = repcard_appointments.office_id
  );

-- Add foreign key constraints to link customers and appointments to offices
-- Note: We make these DEFERRABLE INITIALLY DEFERRED to allow bulk inserts during sync
ALTER TABLE repcard_customers
DROP CONSTRAINT IF EXISTS fk_repcard_customers_office;

ALTER TABLE repcard_customers
ADD CONSTRAINT fk_repcard_customers_office
FOREIGN KEY (office_id)
REFERENCES repcard_offices(repcard_office_id)
ON DELETE SET NULL
DEFERRABLE INITIALLY DEFERRED;

ALTER TABLE repcard_appointments
DROP CONSTRAINT IF EXISTS fk_repcard_appointments_office;

ALTER TABLE repcard_appointments
ADD CONSTRAINT fk_repcard_appointments_office
FOREIGN KEY (office_id)
REFERENCES repcard_offices(repcard_office_id)
ON DELETE SET NULL
DEFERRABLE INITIALLY DEFERRED;

-- Create helper function to find QuickBase office ID from RepCard office ID
CREATE OR REPLACE FUNCTION get_quickbase_office_id(p_repcard_office_id INTEGER)
RETURNS INTEGER AS $$
  SELECT quickbase_office_id
  FROM repcard_office_mappings
  WHERE repcard_office_id = p_repcard_office_id
  ORDER BY confidence_score DESC
  LIMIT 1;
$$ LANGUAGE SQL STABLE;

-- Create helper function to find RepCard office ID from QuickBase office ID
CREATE OR REPLACE FUNCTION get_repcard_office_id(p_quickbase_office_id INTEGER)
RETURNS INTEGER AS $$
  SELECT repcard_office_id
  FROM repcard_office_mappings
  WHERE quickbase_office_id = p_quickbase_office_id
  ORDER BY confidence_score DESC
  LIMIT 1;
$$ LANGUAGE SQL STABLE;

-- Auto-populate mappings where office names match exactly
INSERT INTO repcard_office_mappings (
  repcard_office_id,
  quickbase_office_id,
  repcard_office_name,
  quickbase_office_name,
  confidence_score,
  mapping_method,
  created_by
)
SELECT DISTINCT
  ro.repcard_office_id,
  o.quickbase_office_id,
  ro.name as repcard_office_name,
  o.name as quickbase_office_name,
  1.00 as confidence_score,
  'exact_name' as mapping_method,
  'migration_029' as created_by
FROM repcard_offices ro
INNER JOIN offices o ON LOWER(TRIM(ro.name)) = LOWER(TRIM(o.name))
WHERE ro.name IS NOT NULL
  AND o.name IS NOT NULL
ON CONFLICT (repcard_office_id, quickbase_office_id) DO NOTHING;

-- Auto-populate mappings where office names are similar (fuzzy match)
-- This uses ILIKE for case-insensitive partial matching
INSERT INTO repcard_office_mappings (
  repcard_office_id,
  quickbase_office_id,
  repcard_office_name,
  quickbase_office_name,
  confidence_score,
  mapping_method,
  created_by
)
SELECT DISTINCT
  ro.repcard_office_id,
  o.quickbase_office_id,
  ro.name as repcard_office_name,
  o.name as quickbase_office_name,
  0.85 as confidence_score,
  'fuzzy_name' as mapping_method,
  'migration_029' as created_by
FROM repcard_offices ro
INNER JOIN offices o ON (
  LOWER(TRIM(ro.name)) ILIKE '%' || LOWER(TRIM(o.name)) || '%'
  OR LOWER(TRIM(o.name)) ILIKE '%' || LOWER(TRIM(ro.name)) || '%'
)
WHERE ro.name IS NOT NULL
  AND o.name IS NOT NULL
  -- Exclude if exact match already exists
  AND NOT EXISTS (
    SELECT 1 FROM repcard_office_mappings m
    WHERE m.repcard_office_id = ro.repcard_office_id
      AND m.quickbase_office_id = o.quickbase_office_id
  )
ON CONFLICT (repcard_office_id, quickbase_office_id) DO NOTHING;

-- Create view showing all offices with their mappings
CREATE OR REPLACE VIEW repcard_offices_with_mappings AS
SELECT
  ro.repcard_office_id,
  ro.name as repcard_name,
  ro.city as repcard_city,
  ro.state as repcard_state,
  rom.quickbase_office_id,
  o.name as quickbase_name,
  rom.confidence_score,
  rom.mapping_method,
  CASE
    WHEN rom.quickbase_office_id IS NOT NULL THEN 'mapped'
    ELSE 'unmapped'
  END as mapping_status
FROM repcard_offices ro
LEFT JOIN repcard_office_mappings rom ON rom.repcard_office_id = ro.repcard_office_id
LEFT JOIN offices o ON o.quickbase_office_id = rom.quickbase_office_id
ORDER BY ro.name;

-- Add comments
COMMENT ON TABLE repcard_office_mappings IS 'Maps RepCard office IDs to QuickBase office IDs for consistent filtering';
COMMENT ON COLUMN repcard_office_mappings.confidence_score IS '1.00 = exact match, <1.00 = fuzzy/manual match';
COMMENT ON COLUMN repcard_office_mappings.mapping_method IS 'How the mapping was created: exact_name, fuzzy_name, manual, or id_match';
COMMENT ON FUNCTION get_quickbase_office_id(INTEGER) IS 'Returns QuickBase office ID for a given RepCard office ID';
COMMENT ON FUNCTION get_repcard_office_id(INTEGER) IS 'Returns RepCard office ID for a given QuickBase office ID';
COMMENT ON VIEW repcard_offices_with_mappings IS 'Shows all RepCard offices with their QuickBase mappings';
