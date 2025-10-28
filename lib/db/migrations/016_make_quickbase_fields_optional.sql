-- migrations/016_make_quickbase_fields_optional.sql
-- Make QuickBase fields optional in arrivy_tasks table
-- Tasks originating in Arrivy don't have QuickBase associations

BEGIN;

-- Make QuickBase project ID nullable
ALTER TABLE arrivy_tasks ALTER COLUMN quickbase_project_id DROP NOT NULL;

-- Make QuickBase record ID nullable
ALTER TABLE arrivy_tasks ALTER COLUMN quickbase_record_id DROP NOT NULL;

-- Clean up placeholder data
-- Update tasks with ARRIVY-* placeholder project IDs to null
UPDATE arrivy_tasks 
SET quickbase_project_id = NULL 
WHERE quickbase_project_id LIKE 'ARRIVY-%';

-- Update tasks with placeholder record ID (0) to null
UPDATE arrivy_tasks 
SET quickbase_record_id = NULL 
WHERE quickbase_record_id = 0;

-- Update table comment to reflect new reality
COMMENT ON TABLE arrivy_tasks IS 'Tasks from Arrivy field operations. May be linked to QuickBase projects via quickbase_project_id.';

-- Update column comments
COMMENT ON COLUMN arrivy_tasks.quickbase_project_id IS 'Optional link to QuickBase project (external_id in Arrivy). NULL for tasks originating in Arrivy.';
COMMENT ON COLUMN arrivy_tasks.quickbase_record_id IS 'Optional QuickBase record ID. NULL for tasks originating in Arrivy.';

COMMIT;

