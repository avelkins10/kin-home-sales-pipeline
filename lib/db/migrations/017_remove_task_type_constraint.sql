-- migrations/017_remove_task_type_constraint.sql
-- Remove CHECK constraint on task_type to allow any upstream values from Arrivy
-- This migration only needed if you ran 014 with the constraint before this fix

BEGIN;

-- Drop the CHECK constraint if it exists
-- This will fail silently if constraint doesn't exist, which is fine
DO $$
BEGIN
    ALTER TABLE arrivy_tasks DROP CONSTRAINT IF EXISTS arrivy_tasks_task_type_check;
EXCEPTION
    WHEN undefined_object THEN
        -- Constraint doesn't exist, nothing to do
        NULL;
END$$;

COMMIT;

-- Note: Migration 014 has been updated to not create this constraint
-- This migration is only needed if you ran the old version of 014

