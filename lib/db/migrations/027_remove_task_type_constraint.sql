-- Migration 027: Remove task_type CHECK constraint to allow detailed task types
-- This allows task types like "Surveys - Site Survey", "Installations - Full Install", etc.
-- instead of just simple types like 'survey', 'install', 'inspection', 'service'

-- Drop the CHECK constraint on task_type
ALTER TABLE arrivy_tasks
DROP CONSTRAINT IF EXISTS arrivy_tasks_task_type_check;

-- Verify the constraint is removed
-- (No check constraint, task_type can now be any TEXT value)
