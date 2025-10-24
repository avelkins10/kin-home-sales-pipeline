-- Migration: Add Operations Roles
-- This migration adds operations-specific roles to the users table constraint
-- to support the new Operations app alongside the existing Sales app.

BEGIN;

-- Preflight check: List existing constraints on users.role
-- This helps identify the current constraint name before dropping it
SELECT 
  conname as constraint_name,
  pg_get_constraintdef(oid) as constraint_definition
FROM pg_constraint 
WHERE conrelid = 'users'::regclass 
  AND contype = 'c' 
  AND conname LIKE '%role%';

-- Drop existing role constraint
ALTER TABLE users DROP CONSTRAINT IF EXISTS users_role_check;

-- Add new constraint with operations roles
ALTER TABLE users ADD CONSTRAINT users_role_check 
  CHECK (role IN (
    'closer', 
    'setter', 
    'coordinator', 
    'team_lead', 
    'office_leader', 
    'area_director', 
    'divisional', 
    'regional', 
    'super_admin',
    'operations_coordinator',
    'operations_manager'
  ));

-- Add comments to document operations roles
COMMENT ON CONSTRAINT users_role_check ON users IS 'User roles including sales roles (closer, setter, etc.) and operations roles (operations_coordinator, operations_manager)';

-- Log this migration
INSERT INTO migrations_log (migration_name) 
VALUES ('010_add_operations_roles') 
ON CONFLICT (migration_name) DO NOTHING;

COMMIT;
