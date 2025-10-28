-- migrations/016_drop_assigned_entity_ids_array.sql
-- Drop the legacy assigned_entity_ids array column after verifying join table works
-- ⚠️ ONLY RUN THIS AFTER:
-- 1. Migration 015 has been executed
-- 2. Application code updated to use arrivy_task_entities join table
-- 3. Thorough testing confirms join table works correctly

BEGIN;

-- Drop the legacy array column
-- This will fail if any code still depends on this column
ALTER TABLE arrivy_tasks 
  DROP COLUMN IF EXISTS assigned_entity_ids;

COMMIT;

-- Rollback instructions:
-- If you need to rollback, you'll need to:
-- 1. Add the column back: ALTER TABLE arrivy_tasks ADD COLUMN assigned_entity_ids BIGINT[];
-- 2. Repopulate from join table: UPDATE arrivy_tasks t SET assigned_entity_ids = ARRAY(SELECT arrivy_entity_id FROM arrivy_task_entities WHERE arrivy_task_id = t.arrivy_task_id);

