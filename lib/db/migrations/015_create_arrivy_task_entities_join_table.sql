-- migrations/015_create_arrivy_task_entities_join_table.sql
-- Replace BIGINT[] with proper join table for task-entity relationships
-- Improves referential integrity and query performance

BEGIN;

-- Create join table for task-entity many-to-many relationship
CREATE TABLE IF NOT EXISTS arrivy_task_entities (
  id SERIAL PRIMARY KEY,
  arrivy_task_id BIGINT NOT NULL REFERENCES arrivy_tasks(arrivy_task_id) ON DELETE CASCADE,
  arrivy_entity_id BIGINT NOT NULL REFERENCES arrivy_entities(arrivy_entity_id) ON DELETE CASCADE,
  assigned_at TIMESTAMPTZ DEFAULT NOW(),
  assigned_by TEXT,
  notes TEXT,
  UNIQUE(arrivy_task_id, arrivy_entity_id)
);

-- Create indexes for efficient queries
CREATE INDEX IF NOT EXISTS idx_arrivy_task_entities_task 
  ON arrivy_task_entities(arrivy_task_id);

CREATE INDEX IF NOT EXISTS idx_arrivy_task_entities_entity 
  ON arrivy_task_entities(arrivy_entity_id);

CREATE INDEX IF NOT EXISTS idx_arrivy_task_entities_composite 
  ON arrivy_task_entities(arrivy_task_id, arrivy_entity_id);

CREATE INDEX IF NOT EXISTS idx_arrivy_task_entities_assigned_at 
  ON arrivy_task_entities(assigned_at DESC);

-- Backfill from existing assigned_entity_ids array
-- Only migrate records where assigned_entity_ids is not null and not empty
INSERT INTO arrivy_task_entities (arrivy_task_id, arrivy_entity_id, assigned_at)
SELECT 
  t.arrivy_task_id,
  unnest(t.assigned_entity_ids) as entity_id,
  t.updated_at
FROM arrivy_tasks t
WHERE t.assigned_entity_ids IS NOT NULL 
  AND array_length(t.assigned_entity_ids, 1) > 0
ON CONFLICT (arrivy_task_id, arrivy_entity_id) DO NOTHING;

-- Add comment for documentation
COMMENT ON TABLE arrivy_task_entities IS 'Join table for many-to-many relationship between tasks and entities (crew members)';
COMMENT ON COLUMN arrivy_task_entities.arrivy_task_id IS 'Reference to arrivy_tasks table';
COMMENT ON COLUMN arrivy_task_entities.arrivy_entity_id IS 'Reference to arrivy_entities table';
COMMENT ON COLUMN arrivy_task_entities.assigned_at IS 'When this entity was assigned to the task';
COMMENT ON COLUMN arrivy_task_entities.assigned_by IS 'User who made the assignment (optional)';

COMMIT;

-- Note: The assigned_entity_ids column in arrivy_tasks is kept for backward compatibility
-- during transition period. After verifying the join table works correctly, you can:
-- 1. Update all application code to use the join table
-- 2. Run a follow-up migration to drop the assigned_entity_ids column
-- 3. See migration 018_drop_assigned_entity_ids_array.sql (run only after code uses arrivy_task_entities exclusively)

