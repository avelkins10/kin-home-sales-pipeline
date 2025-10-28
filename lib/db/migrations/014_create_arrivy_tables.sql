-- migrations/014_create_arrivy_tables.sql
-- Arrivy integration tables for field operations tracking

BEGIN;

-- Create function to update updated_at timestamp if it doesn't exist
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = NOW();
    RETURN NEW;
END;
$$ language 'plpgsql';

-- Arrivy Tasks Table
-- Stores tasks from Arrivy field operations (may be linked to QuickBase)
CREATE TABLE IF NOT EXISTS arrivy_tasks (
  id SERIAL PRIMARY KEY,
  arrivy_task_id BIGINT UNIQUE NOT NULL,
  url_safe_id TEXT NOT NULL,
  quickbase_project_id TEXT,
  quickbase_record_id INTEGER,
  customer_name TEXT,
  customer_phone TEXT,
  customer_email TEXT,
  customer_address TEXT,
  task_type TEXT CHECK (task_type IN ('survey', 'install', 'inspection', 'service', 'other')),
  scheduled_start TIMESTAMPTZ,
  scheduled_end TIMESTAMPTZ,
  assigned_entity_ids BIGINT[],
  current_status TEXT,
  tracker_url TEXT,
  template_id TEXT,
  extra_fields JSONB,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  synced_at TIMESTAMPTZ
);

-- Arrivy Entities Table
-- Stores field crew members synced from QuickBase or created directly
CREATE TABLE IF NOT EXISTS arrivy_entities (
  id SERIAL PRIMARY KEY,
  arrivy_entity_id BIGINT UNIQUE NOT NULL,
  name TEXT NOT NULL,
  email TEXT,
  phone TEXT,
  entity_type TEXT,
  quickbase_user_id TEXT,
  extra_fields JSONB,
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW()
);

-- Arrivy Events Table
-- Stores webhook events from Arrivy for audit trail and activity feed
CREATE TABLE IF NOT EXISTS arrivy_events (
  id SERIAL PRIMARY KEY,
  event_id BIGINT UNIQUE NOT NULL,
  event_type TEXT NOT NULL,
  event_sub_type TEXT,
  event_time TIMESTAMPTZ NOT NULL,
  arrivy_task_id BIGINT REFERENCES arrivy_tasks(arrivy_task_id) ON DELETE CASCADE,
  reporter_id BIGINT,
  reporter_name TEXT,
  title TEXT,
  message TEXT,
  object_fields JSONB,
  extra_fields JSONB,
  is_transient BOOLEAN DEFAULT FALSE,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Arrivy Task Status Table
-- Stores status updates for tasks (ENROUTE, STARTED, COMPLETE, etc.)
CREATE TABLE IF NOT EXISTS arrivy_task_status (
  id SERIAL PRIMARY KEY,
  arrivy_task_id BIGINT REFERENCES arrivy_tasks(arrivy_task_id) ON DELETE CASCADE,
  status_type TEXT NOT NULL,
  reporter_id BIGINT,
  reporter_name TEXT,
  reported_at TIMESTAMPTZ NOT NULL,
  notes TEXT,
  has_attachments BOOLEAN DEFAULT FALSE,
  visible_to_customer BOOLEAN DEFAULT TRUE,
  source TEXT,
  created_at TIMESTAMPTZ DEFAULT NOW()
);

-- Create Indexes
CREATE INDEX IF NOT EXISTS idx_arrivy_tasks_qb_project ON arrivy_tasks(quickbase_project_id);
CREATE INDEX IF NOT EXISTS idx_arrivy_tasks_status ON arrivy_tasks(current_status);
CREATE INDEX IF NOT EXISTS idx_arrivy_tasks_scheduled ON arrivy_tasks(scheduled_start);
CREATE INDEX IF NOT EXISTS idx_arrivy_tasks_type ON arrivy_tasks(task_type);
CREATE INDEX IF NOT EXISTS idx_arrivy_tasks_synced ON arrivy_tasks(synced_at);

CREATE INDEX IF NOT EXISTS idx_arrivy_entities_email ON arrivy_entities(email);
CREATE INDEX IF NOT EXISTS idx_arrivy_entities_qb_user ON arrivy_entities(quickbase_user_id);

CREATE INDEX IF NOT EXISTS idx_arrivy_events_task ON arrivy_events(arrivy_task_id, event_time DESC);
CREATE INDEX IF NOT EXISTS idx_arrivy_events_type ON arrivy_events(event_type, event_time DESC);
CREATE INDEX IF NOT EXISTS idx_arrivy_events_time ON arrivy_events(event_time DESC);

CREATE INDEX IF NOT EXISTS idx_arrivy_task_status_task ON arrivy_task_status(arrivy_task_id, reported_at DESC);
CREATE INDEX IF NOT EXISTS idx_arrivy_task_status_type ON arrivy_task_status(status_type);

-- Create Triggers for updated_at columns
DROP TRIGGER IF EXISTS update_arrivy_tasks_updated_at ON arrivy_tasks;
CREATE TRIGGER update_arrivy_tasks_updated_at
  BEFORE UPDATE ON arrivy_tasks
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

DROP TRIGGER IF EXISTS update_arrivy_entities_updated_at ON arrivy_entities;
CREATE TRIGGER update_arrivy_entities_updated_at
  BEFORE UPDATE ON arrivy_entities
  FOR EACH ROW
  EXECUTE FUNCTION update_updated_at_column();

-- Add comments for documentation
COMMENT ON TABLE arrivy_tasks IS 'Tasks from Arrivy field operations. May be linked to QuickBase projects via quickbase_project_id.';
COMMENT ON TABLE arrivy_entities IS 'Field crew members and technicians in Arrivy';
COMMENT ON TABLE arrivy_events IS 'Webhook events from Arrivy for audit trail and activity feed';
COMMENT ON TABLE arrivy_task_status IS 'Status updates for Arrivy tasks (ENROUTE, STARTED, COMPLETE, etc.)';

COMMENT ON COLUMN arrivy_tasks.arrivy_task_id IS 'Unique task ID from Arrivy API';
COMMENT ON COLUMN arrivy_tasks.url_safe_id IS 'URL-safe ID for customer tracker links';
COMMENT ON COLUMN arrivy_tasks.quickbase_project_id IS 'Optional link to QuickBase project (external_id in Arrivy). NULL for tasks originating in Arrivy.';
COMMENT ON COLUMN arrivy_tasks.quickbase_record_id IS 'Optional QuickBase record ID. NULL for tasks originating in Arrivy.';
COMMENT ON COLUMN arrivy_tasks.tracker_url IS 'Customer-facing live tracker URL';
COMMENT ON COLUMN arrivy_tasks.assigned_entity_ids IS 'Array of Arrivy entity IDs assigned to this task';

COMMENT ON COLUMN arrivy_entities.arrivy_entity_id IS 'Unique entity ID from Arrivy API';
COMMENT ON COLUMN arrivy_entities.quickbase_user_id IS 'Linked QuickBase user ID if applicable';

COMMENT ON COLUMN arrivy_events.event_id IS 'Unique event ID from Arrivy webhook';
COMMENT ON COLUMN arrivy_events.is_transient IS 'True if this event is transient and may be removed/changed';

COMMIT;

