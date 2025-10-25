CREATE TABLE IF NOT EXISTS user_sync_runs (
  id SERIAL PRIMARY KEY,
  started_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMP WITH TIME ZONE,
  status TEXT NOT NULL CHECK (status IN ('running', 'success', 'partial', 'failed')),
  total_users INTEGER NOT NULL DEFAULT 0,
  enriched INTEGER NOT NULL DEFAULT 0,
  not_found INTEGER NOT NULL DEFAULT 0,
  already_up_to_date INTEGER NOT NULL DEFAULT 0,
  errors INTEGER NOT NULL DEFAULT 0,
  error_details JSONB,
  not_found_samples JSONB,
  triggered_by TEXT NOT NULL CHECK (triggered_by IN ('cron', 'manual')),
  triggered_by_user_id TEXT,
  execution_time_ms INTEGER,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_user_sync_runs_started_at ON user_sync_runs(started_at DESC);
CREATE INDEX idx_user_sync_runs_status ON user_sync_runs(status);
CREATE INDEX idx_user_sync_runs_triggered_by ON user_sync_runs(triggered_by);

COMMENT ON TABLE user_sync_runs IS 'Tracks user sync runs from QuickBase Contacts table';
COMMENT ON COLUMN user_sync_runs.status IS 'running: in progress, success: all users synced, partial: some errors, failed: sync failed';
COMMENT ON COLUMN user_sync_runs.triggered_by IS 'cron: scheduled run, manual: admin-triggered';
COMMENT ON COLUMN user_sync_runs.error_details IS 'Array of {email, error} objects for failed syncs';
COMMENT ON COLUMN user_sync_runs.not_found_samples IS 'Array of sample emails not found in Contacts';
