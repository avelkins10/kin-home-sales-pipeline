-- Migration 039: RepCard Metric Configuration Table
-- Allows admins to configure which dispositions/statuses count toward each metric
-- This makes the system flexible and adaptable to business rule changes without code changes

BEGIN;

-- Create configuration table
CREATE TABLE IF NOT EXISTS repcard_metric_config (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  metric_name TEXT NOT NULL,  -- e.g., 'door_knocked', 'appointment_set', 'sit', 'sat_closed'
  disposition_pattern TEXT,   -- Pattern to match disposition field (ILIKE pattern, e.g., '%cancel%')
  status_category TEXT,       -- Exact status_category match (e.g., 'cancelled', 'sat_closed')
  is_included BOOLEAN DEFAULT TRUE,  -- Whether this pattern includes or excludes
  priority INTEGER DEFAULT 0, -- Higher priority patterns checked first
  created_at TIMESTAMPTZ DEFAULT NOW(),
  updated_at TIMESTAMPTZ DEFAULT NOW(),
  UNIQUE(metric_name, disposition_pattern, status_category)
);

-- Index for fast lookups
CREATE INDEX IF NOT EXISTS idx_repcard_metric_config_metric ON repcard_metric_config(metric_name);
CREATE INDEX IF NOT EXISTS idx_repcard_metric_config_priority ON repcard_metric_config(metric_name, priority DESC);

-- Function to check if a disposition/status should count for a metric
CREATE OR REPLACE FUNCTION should_count_for_metric(
  p_metric_name TEXT,
  p_disposition TEXT,
  p_status_category TEXT
) RETURNS BOOLEAN AS $$
DECLARE
  v_result BOOLEAN := FALSE;
  v_config RECORD;
BEGIN
  -- Get all configurations for this metric, ordered by priority (highest first)
  FOR v_config IN
    SELECT * FROM repcard_metric_config
    WHERE metric_name = p_metric_name
    ORDER BY priority DESC, created_at ASC
  LOOP
    -- Check if this configuration matches
    -- Match if:
    -- 1. disposition_pattern matches (if provided) AND
    -- 2. status_category matches (if provided) OR both are NULL (catch-all)
    IF (
      (v_config.disposition_pattern IS NULL OR (p_disposition IS NOT NULL AND p_disposition ILIKE v_config.disposition_pattern))
      AND
      (v_config.status_category IS NULL OR p_status_category = v_config.status_category)
    ) THEN
      -- This configuration matches, use its is_included value
      -- First match wins (due to priority ordering)
      RETURN v_config.is_included;
    END IF;
  END LOOP;
  
  -- If no configuration matches, default to FALSE (conservative)
  RETURN FALSE;
END;
$$ LANGUAGE plpgsql STABLE;

-- Insert default configurations (matching current behavior)
-- These can be modified by admins through the UI

-- Door Knocked: All customers created (no filter needed, but can exclude if needed)
INSERT INTO repcard_metric_config (metric_name, disposition_pattern, status_category, is_included, priority) VALUES
  ('door_knocked', NULL, NULL, TRUE, 0)
ON CONFLICT (metric_name, disposition_pattern, status_category) DO NOTHING;

-- Appointment Set: Exclude ONLY reschedules (for gamification - include cancelled/no-show)
INSERT INTO repcard_metric_config (metric_name, disposition_pattern, status_category, is_included, priority) VALUES
  ('appointment_set', '%reschedule%', NULL, FALSE, 10),
  ('appointment_set', NULL, 'rescheduled', FALSE, 10),
  ('appointment_set', NULL, NULL, TRUE, 0)  -- Include everything else
ON CONFLICT (metric_name, disposition_pattern, status_category) DO NOTHING;

-- Sit: Include sat_closed, sat_no_close, completed (any appointment where closer sat with customer)
INSERT INTO repcard_metric_config (metric_name, disposition_pattern, status_category, is_included, priority) VALUES
  ('sit', '%sat.closed%', NULL, TRUE, 10),
  ('sit', '%sat_closed%', NULL, TRUE, 10),
  ('sit', '%closed%', NULL, TRUE, 10),
  ('sit', NULL, 'sat_closed', TRUE, 10),
  ('sit', NULL, 'sat_no_close', TRUE, 10),
  ('sit', NULL, 'completed', TRUE, 10)
ON CONFLICT (metric_name, disposition_pattern, status_category) DO NOTHING;

-- Sat Closed: Only sat_closed and completed
INSERT INTO repcard_metric_config (metric_name, disposition_pattern, status_category, is_included, priority) VALUES
  ('sat_closed', '%sat.closed%', NULL, TRUE, 10),
  ('sat_closed', '%sat_closed%', NULL, TRUE, 10),
  ('sat_closed', '%closed%', NULL, TRUE, 10),
  ('sat_closed', NULL, 'sat_closed', TRUE, 10),
  ('sat_closed', NULL, 'completed', TRUE, 10)
ON CONFLICT (metric_name, disposition_pattern, status_category) DO NOTHING;

-- Sat No Close: Only sat_no_close
INSERT INTO repcard_metric_config (metric_name, disposition_pattern, status_category, is_included, priority) VALUES
  ('sat_no_close', '%sat.no.close%', NULL, TRUE, 10),
  ('sat_no_close', '%sat_no_close%', NULL, TRUE, 10),
  ('sat_no_close', NULL, 'sat_no_close', TRUE, 10)
ON CONFLICT (metric_name, disposition_pattern, status_category) DO NOTHING;

-- Quality Metrics: Exclude cancelled/no-show (for quality calculations)
INSERT INTO repcard_metric_config (metric_name, disposition_pattern, status_category, is_included, priority) VALUES
  ('quality_metric', '%cancel%', NULL, FALSE, 10),
  ('quality_metric', '%no.show%', NULL, FALSE, 10),
  ('quality_metric', '%no_show%', NULL, FALSE, 10),
  ('quality_metric', NULL, 'cancelled', FALSE, 10),
  ('quality_metric', NULL, 'no_show', FALSE, 10),
  ('quality_metric', NULL, NULL, TRUE, 0)  -- Include everything else
ON CONFLICT (metric_name, disposition_pattern, status_category) DO NOTHING;

-- Appointments Run: All appointments where closer_user_id is set (no filter)
INSERT INTO repcard_metric_config (metric_name, disposition_pattern, status_category, is_included, priority) VALUES
  ('appointments_run', NULL, NULL, TRUE, 0)
ON CONFLICT (metric_name, disposition_pattern, status_category) DO NOTHING;

COMMIT;
