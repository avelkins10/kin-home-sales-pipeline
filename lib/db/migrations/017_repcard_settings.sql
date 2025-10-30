-- migrations/017_repcard_settings.sql
-- RepCard Leaderboard and Analytics Configuration Settings

BEGIN;

-- RepCard Leaderboard Configuration
CREATE TABLE IF NOT EXISTS repcard_leaderboard_config (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  name TEXT NOT NULL UNIQUE, -- e.g., 'Default', 'D2D Focus', 'Closer Performance'
  description TEXT,
  leaderboard_type TEXT NOT NULL, -- 'd2d', 'overview', 'engagements', 'recruiting', 'custom'
  enabled_metrics TEXT[] NOT NULL, -- Array of metric keys
  rank_by_metric TEXT NOT NULL, -- Which metric to rank by
  display_order INTEGER DEFAULT 0,
  date_range_default TEXT DEFAULT 'month', -- 'today', 'week', 'month', 'quarter', 'ytd', 'custom'
  roles TEXT[], -- Which roles can see this leaderboard (empty = all)
  office_ids INTEGER[], -- Which offices to include (empty = all)
  is_default BOOLEAN DEFAULT FALSE,
  enabled BOOLEAN DEFAULT TRUE,
  created_by TEXT REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Analytics Configuration
CREATE TABLE IF NOT EXISTS repcard_analytics_config (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  name TEXT NOT NULL UNIQUE,
  description TEXT,
  widget_type TEXT NOT NULL, -- 'card', 'chart', 'table', 'leaderboard'
  metric_type TEXT NOT NULL, -- 'doors_knocked', 'appointments_set', 'conversion_rate', etc.
  date_range_default TEXT DEFAULT 'month',
  refresh_interval INTEGER DEFAULT 30, -- seconds
  enabled BOOLEAN DEFAULT TRUE,
  display_order INTEGER DEFAULT 0,
  roles TEXT[], -- Which roles can see this widget (empty = all)
  office_ids INTEGER[], -- Which offices to include (empty = all)
  config JSONB, -- Widget-specific configuration (colors, thresholds, etc.)
  created_by TEXT REFERENCES users(id),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Metric Definitions (defines what metrics are available)
CREATE TABLE IF NOT EXISTS repcard_metric_definitions (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  metric_key TEXT NOT NULL UNIQUE, -- e.g., 'doors_knocked', 'appointment_speed'
  display_name TEXT NOT NULL,
  description TEXT,
  category TEXT NOT NULL, -- 'volume', 'quality', 'revenue', 'engagement'
  data_source TEXT NOT NULL, -- 'repcard_api', 'database', 'calculated'
  unit TEXT, -- 'count', 'percentage', 'currency', 'hours', etc.
  format TEXT DEFAULT 'number', -- 'number', 'percentage', 'currency', 'duration'
  aggregation_type TEXT DEFAULT 'sum', -- 'sum', 'avg', 'count', 'max', 'min'
  enabled BOOLEAN DEFAULT TRUE,
  leaderboard_supported BOOLEAN DEFAULT TRUE,
  analytics_supported BOOLEAN DEFAULT TRUE,
  config JSONB, -- Calculation/formula config if calculated
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW()
);

-- Pre-populate metric definitions
INSERT INTO repcard_metric_definitions (metric_key, display_name, description, category, data_source, unit, format, aggregation_type, leaderboard_supported, analytics_supported) VALUES
-- Volume Metrics
('doors_knocked', 'Doors Knocked', 'Total doors knocked/leads created', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('appointments_set', 'Appointments Set', 'Total appointments scheduled', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('lead_count', 'Leads Created', 'Total leads created', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('customer_count', 'Customers Created', 'Total customers created', 'volume', 'repcard_api', 'count', 'number', 'sum', true, true),
('appointment_count', 'Appointments Completed', 'Total appointments completed', 'volume', 'database', 'count', 'number', 'sum', true, true),

-- Quality Metrics
('appointment_speed', 'Appointment Speed', 'Percentage of appointments set within 24 hours', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('attachment_rate', 'Power Bill Rate', 'Percentage of customers with power bill attachments', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('quality_score', 'Quality Score', 'Composite quality score', 'quality', 'calculated', 'score', 'number', 'avg', true, true),
('reschedule_rate', 'Reschedule Rate', 'Average number of reschedules per appointment', 'quality', 'calculated', 'count', 'number', 'avg', true, true),
('follow_up_consistency', 'Follow-Up Consistency', 'Percentage of appointments with follow-up', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),

-- Revenue Metrics
('sales_closed', 'Sales Closed', 'Total sales closed', 'revenue', 'database', 'count', 'number', 'sum', true, true),
('revenue', 'Revenue', 'Total revenue from closed sales', 'revenue', 'database', 'currency', 'currency', 'sum', true, true),

-- Engagement Metrics
('avg_rating', 'Average Rating', 'Average customer rating', 'engagement', 'repcard_api', 'rating', 'number', 'avg', true, true),
('review_count', 'Reviews', 'Total customer reviews', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),
('referral_count', 'Referrals', 'Total referrals received', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),
('video_viewed', 'Video Views', 'Total video views', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),
('engagement_count', 'Engagements', 'Total customer engagements', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),
('card_sent_count', 'Cards Sent', 'Total cards sent', 'engagement', 'repcard_api', 'count', 'number', 'sum', true, true),

-- Calculated Metrics
('set_rate', 'Set Rate', 'Appointments set / Doors knocked', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('close_rate', 'Close Rate', 'Sales closed / Appointments set', 'quality', 'calculated', 'percentage', 'percentage', 'avg', true, true),
('avg_door_knocks_per_day', 'Avg Doors/Day', 'Average doors knocked per day', 'volume', 'repcard_api', 'count', 'number', 'avg', true, true),
('avg_distance_per_knock', 'Avg Distance', 'Average distance per door knock', 'volume', 'repcard_api', 'distance', 'number', 'avg', true, true)
ON CONFLICT (metric_key) DO NOTHING;

CREATE INDEX IF NOT EXISTS idx_repcard_lb_config_enabled ON repcard_leaderboard_config(enabled);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_config_default ON repcard_leaderboard_config(is_default);
CREATE INDEX IF NOT EXISTS idx_repcard_lb_config_type ON repcard_leaderboard_config(leaderboard_type);
CREATE INDEX IF NOT EXISTS idx_repcard_analytics_config_enabled ON repcard_analytics_config(enabled);
CREATE INDEX IF NOT EXISTS idx_repcard_analytics_config_widget_type ON repcard_analytics_config(widget_type);
CREATE INDEX IF NOT EXISTS idx_repcard_metrics_category ON repcard_metric_definitions(category);
CREATE INDEX IF NOT EXISTS idx_repcard_metrics_enabled ON repcard_metric_definitions(enabled);
CREATE INDEX IF NOT EXISTS idx_repcard_metrics_key ON repcard_metric_definitions(metric_key);

-- Create default leaderboard configurations
INSERT INTO repcard_leaderboard_config (name, description, leaderboard_type, enabled_metrics, rank_by_metric, is_default, enabled) VALUES
('Default D2D Leaderboard', 'Default door-to-door leaderboard with key metrics', 'd2d', ARRAY['doors_knocked', 'appointments_set', 'set_rate', 'lead_count'], 'doors_knocked', true, true),
('Quality Focus', 'Leaderboard focused on quality metrics', 'custom', ARRAY['quality_score', 'appointment_speed', 'attachment_rate'], 'quality_score', false, true),
('Closer Performance', 'Leaderboard for closer performance metrics', 'custom', ARRAY['sales_closed', 'revenue', 'close_rate'], 'sales_closed', false, true)
ON CONFLICT (name) DO NOTHING;

COMMIT;

