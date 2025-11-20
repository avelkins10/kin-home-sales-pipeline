-- Check RepCard Database Tables
-- 
-- This script checks if all required RepCard tables exist
-- and shows their record counts.
--
-- Usage:
--   psql "$DATABASE_URL" -f scripts/check-repcard-tables.sql

-- Check if tables exist
SELECT 
  CASE 
    WHEN EXISTS (
      SELECT 1 FROM information_schema.tables 
      WHERE table_schema = 'public' 
      AND table_name = table_name_check
    ) THEN '✅ EXISTS'
    ELSE '❌ MISSING'
  END as status,
  table_name_check as table_name
FROM (
  VALUES 
    ('repcard_users'),
    ('repcard_offices'),
    ('repcard_customers'),
    ('repcard_appointments'),
    ('repcard_status_logs'),
    ('repcard_customer_attachments'),
    ('repcard_appointment_attachments'),
    ('repcard_customer_notes'),
    ('repcard_customer_statuses'),
    ('repcard_calendars'),
    ('repcard_custom_fields'),
    ('repcard_leaderboard_snapshots'),
    ('repcard_teams'),
    ('repcard_metric_definitions'),
    ('repcard_leaderboard_config'),
    ('repcard_analytics_config')
) AS t(table_name_check)
ORDER BY table_name_check;

-- Show record counts for existing tables
DO $$
DECLARE
  table_name text;
  count_result bigint;
BEGIN
  FOR table_name IN 
    SELECT table_name 
    FROM information_schema.tables 
    WHERE table_schema = 'public' 
    AND table_name LIKE 'repcard_%'
    ORDER BY table_name
  LOOP
    EXECUTE format('SELECT COUNT(*) FROM %I', table_name) INTO count_result;
    RAISE NOTICE 'Table: % | Records: %', table_name, count_result;
  END LOOP;
END $$;

-- Summary
SELECT 
  (SELECT COUNT(*) FROM information_schema.tables 
   WHERE table_schema = 'public' AND table_name LIKE 'repcard_%') as total_tables,
  (SELECT COUNT(*) FROM repcard_users) as users_count,
  (SELECT COUNT(*) FROM repcard_customers) as customers_count,
  (SELECT COUNT(*) FROM repcard_appointments) as appointments_count,
  (SELECT COUNT(*) FROM repcard_status_logs) as status_logs_count;



