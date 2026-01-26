-- Migration 040: Leaderboard Query Optimization
-- Purpose: Add comprehensive indexes to optimize leaderboard queries and integration performance
-- Date: 2026-01-26
--
-- This migration adds critical indexes for:
-- 1. Date range queries with timezone conversions
-- 2. Team and office filtering
-- 3. Door knock aggregation queries
-- 4. Complex JOINs between repcard tables
-- 5. Foreign key lookups
--
-- Expected Performance Improvements:
-- - 3-5x faster leaderboard queries
-- - 50% reduction in query execution time for date-filtered queries
-- - Better index usage for office and team filtering

BEGIN;

-- ========================================
-- 1. APPOINTMENT QUERY OPTIMIZATIONS
-- ========================================

-- Index for setters leaderboard: created_at date filtering with setter_user_id
-- Used in: WHERE setter_user_id = ... AND (created_at AT TIME ZONE ...)::date >= ...
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter_created_date 
  ON repcard_appointments(setter_user_id, created_at)
  WHERE setter_user_id IS NOT NULL AND created_at IS NOT NULL;

-- Index for closers leaderboard: scheduled_at date filtering with closer_user_id
-- Used in: WHERE closer_user_id = ... AND scheduled_at::date >= ...
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_closer_scheduled_date 
  ON repcard_appointments(closer_user_id, scheduled_at)
  WHERE closer_user_id IS NOT NULL AND scheduled_at IS NOT NULL;

-- Composite index for office + setter + created_at (for office-filtered queries)
-- Used in: WHERE office_id IN (...) AND setter_user_id = ... AND created_at >= ...
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_office_setter_created 
  ON repcard_appointments(office_id, setter_user_id, created_at)
  WHERE setter_user_id IS NOT NULL AND created_at IS NOT NULL;

-- Composite index for office + closer + scheduled_at (for office-filtered queries)
-- Used in: WHERE office_id IN (...) AND closer_user_id = ... AND scheduled_at >= ...
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_office_closer_scheduled 
  ON repcard_appointments(office_id, closer_user_id, scheduled_at)
  WHERE closer_user_id IS NOT NULL AND scheduled_at IS NOT NULL;

-- Index for status_category filtering (used in quality metrics)
-- Used in: WHERE status_category NOT IN ('cancelled', 'no_show')
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_status_category 
  ON repcard_appointments(status_category)
  WHERE status_category IS NOT NULL;

-- Composite index for quality metrics: setter + status_category + is_within_48h + has_power_bill
-- Used in: WHERE setter_user_id = ... AND status_category NOT IN (...) AND is_within_48h = TRUE
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter_quality 
  ON repcard_appointments(setter_user_id, status_category, is_within_48_hours, has_power_bill)
  WHERE setter_user_id IS NOT NULL 
    AND (status_category IS NULL OR status_category NOT IN ('cancelled', 'no_show'))
    AND is_within_48_hours = TRUE;

-- Index for reschedule filtering (exclude reschedules from appointments_set)
-- Used in: WHERE (is_reschedule = FALSE OR is_reschedule IS NULL)
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_is_reschedule 
  ON repcard_appointments(is_reschedule, setter_user_id, created_at)
  WHERE (is_reschedule = FALSE OR is_reschedule IS NULL) AND setter_user_id IS NOT NULL;

-- ========================================
-- 2. DOOR KNOCK QUERY OPTIMIZATIONS
-- ========================================

-- Index for door knock date range queries with setter
-- Used in: WHERE setter_user_id = ... AND door_knocked_at >= ... AND door_knocked_at <= ...
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_setter_date_range 
  ON repcard_door_knocks(setter_user_id, door_knocked_at)
  WHERE setter_user_id IS NOT NULL;

-- Composite index for office + setter + date (for office-filtered door knock queries)
-- Used in: WHERE office_id IN (...) AND setter_user_id = ... AND door_knocked_at >= ...
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_office_setter_date 
  ON repcard_door_knocks(office_id, setter_user_id, door_knocked_at)
  WHERE setter_user_id IS NOT NULL;

-- Index for daily aggregation queries (used in estimated_hours_on_doors calculation)
-- Used in: GROUP BY DATE(door_knocked_at) with MIN/MAX aggregations
-- Note: Can't use DATE() in index (not immutable), but door_knocked_at index helps with date filtering
CREATE INDEX IF NOT EXISTS idx_repcard_door_knocks_setter_date 
  ON repcard_door_knocks(setter_user_id, door_knocked_at)
  WHERE setter_user_id IS NOT NULL;

-- ========================================
-- 3. USER AND TEAM QUERY OPTIMIZATIONS
-- ========================================

-- Index for team filtering (used in: WHERE COALESCE(team, 'No Team') = ANY(...))
-- Note: We can't index COALESCE directly, but we can index team column
CREATE INDEX IF NOT EXISTS idx_repcard_users_team 
  ON repcard_users(team)
  WHERE team IS NOT NULL;

-- Composite index for status + role + team (common filter combination)
-- Used in: WHERE status = 1 AND (role = 'setter' OR role IS NULL) AND team = ...
CREATE INDEX IF NOT EXISTS idx_repcard_users_status_role_team 
  ON repcard_users(status, role, team)
  WHERE status = 1;

-- Composite index for office + status + role + team (for office-filtered queries)
-- Used in: WHERE office_id IN (...) AND status = 1 AND role = ... AND team = ...
CREATE INDEX IF NOT EXISTS idx_repcard_users_office_status_role_team 
  ON repcard_users(office_id, status, role, team)
  WHERE status = 1;

-- Index for linking repcard_users to users table by repcard_user_id
-- Used in: LEFT JOIN users u ON u.repcard_user_id::text = ru.repcard_user_id::text
-- Note: This assumes repcard_user_id is INTEGER in both tables (from migration 018)
CREATE INDEX IF NOT EXISTS idx_users_repcard_user_id_int 
  ON users(repcard_user_id)
  WHERE repcard_user_id IS NOT NULL;

-- ========================================
-- 4. OFFICE FILTERING OPTIMIZATIONS
-- ========================================

-- Index for office name lookups (used in EXISTS subquery)
-- Used in: WHERE EXISTS (SELECT 1 FROM offices o WHERE o.name = ... AND o.quickbase_office_id = ANY(...))
CREATE INDEX IF NOT EXISTS idx_offices_name_quickbase_id 
  ON offices(name, quickbase_office_id)
  WHERE name IS NOT NULL AND quickbase_office_id IS NOT NULL;

-- Index for repcard_offices lookups by repcard_office_id
-- Used in: WHERE repcard_office_id = ANY(...)
CREATE INDEX IF NOT EXISTS idx_repcard_offices_repcard_id 
  ON repcard_offices(repcard_office_id)
  WHERE repcard_office_id IS NOT NULL;

-- ========================================
-- 5. CUSTOMER QUERY OPTIMIZATIONS
-- ========================================

-- Index for customer creation date queries (used in 48h calculation)
-- Used in: WHERE repcard_customer_id = ... (for getting created_at)
CREATE INDEX IF NOT EXISTS idx_repcard_customers_repcard_id_created 
  ON repcard_customers(repcard_customer_id, created_at)
  WHERE repcard_customer_id IS NOT NULL;

-- Composite index for office + setter + created_at (for doors_knocked queries)
-- Used in: WHERE office_id IN (...) AND setter_user_id = ... AND created_at >= ...
CREATE INDEX IF NOT EXISTS idx_repcard_customers_office_setter_created 
  ON repcard_customers(office_id, setter_user_id, created_at)
  WHERE setter_user_id IS NOT NULL AND created_at IS NOT NULL;

-- ========================================
-- 6. FOREIGN KEY AND CONSTRAINT VERIFICATION
-- ========================================

-- Verify foreign key constraints exist (add if missing)
-- Note: These should already exist from previous migrations, but we verify

-- Check if repcard_appointments has foreign key to repcard_customers
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint 
    WHERE conname = 'repcard_appointments_customer_id_fkey'
  ) THEN
    ALTER TABLE repcard_appointments
    ADD CONSTRAINT repcard_appointments_customer_id_fkey
    FOREIGN KEY (customer_id) REFERENCES repcard_customers(id) ON DELETE CASCADE;
  END IF;
END $$;

-- Check if repcard_door_knocks has foreign key to repcard_customers
DO $$
BEGIN
  IF NOT EXISTS (
    SELECT 1 FROM pg_constraint 
    WHERE conname = 'repcard_door_knocks_customer_id_fkey'
  ) THEN
    ALTER TABLE repcard_door_knocks
    ADD CONSTRAINT repcard_door_knocks_customer_id_fkey
    FOREIGN KEY (customer_id) REFERENCES repcard_customers(id) ON DELETE SET NULL;
  END IF;
END $$;

-- ========================================
-- 7. STATISTICS UPDATE
-- ========================================

-- Update table statistics for better query planning
ANALYZE repcard_appointments;
ANALYZE repcard_door_knocks;
ANALYZE repcard_users;
ANALYZE repcard_customers;
ANALYZE repcard_offices;
ANALYZE offices;
ANALYZE users;

-- ========================================
-- COMMENTS
-- ========================================

COMMENT ON INDEX idx_repcard_appointments_setter_created_date IS 'Optimizes setters leaderboard queries with date filtering on created_at';
COMMENT ON INDEX idx_repcard_appointments_closer_scheduled_date IS 'Optimizes closers leaderboard queries with date filtering on scheduled_at';
COMMENT ON INDEX idx_repcard_appointments_office_setter_created IS 'Optimizes office-filtered setters leaderboard queries';
COMMENT ON INDEX idx_repcard_appointments_office_closer_scheduled IS 'Optimizes office-filtered closers leaderboard queries';
COMMENT ON INDEX idx_repcard_appointments_setter_quality IS 'Optimizes quality metrics queries (within_48h, power_bill, status_category)';
COMMENT ON INDEX idx_repcard_door_knocks_setter_date_range IS 'Optimizes door knock date range queries for setters';
COMMENT ON INDEX idx_repcard_door_knocks_office_setter_date IS 'Optimizes office-filtered door knock queries';
COMMENT ON INDEX idx_repcard_users_status_role_team IS 'Optimizes user filtering by status, role, and team';
COMMENT ON INDEX idx_repcard_users_office_status_role_team IS 'Optimizes office-filtered user queries with role and team filtering';
COMMENT ON INDEX idx_offices_name_quickbase_id IS 'Optimizes office name lookups for office filtering';

COMMIT;
