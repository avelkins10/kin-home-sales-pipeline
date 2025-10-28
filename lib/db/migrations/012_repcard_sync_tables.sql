-- migrations/012_repcard_sync_tables.sql
-- Tables for syncing and storing RepCard data locally for fast analytics

BEGIN;

-- RepCard Customers (leads/door knocks)
-- Stores all customer records from RepCard with setter attribution
CREATE TABLE IF NOT EXISTS repcard_customers (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_customer_id INTEGER UNIQUE NOT NULL,
  setter_user_id INTEGER,  -- RepCard user ID of the setter who created this lead
  office_id INTEGER,       -- RepCard office ID
  name TEXT,
  email TEXT,
  phone TEXT,
  address TEXT,
  city TEXT,
  state TEXT,
  zip TEXT,
  status TEXT,             -- Customer status (e.g., 'lead', 'qualified', 'closed', etc.)
  created_at TIMESTAMP,    -- When customer was created (used for "doors knocked" trending)
  updated_at TIMESTAMP,
  raw_data JSONB,          -- Full RepCard response for flexibility
  synced_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Appointments
-- Stores all appointments with setter and closer attribution
CREATE TABLE IF NOT EXISTS repcard_appointments (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_appointment_id INTEGER UNIQUE NOT NULL,
  customer_id TEXT REFERENCES repcard_customers(id) ON DELETE CASCADE,
  repcard_customer_id INTEGER,  -- For easier lookups before customer_id is linked
  setter_user_id INTEGER,       -- Inherited from customer (who knocked the door)
  closer_user_id INTEGER,       -- Rep assigned to run the appointment
  disposition TEXT,             -- sat_closed, sat_no_close, no_show, cancelled, rescheduled, etc.
  scheduled_at TIMESTAMP,       -- When appointment is scheduled
  completed_at TIMESTAMP,       -- When appointment actually happened (for "sat" trending)
  duration INTEGER,             -- Duration in minutes
  notes TEXT,
  created_at TIMESTAMP,         -- When appointment was created
  updated_at TIMESTAMP,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Customer Status Logs
-- Tracks status changes for customers (for disposition tracking)
CREATE TABLE IF NOT EXISTS repcard_status_logs (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  repcard_log_id INTEGER UNIQUE NOT NULL,
  customer_id TEXT REFERENCES repcard_customers(id) ON DELETE CASCADE,
  repcard_customer_id INTEGER,
  old_status TEXT,
  new_status TEXT,
  changed_at TIMESTAMP,
  changed_by_user_id INTEGER,  -- RepCard user ID who made the change
  notes TEXT,
  raw_data JSONB,
  synced_at TIMESTAMP DEFAULT NOW()
);

-- RepCard Sync Log
-- Tracks sync operations for monitoring and debugging
CREATE TABLE IF NOT EXISTS repcard_sync_log (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  entity_type TEXT NOT NULL,   -- 'customers', 'appointments', 'status_logs'
  sync_type TEXT NOT NULL,     -- 'full', 'incremental'
  started_at TIMESTAMP NOT NULL DEFAULT NOW(),
  completed_at TIMESTAMP,
  status TEXT NOT NULL,        -- 'running', 'completed', 'failed'
  records_fetched INTEGER DEFAULT 0,
  records_inserted INTEGER DEFAULT 0,
  records_updated INTEGER DEFAULT 0,
  records_failed INTEGER DEFAULT 0,
  last_record_date TIMESTAMP,  -- Last updated_at timestamp processed (for incremental syncs)
  error_message TEXT,
  error_details JSONB,
  created_at TIMESTAMP DEFAULT NOW()
);

-- Indexes for repcard_customers
CREATE INDEX IF NOT EXISTS idx_repcard_customers_repcard_id ON repcard_customers(repcard_customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_customers_setter ON repcard_customers(setter_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_customers_office ON repcard_customers(office_id);
CREATE INDEX IF NOT EXISTS idx_repcard_customers_created_at ON repcard_customers(created_at);
CREATE INDEX IF NOT EXISTS idx_repcard_customers_updated_at ON repcard_customers(updated_at);
CREATE INDEX IF NOT EXISTS idx_repcard_customers_status ON repcard_customers(status);

-- Indexes for repcard_appointments
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_repcard_id ON repcard_appointments(repcard_appointment_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_customer ON repcard_appointments(customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_repcard_customer ON repcard_appointments(repcard_customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter ON repcard_appointments(setter_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_closer ON repcard_appointments(closer_user_id);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_disposition ON repcard_appointments(disposition);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_scheduled_at ON repcard_appointments(scheduled_at);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_completed_at ON repcard_appointments(completed_at);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_created_at ON repcard_appointments(created_at);

-- Indexes for repcard_status_logs
CREATE INDEX IF NOT EXISTS idx_repcard_status_logs_repcard_id ON repcard_status_logs(repcard_log_id);
CREATE INDEX IF NOT EXISTS idx_repcard_status_logs_customer ON repcard_status_logs(customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_status_logs_repcard_customer ON repcard_status_logs(repcard_customer_id);
CREATE INDEX IF NOT EXISTS idx_repcard_status_logs_changed_at ON repcard_status_logs(changed_at);
CREATE INDEX IF NOT EXISTS idx_repcard_status_logs_changed_by ON repcard_status_logs(changed_by_user_id);

-- Indexes for repcard_sync_log
CREATE INDEX IF NOT EXISTS idx_repcard_sync_log_entity_type ON repcard_sync_log(entity_type);
CREATE INDEX IF NOT EXISTS idx_repcard_sync_log_started_at ON repcard_sync_log(started_at);
CREATE INDEX IF NOT EXISTS idx_repcard_sync_log_status ON repcard_sync_log(status);

-- Composite indexes for common query patterns
CREATE INDEX IF NOT EXISTS idx_repcard_customers_setter_created ON repcard_customers(setter_user_id, created_at);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter_created ON repcard_appointments(setter_user_id, created_at);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_closer_scheduled ON repcard_appointments(closer_user_id, scheduled_at);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_disposition_completed ON repcard_appointments(disposition, completed_at);

COMMIT;
