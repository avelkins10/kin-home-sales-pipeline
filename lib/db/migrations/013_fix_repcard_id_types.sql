-- migrations/013_fix_repcard_id_types.sql
-- Fix RepCard ID columns from INTEGER to TEXT to handle hex string IDs
-- RepCard API returns IDs as hex strings like "6901159a7a2eb5b0ed0a231e"

BEGIN;

-- Drop indexes that depend on the columns we're changing
DROP INDEX IF EXISTS idx_repcard_customers_repcard_id;
DROP INDEX IF EXISTS idx_repcard_customers_setter_user;
DROP INDEX IF EXISTS idx_repcard_customers_office;
DROP INDEX IF EXISTS idx_repcard_appointments_repcard_id;
DROP INDEX IF EXISTS idx_repcard_appointments_repcard_customer_id;
DROP INDEX IF EXISTS idx_repcard_appointments_setter_user;
DROP INDEX IF EXISTS idx_repcard_appointments_closer_user;
DROP INDEX IF EXISTS idx_repcard_status_logs_repcard_id;
DROP INDEX IF EXISTS idx_repcard_status_logs_repcard_customer_id;
DROP INDEX IF EXISTS idx_repcard_status_logs_changed_by;

-- Alter repcard_customers table
ALTER TABLE repcard_customers
  ALTER COLUMN repcard_customer_id TYPE TEXT USING repcard_customer_id::TEXT,
  ALTER COLUMN setter_user_id TYPE TEXT USING setter_user_id::TEXT,
  ALTER COLUMN office_id TYPE TEXT USING office_id::TEXT;

-- Alter repcard_appointments table
ALTER TABLE repcard_appointments
  ALTER COLUMN repcard_appointment_id TYPE TEXT USING repcard_appointment_id::TEXT,
  ALTER COLUMN repcard_customer_id TYPE TEXT USING repcard_customer_id::TEXT,
  ALTER COLUMN setter_user_id TYPE TEXT USING setter_user_id::TEXT,
  ALTER COLUMN closer_user_id TYPE TEXT USING closer_user_id::TEXT;

-- Alter repcard_status_logs table
ALTER TABLE repcard_status_logs
  ALTER COLUMN repcard_log_id TYPE TEXT USING repcard_log_id::TEXT,
  ALTER COLUMN repcard_customer_id TYPE TEXT USING repcard_customer_id::TEXT,
  ALTER COLUMN changed_by_user_id TYPE TEXT USING changed_by_user_id::TEXT;

-- Recreate only the indexes that use the modified columns (with TEXT type)
CREATE UNIQUE INDEX idx_repcard_customers_repcard_id ON repcard_customers(repcard_customer_id);
CREATE INDEX idx_repcard_customers_setter_user ON repcard_customers(setter_user_id);
CREATE INDEX idx_repcard_customers_office ON repcard_customers(office_id);

CREATE UNIQUE INDEX idx_repcard_appointments_repcard_id ON repcard_appointments(repcard_appointment_id);
CREATE INDEX idx_repcard_appointments_repcard_customer_id ON repcard_appointments(repcard_customer_id);
CREATE INDEX idx_repcard_appointments_setter_user ON repcard_appointments(setter_user_id);
CREATE INDEX idx_repcard_appointments_closer_user ON repcard_appointments(closer_user_id);

CREATE UNIQUE INDEX idx_repcard_status_logs_repcard_id ON repcard_status_logs(repcard_log_id);
CREATE INDEX idx_repcard_status_logs_repcard_customer_id ON repcard_status_logs(repcard_customer_id);
CREATE INDEX idx_repcard_status_logs_changed_by ON repcard_status_logs(changed_by_user_id);

COMMIT;
