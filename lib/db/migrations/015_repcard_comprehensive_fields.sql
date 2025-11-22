-- migrations/015_repcard_comprehensive_fields.sql
-- Comprehensive fields and calculated metrics for RepCard data
-- Ensures all attribution, statuses, dispositions, and relationships are captured

BEGIN;

-- Add office_id to appointments (inherit from customer, setter, or closer)
ALTER TABLE repcard_appointments 
ADD COLUMN IF NOT EXISTS office_id INTEGER;

-- Add calculated/metric fields
ALTER TABLE repcard_appointments
ADD COLUMN IF NOT EXISTS is_within_48_hours BOOLEAN DEFAULT FALSE,
ADD COLUMN IF NOT EXISTS has_power_bill BOOLEAN DEFAULT FALSE,
ADD COLUMN IF NOT EXISTS status_category TEXT, -- 'scheduled', 'completed', 'cancelled', 'rescheduled', 'no_show', 'sat_closed', 'sat_no_close'
ADD COLUMN IF NOT EXISTS appointment_source TEXT; -- 'door_knock', 'referral', 'online', etc.

-- Add office_id index for appointments
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_office ON repcard_appointments(office_id);

-- Add calculated field indexes
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_within_48h ON repcard_appointments(is_within_48_hours) WHERE is_within_48_hours = TRUE;
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_power_bill ON repcard_appointments(has_power_bill) WHERE has_power_bill = TRUE;
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_status_category ON repcard_appointments(status_category);

-- Add composite index for common queries
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_setter_office_created ON repcard_appointments(setter_user_id, office_id, created_at);
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_closer_office_completed ON repcard_appointments(closer_user_id, office_id, completed_at);

-- Update customers table to ensure office is properly indexed
CREATE INDEX IF NOT EXISTS idx_repcard_customers_office_created ON repcard_customers(office_id, created_at);

-- Function to calculate and update appointment metrics
CREATE OR REPLACE FUNCTION update_appointment_metrics()
RETURNS TRIGGER AS $$
BEGIN
  -- Calculate is_within_48_hours: appointment scheduled within 48 hours of customer creation (door knock)
  IF NEW.repcard_customer_id IS NOT NULL AND NEW.scheduled_at IS NOT NULL THEN
    SELECT
      CASE
        WHEN (NEW.scheduled_at - c.created_at) <= INTERVAL '48 hours' THEN TRUE
        ELSE FALSE
      END
    INTO NEW.is_within_48_hours
    FROM repcard_customers c
    WHERE c.repcard_customer_id = NEW.repcard_customer_id
    LIMIT 1;
  ELSE
    -- If no scheduled_at, mark as FALSE
    NEW.is_within_48_hours := FALSE;
  END IF;

  -- Set office_id from customer if not set
  IF NEW.office_id IS NULL AND NEW.repcard_customer_id IS NOT NULL THEN
    SELECT office_id INTO NEW.office_id
    FROM repcard_customers
    WHERE repcard_customer_id = NEW.repcard_customer_id
    LIMIT 1;
  END IF;

  -- Set office_id from setter if customer doesn't have it
  IF NEW.office_id IS NULL AND NEW.setter_user_id IS NOT NULL THEN
    SELECT office_id INTO NEW.office_id
    FROM repcard_users
    WHERE repcard_user_id::text = NEW.setter_user_id::text
    LIMIT 1;
  END IF;

  -- Set office_id from closer if still not set
  IF NEW.office_id IS NULL AND NEW.closer_user_id IS NOT NULL THEN
    SELECT office_id INTO NEW.office_id
    FROM repcard_users
    WHERE repcard_user_id::text = NEW.closer_user_id::text
    LIMIT 1;
  END IF;

  -- Determine status_category from disposition
  IF NEW.disposition IS NOT NULL THEN
    NEW.status_category := CASE
      WHEN LOWER(NEW.disposition) LIKE '%cancel%' THEN 'cancelled'
      WHEN LOWER(NEW.disposition) LIKE '%reschedule%' THEN 'rescheduled'
      WHEN LOWER(NEW.disposition) LIKE '%no.show%' OR LOWER(NEW.disposition) LIKE '%no_show%' THEN 'no_show'
      WHEN LOWER(NEW.disposition) LIKE '%sat.closed%' OR LOWER(NEW.disposition) LIKE '%sat_closed%' OR LOWER(NEW.disposition) LIKE '%closed%' THEN 'sat_closed'
      WHEN LOWER(NEW.disposition) LIKE '%sat.no.close%' OR LOWER(NEW.disposition) LIKE '%sat_no_close%' THEN 'sat_no_close'
      WHEN NEW.completed_at IS NOT NULL THEN 'completed'
      WHEN NEW.scheduled_at IS NOT NULL THEN 'scheduled'
      ELSE 'pending'
    END;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger to auto-update metrics on insert/update
DROP TRIGGER IF EXISTS trigger_update_appointment_metrics ON repcard_appointments;
CREATE TRIGGER trigger_update_appointment_metrics
BEFORE INSERT OR UPDATE ON repcard_appointments
FOR EACH ROW
EXECUTE FUNCTION update_appointment_metrics();

-- Function to update has_power_bill flag
CREATE OR REPLACE FUNCTION update_appointment_power_bill_status()
RETURNS TRIGGER AS $$
BEGIN
  -- Check if customer has power bill attachment
  IF NEW.repcard_customer_id IS NOT NULL THEN
    SELECT 
      CASE 
        WHEN EXISTS (
          SELECT 1 FROM repcard_customer_attachments
          WHERE repcard_customer_id::text = NEW.repcard_customer_id::text
            AND (attachment_type ILIKE '%power%' OR attachment_type ILIKE '%bill%' OR file_name ILIKE '%power%' OR file_name ILIKE '%bill%')
        ) THEN TRUE
        ELSE FALSE
      END
    INTO NEW.has_power_bill;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger for power bill status (run after attachments are synced)
DROP TRIGGER IF EXISTS trigger_update_power_bill_status ON repcard_appointments;
CREATE TRIGGER trigger_update_power_bill_status
AFTER INSERT OR UPDATE ON repcard_appointments
FOR EACH ROW
EXECUTE FUNCTION update_appointment_power_bill_status();

-- Note: Bulk UPDATE of existing records is handled by the migration script
-- Triggers will automatically populate these fields for new records going forward

COMMIT;

