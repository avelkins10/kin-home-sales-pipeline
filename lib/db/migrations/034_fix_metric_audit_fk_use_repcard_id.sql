-- Migration 034: Fix repcard_metric_audit to use repcard_appointment_id instead of UUID id
-- This fixes the foreign key constraint violation in BEFORE INSERT triggers
-- The trigger can now reference repcard_appointment_id (TEXT) which is available immediately

BEGIN;

-- Drop the existing foreign key constraint
ALTER TABLE repcard_metric_audit
DROP CONSTRAINT IF EXISTS repcard_metric_audit_appointment_id_fkey;

-- Change appointment_id column to reference repcard_appointment_id (TEXT) instead of id (UUID)
-- First, add a new column for the RepCard appointment ID
ALTER TABLE repcard_metric_audit
ADD COLUMN IF NOT EXISTS repcard_appointment_id TEXT;

-- Populate the new column from existing data (if any)
UPDATE repcard_metric_audit a
SET repcard_appointment_id = ap.repcard_appointment_id
FROM repcard_appointments ap
WHERE a.appointment_id = ap.id::text;

-- Drop the old appointment_id column (or keep it for backward compatibility)
-- Actually, let's keep both for now and make appointment_id nullable
ALTER TABLE repcard_metric_audit
ALTER COLUMN appointment_id DROP NOT NULL;

-- Add foreign key constraint on repcard_appointment_id
ALTER TABLE repcard_metric_audit
ADD CONSTRAINT repcard_metric_audit_repcard_appointment_id_fkey
FOREIGN KEY (repcard_appointment_id) 
REFERENCES repcard_appointments(repcard_appointment_id) 
ON DELETE CASCADE
DEFERRABLE INITIALLY DEFERRED;

-- Update the trigger function to use repcard_appointment_id instead of id
CREATE OR REPLACE FUNCTION update_appointment_metrics() RETURNS TRIGGER AS $$
BEGIN
  -- CRITICAL: Always set is_within_48_hours (never NULL)
  IF NEW.repcard_customer_id IS NOT NULL AND NEW.scheduled_at IS NOT NULL THEN
    NEW.is_within_48_hours := calculate_is_within_48_hours(
      COALESCE(NEW.repcard_appointment_id::text, NEW.id::text),
      NEW.scheduled_at,
      NEW.repcard_customer_id::text
    );
  ELSE
    NEW.is_within_48_hours := FALSE;
    -- Log why it's FALSE
    INSERT INTO repcard_metric_audit (
      repcard_appointment_id,
      metric_name,
      metric_value,
      calculation_reason,
      raw_data,
      calculated_by
    ) VALUES (
      COALESCE(NEW.repcard_appointment_id::text, NEW.id::text),
      'is_within_48_hours',
      FALSE,
      CASE 
        WHEN NEW.repcard_customer_id IS NULL THEN 'repcard_customer_id is NULL'
        WHEN NEW.scheduled_at IS NULL THEN 'scheduled_at is NULL'
        ELSE 'unknown error'
      END,
      jsonb_build_object(
        'repcard_customer_id', NEW.repcard_customer_id,
        'scheduled_at', NEW.scheduled_at
      ),
      'trigger'
    );
  END IF;

  -- CRITICAL: Always set has_power_bill (never NULL)
  IF NEW.repcard_customer_id IS NOT NULL OR NEW.repcard_appointment_id IS NOT NULL THEN
    NEW.has_power_bill := calculate_has_power_bill(
      COALESCE(NEW.repcard_appointment_id::text, NEW.id::text),
      COALESCE(NEW.repcard_appointment_id::text, ''),
      COALESCE(NEW.repcard_customer_id::text, '')
    );
  ELSE
    NEW.has_power_bill := FALSE;
    -- Log why it's FALSE
    INSERT INTO repcard_metric_audit (
      repcard_appointment_id,
      metric_name,
      metric_value,
      calculation_reason,
      raw_data,
      calculated_by
    ) VALUES (
      COALESCE(NEW.repcard_appointment_id::text, NEW.id::text),
      'has_power_bill',
      FALSE,
      'repcard_customer_id and repcard_appointment_id are both NULL',
      jsonb_build_object(
        'repcard_customer_id', NEW.repcard_customer_id,
        'repcard_appointment_id', NEW.repcard_appointment_id
      ),
      'trigger'
    );
  END IF;

  -- Set office_id from customer if not set
  IF NEW.office_id IS NULL AND NEW.repcard_customer_id IS NOT NULL THEN
    SELECT office_id INTO NEW.office_id
    FROM repcard_customers
    WHERE repcard_customer_id::text = NEW.repcard_customer_id::text
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

-- Update the calculate functions to use repcard_appointment_id
CREATE OR REPLACE FUNCTION calculate_is_within_48_hours(
  appointment_id_val TEXT,
  scheduled_at_val TIMESTAMPTZ,
  customer_id_val TEXT
) RETURNS BOOLEAN AS $$
DECLARE
  customer_created TIMESTAMPTZ;
  hours_diff NUMERIC;
  result BOOLEAN;
  reason TEXT;
  raw_data_json JSONB;
BEGIN
  -- Get customer created_at
  SELECT created_at INTO customer_created
  FROM repcard_customers
  WHERE repcard_customer_id::text = customer_id_val::text
  LIMIT 1;

  -- Calculate result
  IF scheduled_at_val IS NOT NULL AND customer_created IS NOT NULL THEN
    hours_diff := EXTRACT(EPOCH FROM (
      (scheduled_at_val::timestamptz AT TIME ZONE 'America/New_York') - 
      (customer_created::timestamptz AT TIME ZONE 'America/New_York')
    )) / 3600;
    
    IF hours_diff >= 0 AND hours_diff <= 48 THEN
      result := TRUE;
      reason := format('scheduled_at - customer.created_at = %s hours (within 48h, Eastern Time)', ROUND(hours_diff, 1)::text);
    ELSE
      result := FALSE;
      reason := format('scheduled_at - customer.created_at = %s hours (exceeds 48h, Eastern Time)', ROUND(hours_diff, 1)::text);
    END IF;
    
    raw_data_json := jsonb_build_object(
      'scheduled_at', scheduled_at_val,
      'scheduled_at_et', scheduled_at_val AT TIME ZONE 'America/New_York',
      'customer_created_at', customer_created,
      'customer_created_at_et', customer_created AT TIME ZONE 'America/New_York',
      'hours_diff', hours_diff,
      'timezone_note', 'Both timestamps converted to Eastern Time (America/New_York) for calculation to preserve correct day boundaries'
    );
  ELSIF scheduled_at_val IS NULL THEN
    result := FALSE;
    reason := 'scheduled_at is NULL';
    raw_data_json := jsonb_build_object('scheduled_at', NULL);
  ELSIF customer_created IS NULL THEN
    result := FALSE;
    reason := 'customer.created_at is NULL or customer not found';
    raw_data_json := jsonb_build_object('customer_id', customer_id_val, 'customer_found', false);
  ELSE
    result := FALSE;
    reason := 'unknown error';
    raw_data_json := jsonb_build_object();
  END IF;

  -- Log to audit trail using repcard_appointment_id
  INSERT INTO repcard_metric_audit (
    repcard_appointment_id,
    metric_name,
    metric_value,
    calculation_reason,
    raw_data,
    calculated_by
  ) VALUES (
    appointment_id_val,
    'is_within_48_hours',
    result,
    reason,
    raw_data_json,
    'trigger'
  );

  RETURN result;
END;
$$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION calculate_has_power_bill(
  appointment_id_val TEXT,
  appointment_repcard_id_val TEXT,
  customer_id_val TEXT
) RETURNS BOOLEAN AS $$
DECLARE
  customer_att_count INTEGER := 0;
  appointment_att_count INTEGER := 0;
  result BOOLEAN;
  reason TEXT;
  raw_data_json JSONB;
BEGIN
  -- Count customer attachments
  IF customer_id_val IS NOT NULL AND customer_id_val != '' THEN
    SELECT COUNT(*)::int INTO customer_att_count
    FROM repcard_customer_attachments
    WHERE repcard_customer_id::text = customer_id_val::text;
  END IF;

  -- Count appointment attachments
  IF appointment_repcard_id_val IS NOT NULL AND appointment_repcard_id_val != '' THEN
    SELECT COUNT(*)::int INTO appointment_att_count
    FROM repcard_appointment_attachments
    WHERE repcard_appointment_id::text = appointment_repcard_id_val::text;
  END IF;

  -- Has power bill if ANY attachment exists (customer OR appointment)
  result := (customer_att_count > 0 OR appointment_att_count > 0);
  
  IF result THEN
    reason := format('Found %s customer attachment(s) and %s appointment attachment(s)', customer_att_count, appointment_att_count);
  ELSE
    reason := format('No attachments found (customer: %s, appointment: %s)', customer_att_count, appointment_att_count);
  END IF;

  raw_data_json := jsonb_build_object(
    'customer_id', customer_id_val,
    'appointment_repcard_id', appointment_repcard_id_val,
    'customer_attachments', customer_att_count,
    'appointment_attachments', appointment_att_count
  );

  -- Log to audit trail using repcard_appointment_id
  INSERT INTO repcard_metric_audit (
    repcard_appointment_id,
    metric_name,
    metric_value,
    calculation_reason,
    raw_data,
    calculated_by
  ) VALUES (
    appointment_id_val,
    'has_power_bill',
    result,
    reason,
    raw_data_json,
    'trigger'
  );

  RETURN result;
END;
$$ LANGUAGE plpgsql;

-- Update index to use repcard_appointment_id
DROP INDEX IF EXISTS idx_repcard_metric_audit_appointment;
CREATE INDEX IF NOT EXISTS idx_repcard_metric_audit_repcard_appointment ON repcard_metric_audit(repcard_appointment_id);

COMMENT ON COLUMN repcard_metric_audit.repcard_appointment_id IS 'RepCard appointment ID (TEXT). Used instead of UUID id to avoid foreign key constraint violations in BEFORE INSERT triggers.';

COMMIT;
