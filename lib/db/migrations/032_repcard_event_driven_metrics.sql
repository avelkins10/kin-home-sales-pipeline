-- Migration 032: Event-Driven RepCard Metrics with Audit Trail
-- Implements event-driven architecture: calculate metrics when data changes, store results, never NULL
-- Adds audit trail to track why metrics are TRUE/FALSE for easy debugging

BEGIN;

-- ========================================
-- Phase 2: Create Audit Trail Table
-- ========================================
CREATE TABLE IF NOT EXISTS repcard_metric_audit (
  id UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  appointment_id TEXT REFERENCES repcard_appointments(id) ON DELETE CASCADE,
  metric_name TEXT NOT NULL CHECK (metric_name IN ('is_within_48_hours', 'has_power_bill')),
  metric_value BOOLEAN NOT NULL,
  calculation_reason TEXT NOT NULL, -- Human-readable explanation
  raw_data JSONB, -- Store all calculation inputs (customer.created_at, scheduled_at, attachment counts, etc.)
  calculated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  calculated_by TEXT DEFAULT 'trigger' CHECK (calculated_by IN ('trigger', 'backfill', 'sync', 'manual'))
);

-- Indexes for audit table
CREATE INDEX IF NOT EXISTS idx_repcard_metric_audit_appointment ON repcard_metric_audit(appointment_id);
CREATE INDEX IF NOT EXISTS idx_repcard_metric_audit_metric ON repcard_metric_audit(metric_name);
CREATE INDEX IF NOT EXISTS idx_repcard_metric_audit_calculated_at ON repcard_metric_audit(calculated_at DESC);

COMMENT ON TABLE repcard_metric_audit IS 'Audit trail for RepCard metric calculations. Tracks why each metric is TRUE/FALSE for debugging.';
COMMENT ON COLUMN repcard_metric_audit.calculation_reason IS 'Human-readable explanation of why metric is TRUE/FALSE (e.g., "scheduled_at - customer.created_at = 23.5 hours")';
COMMENT ON COLUMN repcard_metric_audit.raw_data IS 'JSONB storing all calculation inputs for debugging (customer.created_at, scheduled_at, attachment counts, etc.)';

-- ========================================
-- Phase 1: Enhanced Trigger Functions
-- ========================================

-- Function to calculate is_within_48_hours with audit trail
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
    hours_diff := EXTRACT(EPOCH FROM (scheduled_at_val - customer_created)) / 3600;
    
    IF hours_diff >= 0 AND hours_diff <= 48 THEN
      result := TRUE;
      reason := format('scheduled_at - customer.created_at = %.1f hours (within 48h)', hours_diff);
    ELSE
      result := FALSE;
      reason := format('scheduled_at - customer.created_at = %s hours (exceeds 48h)', ROUND(hours_diff, 1)::text);
    END IF;
    
    raw_data_json := jsonb_build_object(
      'scheduled_at', scheduled_at_val,
      'customer_created_at', customer_created,
      'hours_diff', hours_diff
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

  -- Log to audit trail
  INSERT INTO repcard_metric_audit (
    appointment_id,
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

-- Function to calculate has_power_bill with audit trail
CREATE OR REPLACE FUNCTION calculate_has_power_bill(
  appointment_id_val TEXT,
  appointment_repcard_id_val TEXT,
  customer_id_val TEXT
) RETURNS BOOLEAN AS $$
DECLARE
  customer_att_count INT;
  appointment_att_count INT;
  result BOOLEAN;
  reason TEXT;
  raw_data_json JSONB;
BEGIN
  -- Count customer attachments (ANY attachment = power bill per user requirement)
  SELECT COUNT(*)::int INTO customer_att_count
  FROM repcard_customer_attachments
  WHERE repcard_customer_id::text = customer_id_val::text;

  -- Count appointment attachments
  SELECT COUNT(*)::int INTO appointment_att_count
  FROM repcard_appointment_attachments
  WHERE repcard_appointment_id::text = appointment_repcard_id_val::text;

  -- Calculate result (ANY attachment = power bill)
  IF customer_att_count > 0 OR appointment_att_count > 0 THEN
    result := TRUE;
    reason := format('Found %s customer attachment(s) and %s appointment attachment(s) (any attachment = power bill)', 
                     customer_att_count, appointment_att_count);
  ELSE
    result := FALSE;
    reason := 'No attachments found for customer or appointment';
  END IF;

  raw_data_json := jsonb_build_object(
    'customer_attachment_count', customer_att_count,
    'appointment_attachment_count', appointment_att_count,
    'customer_id', customer_id_val,
    'appointment_repcard_id', appointment_repcard_id_val
  );

  -- Log to audit trail
  INSERT INTO repcard_metric_audit (
    appointment_id,
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

-- Enhanced trigger function for appointment metrics
CREATE OR REPLACE FUNCTION update_appointment_metrics()
RETURNS TRIGGER AS $$
BEGIN
  -- CRITICAL: Always set is_within_48_hours (never NULL)
  IF NEW.repcard_customer_id IS NOT NULL AND NEW.scheduled_at IS NOT NULL THEN
    NEW.is_within_48_hours := calculate_is_within_48_hours(
      NEW.id::text,
      NEW.scheduled_at,
      NEW.repcard_customer_id::text
    );
  ELSE
    NEW.is_within_48_hours := FALSE;
    -- Log why it's FALSE
    INSERT INTO repcard_metric_audit (
      appointment_id,
      metric_name,
      metric_value,
      calculation_reason,
      raw_data,
      calculated_by
    ) VALUES (
      NEW.id,
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
      NEW.id::text,
      COALESCE(NEW.repcard_appointment_id::text, ''),
      COALESCE(NEW.repcard_customer_id::text, '')
    );
  ELSE
    NEW.has_power_bill := FALSE;
    -- Log why it's FALSE
    INSERT INTO repcard_metric_audit (
      appointment_id,
      metric_name,
      metric_value,
      calculation_reason,
      raw_data,
      calculated_by
    ) VALUES (
      NEW.id,
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

-- Replace existing trigger
DROP TRIGGER IF EXISTS trigger_update_appointment_metrics ON repcard_appointments;
CREATE TRIGGER trigger_update_appointment_metrics
BEFORE INSERT OR UPDATE ON repcard_appointments
FOR EACH ROW
EXECUTE FUNCTION update_appointment_metrics();

-- Remove old power bill trigger (now handled in main trigger)
DROP TRIGGER IF EXISTS trigger_update_power_bill_status ON repcard_appointments;
DROP FUNCTION IF EXISTS update_appointment_power_bill_status();

-- ========================================
-- Trigger for attachment changes (recalculate has_power_bill)
-- ========================================

-- Function to recalculate has_power_bill when attachments change
CREATE OR REPLACE FUNCTION recalculate_power_bill_on_attachment_change()
RETURNS TRIGGER AS $$
DECLARE
  affected_appointments RECORD;
  customer_id_val TEXT;
  appointment_id_val TEXT;
BEGIN
  -- Determine which appointments are affected
  IF TG_TABLE_NAME = 'repcard_customer_attachments' THEN
    customer_id_val := COALESCE(NEW.repcard_customer_id::text, OLD.repcard_customer_id::text);
    
    -- Find all appointments for this customer
    FOR affected_appointments IN
      SELECT id::text as id, repcard_appointment_id, repcard_customer_id
      FROM repcard_appointments
      WHERE repcard_customer_id::text = customer_id_val
    LOOP
      -- Recalculate by updating the appointment (triggers main trigger)
      UPDATE repcard_appointments
      SET updated_at = NOW()
      WHERE id = affected_appointments.id;
    END LOOP;
  ELSIF TG_TABLE_NAME = 'repcard_appointment_attachments' THEN
    appointment_id_val := COALESCE(NEW.repcard_appointment_id::text, OLD.repcard_appointment_id::text);
    
    -- Find the appointment
    FOR affected_appointments IN
      SELECT id::text as id, repcard_appointment_id, repcard_customer_id
      FROM repcard_appointments
      WHERE repcard_appointment_id::text = appointment_id_val
    LOOP
      -- Recalculate by updating the appointment (triggers main trigger)
      UPDATE repcard_appointments
      SET updated_at = NOW()
      WHERE id = affected_appointments.id;
    END LOOP;
  END IF;

  RETURN COALESCE(NEW, OLD);
END;
$$ LANGUAGE plpgsql;

-- Create triggers for attachment changes
DROP TRIGGER IF EXISTS trigger_recalculate_pb_on_customer_attachment ON repcard_customer_attachments;
CREATE TRIGGER trigger_recalculate_pb_on_customer_attachment
AFTER INSERT OR UPDATE OR DELETE ON repcard_customer_attachments
FOR EACH ROW
EXECUTE FUNCTION recalculate_power_bill_on_attachment_change();

DROP TRIGGER IF EXISTS trigger_recalculate_pb_on_appointment_attachment ON repcard_appointment_attachments;
CREATE TRIGGER trigger_recalculate_pb_on_appointment_attachment
AFTER INSERT OR UPDATE OR DELETE ON repcard_appointment_attachments
FOR EACH ROW
EXECUTE FUNCTION recalculate_power_bill_on_attachment_change();

-- ========================================
-- Trigger for customer.created_at changes (recalculate is_within_48_hours)
-- ========================================

CREATE OR REPLACE FUNCTION recalculate_48h_on_customer_change()
RETURNS TRIGGER AS $$
BEGIN
  -- If created_at changed, recalculate is_within_48_hours for all related appointments
  IF OLD.created_at IS DISTINCT FROM NEW.created_at THEN
    UPDATE repcard_appointments
    SET updated_at = NOW()
    WHERE repcard_customer_id::text = NEW.repcard_customer_id::text;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

DROP TRIGGER IF EXISTS trigger_recalculate_48h_on_customer_change ON repcard_customers;
CREATE TRIGGER trigger_recalculate_48h_on_customer_change
AFTER UPDATE ON repcard_customers
FOR EACH ROW
WHEN (OLD.created_at IS DISTINCT FROM NEW.created_at)
EXECUTE FUNCTION recalculate_48h_on_customer_change();

COMMIT;
