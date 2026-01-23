-- Migration 038: Change 48-hour calculation to "within 2 calendar days"
-- Instead of strict 48 hours, count appointments scheduled for:
-- - Same day (day 0)
-- - Next day (day 1)
-- - Day after next (day 2)
-- This is more business-appropriate (e.g., Friday morning -> Sunday night = within 2 days)

BEGIN;

-- Update the calculation function to use calendar days
CREATE OR REPLACE FUNCTION calculate_is_within_48_hours(
  appointment_id_val TEXT,
  scheduled_at_val TIMESTAMPTZ,
  customer_id_val TEXT
) RETURNS BOOLEAN AS $func$
DECLARE
  customer_created TIMESTAMPTZ;
  customer_date DATE;
  scheduled_date DATE;
  days_diff INTEGER;
  result BOOLEAN;
  reason TEXT;
  raw_data_json JSONB;
BEGIN
  -- Get customer created_at
  SELECT created_at INTO customer_created
  FROM repcard_customers
  WHERE repcard_customer_id::text = customer_id_val::text
  LIMIT 1;

  -- Calculate result using calendar days in Eastern Time
  -- Convert both timestamps to Eastern Time, then extract dates
  -- This ensures appointments scheduled on the same/next/2nd day count correctly
  IF scheduled_at_val IS NOT NULL AND customer_created IS NOT NULL THEN
    -- Convert to Eastern Time and extract dates
    customer_date := DATE((customer_created::timestamptz AT TIME ZONE 'America/New_York'));
    scheduled_date := DATE((scheduled_at_val::timestamptz AT TIME ZONE 'America/New_York'));
    
    -- Calculate difference in calendar days
    days_diff := scheduled_date - customer_date;
    
    -- Within 2 calendar days: same day (0), next day (1), or day after next (2)
    IF days_diff >= 0 AND days_diff <= 2 THEN
      result := TRUE;
      reason := format('scheduled_date - customer.created_date = %s days (within 2 calendar days, Eastern Time)', days_diff::text);
    ELSE
      result := FALSE;
      reason := format('scheduled_date - customer.created_date = %s days (exceeds 2 calendar days, Eastern Time)', days_diff::text);
    END IF;
    
    raw_data_json := jsonb_build_object(
      'scheduled_at', scheduled_at_val,
      'scheduled_at_et', scheduled_at_val AT TIME ZONE 'America/New_York',
      'scheduled_date_et', scheduled_date,
      'customer_created_at', customer_created,
      'customer_created_at_et', customer_created AT TIME ZONE 'America/New_York',
      'customer_date_et', customer_date,
      'days_diff', days_diff,
      'timezone_note', 'Both timestamps converted to Eastern Time (America/New_York) and compared as calendar days. Counts same day (0), next day (1), or day after next (2).'
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
  )
  -- Note: No ON CONFLICT clause as repcard_metric_audit may not have unique constraint

  RETURN result;
END;
$func$ LANGUAGE plpgsql;

-- Update the column comment to reflect the new logic
COMMENT ON COLUMN repcard_appointments.is_within_48_hours IS 'True if appointment is scheduled within 2 calendar days of customer creation (door knock). Counts same day (0), next day (1), or day after next (2) in Eastern Time. Measures appointment scheduling speed, not data entry speed.';

COMMIT;
