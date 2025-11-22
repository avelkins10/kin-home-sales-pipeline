-- Migration 031: Fix 48-hour speed calculation
-- Changes the is_within_48_hours calculation to measure appointment scheduling speed
-- instead of data entry speed

-- Update the trigger function to use scheduled_at instead of created_at
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
    WHERE repcard_user_id = NEW.setter_user_id::integer
    LIMIT 1;
  END IF;

  -- Set office_id from closer if still not set
  IF NEW.office_id IS NULL AND NEW.closer_user_id IS NOT NULL THEN
    SELECT office_id INTO NEW.office_id
    FROM repcard_users
    WHERE repcard_user_id = NEW.closer_user_id::integer
    LIMIT 1;
  END IF;

  RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Backfill existing appointments with corrected is_within_48_hours values
-- This recalculates for all appointments that have a scheduled_at date
UPDATE repcard_appointments a
SET is_within_48_hours = (
  CASE
    WHEN a.scheduled_at IS NOT NULL AND c.created_at IS NOT NULL
      AND (a.scheduled_at - c.created_at) <= INTERVAL '48 hours' THEN TRUE
    ELSE FALSE
  END
)
FROM repcard_customers c
WHERE a.repcard_customer_id = c.repcard_customer_id;

-- Add comment explaining the metric
COMMENT ON COLUMN repcard_appointments.is_within_48_hours IS 'True if appointment is scheduled to occur within 48 hours of customer creation (door knock). Measures appointment scheduling speed, not data entry speed.';
