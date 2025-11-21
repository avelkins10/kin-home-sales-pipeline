-- Migration 028: Add Reschedule Tracking to RepCard Appointments
-- This enables tracking of appointment reschedules, multiple appointments per customer,
-- and appointment chains for analytics

-- Add reschedule tracking columns
ALTER TABLE repcard_appointments
ADD COLUMN IF NOT EXISTS original_appointment_id TEXT,
ADD COLUMN IF NOT EXISTS reschedule_count INTEGER DEFAULT 0,
ADD COLUMN IF NOT EXISTS reschedule_reason TEXT,
ADD COLUMN IF NOT EXISTS is_reschedule BOOLEAN DEFAULT FALSE;

-- Add foreign key to link rescheduled appointments to original
ALTER TABLE repcard_appointments
ADD CONSTRAINT fk_repcard_appointments_original
FOREIGN KEY (original_appointment_id)
REFERENCES repcard_appointments(id)
ON DELETE SET NULL;

-- Add index for performance on reschedule queries
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_original_id
ON repcard_appointments(original_appointment_id)
WHERE original_appointment_id IS NOT NULL;

-- Add index for finding rescheduled appointments
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_is_reschedule
ON repcard_appointments(is_reschedule)
WHERE is_reschedule = TRUE;

-- Add composite index for customer appointment history
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_customer_scheduled
ON repcard_appointments(repcard_customer_id, scheduled_at DESC NULLS LAST);

-- Backfill is_reschedule flag for existing appointments
-- Mark appointments as reschedules if there are multiple appointments for the same customer
WITH customer_appointment_counts AS (
  SELECT
    repcard_customer_id,
    COUNT(*) as appointment_count
  FROM repcard_appointments
  GROUP BY repcard_customer_id
  HAVING COUNT(*) > 1
),
ranked_appointments AS (
  SELECT
    a.id,
    a.repcard_customer_id,
    ROW_NUMBER() OVER (
      PARTITION BY a.repcard_customer_id
      ORDER BY COALESCE(a.scheduled_at, a.created_at) ASC
    ) as appointment_rank
  FROM repcard_appointments a
  INNER JOIN customer_appointment_counts cac ON cac.repcard_customer_id = a.repcard_customer_id
)
UPDATE repcard_appointments a
SET
  is_reschedule = TRUE,
  reschedule_count = ra.appointment_rank - 1
FROM ranked_appointments ra
WHERE a.id = ra.id
  AND ra.appointment_rank > 1;

-- Link rescheduled appointments to their original appointment
-- Original appointment is the first appointment for each customer
WITH first_appointments AS (
  SELECT DISTINCT ON (repcard_customer_id)
    id as first_appointment_id,
    repcard_customer_id
  FROM repcard_appointments
  ORDER BY repcard_customer_id, COALESCE(scheduled_at, created_at) ASC
),
subsequent_appointments AS (
  SELECT
    a.id,
    a.repcard_customer_id,
    fa.first_appointment_id
  FROM repcard_appointments a
  INNER JOIN first_appointments fa ON fa.repcard_customer_id = a.repcard_customer_id
  WHERE a.id != fa.first_appointment_id
)
UPDATE repcard_appointments a
SET original_appointment_id = sa.first_appointment_id
FROM subsequent_appointments sa
WHERE a.id = sa.id;

-- Create view for appointment chains
CREATE OR REPLACE VIEW repcard_appointment_chains AS
SELECT
  original.id as original_appointment_id,
  original.repcard_customer_id,
  original.setter_user_id,
  original.scheduled_at as original_scheduled_at,
  original.disposition as original_disposition,
  COUNT(reschedule.id) as reschedule_count,
  ARRAY_AGG(
    reschedule.id ORDER BY reschedule.scheduled_at
  ) FILTER (WHERE reschedule.id IS NOT NULL) as reschedule_ids,
  ARRAY_AGG(
    reschedule.scheduled_at ORDER BY reschedule.scheduled_at
  ) FILTER (WHERE reschedule.scheduled_at IS NOT NULL) as reschedule_dates,
  MAX(reschedule.scheduled_at) as latest_reschedule_date
FROM repcard_appointments original
LEFT JOIN repcard_appointments reschedule
  ON reschedule.original_appointment_id = original.id
WHERE original.is_reschedule = FALSE OR original.is_reschedule IS NULL
GROUP BY
  original.id,
  original.repcard_customer_id,
  original.setter_user_id,
  original.scheduled_at,
  original.disposition;

-- Create index on the view's underlying columns for performance
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_reschedule_lookup
ON repcard_appointments(original_appointment_id, scheduled_at)
WHERE original_appointment_id IS NOT NULL;

-- Add comment explaining the schema
COMMENT ON COLUMN repcard_appointments.original_appointment_id IS 'References the first/original appointment if this is a reschedule';
COMMENT ON COLUMN repcard_appointments.reschedule_count IS 'Number indicating how many times this customer has been rescheduled (0 = original, 1 = first reschedule, etc)';
COMMENT ON COLUMN repcard_appointments.is_reschedule IS 'True if this appointment is a reschedule of an earlier appointment';
COMMENT ON VIEW repcard_appointment_chains IS 'Shows original appointments with their full reschedule chain';
