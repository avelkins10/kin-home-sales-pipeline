-- Migration 035: Extract useful fields from webhook payloads into dedicated columns
-- These fields are available in raw_data but extracting them makes queries easier

BEGIN;

-- Add useful fields to repcard_appointments
ALTER TABLE repcard_appointments
ADD COLUMN IF NOT EXISTS appointment_link TEXT,
ADD COLUMN IF NOT EXISTS remind_at TIMESTAMPTZ,
ADD COLUMN IF NOT EXISTS remind_text TEXT,
ADD COLUMN IF NOT EXISTS appointment_location TEXT,
ADD COLUMN IF NOT EXISTS latitude NUMERIC(10, 8),
ADD COLUMN IF NOT EXISTS longitude NUMERIC(11, 8),
ADD COLUMN IF NOT EXISTS contact_source TEXT;

-- Add useful fields to repcard_customers
ALTER TABLE repcard_customers
ADD COLUMN IF NOT EXISTS contact_source TEXT,
ADD COLUMN IF NOT EXISTS latitude NUMERIC(10, 8),
ADD COLUMN IF NOT EXISTS longitude NUMERIC(11, 8);

-- Create indexes for new fields
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_appointment_link ON repcard_appointments(appointment_link) WHERE appointment_link IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_remind_at ON repcard_appointments(remind_at) WHERE remind_at IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_repcard_appointments_location ON repcard_appointments(latitude, longitude) WHERE latitude IS NOT NULL AND longitude IS NOT NULL;
CREATE INDEX IF NOT EXISTS idx_repcard_customers_contact_source ON repcard_customers(contact_source) WHERE contact_source IS NOT NULL;

-- Backfill from raw_data for existing records
UPDATE repcard_appointments
SET 
  appointment_link = COALESCE(
    raw_data->>'appointmentLink',
    raw_data->>'appointment_link'
  ),
  remind_at = CASE 
    WHEN raw_data->>'remind_at' IS NOT NULL THEN (raw_data->>'remind_at')::timestamptz
    ELSE NULL
  END,
  remind_text = raw_data->>'remind_text',
  appointment_location = COALESCE(
    raw_data->>'appointmentLocation',
    raw_data->>'appointment_location'
  ),
  latitude = CASE 
    WHEN raw_data->>'latitude' IS NOT NULL THEN (raw_data->>'latitude')::numeric
    ELSE NULL
  END,
  longitude = CASE 
    WHEN raw_data->>'longitude' IS NOT NULL THEN (raw_data->>'longitude')::numeric
    ELSE NULL
  END,
  contact_source = COALESCE(
    raw_data->'contact'->>'contactSource',
    raw_data->>'contactSource'
  )
WHERE raw_data IS NOT NULL;

UPDATE repcard_customers
SET 
  contact_source = raw_data->>'contactSource',
  latitude = CASE 
    WHEN raw_data->>'latitude' IS NOT NULL THEN (raw_data->>'latitude')::numeric
    ELSE NULL
  END,
  longitude = CASE 
    WHEN raw_data->>'longitude' IS NOT NULL THEN (raw_data->>'longitude')::numeric
    ELSE NULL
  END
WHERE raw_data IS NOT NULL;

COMMENT ON COLUMN repcard_appointments.appointment_link IS 'RepCard appointment link/URL from webhook payload';
COMMENT ON COLUMN repcard_appointments.remind_at IS 'Reminder time for appointment from webhook payload';
COMMENT ON COLUMN repcard_appointments.appointment_location IS 'Appointment location/address from webhook payload';
COMMENT ON COLUMN repcard_appointments.contact_source IS 'Source of the contact/lead (e.g., door knock, referral, online)';
COMMENT ON COLUMN repcard_customers.contact_source IS 'Source of the contact/lead (e.g., door knock, referral, online)';

COMMIT;
