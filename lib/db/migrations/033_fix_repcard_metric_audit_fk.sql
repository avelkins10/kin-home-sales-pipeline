-- Migration 033: Fix repcard_metric_audit foreign key constraint
-- Make the foreign key DEFERRABLE so it can be checked at end of transaction
-- This allows the trigger to insert audit records before the appointment is committed

BEGIN;

-- Drop the existing foreign key constraint
ALTER TABLE repcard_metric_audit
DROP CONSTRAINT IF EXISTS repcard_metric_audit_appointment_id_fkey;

-- Recreate it as DEFERRABLE INITIALLY DEFERRED
-- This means the constraint check is deferred until the end of the transaction
-- This allows BEFORE INSERT triggers to insert audit records before the appointment exists
ALTER TABLE repcard_metric_audit
ADD CONSTRAINT repcard_metric_audit_appointment_id_fkey
FOREIGN KEY (appointment_id) 
REFERENCES repcard_appointments(id) 
ON DELETE CASCADE
DEFERRABLE INITIALLY DEFERRED;

COMMENT ON CONSTRAINT repcard_metric_audit_appointment_id_fkey ON repcard_metric_audit IS 
'Foreign key to repcard_appointments. DEFERRABLE to allow BEFORE INSERT triggers to insert audit records before appointment is committed.';

COMMIT;
