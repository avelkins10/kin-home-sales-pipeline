-- Migration: Make password_hash nullable for external users
-- Purpose: Allow creation of external users from QuickBase who won't log in
-- Date: 2025-01-25
--
-- External users (from QuickBase Contacts/Projects) are data records only.
-- They exist to link projects and metrics to stable IDs, not for authentication.

ALTER TABLE users
ALTER COLUMN password_hash DROP NOT NULL;

-- Add comment for documentation
COMMENT ON COLUMN users.password_hash IS 'Hashed password for authentication. NULL for external users (QuickBase sync).';
