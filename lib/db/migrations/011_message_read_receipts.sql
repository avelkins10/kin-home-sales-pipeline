-- migrations/011_message_read_receipts.sql

BEGIN;

-- Message read receipts table
CREATE TABLE IF NOT EXISTS message_read_receipts (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  message_id INTEGER NOT NULL, -- QuickBase message record ID
  user_email TEXT NOT NULL, -- User who read the message
  read_at TIMESTAMP DEFAULT NOW(),
  created_at TIMESTAMP DEFAULT NOW(),
  updated_at TIMESTAMP DEFAULT NOW(),
  
  -- Composite unique constraint to prevent duplicate receipts
  UNIQUE(message_id, user_email)
);

-- Index for efficient lookups
CREATE INDEX IF NOT EXISTS idx_message_read_receipts_message_id ON message_read_receipts(message_id);
CREATE INDEX IF NOT EXISTS idx_message_read_receipts_user_email ON message_read_receipts(user_email);
CREATE INDEX IF NOT EXISTS idx_message_read_receipts_read_at ON message_read_receipts(read_at);

COMMIT;
