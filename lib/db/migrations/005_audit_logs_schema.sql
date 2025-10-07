-- Migration: 005_audit_logs_schema.sql
-- Purpose: Add audit_logs table for compliance tracking and activity monitoring
-- 
-- This migration adds comprehensive audit logging infrastructure for compliance and security monitoring.
-- The audit_logs table tracks all administrative actions with full change history.

BEGIN;

-- 1. Create audit_logs table
CREATE TABLE IF NOT EXISTS audit_logs (
  id TEXT PRIMARY KEY DEFAULT gen_random_uuid()::text,
  timestamp TIMESTAMP DEFAULT NOW(),
  user_id TEXT REFERENCES users(id) ON DELETE SET NULL,
  user_name TEXT NOT NULL,
  action TEXT NOT NULL CHECK (action IN ('login', 'logout', 'create', 'update', 'delete', 'export')),
  resource TEXT NOT NULL,
  resource_id TEXT NOT NULL,
  changes JSONB DEFAULT '{}'::jsonb,
  ip_address TEXT,
  user_agent TEXT,
  created_at TIMESTAMP DEFAULT NOW()
);

-- 2. Create indexes for efficient filtering
CREATE INDEX IF NOT EXISTS idx_audit_logs_timestamp ON audit_logs(timestamp DESC);
CREATE INDEX IF NOT EXISTS idx_audit_logs_user ON audit_logs(user_id);
CREATE INDEX IF NOT EXISTS idx_audit_logs_action ON audit_logs(action);
CREATE INDEX IF NOT EXISTS idx_audit_logs_resource ON audit_logs(resource);
CREATE INDEX IF NOT EXISTS idx_audit_logs_changes ON audit_logs USING gin(changes);

-- 3. Create composite index for common query pattern
CREATE INDEX IF NOT EXISTS idx_audit_logs_timestamp_action 
  ON audit_logs(timestamp DESC, action);

COMMIT;