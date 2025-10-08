export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

/**
 * POST /api/admin/migrate/notifications
 *
 * Run notifications database migration in production
 *
 * SECURITY: Only accessible by super_admin users
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', '/api/admin/migrate/notifications', undefined, reqId);

  // Auth check - require super_admin
  const auth = await requireAuth();
  if (!auth.authorized) {
    return auth.response;
  }

  const { user } = auth.session as any;
  if (user.role !== 'super_admin') {
    return NextResponse.json({ error: 'Forbidden - Admin only' }, { status: 403 });
  }

  try {
    // Read migration SQL
    const migrationSql = `
      -- Drop existing table if re-running migration
      DROP TABLE IF EXISTS notifications CASCADE;

      -- Create notifications table
      CREATE TABLE notifications (
        -- Primary key
        id SERIAL PRIMARY KEY,

        -- User and Project associations
        user_id VARCHAR(255) NOT NULL,
        project_id INTEGER NOT NULL,

        -- Type and Priority
        type VARCHAR(50) NOT NULL,
        priority VARCHAR(20) NOT NULL,
        source VARCHAR(50) NOT NULL,

        -- Content
        title VARCHAR(500) NOT NULL,
        message TEXT,
        metadata JSONB DEFAULT '{}'::jsonb,

        -- Sender information (null for system notifications)
        sender_id VARCHAR(255),
        sender_name VARCHAR(255),
        sender_role VARCHAR(50),

        -- Display properties
        icon VARCHAR(50) DEFAULT 'bell',
        color VARCHAR(50) DEFAULT 'blue',
        action_url VARCHAR(500),

        -- Read status
        is_read BOOLEAN DEFAULT FALSE,
        read_at TIMESTAMP WITH TIME ZONE,

        -- Timestamps
        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
      );

      -- Indexes for performance
      CREATE INDEX idx_notifications_user_unread ON notifications(user_id, is_read, created_at DESC);
      CREATE INDEX idx_notifications_project ON notifications(project_id, created_at DESC);
      CREATE INDEX idx_notifications_user_project ON notifications(user_id, project_id, created_at DESC);
      CREATE INDEX idx_notifications_type ON notifications(type);

      -- Function to auto-update updated_at
      CREATE OR REPLACE FUNCTION update_updated_at_column()
      RETURNS TRIGGER AS $$
      BEGIN
        NEW.updated_at = NOW();
        RETURN NEW;
      END;
      $$ language 'plpgsql';

      -- Trigger to auto-update updated_at
      CREATE TRIGGER update_notifications_updated_at
        BEFORE UPDATE ON notifications
        FOR EACH ROW
        EXECUTE FUNCTION update_updated_at_column();

      -- Comments for documentation
      COMMENT ON TABLE notifications IS 'Unified notification system for QuickBase notes, internal messages, and system alerts';
      COMMENT ON COLUMN notifications.type IS 'quickbase_note, internal_message, system_alert';
      COMMENT ON COLUMN notifications.priority IS 'critical, normal, info';
      COMMENT ON COLUMN notifications.source IS 'quickbase, internal, system';
      COMMENT ON COLUMN notifications.metadata IS 'Type-specific data (note_id, message_id, etc.)';
    `;

    // Execute migration
    await sql.query(migrationSql);

    // Verify table creation
    const tableCheck = await sql.query(`SELECT table_name FROM information_schema.tables WHERE table_name = 'notifications'`);

    const duration = Date.now() - startedAt;
    logApiResponse('POST', '/api/admin/migrate/notifications', duration, {
      success: tableCheck.rows.length > 0,
      userId: user.email || user.id,
    }, reqId);

    return NextResponse.json({
      success: true,
      message: 'Notifications migration completed',
      tableCreated: tableCheck.rows.length > 0,
    }, { status: 200 });

  } catch (error) {
    logError('Notifications migration failed', error as Error, { userId: user.email || user.id });
    return NextResponse.json({
      error: 'Migration failed',
      details: (error as Error).message,
    }, { status: 500 });
  }
}
