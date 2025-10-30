export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

import { NextResponse } from 'next/server';
import { sql } from '@vercel/postgres';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

/**
 * POST /api/admin/migrate/messages
 *
 * Run project messages database migration in production
 *
 * SECURITY: Only accessible by super_admin users
 */
export async function POST(req: Request) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('POST', '/api/admin/migrate/messages', undefined, reqId);

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
      DROP TABLE IF EXISTS project_messages CASCADE;

      -- Create project_messages table
      CREATE TABLE project_messages (
        -- Primary key
        id SERIAL PRIMARY KEY,

        -- Project and User associations
        project_id INTEGER NOT NULL,
        sender_id VARCHAR(255) NOT NULL,
        sender_name VARCHAR(255) NOT NULL,
        sender_role VARCHAR(50) NOT NULL,

        -- Message content
        message TEXT NOT NULL,

        -- Message metadata
        is_system_message BOOLEAN DEFAULT FALSE,
        metadata JSONB DEFAULT '{}'::jsonb,

        -- Soft delete (for audit trail)
        is_deleted BOOLEAN DEFAULT FALSE,
        deleted_at TIMESTAMP WITH TIME ZONE,
        deleted_by VARCHAR(255),

        -- Timestamps
        created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW(),
        updated_at TIMESTAMP WITH TIME ZONE DEFAULT NOW()
      );

      -- Indexes for performance
      CREATE INDEX idx_project_messages_project ON project_messages(project_id, created_at DESC);
      CREATE INDEX idx_project_messages_sender ON project_messages(sender_id);
      CREATE INDEX idx_project_messages_created ON project_messages(created_at DESC);
      CREATE INDEX idx_project_messages_not_deleted ON project_messages(is_deleted) WHERE is_deleted = FALSE;

      -- Auto-update updated_at timestamp
      CREATE TRIGGER update_project_messages_updated_at
        BEFORE UPDATE ON project_messages
        FOR EACH ROW
        EXECUTE FUNCTION update_updated_at_column();

      -- Comments for documentation
      COMMENT ON TABLE project_messages IS 'Internal messaging system for project collaboration';
      COMMENT ON COLUMN project_messages.project_id IS 'QuickBase project record ID';
      COMMENT ON COLUMN project_messages.sender_id IS 'User email or QB ID of message sender';
      COMMENT ON COLUMN project_messages.is_system_message IS 'True for automated messages (status changes, etc.)';
      COMMENT ON COLUMN project_messages.is_deleted IS 'Soft delete flag - messages never truly deleted for audit trail';
      COMMENT ON COLUMN project_messages.metadata IS 'Additional message data (file attachments, @mentions, etc.)';
    `;

    // Execute migration
    await sql.query(migrationSql);

    // Verify table creation
    const tableCheck = await sql.query(`SELECT table_name FROM information_schema.tables WHERE table_name = 'project_messages'`);

    const duration = Date.now() - startedAt;
    logApiResponse('POST', '/api/admin/migrate/messages', duration, {
      success: tableCheck.rows.length > 0,
      userId: user.email || user.id,
    }, reqId);

    return NextResponse.json({
      success: true,
      message: 'Project messages migration completed',
      tableCreated: tableCheck.rows.length > 0,
    }, { status: 200 });

  } catch (error) {
    logError('Messages migration failed', error as Error, { userId: user.email || user.id });
    return NextResponse.json({
      error: 'Migration failed',
      details: (error as Error).message,
    }, { status: 500 });
  }
}
