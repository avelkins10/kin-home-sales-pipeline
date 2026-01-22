import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { sql } from '@/lib/db/client';
import { readFileSync } from 'fs';
import { resolve } from 'path';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';
export const maxDuration = 300; // 5 minutes max

/**
 * POST /api/admin/repcard/run-migration-032
 * 
 * Runs migration 032: Event-Driven RepCard Metrics
 * Creates audit trail table and enhanced triggers
 */
export async function POST(request: NextRequest) {
  const start = Date.now();
  const path = new URL(request.url).pathname;
  const requestId = request.headers.get('x-request-id') || `migration-${Date.now()}`;

  try {
    logApiRequest('POST', path, { endpoint: 'run-migration-032', requestId });

    // Authentication - admin only
    const auth = await requireRole(['super_admin']);
    if (!auth.authorized) return auth.response;

    console.log('[Migration 032] Starting migration...');

    const migrationFile = '032_repcard_event_driven_metrics.sql';
    const migrationPath = resolve(process.cwd(), 'lib/db/migrations', migrationFile);

    // Read migration file
    const migrationSQL = readFileSync(migrationPath, 'utf-8');

    // Execute migration
    await (sql as any).query(migrationSQL);

    console.log('[Migration 032] Migration completed successfully');

    // Verify the migration
    const auditTableCheck = await sql`
      SELECT table_name 
      FROM information_schema.tables 
      WHERE table_schema = 'public' 
        AND table_name = 'repcard_metric_audit'
    `;
    const auditTableExists = Array.isArray(auditTableCheck) 
      ? auditTableCheck.length > 0 
      : (auditTableCheck?.rows?.length || 0) > 0;

    const functionCheck = await sql`
      SELECT routine_name 
      FROM information_schema.routines 
      WHERE routine_schema = 'public' 
        AND routine_name IN ('calculate_is_within_48_hours', 'calculate_has_power_bill', 'update_appointment_metrics')
    `;
    const functions = Array.isArray(functionCheck) 
      ? functionCheck 
      : (functionCheck?.rows || []);

    const duration = Date.now() - start;
    logApiResponse('POST', path, duration, { status: 200, requestId });

    return NextResponse.json({
      success: true,
      message: 'Migration 032 completed successfully',
      verification: {
        auditTableExists,
        triggerFunctionsFound: functions.length,
        functions: functions.map((f: any) => f.routine_name)
      },
      duration
    });

  } catch (error) {
    const duration = Date.now() - start;
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    
    console.error('[Migration 032] Error:', errorMessage, error);
    logError('run-migration-032', error as Error, { requestId });
    logApiResponse('POST', path, duration, { status: 500, requestId });
    
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to run migration',
        message: errorMessage,
      },
      { status: 500 }
    );
  }
}
