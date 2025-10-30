import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { runComprehensiveSync } from '@/lib/repcard/comprehensive-sync';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';
export const maxDuration = 300; // 5 minutes (Vercel Pro plan max)

/**
 * POST /api/admin/repcard/comprehensive-sync
 * Trigger comprehensive sync of ALL RepCard data
 * 
 * Query params:
 * - startDate: YYYY-MM-DD (optional, for date range filtering)
 * - endDate: YYYY-MM-DD (optional)
 * - incremental: true/false (default: false)
 * - skipUsers: true/false (default: false)
 * - skipOffices: true/false (default: false)
 * - skipCustomers: true/false (default: false)
 * - skipAppointments: true/false (default: false)
 * - skipStatusLogs: true/false (default: false)
 * - skipCustomerAttachments: true/false (default: false)
 * - skipAppointmentAttachments: true/false (default: false)
 */
export async function POST(request: NextRequest) {
  const requestId = crypto.randomUUID();
  const start = Date.now();

  try {
    logApiRequest('POST', '/api/admin/repcard/comprehensive-sync', requestId);

    // Require authentication and super_admin role
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    if (userRole !== 'super_admin') {
      const duration = Date.now() - start;
      logApiResponse('POST', '/api/admin/repcard/comprehensive-sync', duration, { status: 403, requestId });
      return NextResponse.json(
        { error: 'Forbidden - super_admin role required' },
        { status: 403 }
      );
    }

    // Parse query parameters
    const { searchParams } = new URL(request.url);
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;
    const incremental = searchParams.get('incremental') === 'true';
    const skipUsers = searchParams.get('skipUsers') === 'true';
    const skipOffices = searchParams.get('skipOffices') === 'true';
    const skipCustomers = searchParams.get('skipCustomers') === 'true';
    const skipAppointments = searchParams.get('skipAppointments') === 'true';
    const skipStatusLogs = searchParams.get('skipStatusLogs') === 'true';
    const skipCustomerAttachments = searchParams.get('skipCustomerAttachments') === 'true';
    const skipAppointmentAttachments = searchParams.get('skipAppointmentAttachments') === 'true';

    console.log('[RepCard Comprehensive Sync API] Starting sync with options:', {
      startDate,
      endDate,
      incremental,
      skipUsers,
      skipOffices,
      skipCustomers,
      skipAppointments,
      skipStatusLogs,
      skipCustomerAttachments,
      skipAppointmentAttachments
    });

    // Run comprehensive sync
    const results = await runComprehensiveSync({
      startDate,
      endDate,
      incremental,
      skipUsers,
      skipOffices,
      skipCustomers,
      skipAppointments,
      skipStatusLogs,
      skipCustomerAttachments,
      skipAppointmentAttachments
    });

    const duration = Date.now() - start;
    logApiResponse('POST', '/api/admin/repcard/comprehensive-sync', duration, { status: 200, requestId });

    return NextResponse.json({
      success: true,
      results,
      message: 'Comprehensive sync completed successfully'
    }, { status: 200 });

  } catch (error) {
    const duration = Date.now() - start;
    logError('repcard-comprehensive-sync', error as Error, { requestId });
    logApiResponse('POST', '/api/admin/repcard/comprehensive-sync', duration, { status: 500, requestId });

    return NextResponse.json(
      {
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error',
        stack: process.env.NODE_ENV === 'development' ? (error instanceof Error ? error.stack : undefined) : undefined
      },
      { status: 500 }
    );
  }
}

/**
 * GET /api/admin/repcard/comprehensive-sync
 * Get status of last comprehensive sync
 */
export async function GET(request: NextRequest) {
  const requestId = crypto.randomUUID();
  const start = Date.now();

  try {
    logApiRequest('GET', '/api/admin/repcard/comprehensive-sync', requestId);

    // Require authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const userRole = auth.session.user.role;
    if (userRole !== 'super_admin') {
      const duration = Date.now() - start;
      logApiResponse('GET', '/api/admin/repcard/comprehensive-sync', duration, { status: 403, requestId });
      return NextResponse.json(
        { error: 'Forbidden - super_admin role required' },
        { status: 403 }
      );
    }

    // Get last sync logs for each entity type
    const { sql } = await import('@/lib/db/client');
    const syncLogs = await sql`
      SELECT 
        entity_type,
        sync_type,
        started_at,
        completed_at,
        status,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        last_record_date,
        error_message
      FROM repcard_sync_log
      WHERE entity_type IN ('users', 'offices', 'customers', 'appointments', 'status_logs', 'customer_attachments', 'appointment_attachments')
      ORDER BY started_at DESC
      LIMIT 50
    `;

    const logs = Array.from(syncLogs);

    const duration = Date.now() - start;
    logApiResponse('GET', '/api/admin/repcard/comprehensive-sync', duration, { status: 200, requestId });

    return NextResponse.json({
      syncLogs: logs,
      summary: {
        totalSyncs: logs.length,
        successful: logs.filter((l: any) => l.status === 'completed').length,
        failed: logs.filter((l: any) => l.status === 'failed').length,
        running: logs.filter((l: any) => l.status === 'running').length
      }
    }, { status: 200 });

  } catch (error) {
    const duration = Date.now() - start;
    logError('repcard-comprehensive-sync-status', error as Error, { requestId });
    logApiResponse('GET', '/api/admin/repcard/comprehensive-sync', duration, { status: 500, requestId });

    return NextResponse.json(
      {
        error: 'Internal server error',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}

