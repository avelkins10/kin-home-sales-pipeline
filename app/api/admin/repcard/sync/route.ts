import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import {
  runFullSync,
  runIncrementalSync,
  syncCustomers,
  syncAppointments,
  syncStatusLogs
} from '@/lib/repcard/sync-service';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';
export const maxDuration = 300; // 5 minutes for full sync

/**
 * Admin endpoint for manually triggering RepCard data syncs
 *
 * Query parameters:
 * - type: 'full' | 'incremental' | 'customers' | 'appointments' | 'status_logs'
 * - startDate: YYYY-MM-DD (optional, for full sync)
 * - endDate: YYYY-MM-DD (optional, for full sync)
 *
 * Authentication: Requires super_admin role
 */
export async function POST(request: NextRequest) {
  try {
    console.log('[Admin Sync] POST request received');

    // Authentication - only super_admin can trigger sync
    const session = await getServerSession(authOptions);
    console.log('[Admin Sync] Session:', session?.user?.email, session?.user?.role);

    if (!session || session.user.role !== 'super_admin') {
      console.log('[Admin Sync] Unauthorized access attempt');
      return NextResponse.json(
        { error: 'Unauthorized', message: 'Only super_admin can trigger sync' },
        { status: 403 }
      );
    }

    const { searchParams } = new URL(request.url);
    const syncType = searchParams.get('type') || 'incremental';
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;

    console.log(`[Admin Sync] Starting ${syncType} sync requested by ${session.user.email}`, { startDate, endDate });

    let results;

    switch (syncType) {
      case 'full':
        results = await runFullSync({ startDate, endDate });
        break;

      case 'incremental':
        results = await runIncrementalSync();
        break;

      case 'customers':
        results = [await syncCustomers({
          startDate,
          endDate,
          incremental: false
        })];
        break;

      case 'appointments':
        results = [await syncAppointments({
          fromDate: startDate,
          toDate: endDate,
          incremental: false
        })];
        break;

      case 'status_logs':
        results = [await syncStatusLogs({
          fromDate: startDate,
          toDate: endDate,
          incremental: false
        })];
        break;

      case 'users':
        const { syncUsers } = await import('@/lib/repcard/comprehensive-sync');
        results = [await syncUsers({ incremental: false })];
        break;

      default:
        return NextResponse.json(
          {
            error: 'Invalid sync type',
            message: 'type must be one of: full, incremental, customers, appointments, status_logs, users'
          },
          { status: 400 }
        );
    }

    // Check if any syncs failed
    const hasErrors = results.some(r => r.error);
    const status = hasErrors ? 206 : 200; // 206 = Partial Content (some succeeded, some failed)

    return NextResponse.json(
      {
        message: `${syncType} sync completed`,
        results,
        summary: {
          totalFetched: results.reduce((sum, r) => sum + r.recordsFetched, 0),
          totalInserted: results.reduce((sum, r) => sum + r.recordsInserted, 0),
          totalUpdated: results.reduce((sum, r) => sum + r.recordsUpdated, 0),
          totalFailed: results.reduce((sum, r) => sum + r.recordsFailed, 0),
          totalDuration: results.reduce((sum, r) => sum + r.duration, 0)
        }
      },
      { status }
    );

  } catch (error) {
    console.error('[Admin Sync] Error:', error);
    console.error('[Admin Sync] Error stack:', error instanceof Error ? error.stack : 'No stack trace');
    return NextResponse.json(
      {
        error: 'Sync failed',
        message: error instanceof Error ? error.message : 'Unknown error',
        stack: error instanceof Error ? error.stack : undefined
      },
      { status: 500 }
    );
  }
}

/**
 * GET endpoint to check sync status and history
 */
export async function GET(request: NextRequest) {
  try {
    // Authentication
    const session = await getServerSession(authOptions);
    if (!session || session.user.role !== 'super_admin') {
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 403 }
      );
    }

    const { searchParams } = new URL(request.url);
    const limit = parseInt(searchParams.get('limit') || '20');
    const entityType = searchParams.get('entityType') || undefined;

    // Query sync log
    const { sql } = await import('@/lib/db/client');

    let query;
    if (entityType) {
      query = sql`
        SELECT *
        FROM repcard_sync_log
        WHERE entity_type = ${entityType}
        ORDER BY started_at DESC
        LIMIT ${limit}
      `;
    } else {
      query = sql`
        SELECT *
        FROM repcard_sync_log
        ORDER BY started_at DESC
        LIMIT ${limit}
      `;
    }

    const syncHistoryResult = await query;
    
    // @vercel/postgres returns results with .rows property
    const getRows = (result: any) => result.rows || (Array.isArray(result) ? result : []);
    
    const syncHistory = getRows(syncHistoryResult);

    // Get latest sync for each entity type (including failed/running)
    const latestSyncsRaw = await sql`
      SELECT DISTINCT ON (entity_type)
        id,
        entity_type,
        sync_type,
        status,
        started_at,
        completed_at,
        records_fetched,
        records_inserted,
        records_updated,
        records_failed,
        last_record_date,
        error_message
      FROM repcard_sync_log
      ORDER BY entity_type, started_at DESC
    `;
    const latestSyncs = getRows(latestSyncsRaw);

    // Get record counts from tables (actual database counts - always accurate)
    const customerCountResult = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const appointmentCountResult = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    const statusLogCountResult = await sql`SELECT COUNT(*) as count FROM repcard_status_logs`;
    const userCountResult = await sql`SELECT COUNT(*) as count FROM repcard_users`;

    return NextResponse.json({
      latestSyncs,
      syncHistory,
      recordCounts: {
        customers: Number(getRows(customerCountResult)[0]?.count || 0),
        appointments: Number(getRows(appointmentCountResult)[0]?.count || 0),
        statusLogs: Number(getRows(statusLogCountResult)[0]?.count || 0),
        users: Number(getRows(userCountResult)[0]?.count || 0)
      }
    });

  } catch (error) {
    console.error('[Admin Sync] Error fetching sync status:', error);
    return NextResponse.json(
      {
        error: 'Failed to fetch sync status',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
