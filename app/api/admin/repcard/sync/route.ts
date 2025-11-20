import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/config';
import { runComprehensiveSync } from '@/lib/repcard/comprehensive-sync';
import { ensureRepCardTables } from '@/lib/repcard/ensure-tables';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';
export const maxDuration = 300; // 5 minutes for full sync

/**
 * Admin endpoint for manually triggering RepCard data syncs
 *
 * Query parameters:
 * - type: 'full' | 'incremental' | 'quick' | 'customers' | 'appointments' | 'status_logs' | 'users'
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

    // Tables should already exist in production - only create if truly missing
    // (This is a safety net, not the normal path)
    try {
      const { sql } = await import('@/lib/db/client');
      const tableCheck = await sql`
        SELECT COUNT(*) as count 
        FROM information_schema.tables 
        WHERE table_schema = 'public' AND table_name LIKE 'repcard_%'
      `;
      const tableCount = Number(Array.from(tableCheck)[0]?.count || 0);
      
      if (tableCount === 0) {
        console.log('[Admin Sync] ⚠️ No RepCard tables found - running migrations...');
        await ensureRepCardTables();
        console.log('[Admin Sync] ✅ Tables created');
      } else {
        console.log(`[Admin Sync] ✅ Tables exist (${tableCount} found)`);
      }
    } catch (tableError) {
      console.error('[Admin Sync] ⚠️ Table check failed (non-fatal):', tableError);
      // Continue - assume tables exist
    }

    const { searchParams } = new URL(request.url);
    const syncType = searchParams.get('type') || 'incremental';
    const startDate = searchParams.get('startDate') || undefined;
    const endDate = searchParams.get('endDate') || undefined;

    console.log(`[Admin Sync] Starting ${syncType} sync requested by ${session.user.email}`, { startDate, endDate });

    // Handle individual entity types (for backward compatibility)
    if (syncType === 'users') {
      const result = await runComprehensiveSync({
        skipOffices: false,
        skipCustomers: true,
        skipAppointments: true,
        skipStatusLogs: true,
        skipCustomerAttachments: true,
        skipAppointmentAttachments: true
      });
      return NextResponse.json({
        message: 'Users sync completed',
        result: result.users,
        recordsFetched: result.users.recordsFetched,
        recordsInserted: result.users.recordsInserted,
        recordsUpdated: result.users.recordsUpdated,
        recordsFailed: result.users.recordsFailed,
        duration: result.users.duration
      });
    }

    if (syncType === 'customers') {
      const result = await runComprehensiveSync({
        skipUsers: false,
        skipOffices: false,
        skipCustomers: false,
        skipAppointments: true,
        skipStatusLogs: true,
        startDate,
        endDate,
        incremental: false
      });
      return NextResponse.json({
        message: 'Customers sync completed',
        result: result.customers,
        recordsFetched: result.customers.recordsFetched,
        recordsInserted: result.customers.recordsInserted,
        recordsUpdated: result.customers.recordsUpdated,
        recordsFailed: result.customers.recordsFailed,
        duration: result.customers.duration
      });
    }

    if (syncType === 'appointments') {
      const result = await runComprehensiveSync({
        skipUsers: false,
        skipOffices: false,
        skipCustomers: true,
        skipAppointments: false,
        skipStatusLogs: true,
        startDate,
        endDate,
        incremental: false
      });
      return NextResponse.json({
        message: 'Appointments sync completed',
        result: result.appointments,
        recordsFetched: result.appointments.recordsFetched,
        recordsInserted: result.appointments.recordsInserted,
        recordsUpdated: result.appointments.recordsUpdated,
        recordsFailed: result.appointments.recordsFailed,
        duration: result.appointments.duration
      });
    }

    // Handle comprehensive sync types
    let syncOptions: any = {
      startDate,
      endDate,
      incremental: syncType === 'incremental'
    };

    // Quick sync: last 7 days, skip attachments and status logs for speed
    if (syncType === 'quick') {
      const sevenDaysAgo = new Date();
      sevenDaysAgo.setDate(sevenDaysAgo.getDate() - 7);
      syncOptions = {
        startDate: sevenDaysAgo.toISOString().split('T')[0],
        endDate: new Date().toISOString().split('T')[0],
        incremental: false,
        skipCustomerAttachments: true,
        skipAppointmentAttachments: true,
        skipStatusLogs: true
      };
    }

    // Full sync: include everything
    if (syncType === 'full') {
      syncOptions = {
        startDate,
        endDate,
        incremental: false,
        skipCustomerAttachments: false,
        skipAppointmentAttachments: false
      };
    }

    // Run comprehensive sync (handles tables, users, linking automatically)
    const result = await runComprehensiveSync(syncOptions);

    // Check if any syncs failed
    const hasErrors = !!(
      result.users.error ||
      result.offices.error ||
      result.customers.error ||
      result.appointments.error ||
      result.statusLogs.error
    );
    const status = hasErrors ? 206 : 200;

    return NextResponse.json(
      {
        message: `${syncType} sync completed`,
        result,
        summary: {
          totalFetched: 
            result.users.recordsFetched +
            result.offices.recordsFetched +
            result.customers.recordsFetched +
            result.appointments.recordsFetched +
            result.statusLogs.recordsFetched,
          totalInserted:
            result.users.recordsInserted +
            result.offices.recordsInserted +
            result.customers.recordsInserted +
            result.appointments.recordsInserted +
            result.statusLogs.recordsInserted,
          totalUpdated:
            result.users.recordsUpdated +
            result.offices.recordsUpdated +
            result.customers.recordsUpdated +
            result.appointments.recordsUpdated +
            result.statusLogs.recordsUpdated,
          totalFailed:
            result.users.recordsFailed +
            result.offices.recordsFailed +
            result.customers.recordsFailed +
            result.appointments.recordsFailed +
            result.statusLogs.recordsFailed,
          totalDuration: result.totalDuration
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

    const syncHistory = await query;

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
    const latestSyncs = Array.from(latestSyncsRaw);

    // Get record counts from tables (actual database counts - always accurate)
    const customerCountResult = await sql`SELECT COUNT(*) as count FROM repcard_customers`;
    const appointmentCountResult = await sql`SELECT COUNT(*) as count FROM repcard_appointments`;
    const statusLogCountResult = await sql`SELECT COUNT(*) as count FROM repcard_status_logs`;
    const userCountResult = await sql`SELECT COUNT(*) as count FROM repcard_users`;

    return NextResponse.json({
      latestSyncs,
      syncHistory: Array.from(syncHistory),
      recordCounts: {
        customers: Number(Array.from(customerCountResult)[0]?.count || 0),
        appointments: Number(Array.from(appointmentCountResult)[0]?.count || 0),
        statusLogs: Number(Array.from(statusLogCountResult)[0]?.count || 0),
        users: Number(Array.from(userCountResult)[0]?.count || 0)
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
