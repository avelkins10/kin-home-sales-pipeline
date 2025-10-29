import { NextRequest, NextResponse } from 'next/server';
import { runComprehensiveSync } from '@/lib/repcard/comprehensive-sync';

export const runtime = 'nodejs';
export const maxDuration = 300; // 5 minutes max

/**
 * Vercel Cron endpoint for automatic RepCard data syncing
 *
 * Runs comprehensive incremental sync every 5-10 minutes to keep data fresh
 * Syncs: users, offices, customers, appointments, status logs, attachments
 * 
 * Vercel will call this endpoint automatically based on vercel.json cron config
 *
 * Authentication: Uses CRON_SECRET env variable to verify requests from Vercel
 */
export async function GET(request: NextRequest) {
  try {
    // Verify this request is from Vercel Cron
    const authHeader = request.headers.get('authorization');
    const cronSecret = process.env.CRON_SECRET;

    if (cronSecret && authHeader !== `Bearer ${cronSecret}`) {
      console.error('[RepCard Cron] Unauthorized cron request');
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }

    console.log('[RepCard Cron] Starting automatic comprehensive incremental sync...');
    const startTime = Date.now();

    // Run comprehensive incremental sync (skips attachments for speed - run full sync less frequently)
    const result = await runComprehensiveSync({
      incremental: true,
      skipCustomerAttachments: true, // Skip attachments in frequent syncs (too slow)
      skipAppointmentAttachments: true
    });

    const duration = Date.now() - startTime;
    const hasErrors = !!(
      result.users.error ||
      result.offices.error ||
      result.customers.error ||
      result.appointments.error ||
      result.statusLogs.error ||
      result.customerAttachments.error ||
      result.appointmentAttachments.error
    );

    // Log summary
    const summary = {
      duration,
      totalFetched: 
        result.users.recordsFetched +
        result.offices.recordsFetched +
        result.customers.recordsFetched +
        result.appointments.recordsFetched +
        result.statusLogs.recordsFetched +
        result.customerAttachments.recordsFetched +
        result.appointmentAttachments.recordsFetched,
      totalInserted:
        result.users.recordsInserted +
        result.offices.recordsInserted +
        result.customers.recordsInserted +
        result.appointments.recordsInserted +
        result.statusLogs.recordsInserted +
        result.customerAttachments.recordsInserted +
        result.appointmentAttachments.recordsInserted,
      totalUpdated:
        result.users.recordsUpdated +
        result.offices.recordsUpdated +
        result.customers.recordsUpdated +
        result.appointments.recordsUpdated +
        result.statusLogs.recordsUpdated +
        result.customerAttachments.recordsUpdated +
        result.appointmentAttachments.recordsUpdated,
      totalFailed:
        result.users.recordsFailed +
        result.offices.recordsFailed +
        result.customers.recordsFailed +
        result.appointments.recordsFailed +
        result.statusLogs.recordsFailed +
        result.customerAttachments.recordsFailed +
        result.appointmentAttachments.recordsFailed,
      hasErrors
    };

    console.log('[RepCard Cron] Comprehensive sync completed:', summary);

    // Return appropriate status
    if (hasErrors) {
      return NextResponse.json(
        {
          success: false,
          message: 'Sync completed with errors',
          result,
          summary
        },
        { status: 206 } // 206 Partial Content
      );
    }

    return NextResponse.json({
      success: true,
      message: 'Comprehensive incremental sync completed successfully',
      result,
      summary
    });

  } catch (error) {
    console.error('[RepCard Cron] Sync failed:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Sync failed',
        message: error instanceof Error ? error.message : 'Unknown error'
      },
      { status: 500 }
    );
  }
}
