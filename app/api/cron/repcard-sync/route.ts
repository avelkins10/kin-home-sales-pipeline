import { NextRequest, NextResponse } from 'next/server';
import { runIncrementalSync } from '@/lib/repcard/sync-service';

export const runtime = 'nodejs';
export const maxDuration = 300; // 5 minutes max

/**
 * Vercel Cron endpoint for automatic RepCard data syncing
 *
 * Runs incremental sync every 10-15 minutes to keep data fresh
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

    console.log('[RepCard Cron] Starting automatic incremental sync...');
    const startTime = Date.now();

    // Run incremental sync
    const results = await runIncrementalSync();

    const duration = Date.now() - startTime;
    const hasErrors = results.some(r => r.error);

    // Log summary
    const summary = {
      duration,
      totalFetched: results.reduce((sum, r) => sum + r.recordsFetched, 0),
      totalInserted: results.reduce((sum, r) => sum + r.recordsInserted, 0),
      totalUpdated: results.reduce((sum, r) => sum + r.recordsUpdated, 0),
      totalFailed: results.reduce((sum, r) => sum + r.recordsFailed, 0),
      hasErrors
    };

    console.log('[RepCard Cron] Sync completed:', summary);

    // Return appropriate status
    if (hasErrors) {
      return NextResponse.json(
        {
          success: false,
          message: 'Sync completed with errors',
          results,
          summary
        },
        { status: 206 } // 206 Partial Content
      );
    }

    return NextResponse.json({
      success: true,
      message: 'Incremental sync completed successfully',
      results,
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
