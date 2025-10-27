export const runtime = 'nodejs';

import { NextResponse } from 'next/server';
import { sendDailyDigestToAllUsers } from '@/lib/utils/daily-digest';
import { logInfo, logError } from '@/lib/logging/logger';

/**
 * POST /api/cron/daily-digest
 * Vercel Cron job to send daily digest emails
 *
 * Scheduled to run daily at 8:00 AM via vercel.json
 */
export async function GET(request: Request) {
  const startTime = Date.now();

  try {
    // Verify cron secret (Vercel sets this in production)
    const authHeader = request.headers.get('authorization');
    const cronSecret = process.env.CRON_SECRET;

    if (cronSecret && authHeader !== `Bearer ${cronSecret}`) {
      logError('Unauthorized cron request', new Error('Invalid authorization'), {});
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    logInfo('Starting daily digest cron job');

    // Send digests to all users
    const summary = await sendDailyDigestToAllUsers();

    const executionTime = Date.now() - startTime;
    logInfo('Daily digest cron job completed', { ...summary, executionTimeMs: executionTime });

    return NextResponse.json({
      success: true,
      summary,
      executionTimeMs: executionTime
    }, { status: 200 });

  } catch (error) {
    const executionTime = Date.now() - startTime;
    logError('Daily digest cron job failed', error as Error, { executionTimeMs: executionTime });

    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : 'Unknown error'
    }, { status: 500 });
  }
}
