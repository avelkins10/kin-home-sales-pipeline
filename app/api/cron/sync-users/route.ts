/**
 * API Route: GET /api/cron/sync-users
 * Vercel Cron endpoint for scheduled user sync from QuickBase Contacts
 */

import { NextRequest, NextResponse } from 'next/server';
import { runSync } from '@/scripts/sync-users-from-contacts';
import { createSyncRun, updateSyncRun } from '@/lib/sync/syncRunLogger';
import { sendSyncFailureNotification, sendSyncTimeoutNotification } from '@/lib/notifications/slack';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  const startTime = Date.now();
  let runId: number | null = null;
  
  try {
    // Verify request is from Vercel Cron
    const authHeader = request.headers.get('authorization');
    const cronSecret = process.env.CRON_SECRET;
    
    if (!cronSecret) {
      logError('CRON_SECRET environment variable not set');
      return NextResponse.json(
        { error: 'Cron secret not configured' },
        { status: 500 }
      );
    }

    if (!authHeader || authHeader !== `Bearer ${cronSecret}`) {
      logError('Invalid cron authentication', null, {
        authHeader: authHeader ? 'present' : 'missing',
        expectedFormat: 'Bearer [secret]'
      });
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }

    logApiRequest('GET', '/api/cron/sync-users', {
      source: 'vercel-cron',
      timestamp: new Date().toISOString()
    });

    // Create sync run record
    runId = await createSyncRun('cron');
    console.log(`📊 Created sync run record: ${runId}`);

    // Run user sync
    const result = await runSync({
      dryRun: false,
      verbose: false,
      force: false,
      includeTest: false
    });

    // Update sync run record
    await updateSyncRun(runId, result);

    const executionTime = Date.now() - startTime;
    const errorRate = result.stats.totalUsers > 0 
      ? Math.round((result.stats.errors / result.stats.totalUsers) * 100) 
      : 0;

    // Check for failure conditions and send notifications
    if (!result.success) {
      console.log('🚨 Sync failed - sending critical notification');
      await sendSyncFailureNotification(result.stats, 'Complete sync failure');
    } else if (errorRate > 10) {
      console.log('⚠️ High error rate - sending warning notification');
      await sendSyncFailureNotification(result.stats);
    }

    // Check for timeout warning
    if (executionTime > 600000) { // 10 minutes
      console.log('⏰ Sync timeout warning - sending timeout notification');
      await sendSyncTimeoutNotification(executionTime, result.stats);
    }

    const response = {
      success: result.success,
      stats: {
        totalUsers: result.stats.totalUsers,
        enriched: result.stats.enriched,
        notFoundInContacts: result.stats.notFoundInContacts,
        alreadyUpToDate: result.stats.alreadyUpToDate,
        errors: result.stats.errors
      },
      execution_time_ms: executionTime,
      timestamp: result.timestamp,
      run_id: runId
    };

    logApiResponse('GET', '/api/cron/sync-users', 200, {
      runId,
      totalUsers: result.stats.totalUsers,
      enriched: result.stats.enriched,
      errors: result.stats.errors,
      errorRate,
      executionTimeMs: executionTime
    });

    return NextResponse.json(response);

  } catch (error) {
    const executionTime = Date.now() - startTime;
    
    // Update sync run as failed if we have a runId
    if (runId) {
      try {
        await updateSyncRun(runId, {
          success: false,
          stats: {
            totalUsers: 0,
            enriched: 0,
            notFoundInContacts: 0,
            alreadyUpToDate: 0,
            errors: 1,
            errorDetails: [{ email: 'system', error: error instanceof Error ? error.message : String(error) }],
            notFoundSamples: []
          },
          executionTimeMs: executionTime,
          timestamp: new Date().toISOString()
        });
      } catch (updateError) {
        console.error('❌ Failed to update sync run as failed:', updateError);
      }
    }

    // Send failure notification
    try {
      await sendSyncFailureNotification({
        totalUsers: 0,
        enriched: 0,
        notFoundInContacts: 0,
        alreadyUpToDate: 0,
        errors: 1,
        errorDetails: [{ email: 'system', error: error instanceof Error ? error.message : String(error) }],
        notFoundSamples: []
      }, error instanceof Error ? error.message : String(error));
    } catch (notificationError) {
      console.error('❌ Failed to send failure notification:', notificationError);
    }
    
    logError('Cron user sync failed', error, {
      url: request.url,
      method: 'GET',
      runId,
      executionTimeMs: executionTime
    });

    return NextResponse.json(
      { 
        success: false,
        error: 'Internal server error',
        execution_time_ms: executionTime,
        timestamp: new Date().toISOString(),
        run_id: runId
      },
      { status: 500 }
    );
  }
}
