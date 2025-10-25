/**
 * API Route: POST /api/admin/sync-users
 * Admin endpoint for manually triggering user sync
 */

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { runSync } from '@/scripts/sync-users-from-contacts';
import { createSyncRun, updateSyncRun, isSyncRunning } from '@/lib/sync/syncRunLogger';
import { sendSyncFailureNotification, sendSyncTimeoutNotification } from '@/lib/notifications/slack';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export const runtime = 'nodejs';

interface ManualSyncRequest {
  force?: boolean;
  limit?: number;
  verbose?: boolean;
}

export async function POST(request: NextRequest) {
  const startTime = Date.now();
  let runId: number | null = null;
  
  try {
    // Auth check - super_admin only
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    // Only super_admin can trigger manual sync
    const userRole = auth.session.user?.role;
    if (userRole !== 'super_admin') {
      return NextResponse.json({ 
        error: 'Unauthorized - super admin access required' 
      }, { status: 403 });
    }

    // Check if sync is already running
    const syncRunning = await isSyncRunning();
    if (syncRunning) {
      return NextResponse.json({
        error: 'Sync is already running',
        message: 'Please wait for the current sync to complete before starting a new one'
      }, { status: 429 });
    }

    // Parse request body
    const body: ManualSyncRequest = await request.json().catch(() => ({}));
    const options = {
      dryRun: false,
      verbose: body.verbose || false,
      force: body.force || false,
      includeTest: false,
      limit: body.limit
    };

    const userId = auth.session.user?.id;
    const userEmail = auth.session.user?.email;

    logApiRequest('POST', '/api/admin/sync-users', {
      source: 'admin-manual',
      userId,
      userEmail,
      options,
      timestamp: new Date().toISOString()
    });

    // Create sync run record
    runId = await createSyncRun('manual', userId);
    console.log(`📊 Created manual sync run record: ${runId} by ${userEmail}`);

    // Run user sync
    const result = await runSync(options);

    // Update sync run record
    await updateSyncRun(runId, result);

    const executionTime = Date.now() - startTime;
    const errorRate = result.stats.totalUsers > 0 
      ? Math.round((result.stats.errors / result.stats.totalUsers) * 100) 
      : 0;

    // Check for failure conditions and send notifications
    if (!result.success) {
      console.log('🚨 Manual sync failed - sending critical notification');
      await sendSyncFailureNotification(result.stats, 'Manual sync failure');
    } else if (errorRate > 10) {
      console.log('⚠️ High error rate in manual sync - sending warning notification');
      await sendSyncFailureNotification(result.stats);
    }

    // Check for timeout warning
    if (executionTime > 600000) { // 10 minutes
      console.log('⏰ Manual sync timeout warning - sending timeout notification');
      await sendSyncTimeoutNotification(executionTime, result.stats);
    }

    const response = {
      success: result.success,
      message: result.success 
        ? 'User sync completed successfully' 
        : 'User sync completed with errors',
      stats: {
        totalUsers: result.stats.totalUsers,
        enriched: result.stats.enriched,
        notFoundInContacts: result.stats.notFoundInContacts,
        alreadyUpToDate: result.stats.alreadyUpToDate,
        errors: result.stats.errors
      },
      execution_time_ms: executionTime,
      run_id: runId,
      triggered_by: userEmail
    };

    logApiResponse('POST', '/api/admin/sync-users', 200, {
      runId,
      userId,
      userEmail,
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
    
    logError('Admin manual sync failed', error, {
      url: request.url,
      method: 'POST',
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
