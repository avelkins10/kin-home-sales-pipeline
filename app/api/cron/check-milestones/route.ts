/**
 * API Route: GET /api/cron/check-milestones
 * Vercel Cron endpoint for scheduled milestone checks
 */

import { NextRequest, NextResponse } from 'next/server';
import { checkMilestones } from '@/lib/jobs/milestone-monitor';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCNotificationSummary } from '@/lib/types/operations';

export const runtime = 'nodejs';

export async function GET(request: NextRequest) {
  const startTime = Date.now();
  
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

    logApiRequest('GET', '/api/cron/check-milestones', {
      source: 'vercel-cron',
      timestamp: new Date().toISOString()
    });

    // Run milestone check for all PCs
    const summary: PCNotificationSummary = await checkMilestones();

    const executionTime = Date.now() - startTime;
    const response = {
      success: true,
      summary,
      execution_time_ms: executionTime,
      timestamp: new Date().toISOString()
    };

    logApiResponse('GET', '/api/cron/check-milestones', 200, {
      totalNotifications: summary.total_notifications,
      createdCount: summary.created_count,
      skippedCount: summary.skipped_count,
      errorCount: summary.error_count,
      executionTimeMs: executionTime
    });

    return NextResponse.json(response);

  } catch (error) {
    const executionTime = Date.now() - startTime;
    
    logError('Cron milestone check failed', error, {
      url: request.url,
      method: 'GET',
      executionTimeMs: executionTime
    });

    return NextResponse.json(
      { 
        success: false,
        error: 'Internal server error',
        execution_time_ms: executionTime,
        timestamp: new Date().toISOString()
      },
      { status: 500 }
    );
  }
}
