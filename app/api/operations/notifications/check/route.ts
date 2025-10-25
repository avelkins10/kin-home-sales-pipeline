/**
 * API Route: POST /api/operations/notifications/check
 * Manually trigger milestone checks for PC notifications
 */

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { checkMilestones } from '@/lib/jobs/milestone-monitor';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCNotificationSummary } from '@/lib/types/operations';

export const runtime = 'nodejs';

// Rate limiting cache (in-memory for simplicity)
const checkCache = new Map<string, number>();
const RATE_LIMIT_WINDOW = 5 * 60 * 1000; // 5 minutes
const CACHE_CLEANUP_INTERVAL = 10 * 60 * 1000; // 10 minutes

export async function POST(request: NextRequest) {
  const startTime = Date.now();
  
  try {
    // Verify authentication
    const auth = await requireAuth();
    if (!auth.authorized) {
      return auth.response;
    }

    const { user } = auth.session as any;

    // Check user role for operations access
    const allowedRoles = [
      'operations_coordinator', 
      'operations_manager', 
      'office_leader', 
      'regional', 
      'super_admin'
    ];
    
    if (!allowedRoles.includes(user.role)) {
      return NextResponse.json({ error: 'Forbidden' }, { status: 403 });
    }

    // Extract PC email from session
    const pcEmail = user.email;
    if (!pcEmail) {
      return NextResponse.json({ error: 'User email not found' }, { status: 400 });
    }

    // Parse request body
    const body = await request.json().catch(() => ({}));
    const { pcEmail: targetPcEmail, force = false } = body;

    // Determine which PC to check
    const checkEmail = targetPcEmail || pcEmail;
    const isSuperAdmin = user.role === 'super_admin';

    logApiRequest('POST', '/api/operations/notifications/check', {
      pcEmail,
      checkEmail,
      force,
      isSuperAdmin
    });

    // Clean up old cache entries before checking rate limit
    const now = Date.now();
    for (const [email, timestamp] of checkCache.entries()) {
      if (now - timestamp > CACHE_CLEANUP_INTERVAL) {
        checkCache.delete(email);
      }
    }

    // Check rate limiting (unless forced or super admin)
    if (!force && !isSuperAdmin) {
      const lastCheck = checkCache.get(checkEmail);
      
      if (lastCheck && (now - lastCheck) < RATE_LIMIT_WINDOW) {
        const remainingTime = Math.ceil((RATE_LIMIT_WINDOW - (now - lastCheck)) / 1000);
        return NextResponse.json(
          { 
            error: 'Rate limit exceeded', 
            message: `Please wait ${remainingTime} seconds before checking again`,
            retry_after: remainingTime
          }, 
          { status: 429 }
        );
      }
    }

    // Update rate limit cache
    checkCache.set(checkEmail, Date.now());

    // Run milestone check
    const summary: PCNotificationSummary = await checkMilestones(checkEmail);

    const response = {
      success: true,
      summary,
      message: `Milestone check completed. Created ${summary.created_count} notifications, skipped ${summary.skipped_count} items.`
    };

    const executionTime = Date.now() - startTime;
    logApiResponse('POST', '/api/operations/notifications/check', 200, {
      checkEmail,
      createdCount: summary.created_count,
      skippedCount: summary.skipped_count,
      errorCount: summary.error_count,
      executionTimeMs: executionTime
    });

    return NextResponse.json(response);

  } catch (error) {
    logError('Failed to run milestone check', error, {
      url: request.url,
      method: 'POST'
    });

    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}
