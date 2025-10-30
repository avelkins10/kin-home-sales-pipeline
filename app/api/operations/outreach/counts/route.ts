import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { qbClient } from '@/lib/quickbase/client';
import { OUTREACH_RECORD_FIELDS, QB_TABLE_OUTREACH_RECORDS } from '@/lib/constants/fieldIds';

export const runtime = 'nodejs';
export const dynamic = 'force-dynamic';

/**
 * GET /api/operations/outreach/counts
 * Get lightweight outreach counts for navbar badges
 */
export async function GET(request: NextRequest) {
  const reqId = `outreach-counts-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('GET /api/operations/outreach/counts', {}, reqId);

    // Require authentication and check role
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;
    const session = auth.session;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!session?.user?.role || !allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    // Get user email for filtering
    const userEmail = session.user.email;
    if (!userEmail) {
      return NextResponse.json(
        { error: 'User email not found' },
        { status: 400 }
      );
    }

    // Query for pending outreach counts only
    const countsQuery = {
      from: QB_TABLE_OUTREACH_RECORDS,
      select: [
        OUTREACH_RECORD_FIELDS.RECORD_ID,
        OUTREACH_RECORD_FIELDS.OUTREACH_STATUS,
        OUTREACH_RECORD_FIELDS.PROJECT_COORDINATOR
      ],
      where: `{${OUTREACH_RECORD_FIELDS.PROJECT_COORDINATOR}.EX.'${userEmail}'}AND{${OUTREACH_RECORD_FIELDS.OUTREACH_STATUS}.EX.'Pending'}`
    };

    const response = await qbClient.queryRecords(countsQuery);
    const records = response.data || [];

    // Count pending outreach
    const pendingCount = records.length;

    const result = {
      pendingCount,
      totalCount: pendingCount
    };

    logApiResponse(reqId, 200, result);
    return NextResponse.json(result);

  } catch (error) {
    logError(reqId, 'Outreach counts API error', error);
    
    if (error instanceof Error) {
      return NextResponse.json(
        { 
          error: 'Failed to fetch outreach counts',
          details: error.message 
        },
        { status: 500 }
      );
    }

    return NextResponse.json(
      { error: 'Internal server error' },
      { status: 500 }
    );
  }
}

