import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getProjectsForPC } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import type { OperationsMilestone } from '@/lib/types/operations';

export const runtime = 'nodejs';

/**
 * GET /api/operations/projects
 * Fetch all projects for a Project Coordinator with optional filters
 *
 * Query parameters:
 * - milestone: Optional milestone filter (intake, survey, design, permitting, install, inspection, pto)
 * - search: Search by project ID or customer name
 * - sort: Sort order (newest, oldest, projectId, customer, daysDesc)
 * - office: Filter by sales office
 * - salesRep: Filter by sales rep name
 */
export async function GET(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    logApiRequest('GET', '/api/operations/projects', { reqId });

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

    const pcEmail = session.user.email || '';
    const pcName = session.user.name || '';
    const role = session.user.role;

    // Get query parameters
    const { searchParams } = new URL(request.url);
    const milestone = searchParams.get('milestone') as OperationsMilestone | null;
    const search = searchParams.get('search') || undefined;
    const sort = searchParams.get('sort') || undefined;
    const office = searchParams.get('office') || undefined;
    const salesRep = searchParams.get('salesRep') || undefined;

    // Validate milestone if provided
    const validMilestones: OperationsMilestone[] = [
      'intake', 'survey', 'design', 'permitting', 'install', 'inspection', 'pto'
    ];

    if (milestone && !validMilestones.includes(milestone)) {
      return NextResponse.json(
        { error: 'Invalid milestone', validMilestones },
        { status: 400 }
      );
    }

    // Fetch projects
    const result = await getProjectsForPC(
      pcEmail,
      pcName,
      role,
      {
        milestone: milestone || undefined,
        search,
        sort,
        office,
        salesRep
      },
      reqId
    );

    // Handle different return formats (grouped vs. flat array)
    const isGrouped = milestone && typeof result === 'object' && 'projects' in result;
    const projects = isGrouped ? (result as any).projects : result;

    const duration = Date.now() - startTime;
    logApiResponse('GET', '/api/operations/projects', duration, {
      reqId,
      count: Array.isArray(projects) ? projects.length : (result as any).total || 0,
      milestone: milestone || 'all'
    });

    return NextResponse.json({
      success: true,
      data: result
    });

  } catch (error) {
    logError(`GET /api/operations/projects failed`, error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: (error as Error).message },
      { status: 500 }
    );
  }
}
