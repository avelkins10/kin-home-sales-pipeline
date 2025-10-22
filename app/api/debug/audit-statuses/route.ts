import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { getOfficeMetrics } from '@/lib/quickbase/queries';

/**
 * Debug endpoint to audit project statuses
 * GET /api/debug/audit-statuses?startDate=2025-09-30&endDate=2025-10-20
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);

    if (!session?.user?.quickbaseUserId || !session?.user?.role) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
    }

    const searchParams = request.nextUrl.searchParams;
    const startDate = searchParams.get('startDate') || '2025-09-30';
    const endDate = searchParams.get('endDate') || '2025-10-20';

    const metrics = await getOfficeMetrics(
      session.user.quickbaseUserId,
      session.user.role,
      'custom',
      undefined, // all offices
      { startDate, endDate },
      'debug-audit'
    );

    // Calculate totals
    const totals = {
      total: 0,
      active: 0,
      submitted: 0,
      approved: 0,
      rejected: 0,
      cancelled: 0,
      holds: 0
    };

    const officeDetails = metrics.map(office => {
      totals.total += office.totalProjects;
      totals.active += office.activeProjects;
      totals.submitted += office.projectsSubmitted;
      totals.approved += office.projectsApproved;
      totals.rejected += office.projectsRejected;
      totals.cancelled += office.cancelledProjects;
      totals.holds += office.onHoldProjects;

      const sum = office.activeProjects + office.projectsSubmitted +
                  office.projectsApproved + office.projectsRejected +
                  office.cancelledProjects + office.onHoldProjects;

      return {
        office: office.officeName,
        total: office.totalProjects,
        active: office.activeProjects,
        submitted: office.projectsSubmitted,
        approved: office.projectsApproved,
        rejected: office.projectsRejected,
        cancelled: office.cancelledProjects,
        holds: office.onHoldProjects,
        sum,
        matches: sum === office.totalProjects
      };
    });

    const totalSum = totals.active + totals.submitted + totals.approved +
                     totals.rejected + totals.cancelled + totals.holds;

    return NextResponse.json({
      dateRange: { startDate, endDate },
      officeCount: metrics.length,
      offices: officeDetails,
      aggregated: {
        ...totals,
        sum: totalSum,
        matches: totalSum === totals.total
      }
    });
  } catch (error) {
    console.error('Error in audit-statuses:', error);
    return NextResponse.json(
      { error: 'Failed to audit statuses', message: error instanceof Error ? error.message : String(error) },
      { status: 500 }
    );
  }
}
