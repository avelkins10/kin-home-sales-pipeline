import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { qbClient } from '@/lib/quickbase/client';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { getUserEmail } from '@/lib/auth/userIdentity';

const QB_TABLE_PROJECTS = process.env.QUICKBASE_TABLE_PROJECTS || 'br9kwm8na';

/**
 * Debug endpoint to see ALL unique PROJECT_STATUS values
 * GET /api/debug/project-statuses?startDate=2025-09-30&endDate=2025-10-20
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

    const userId = session.user.quickbaseUserId;
    const role = session.user.role;
    const userEmail = getUserEmail(session);

    // Build access clause
    const accessClause = buildProjectAccessClause(userEmail, role);

    // Build date filter
    const dateFilter = `AND {${PROJECT_FIELDS.SALES_DATE}.OAF.'${startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${endDate}'}`;
    const whereClause = `${accessClause} ${dateFilter}`.trim();

    // Query projects
    const response = await qbClient.queryRecords({
      from: QB_TABLE_PROJECTS,
      where: whereClause,
      select: [
        PROJECT_FIELDS.RECORD_ID,
        PROJECT_FIELDS.PROJECT_ID,
        PROJECT_FIELDS.PROJECT_STATUS,
        PROJECT_FIELDS.ON_HOLD,
        PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED,
        PROJECT_FIELDS.INTAKE_COMPLETED_DATE,
        PROJECT_FIELDS.SALES_OFFICE,
      ],
      options: {
        top: 5000
      }
    });

    const projects = response.data || [];

    // Count by PROJECT_STATUS
    const statusCounts: Record<string, number> = {};
    projects.forEach((project: Record<string, any>) => {
      const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || 'Unknown';
      statusCounts[status] = (statusCounts[status] || 0) + 1;
    });

    // Count by intake status
    let intakeRejectedAwaitingFix = 0;
    projects.forEach((project: Record<string, any>) => {
      if (
        project[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject' &&
        !project[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ) {
        intakeRejectedAwaitingFix++;
      }
    });

    // Sort by count descending
    const sortedStatuses = Object.entries(statusCounts)
      .sort((a, b) => b[1] - a[1])
      .map(([status, count]) => ({ status, count }));

    return NextResponse.json({
      dateRange: { startDate, endDate },
      totalProjects: projects.length,
      uniqueStatusCount: sortedStatuses.length,
      statusBreakdown: sortedStatuses,
      intakeMetrics: {
        rejectedAwaitingResubmit: intakeRejectedAwaitingFix
      },
      note: 'This shows all unique PROJECT_STATUS values from QuickBase field 255'
    });
  } catch (error) {
    console.error('Error in project-statuses:', error);
    return NextResponse.json(
      { error: 'Failed to get project statuses', message: error instanceof Error ? error.message : String(error) },
      { status: 500 }
    );
  }
}
