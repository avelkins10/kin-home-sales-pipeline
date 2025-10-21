import { NextRequest, NextResponse } from 'next/server';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { fetchProjects } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import type { QuickbaseProject } from '@/lib/types/project';
import type { WeeklyIntakeReport, WeeklyIntakeCloserReport } from '@/lib/types/reports';

/**
 * GET /api/reports/intake-weekly
 *
 * Returns weekly intake quality report grouped by closer
 *
 * Query params:
 * - startDate: ISO date string (required)
 * - endDate: ISO date string (required)
 * - closerId: Optional - filter to specific closer
 */
export async function GET(request: NextRequest) {
  try {
    const session = await getServerSession(authOptions);

    if (!session?.user?.quickbaseUserId || !session?.user?.role) {
      return NextResponse.json(
        { error: 'Unauthorized' },
        { status: 401 }
      );
    }

    const searchParams = request.nextUrl.searchParams;
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');
    const closerId = searchParams.get('closerId'); // Optional filter

    if (!startDate || !endDate) {
      return NextResponse.json(
        { error: 'startDate and endDate are required' },
        { status: 400 }
      );
    }

    // Fetch all projects in date range (filtered by sales date)
    const whereClause = `{${PROJECT_FIELDS.SALES_DATE}.OAF.'${startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${endDate}'}`;

    const projects = await fetchProjects({
      userId: session.user.quickbaseUserId,
      role: session.user.role,
      where: whereClause,
    });

    // Group projects by closer email
    const projectsByCloser = new Map<string, QuickbaseProject[]>();

    projects.forEach((project: QuickbaseProject) => {
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
      if (!closerEmail) return; // Skip projects without closer

      if (!projectsByCloser.has(closerEmail)) {
        projectsByCloser.set(closerEmail, []);
      }
      projectsByCloser.get(closerEmail)!.push(project);
    });

    // Calculate metrics for each closer
    const closerReports: WeeklyIntakeCloserReport[] = [];

    for (const [closerEmail, closerProjects] of Array.from(projectsByCloser.entries())) {
      // Get closer name from first project
      const closerName = closerProjects[0][PROJECT_FIELDS.CLOSER_NAME]?.value || closerEmail;
      const officeName = closerProjects[0][PROJECT_FIELDS.SALES_OFFICE]?.value || null;

      // Total submitted = all projects sold in date range
      const totalSubmitted = closerProjects.length;

      // Projects with first pass result
      const projectsWithFirstPass = closerProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value
      );

      // First-time approved = field 1831 = "Approve"
      const firstTimeApproved = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Approve'
      ).length;

      // First-time rejected = field 1831 = "Reject"
      const firstTimeRejected = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      ).length;

      // Pending review = no first pass result yet
      const pendingReview = closerProjects.filter(p =>
        !p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value
      ).length;

      // Resubmitted and approved = field 1831 = "Reject" BUT field 461 exists
      const resubmittedAndApproved = closerProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject' &&
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Calculate rates
      const firstTimePassRate = projectsWithFirstPass.length > 0
        ? (firstTimeApproved / projectsWithFirstPass.length) * 100
        : 0;

      const rejectionRate = projectsWithFirstPass.length > 0
        ? (firstTimeRejected / projectsWithFirstPass.length) * 100
        : 0;

      // Top rejection reasons for this closer
      const reasonCounts: Record<string, number> = {};
      closerProjects.forEach(p => {
        const reasons = p[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value;
        if (Array.isArray(reasons)) {
          reasons.forEach(reason => {
            reasonCounts[reason] = (reasonCounts[reason] || 0) + 1;
          });
        }
      });

      const topRejectionReasons = Object.entries(reasonCounts)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 3)
        .map(([reason, count]) => ({ reason, count }));

      // Average resolution time
      const rejectedProjects = closerProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      );

      const resolutionTimes = rejectedProjects
        .map(p => {
          const firstReview = p[PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE]?.value;
          const finalApproval = p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value;
          if (!firstReview || !finalApproval) return null;

          const days = (new Date(finalApproval).getTime() - new Date(firstReview).getTime())
            / (1000 * 60 * 60 * 24);
          return Math.round(days);
        })
        .filter(time => time !== null && time > 0) as number[];

      const avgResolutionTime = resolutionTimes.length > 0
        ? resolutionTimes.reduce((sum, time) => sum + time, 0) / resolutionTimes.length
        : null;

      closerReports.push({
        closerName,
        closerEmail,
        officeName,
        totalSubmitted,
        firstTimeApproved,
        firstTimeRejected,
        pendingReview,
        resubmittedAndApproved,
        firstTimePassRate: Math.round(firstTimePassRate * 100) / 100,
        rejectionRate: Math.round(rejectionRate * 100) / 100,
        topRejectionReasons,
        avgResolutionTime: avgResolutionTime ? Math.round(avgResolutionTime) : null,
      });
    }

    // Sort by first-time pass rate descending
    closerReports.sort((a, b) => b.firstTimePassRate - a.firstTimePassRate);

    // Calculate overall metrics
    const totalProjects = projects.length;
    const allFirstTimeApproved = projects.filter((p: QuickbaseProject) =>
      p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Approve'
    ).length;
    const projectsWithFirstPass = projects.filter((p: QuickbaseProject) =>
      p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value
    ).length;

    const overallPassRate = projectsWithFirstPass > 0
      ? (allFirstTimeApproved / projectsWithFirstPass) * 100
      : 0;

    const overallRejectionRate = projectsWithFirstPass > 0
      ? 100 - overallPassRate
      : 0;

    // Find most common rejection reason across all closers
    const allReasonCounts: Record<string, number> = {};
    projects.forEach((p: QuickbaseProject) => {
      const reasons = p[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value;
      if (Array.isArray(reasons)) {
        reasons.forEach((reason: string) => {
          allReasonCounts[reason] = (allReasonCounts[reason] || 0) + 1;
        });
      }
    });

    const topRejectionReason = Object.entries(allReasonCounts).length > 0
      ? Object.entries(allReasonCounts).sort((a, b) => b[1] - a[1])[0][0]
      : null;

    // Find top performer and needs attention
    const topPerformer = closerReports.length > 0 && closerReports[0].totalSubmitted >= 3
      ? {
          name: closerReports[0].closerName,
          rate: closerReports[0].firstTimePassRate,
        }
      : null;

    const needsAttention = closerReports.length > 0 && closerReports[closerReports.length - 1].totalSubmitted >= 3
      ? {
          name: closerReports[closerReports.length - 1].closerName,
          rate: closerReports[closerReports.length - 1].firstTimePassRate,
        }
      : null;

    const report: WeeklyIntakeReport = {
      startDate,
      endDate,
      totalProjects,
      overallPassRate: Math.round(overallPassRate * 100) / 100,
      overallRejectionRate: Math.round(overallRejectionRate * 100) / 100,
      topRejectionReason,
      closers: closerReports,
      topPerformer,
      needsAttention,
    };

    return NextResponse.json(report);
  } catch (error) {
    console.error('Error generating weekly intake report:', error);
    return NextResponse.json(
      { error: 'Failed to generate report' },
      { status: 500 }
    );
  }
}
