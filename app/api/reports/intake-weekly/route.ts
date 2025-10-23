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

    projects.forEach(project => {
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
      if (!closerEmail) return; // Skip projects without closer

      if (!projectsByCloser.has(closerEmail)) {
        projectsByCloser.set(closerEmail, []);
      }
      projectsByCloser.get(closerEmail)!.push(project);
    });

    // Calculate metrics for each closer
    const closerReports: WeeklyIntakeCloserReport[] = [];

    for (const [closerEmail, closerProjects] of projectsByCloser) {
      // Get closer name from first project
      const closerName = closerProjects[0][PROJECT_FIELDS.CLOSER_NAME]?.value || closerEmail;
      const officeName = closerProjects[0][PROJECT_FIELDS.SALES_OFFICE]?.value || null;

      // Total submitted = all projects sold in date range
      const totalSubmitted = closerProjects.length;

      // Helper function to check if project is rejected
      const isRejected = (p: QuickbaseProject): boolean => {
        const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
        const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
        return intakeStatus.includes('rejected') || projectStatus.includes('rejected');
      };

      // Never Rejected (first pass approved) - has completion date AND never rejected
      const neverRejected = closerProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
        !isRejected(p)
      ).length;

      // Total Rejections (currently or ever rejected)
      const rejectedProjects = closerProjects.filter(isRejected);
      const totalRejections = rejectedProjects.length;

      // Total Fixed (was rejected, now has completion date)
      const totalFixed = closerProjects.filter(p =>
        isRejected(p) &&
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Still Rejected (rejected, no completion date)
      const stillRejected = closerProjects.filter(p =>
        isRejected(p) &&
        !p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Active/Approved (has completion date - passed intake)
      const activeApproved = closerProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Calculate rates based on total submitted
      const firstTimePassRate = totalSubmitted > 0
        ? (neverRejected / totalSubmitted) * 100
        : 0;

      const rejectionRate = totalSubmitted > 0
        ? (totalRejections / totalSubmitted) * 100
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

      // Average resolution time using task-based calculation
      // Calculate for rejected projects that were fixed (have completion date)
      const fixedProjects = closerProjects.filter(p =>
        isRejected(p) &&
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      );

      const resolutionTimes: number[] = [];
      for (const project of fixedProjects) {
        const projectId = project[PROJECT_FIELDS.RECORD_ID]?.value;
        if (projectId) {
          const { calculateTaskBasedResolutionTime } = await import('@/lib/quickbase/queries');
          const resolutionTime = await calculateTaskBasedResolutionTime(projectId);
          if (resolutionTime !== null && resolutionTime > 0) {
            resolutionTimes.push(resolutionTime);
          }
        }
      }

      const avgResolutionDays = resolutionTimes.length > 0
        ? Math.round((resolutionTimes.reduce((sum, time) => sum + time, 0) / resolutionTimes.length) * 10) / 10
        : null;

      closerReports.push({
        closerName,
        closerEmail,
        officeName,
        totalSubmitted,
        neverRejected,
        totalRejections,
        totalFixed,
        stillRejected,
        activeApproved,
        firstTimePassRate: Math.round(firstTimePassRate * 100) / 100,
        rejectionRate: Math.round(rejectionRate * 100) / 100,
        topRejectionReasons,
        avgResolutionDays,
      });
    }

    // Sort by first-time pass rate descending
    closerReports.sort((a, b) => b.firstTimePassRate - a.firstTimePassRate);

    // Calculate overall metrics
    const totalProjects = projects.length;

    // Helper function to check if project is rejected (at global level)
    const isRejectedGlobal = (p: QuickbaseProject): boolean => {
      const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
      const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
      return intakeStatus.includes('rejected') || projectStatus.includes('rejected');
    };

    // Never Rejected globally (completed and not rejected)
    const allNeverRejected = projects.filter(p =>
      p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
      !isRejectedGlobal(p)
    ).length;

    // Total Rejections globally
    const allRejectedProjects = projects.filter(isRejectedGlobal);
    const totalRejectedAll = allRejectedProjects.length;

    // Calculate rates based on total submitted
    const overallPassRate = totalProjects > 0
      ? (allNeverRejected / totalProjects) * 100
      : 0;

    const overallRejectionRate = totalProjects > 0
      ? (totalRejectedAll / totalProjects) * 100
      : 0;

    // Find most common rejection reason across all closers
    const allReasonCounts: Record<string, number> = {};
    projects.forEach(p => {
      const reasons = p[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value;
      if (Array.isArray(reasons)) {
        reasons.forEach(reason => {
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
