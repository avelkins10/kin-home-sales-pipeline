export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { fetchProjects } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import type { QuickbaseProject } from '@/lib/types/project';

interface OfficeRejectionStats {
  officeName: string;
  officeId: number | null;
  totalSubmitted: number;
  totalReviewed: number;
  firstTimeApproved: number;
  firstTimeRejected: number;
  stillRejected: number;
  resubmittedAndApproved: number;
  firstTimePassRate: number;
  rejectionRate: number;
  avgResolutionDays: number | null;
  topRejectionReasons: Array<{ reason: string; count: number; percentage: number }>;
  closerBreakdown: Array<{
    closerName: string;
    closerEmail: string;
    submitted: number;
    rejected: number;
    rejectionRate: number;
  }>;
}

interface IntakeRejectionsReport {
  offices: OfficeRejectionStats[];
  summary: {
    totalOffices: number;
    totalSubmitted: number;
    totalRejected: number;
    overallRejectionRate: number;
    topRejectionReasons: Array<{ reason: string; count: number; percentage: number }>;
  };
  metadata: {
    startDate: string;
    endDate: string;
    generatedAt: string;
  };
}

/**
 * GET /api/reports/intake-rejections
 *
 * Office-by-office intake rejection analysis report
 * Shows rejection statistics, common reasons, and closer breakdown per office
 *
 * Query params:
 * - startDate: ISO date string (required)
 * - endDate: ISO date string (required)
 * - officeIds: Comma-separated office IDs (optional - filter to specific offices)
 */
export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/reports/intake-rejections', undefined, reqId);

  // Restrict access to managers only
  const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
  if (!auth.authorized) return auth.response;

  try {
    const searchParams = request.nextUrl.searchParams;
    const startDate = searchParams.get('startDate');
    const endDate = searchParams.get('endDate');
    const officeIdsParam = searchParams.get('officeIds');

    if (!startDate || !endDate) {
      return NextResponse.json(
        { error: 'startDate and endDate are required' },
        { status: 400 }
      );
    }

    // Parse office IDs filter if provided
    let officeIdsFilter: number[] | undefined;
    if (officeIdsParam) {
      officeIdsFilter = officeIdsParam.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id));
    }

    const userId = (auth.session.user as any).id as string;
    const userRole = auth.session.user.role;

    // Fetch all projects in date range (filtered by sales date)
    const whereClause = `{${PROJECT_FIELDS.SALES_DATE}.OAF.'${startDate}'} AND {${PROJECT_FIELDS.SALES_DATE}.OBF.'${endDate}'}`;

    const projects = await fetchProjects({
      userId,
      role: userRole,
      where: whereClause,
    });

    // Group projects by office
    const projectsByOffice = new Map<string, QuickbaseProject[]>();

    projects.forEach(project => {
      const officeName = project[PROJECT_FIELDS.SALES_OFFICE]?.value || 'Unknown Office';

      // Filter by office IDs if specified
      if (officeIdsFilter) {
        const officeId = project[PROJECT_FIELDS.OFFICE_RECORD_ID]?.value;
        if (!officeId || !officeIdsFilter.includes(officeId)) {
          return;
        }
      }

      if (!projectsByOffice.has(officeName)) {
        projectsByOffice.set(officeName, []);
      }
      projectsByOffice.get(officeName)!.push(project);
    });

    // Calculate metrics for each office
    const officeStats: OfficeRejectionStats[] = [];
    let totalSubmittedAll = 0;
    let totalRejectedAll = 0;
    const globalReasonCounts: Record<string, number> = {};

    for (const [officeName, officeProjects] of projectsByOffice) {
      const officeId = officeProjects[0][PROJECT_FIELDS.OFFICE_RECORD_ID]?.value || null;
      const totalSubmitted = officeProjects.length;
      totalSubmittedAll += totalSubmitted;

      // Projects with first pass result
      const projectsWithFirstPass = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value
      );

      const totalReviewed = projectsWithFirstPass.length;

      // First-time approved = field 1831 = "Approve"
      const firstTimeApproved = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Approve'
      ).length;

      // First-time rejected = field 1831 = "Reject"
      const firstTimeRejected = projectsWithFirstPass.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      ).length;
      totalRejectedAll += firstTimeRejected;

      // Still rejected = rejected but not yet resolved/resubmitted (no completion date)
      const stillRejected = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject' &&
        !p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Resubmitted and approved = initially rejected but now has completion date
      const resubmittedAndApproved = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject' &&
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Calculate rates
      const firstTimePassRate = totalReviewed > 0
        ? (firstTimeApproved / totalReviewed) * 100
        : 0;

      const rejectionRate = totalReviewed > 0
        ? (firstTimeRejected / totalReviewed) * 100
        : 0;

      // Top rejection reasons for this office
      const officeReasonCounts: Record<string, number> = {};
      officeProjects.forEach(p => {
        const reasons = p[PROJECT_FIELDS.INTAKE_MISSING_ITEMS_COMBINED]?.value;
        if (Array.isArray(reasons)) {
          reasons.forEach(reason => {
            officeReasonCounts[reason] = (officeReasonCounts[reason] || 0) + 1;
            globalReasonCounts[reason] = (globalReasonCounts[reason] || 0) + 1;
          });
        }
      });

      const topRejectionReasons = Object.entries(officeReasonCounts)
        .sort((a, b) => b[1] - a[1])
        .slice(0, 5)
        .map(([reason, count]) => ({
          reason,
          count,
          percentage: firstTimeRejected > 0 ? (count / firstTimeRejected) * 100 : 0
        }));

      // Average resolution time
      const rejectedProjects = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject'
      );

      const resolutionTimes = rejectedProjects
        .map(p => {
          const firstReview = p[PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE]?.value;
          const finalApproval = p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value;
          if (!firstReview || !finalApproval) return null;

          const days = (new Date(finalApproval).getTime() - new Date(firstReview).getTime())
            / (1000 * 60 * 60 * 24);
          return Math.round(days * 10) / 10; // Round to 1 decimal
        })
        .filter(time => time !== null && time > 0) as number[];

      const avgResolutionDays = resolutionTimes.length > 0
        ? Math.round((resolutionTimes.reduce((sum, time) => sum + time, 0) / resolutionTimes.length) * 10) / 10
        : null;

      // Closer breakdown for this office
      const closerStats = new Map<string, { name: string; submitted: number; rejected: number }>();
      officeProjects.forEach(p => {
        const closerEmail = p[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
        const closerName = p[PROJECT_FIELDS.CLOSER_NAME]?.value || closerEmail || 'Unknown';
        if (!closerEmail) return;

        if (!closerStats.has(closerEmail)) {
          closerStats.set(closerEmail, { name: closerName, submitted: 0, rejected: 0 });
        }
        const stats = closerStats.get(closerEmail)!;
        stats.submitted += 1;
        if (p[PROJECT_FIELDS.INTAKE_FIRST_PASS_FINANCE_APPROVED]?.value === 'Reject') {
          stats.rejected += 1;
        }
      });

      const closerBreakdown = Array.from(closerStats.entries())
        .map(([email, stats]) => ({
          closerName: stats.name,
          closerEmail: email,
          submitted: stats.submitted,
          rejected: stats.rejected,
          rejectionRate: stats.submitted > 0 ? (stats.rejected / stats.submitted) * 100 : 0
        }))
        .sort((a, b) => b.rejected - a.rejected);

      officeStats.push({
        officeName,
        officeId,
        totalSubmitted,
        totalReviewed,
        firstTimeApproved,
        firstTimeRejected,
        stillRejected,
        resubmittedAndApproved,
        firstTimePassRate: Math.round(firstTimePassRate * 10) / 10,
        rejectionRate: Math.round(rejectionRate * 10) / 10,
        avgResolutionDays,
        topRejectionReasons,
        closerBreakdown
      });
    }

    // Sort offices by rejection count (highest first)
    officeStats.sort((a, b) => b.firstTimeRejected - a.firstTimeRejected);

    // Global top rejection reasons
    const topGlobalReasons = Object.entries(globalReasonCounts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 10)
      .map(([reason, count]) => ({
        reason,
        count,
        percentage: totalRejectedAll > 0 ? (count / totalRejectedAll) * 100 : 0
      }));

    const report: IntakeRejectionsReport = {
      offices: officeStats,
      summary: {
        totalOffices: officeStats.length,
        totalSubmitted: totalSubmittedAll,
        totalRejected: totalRejectedAll,
        overallRejectionRate: totalSubmittedAll > 0
          ? Math.round((totalRejectedAll / totalSubmittedAll) * 1000) / 10
          : 0,
        topRejectionReasons: topGlobalReasons
      },
      metadata: {
        startDate,
        endDate,
        generatedAt: new Date().toISOString()
      }
    };

    logApiResponse('GET', '/api/reports/intake-rejections', Date.now() - startedAt, {
      offices: officeStats.length,
      totalSubmitted: totalSubmittedAll,
      totalRejected: totalRejectedAll
    }, reqId);

    return NextResponse.json(report, { status: 200 });

  } catch (error) {
    console.error('[/api/reports/intake-rejections] ERROR:', error);
    logError('Failed to generate intake rejections report', error as Error, {});
    return NextResponse.json({
      error: 'Internal Server Error',
      message: error instanceof Error ? error.message : String(error)
    }, { status: 500 });
  }
}
