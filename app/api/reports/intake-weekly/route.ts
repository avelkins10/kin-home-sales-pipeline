export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireRole } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
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
 * - startDate: ISO date string (optional - defaults to start of current month)
 * - endDate: ISO date string (optional - defaults to today)
 * - officeIds: Comma-separated office IDs (optional - filter to specific offices)
 * - sortBy: Sort field - 'totalSubmitted' (default), 'firstTimePassRate', 'rejectionRate', 'stillRejected'
 * - closerId: Optional - filter to specific closer (legacy param)
 */
export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/reports/intake-weekly', undefined, reqId);

  // Restrict access to managers only
  const auth = await requireRole(['office_leader', 'regional', 'super_admin']);
  if (!auth.authorized) return auth.response;

  try {
    const searchParams = request.nextUrl.searchParams;

    // Default to month-to-date if no dates provided
    const now = new Date();
    const defaultStartDate = `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, '0')}-01`;
    const defaultEndDate = `${now.getFullYear()}-${String(now.getMonth() + 1).padStart(2, '0')}-${String(now.getDate()).padStart(2, '0')}`;

    const startDate = searchParams.get('startDate') || defaultStartDate;
    const endDate = searchParams.get('endDate') || defaultEndDate;
    const closerId = searchParams.get('closerId'); // Optional filter (legacy)
    const sortBy = searchParams.get('sortBy') || 'totalSubmitted'; // Default sort by total projects

    // Parse office IDs filter if provided
    let officeIdsFilter: number[] | undefined;
    const officeIdsParam = searchParams.get('officeIds');
    if (officeIdsParam) {
      officeIdsFilter = officeIdsParam.split(',').map(id => parseInt(id.trim())).filter(id => !isNaN(id));
    }

    // Validate sort parameter
    const validSortFields = ['totalSubmitted', 'firstTimePassRate', 'rejectionRate', 'stillRejected'];
    if (!validSortFields.includes(sortBy)) {
      return NextResponse.json(
        { error: `Invalid sortBy parameter. Must be one of: ${validSortFields.join(', ')}` },
        { status: 400 }
      );
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

    // Filter by office IDs if provided
    const filteredProjects = officeIdsFilter
      ? projects.filter((p: QuickbaseProject) => {
          const officeId = p[PROJECT_FIELDS.OFFICE_RECORD_ID]?.value;
          return officeId && officeIdsFilter.includes(officeId);
        })
      : projects;

    // Group projects by closer email
    const projectsByCloser = new Map<string, QuickbaseProject[]>();

    filteredProjects.forEach((project: QuickbaseProject) => {
      const closerEmail = project[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
      if (!closerEmail) return; // Skip projects without closer

      if (!projectsByCloser.has(closerEmail)) {
        projectsByCloser.set(closerEmail, []);
      }
      projectsByCloser.get(closerEmail)!.push(project);
    });

    // Calculate metrics for each closer
    const closerReports: WeeklyIntakeCloserReport[] = [];

    for (const [closerEmail, closerProjects] of Array.from(projectsByCloser)) {
      // Get closer name from first project
      const closerName = closerProjects[0][PROJECT_FIELDS.CLOSER_NAME]?.value || closerEmail;
      const officeName = closerProjects[0][PROJECT_FIELDS.SALES_OFFICE]?.value || null;

      // Total submitted = all projects sold in date range
      const totalSubmitted = closerProjects.length;

      // Helper function to check if project was ever rejected
      // Uses BOTH status strings (real-time) AND binary field (historical) for accuracy
      const wasEverRejected = (p: QuickbaseProject): boolean => {
        // Check current status (real-time - catches newly rejected projects)
        const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
        const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
        const currentlyRejected = intakeStatus.includes('rejected') || projectStatus.includes('rejected');

        // Check binary field (historical - catches projects that were rejected then fixed)
        const priorStatusRejected = p[PROJECT_FIELDS.PRIOR_STATUS_WAS_REJECTED_BINARY]?.value;
        const historicallyRejected = priorStatusRejected === 1 || priorStatusRejected === '1' || priorStatusRejected === true;

        // Return true if either indicates rejection
        return currentlyRejected || historicallyRejected;
      };

      // Helper function to check if project is currently rejected
      const isCurrentlyRejected = (p: QuickbaseProject): boolean => {
        const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
        const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
        return intakeStatus.includes('rejected') || projectStatus.includes('rejected');
      };

      // Never Rejected (first pass approved) - has completion date AND never rejected
      const neverRejected = closerProjects.filter((p: QuickbaseProject) =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
        !wasEverRejected(p)
      ).length;

      // Total Rejections (ever rejected - using hybrid detection)
      const rejectedProjects = closerProjects.filter(wasEverRejected);
      const totalRejections = rejectedProjects.length;

      // Still Rejected (status includes "rejected" - current state)
      const stillRejected = closerProjects.filter((p: QuickbaseProject) =>
        isCurrentlyRejected(p)
      ).length;

      // Total Fixed (was rejected, but status no longer shows rejected)
      const totalFixed = closerProjects.filter((p: QuickbaseProject) =>
        wasEverRejected(p) &&
        !isCurrentlyRejected(p)
      ).length;

      // Active/Approved (has completion date - passed intake)
      const activeApproved = closerProjects.filter((p: QuickbaseProject) =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Calculate rates based on total submitted
      const firstTimePassRate = totalSubmitted > 0
        ? (neverRejected / totalSubmitted) * 100
        : 0;

      const rejectionRate = totalSubmitted > 0
        ? (totalRejections / totalSubmitted) * 100
        : 0;

      // Top rejection reasons for this closer (only from rejected projects)
      const reasonCounts: Record<string, number> = {};
      rejectedProjects.forEach(p => {
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

      // Average resolution time - disabled for performance
      // TODO: Optimize with batched task queries or add pre-calculated field in QuickBase
      // This was causing timeouts for regional users with access to many projects
      // because it made a separate DB query for each fixed project
      const avgResolutionDays = null;

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

    // Sort based on sortBy parameter
    switch (sortBy) {
      case 'totalSubmitted':
        closerReports.sort((a, b) => b.totalSubmitted - a.totalSubmitted);
        break;
      case 'firstTimePassRate':
        closerReports.sort((a, b) => b.firstTimePassRate - a.firstTimePassRate);
        break;
      case 'rejectionRate':
        closerReports.sort((a, b) => b.rejectionRate - a.rejectionRate);
        break;
      case 'stillRejected':
        closerReports.sort((a, b) => b.stillRejected - a.stillRejected);
        break;
      default:
        closerReports.sort((a, b) => b.totalSubmitted - a.totalSubmitted);
    }

    // Calculate overall metrics (use filtered projects for accuracy)
    const totalProjects = filteredProjects.length;

    // Helper function to check if project was ever rejected (at global level)
    const wasEverRejectedGlobal = (p: QuickbaseProject): boolean => {
      // Check current status (real-time)
      const intakeStatus = (p[PROJECT_FIELDS.INTAKE_STATUS]?.value || '').toString().toLowerCase();
      const projectStatus = (p[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toString().toLowerCase();
      const currentlyRejected = intakeStatus.includes('rejected') || projectStatus.includes('rejected');

      // Check binary field (historical)
      const priorStatusRejected = p[PROJECT_FIELDS.PRIOR_STATUS_WAS_REJECTED_BINARY]?.value;
      const historicallyRejected = priorStatusRejected === 1 || priorStatusRejected === '1' || priorStatusRejected === true;

      return currentlyRejected || historicallyRejected;
    };

    // Never Rejected globally (completed and not rejected) - use filtered projects
    const allNeverRejected = filteredProjects.filter((p: QuickbaseProject) =>
      p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
      !wasEverRejectedGlobal(p)
    ).length;

    // Total Rejections globally - use filtered projects
    const allRejectedProjects = filteredProjects.filter(wasEverRejectedGlobal);
    const totalRejectedAll = allRejectedProjects.length;

    // Calculate rates based on total submitted
    const overallPassRate = totalProjects > 0
      ? (allNeverRejected / totalProjects) * 100
      : 0;

    const overallRejectionRate = totalProjects > 0
      ? (totalRejectedAll / totalProjects) * 100
      : 0;

    // Find most common rejection reason across all closers (only from rejected projects)
    const allReasonCounts: Record<string, number> = {};
    allRejectedProjects.forEach((p: QuickbaseProject) => {
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

    // Find top/bottom performers based on current sort
    // For most metrics, higher is better, but for rejectionRate and stillRejected, lower is better
    let topPerformer = null;
    let needsAttention = null;

    if (closerReports.length > 0) {
      if (sortBy === 'rejectionRate' || sortBy === 'stillRejected') {
        // For these metrics, lower is better, so reverse the logic
        needsAttention = closerReports[0].totalSubmitted >= 1 ? {
          name: closerReports[0].closerName,
          rate: closerReports[0].firstTimePassRate,
        } : null;
        topPerformer = closerReports[closerReports.length - 1].totalSubmitted >= 1 ? {
          name: closerReports[closerReports.length - 1].closerName,
          rate: closerReports[closerReports.length - 1].firstTimePassRate,
        } : null;
      } else {
        // For totalSubmitted and firstTimePassRate, higher is better
        topPerformer = closerReports[0].totalSubmitted >= 1 ? {
          name: closerReports[0].closerName,
          rate: closerReports[0].firstTimePassRate,
        } : null;
        needsAttention = closerReports[closerReports.length - 1].totalSubmitted >= 1 ? {
          name: closerReports[closerReports.length - 1].closerName,
          rate: closerReports[closerReports.length - 1].firstTimePassRate,
        } : null;
      }
    }

    // Calculate office breakdown
    const officeBreakdown = new Map<string, { submitted: number; rejected: number }>();
    filteredProjects.forEach((p: QuickbaseProject) => {
      const officeName = p[PROJECT_FIELDS.SALES_OFFICE]?.value || 'Unknown';
      if (!officeBreakdown.has(officeName)) {
        officeBreakdown.set(officeName, { submitted: 0, rejected: 0 });
      }
      const stats = officeBreakdown.get(officeName)!;
      stats.submitted += 1;
      if (wasEverRejectedGlobal(p)) {
        stats.rejected += 1;
      }
    });

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

    // Add enhanced metadata
    const response = {
      ...report,
      metadata: {
        generatedAt: new Date().toISOString(),
        dateRangeUsed: { startDate, endDate },
        defaultedToDates: !searchParams.get('startDate') || !searchParams.get('endDate'),
        totalClosers: closerReports.length,
        sortedBy: sortBy,
        filteredByOffices: officeIdsFilter || null,
        officeBreakdown: Array.from(officeBreakdown.entries()).map(([name, stats]) => ({
          officeName: name,
          submitted: stats.submitted,
          rejected: stats.rejected,
          rejectionRate: stats.submitted > 0 ? Math.round((stats.rejected / stats.submitted) * 1000) / 10 : 0
        })).sort((a, b) => b.submitted - a.submitted)
      }
    };

    logApiResponse('GET', '/api/reports/intake-weekly', Date.now() - startedAt, {
      totalProjects,
      totalClosers: closerReports.length,
      sortBy
    }, reqId);

    return NextResponse.json(response);
  } catch (error) {
    console.error('Error generating weekly intake report:', error);
    logError('Failed to generate weekly intake report', error as Error, {});
    return NextResponse.json(
      { error: 'Failed to generate report' },
      { status: 500 }
    );
  }
}
