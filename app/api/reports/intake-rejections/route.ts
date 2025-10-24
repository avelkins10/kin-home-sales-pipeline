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
  neverRejected: number;
  totalRejections: number;
  totalFixed: number;
  stillRejected: number;
  activeApproved: number;
  firstTimePassRate: number;
  rejectionRate: number;
  avgResolutionDays: number | null;
  topRejectionReasons: Array<{ reason: string; count: number; percentage: number }>;
  closerBreakdown: Array<{
    closerName: string;
    closerEmail: string;
    submitted: number;
    rejected: number;
    stillRejected: number;
    rejectionRate: number;
  }>;
}

interface IntakeRejectionsReport {
  offices: OfficeRejectionStats[];
  summary: {
    totalOffices: number;
    totalSubmitted: number;
    totalRejected: number;
    totalStillRejected: number;
    totalResolved: number;
    overallRejectionRate: number;
    avgResolvedDays: number | null;
    avgStillRejectedDays: number | null;
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
    let totalStillRejectedAll = 0;
    let totalResubmittedAll = 0;
    const globalReasonCounts: Record<string, number> = {};
    const allStillRejectedAges: number[] = [];

    for (const [officeName, officeProjects] of projectsByOffice) {
      const officeId = officeProjects[0][PROJECT_FIELDS.OFFICE_RECORD_ID]?.value || null;
      const totalSubmitted = officeProjects.length;
      totalSubmittedAll += totalSubmitted;

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
      const neverRejected = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value &&
        !wasEverRejected(p)
      ).length;

      // Total Rejections (ever rejected - using binary field)
      const rejectedProjects = officeProjects.filter(wasEverRejected);
      const totalRejections = rejectedProjects.length;
      totalRejectedAll += totalRejections;

      // Total Fixed (was rejected, now has completion date)
      const totalFixed = officeProjects.filter(p =>
        wasEverRejected(p) &&
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Still Rejected (currently rejected, no completion date)
      const stillRejected = officeProjects.filter(p =>
        isCurrentlyRejected(p) &&
        !p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Active/Approved (has completion date - passed intake)
      const activeApproved = officeProjects.filter(p =>
        p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value
      ).length;

      // Track global totals
      totalStillRejectedAll += stillRejected;
      totalResubmittedAll += totalFixed;

      // Calculate rates based on total submitted
      const firstTimePassRate = totalSubmitted > 0
        ? (neverRejected / totalSubmitted) * 100
        : 0;

      const rejectionRate = totalSubmitted > 0
        ? (totalRejections / totalSubmitted) * 100
        : 0;

      // Top rejection reasons for this office (only from rejected projects)
      const officeReasonCounts: Record<string, number> = {};
      rejectedProjects.forEach(p => {
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
          percentage: totalRejections > 0 ? (count / totalRejections) * 100 : 0
        }));

      // Average resolution time - set to null for now to avoid timeout issues
      // TODO: Optimize task-based resolution time calculation for better performance
      const avgResolutionDays = null;

      // Calculate ages for still rejected projects (how long they've been sitting)
      const stillRejectedProjects = rejectedProjects.filter(p => !p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value);
      const stillRejectedAges = stillRejectedProjects
        .map(p => {
          const firstReview = p[PROJECT_FIELDS.INTAKE_FIRST_PASS_COMPLETE]?.value;
          if (!firstReview) return null;

          const today = new Date();
          const days = (today.getTime() - new Date(firstReview).getTime()) / (1000 * 60 * 60 * 24);
          return Math.round(days * 10) / 10;
        })
        .filter(age => age !== null && age > 0) as number[];

      // Add to global still rejected ages
      allStillRejectedAges.push(...stillRejectedAges);

      // Closer breakdown for this office
      const closerStats = new Map<string, { name: string; submitted: number; rejected: number; stillRejected: number }>();
      officeProjects.forEach(p => {
        const closerEmail = p[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
        const closerName = p[PROJECT_FIELDS.CLOSER_NAME]?.value || closerEmail || 'Unknown';
        if (!closerEmail) return;

        if (!closerStats.has(closerEmail)) {
          closerStats.set(closerEmail, { name: closerName, submitted: 0, rejected: 0, stillRejected: 0 });
        }
        const stats = closerStats.get(closerEmail)!;
        stats.submitted += 1;
        // Track total rejections (ever rejected)
        if (wasEverRejected(p)) {
          stats.rejected += 1;
        }
        // Track still rejected (currently rejected, no completion date)
        if (isCurrentlyRejected(p) && !p[PROJECT_FIELDS.INTAKE_COMPLETED_DATE]?.value) {
          stats.stillRejected += 1;
        }
      });

      const closerBreakdown = Array.from(closerStats.entries())
        .map(([email, stats]) => ({
          closerName: stats.name,
          closerEmail: email,
          submitted: stats.submitted,
          rejected: stats.rejected,
          stillRejected: stats.stillRejected,
          rejectionRate: stats.submitted > 0 ? (stats.rejected / stats.submitted) * 100 : 0
        }))
        .sort((a, b) => b.rejected - a.rejected);

      officeStats.push({
        officeName,
        officeId,
        totalSubmitted,
        neverRejected,
        totalRejections,
        totalFixed,
        stillRejected,
        activeApproved,
        firstTimePassRate: Math.round(firstTimePassRate * 10) / 10,
        rejectionRate: Math.round(rejectionRate * 10) / 10,
        avgResolutionDays,
        topRejectionReasons,
        closerBreakdown
      });
    }

    // Sort offices by rejection count (highest first)
    officeStats.sort((a, b) => b.totalRejections - a.totalRejections);

    // Global top rejection reasons
    const topGlobalReasons = Object.entries(globalReasonCounts)
      .sort((a, b) => b[1] - a[1])
      .slice(0, 10)
      .map(([reason, count]) => ({
        reason,
        count,
        percentage: totalRejectedAll > 0 ? (count / totalRejectedAll) * 100 : 0
      }));

    // Calculate global averages
    const avgResolvedDays = null; // Temporarily disabled to avoid timeouts
    const avgStillRejectedDays = allStillRejectedAges.length > 0
      ? Math.round((allStillRejectedAges.reduce((sum, age) => sum + age, 0) / allStillRejectedAges.length) * 10) / 10
      : null;

    const report: IntakeRejectionsReport = {
      offices: officeStats,
      summary: {
        totalOffices: officeStats.length,
        totalSubmitted: totalSubmittedAll,
        totalRejected: totalRejectedAll,
        totalStillRejected: totalStillRejectedAll,
        totalResolved: totalResubmittedAll,
        overallRejectionRate: totalSubmittedAll > 0
          ? Math.round((totalRejectedAll / totalSubmittedAll) * 1000) / 10
          : 0,
        avgResolvedDays,
        avgStillRejectedDays,
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
