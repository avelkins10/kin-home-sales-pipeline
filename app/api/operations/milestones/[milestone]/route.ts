import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getMilestoneProjects } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import type { OperationsMilestone, MilestoneDashboardData, MilestoneDashboardFilters } from '@/lib/types/operations';

export const runtime = 'nodejs';

/**
 * GET /api/operations/milestones/[milestone]
 * Fetch all projects for a specific milestone dashboard
 *
 * Valid milestones: intake, survey, design, permitting, install, inspection, pto
 *
 * Query parameters:
 * - status: Filter by milestone status (e.g., 'deposit_received' for intake)
 * - office: Filter by sales office
 * - salesRep: Filter by sales rep
 * - coordinator: Filter by coordinator
 * - search: Search by project ID or customer name
 * - dateRange: Filter by date range (7days, 30days, 90days, custom, all)
 * - customStartDate: Custom start date (ISO format)
 * - customEndDate: Custom end date (ISO format)
 * - showBlocked: Include blocked projects (true/false)
 * - showOnHold: Include on-hold projects (true/false)
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { milestone: string } }
) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  const startTime = Date.now();

  try {
    const { milestone } = params;

    logApiRequest('GET', `/api/operations/milestones/${milestone}`, {
      reqId,
      milestone
    });

    // Validate milestone parameter
    const validMilestones: OperationsMilestone[] = [
      'intake', 'survey', 'design', 'permitting', 'install', 'inspection', 'pto'
    ];

    if (!validMilestones.includes(milestone as OperationsMilestone)) {
      return NextResponse.json(
        { error: 'Invalid milestone', validMilestones },
        { status: 400 }
      );
    }

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

    // Get search params for filtering
    const { searchParams } = new URL(request.url);
    const status = searchParams.get('status') || 'all';
    const office = searchParams.get('office') || 'all';
    const salesRep = searchParams.get('salesRep') || 'all';
    const coordinator = searchParams.get('coordinator') || 'all';
    const search = searchParams.get('search') || '';
    const dateRange = searchParams.get('dateRange') || 'all';
    const customStartDate = searchParams.get('customStartDate') || null;
    const customEndDate = searchParams.get('customEndDate') || null;
    const showBlocked = searchParams.get('showBlocked') === 'true';
    const showOnHold = searchParams.get('showOnHold') === 'true';

    // Fetch milestone data from QuickBase
    const milestoneData = await getMilestoneProjects(
      milestone as OperationsMilestone,
      pcEmail,
      pcName,
      role,
      reqId
    );

    // Apply client-side filters if needed
    let filteredData: MilestoneDashboardData = milestoneData;

    if (status !== 'all' || office !== 'all' || salesRep !== 'all' ||
        coordinator !== 'all' || search || dateRange !== 'all' ||
        !showBlocked || !showOnHold) {

      // Calculate date filter boundaries
      let dateFilterStart: Date | null = null;
      let dateFilterEnd: Date | null = null;

      if (dateRange !== 'all') {
        const now = new Date();
        switch (dateRange) {
          case '7days':
            dateFilterStart = new Date(now.getTime() - 7 * 24 * 60 * 60 * 1000);
            break;
          case '30days':
            dateFilterStart = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000);
            break;
          case '90days':
            dateFilterStart = new Date(now.getTime() - 90 * 24 * 60 * 60 * 1000);
            break;
          case 'custom':
            if (customStartDate) {
              dateFilterStart = new Date(customStartDate);
            }
            if (customEndDate) {
              dateFilterEnd = new Date(customEndDate);
            }
            break;
        }
      }

      const applyFilters = (projects: any[]) => {
        return projects.filter(project => {
          // Filter by status
          if (status !== 'all' && project.milestoneStatus !== status) {
            return false;
          }

          // Filter by office
          if (office !== 'all' && project.salesOffice !== office) {
            return false;
          }

          // Filter by sales rep
          if (salesRep !== 'all' && project.salesRepName !== salesRep) {
            return false;
          }

          // Filter by coordinator
          if (coordinator !== 'all' && project.coordinatorEmail !== coordinator) {
            return false;
          }

          // Filter by search term (project ID, customer name)
          if (search) {
            const searchLower = search.toLowerCase();
            const matchesProjectId = project.projectId.toLowerCase().includes(searchLower);
            const matchesCustomerName = project.customerName.toLowerCase().includes(searchLower);
            if (!matchesProjectId && !matchesCustomerName) {
              return false;
            }
          }

          // Filter by date range based on milestone-specific date field
          if (dateFilterStart) {
            let projectDate: Date | null = null;

            // Use milestone-specific date field
            switch (milestone) {
              case 'intake':
                projectDate = project.depositDate ? new Date(project.depositDate) : null;
                break;
              case 'survey':
                projectDate = project.surveyScheduledDate ? new Date(project.surveyScheduledDate) : null;
                break;
              case 'design':
                projectDate = project.designStartedDate ? new Date(project.designStartedDate) : null;
                break;
              case 'permitting':
                projectDate = project.ahjSubmitted ? new Date(project.ahjSubmitted) : null;
                break;
              case 'install':
                projectDate = project.installScheduledDate ? new Date(project.installScheduledDate) : null;
                break;
              case 'inspection':
                projectDate = project.inspectionScheduledDate ? new Date(project.inspectionScheduledDate) : null;
                break;
              case 'pto':
                projectDate = project.ptoSubmitted ? new Date(project.ptoSubmitted) : null;
                break;
            }

            if (projectDate) {
              if (projectDate < dateFilterStart) {
                return false;
              }

              // If custom range with end date, check upper bound
              if (dateRange === 'custom' && dateFilterEnd) {
                if (projectDate > dateFilterEnd) {
                  return false;
                }
              }
            }
          }

          // Filter blocked projects if not showing them
          if (!showBlocked && project.isBlocked) {
            return false;
          }

          // Filter on-hold projects if not showing them
          if (!showOnHold && project.isOnHold) {
            return false;
          }

          return true;
        });
      };

      // Apply filters to all projects
      const filteredProjects = applyFilters(milestoneData.projects);

      // Rebuild projectsByStatus
      const filteredProjectsByStatus: Record<string, any[]> = {};
      const filteredStatusCounts: Record<string, number> = {};
      let filteredBlockedCount = 0;
      let filteredOnHoldCount = 0;

      for (const project of filteredProjects) {
        const projectStatus = project.milestoneStatus;

        if (!filteredProjectsByStatus[projectStatus]) {
          filteredProjectsByStatus[projectStatus] = [];
          filteredStatusCounts[projectStatus] = 0;
        }

        filteredProjectsByStatus[projectStatus].push(project);
        filteredStatusCounts[projectStatus]++;

        if (project.isBlocked) filteredBlockedCount++;
        if (project.isOnHold) filteredOnHoldCount++;
      }

      // Recalculate metrics
      let totalDaysInMilestone = 0;
      let totalDaysInStatus = 0;
      let oldestProject: any = null;
      let newestProject: any = null;

      for (const project of filteredProjects) {
        totalDaysInMilestone += project.daysInMilestone;
        totalDaysInStatus += project.daysInStatus;

        if (!oldestProject || project.daysInMilestone > oldestProject.daysInMilestone) {
          oldestProject = {
            projectId: project.projectId,
            customerName: project.customerName,
            daysInMilestone: project.daysInMilestone
          };
        }

        if (!newestProject || project.daysInMilestone < newestProject.daysInMilestone) {
          newestProject = {
            projectId: project.projectId,
            customerName: project.customerName,
            daysInMilestone: project.daysInMilestone
          };
        }
      }

      const totalProjects = filteredProjects.length;
      const avgDaysInMilestone = totalProjects > 0 ? Math.round(totalDaysInMilestone / totalProjects) : 0;
      const avgDaysInStatus = totalProjects > 0 ? Math.round(totalDaysInStatus / totalProjects) : 0;

      filteredData = {
        milestone: milestoneData.milestone,
        projects: filteredProjects,
        projectsByStatus: filteredProjectsByStatus,
        metrics: {
          total: totalProjects,
          byStatus: filteredStatusCounts,
          avgDaysInMilestone,
          avgDaysInStatus,
          oldestProject,
          newestProject,
          blockedCount: filteredBlockedCount,
          onHoldCount: filteredOnHoldCount
        },
        availableStatuses: milestoneData.availableStatuses
      };
    }

    const duration = Date.now() - startTime;
    logApiResponse('GET', `/api/operations/milestones/${milestone}`, duration, {
      reqId,
      milestone,
      total: filteredData.metrics.total,
      statusCounts: filteredData.metrics.byStatus,
      success: true
    });

    return NextResponse.json({
      success: true,
      data: filteredData
    });

  } catch (error) {
    logError(`GET /api/operations/milestones/${params.milestone} failed`, error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: (error as Error).message },
      { status: 500 }
    );
  }
}
