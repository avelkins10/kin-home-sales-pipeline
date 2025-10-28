import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getPTOProjects } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCInspectionData, PCInspectionFilters } from '@/lib/types/operations';

export const runtime = 'nodejs';

/**
 * GET /api/operations/projects/pto
 * Fetch all PTO (Permission to Operate) projects categorized by sub-status
 */
export async function GET(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

  const startTime = Date.now();

  try {
    logApiRequest('GET', '/api/operations/projects/pto', {
      reqId
    });

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
    const office = searchParams.get('office') || 'all';
    const salesRep = searchParams.get('salesRep') || 'all';
    const search = searchParams.get('search') || '';
    const dateRange = searchParams.get('dateRange') || 'all';
    const customStartDate = searchParams.get('customStartDate') || null;
    const customEndDate = searchParams.get('customEndDate') || null;

    // Fetch PTO data from QuickBase
    const ptoData = await getPTOProjects(pcEmail, pcName, role, reqId);

    // Apply client-side filters if needed
    let filteredData: PCInspectionData = ptoData;

    if (office !== 'all' || salesRep !== 'all' || search || dateRange !== 'all') {
      // Calculate date filter boundaries
      let dateFilterStart: Date | null = null;
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
            break;
        }
      }

      const applyFilters = (projects: any[]) => {
        return projects.filter(project => {
          // Filter by office
          if (office !== 'all' && project.salesOffice !== office) {
            return false;
          }

          // Filter by sales rep
          if (salesRep !== 'all' && project.salesRepName !== salesRep) {
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

          // Filter by date range based on install completed date
          if (dateFilterStart && project.installCompletedDate) {
            const installDate = new Date(project.installCompletedDate);
            if (installDate < dateFilterStart) {
              return false;
            }

            // If custom range with end date, check upper bound
            if (dateRange === 'custom' && customEndDate) {
              const endDate = new Date(customEndDate);
              if (installDate > endDate) {
                return false;
              }
            }
          }

          return true;
        });
      };

      const filteredReady = applyFilters(ptoData.ptoReadyForSubmission || []);
      const filteredInProgress = applyFilters(ptoData.ptoInProgress || []);
      const filteredFailed = applyFilters(ptoData.ptoInspectionFailed || []);

      filteredData = {
        waitingForInspection: [],
        inspectionScheduled: [],
        inspectionFailed: [],
        inspectionPassed: [],
        ptoReadyForSubmission: filteredReady,
        ptoInProgress: filteredInProgress,
        ptoInspectionFailed: filteredFailed,
        counts: {
          waitingForInspection: 0,
          inspectionScheduled: 0,
          inspectionFailed: 0,
          inspectionPassed: 0,
          ptoReadyForSubmission: filteredReady.length,
          ptoInProgress: filteredInProgress.length,
          ptoInspectionFailed: filteredFailed.length,
          ptoTotal: filteredReady.length + filteredInProgress.length + filteredFailed.length
        }
      };
    }

    const duration = Date.now() - startTime;
    logApiResponse('GET', '/api/operations/projects/pto', duration, {
      reqId,
      counts: filteredData.counts,
      success: true
    });

    return NextResponse.json({
      success: true,
      data: filteredData
    });

  } catch (error) {
    logError('GET /api/operations/projects/pto failed', error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: (error as Error).message },
      { status: 500 }
    );
  }
}
