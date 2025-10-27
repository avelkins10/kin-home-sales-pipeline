import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { getInspectionProjects } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { PCInspectionData, PCInspectionFilters } from '@/lib/types/operations';

export const runtime = 'nodejs';

/**
 * GET /api/operations/projects/inspections
 * Fetch all inspection projects categorized by status
 */
export async function GET(request: NextRequest) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

  const startTime = Date.now();

  try {
    logApiRequest('GET', '/api/operations/projects/inspections', {
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

    // Get search params for filtering (to be implemented client-side)
    const { searchParams } = new URL(request.url);
    const status = searchParams.get('status') || 'all';
    const office = searchParams.get('office') || 'all';
    const salesRep = searchParams.get('salesRep') || 'all';
    const search = searchParams.get('search') || '';

    // Fetch inspection data from QuickBase
    const inspectionData = await getInspectionProjects(pcEmail, pcName, role, reqId);

    // Apply client-side filters if needed
    let filteredData: PCInspectionData = inspectionData;

    if (status !== 'all' || office !== 'all' || salesRep !== 'all' || search) {
      const applyFilters = (projects: any[]) => {
        return projects.filter(project => {
          // Filter by status (already categorized, so this is redundant but safe)
          if (status !== 'all' && project.inspectionStatus !== status) {
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

          // Filter by search term (project ID, customer name)
          if (search) {
            const searchLower = search.toLowerCase();
            const matchesProjectId = project.projectId.toLowerCase().includes(searchLower);
            const matchesCustomerName = project.customerName.toLowerCase().includes(searchLower);
            if (!matchesProjectId && !matchesCustomerName) {
              return false;
            }
          }

          return true;
        });
      };

      filteredData = {
        waitingForInspection: applyFilters(inspectionData.waitingForInspection),
        inspectionScheduled: applyFilters(inspectionData.inspectionScheduled),
        inspectionFailed: applyFilters(inspectionData.inspectionFailed),
        inspectionPassed: applyFilters(inspectionData.inspectionPassed),
        counts: {
          waitingForInspection: applyFilters(inspectionData.waitingForInspection).length,
          inspectionScheduled: applyFilters(inspectionData.inspectionScheduled).length,
          inspectionFailed: applyFilters(inspectionData.inspectionFailed).length,
          inspectionPassed: applyFilters(inspectionData.inspectionPassed).length
        }
      };
    }

    const duration = Date.now() - startTime;
    logApiResponse('GET', '/api/operations/projects/inspections', duration, {
      reqId,
      counts: filteredData.counts,
      success: true
    });

    return NextResponse.json({
      success: true,
      data: filteredData
    });

  } catch (error) {
    logError('GET /api/operations/projects/inspections failed', error as Error, { reqId });
    return NextResponse.json(
      { error: 'Internal Server Error', message: (error as Error).message },
      { status: 500 }
    );
  }
}
