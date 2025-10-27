export const runtime = 'nodejs'

// app/api/operations/projects/[id]/route.ts
import { NextResponse } from 'next/server';
import { requireAuth, requireProjectAccessById } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { 
  getProjectById, 
  getPCProjectCommunications, 
  getPCProjectDocuments, 
  transformProjectToTeamMembers 
} from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import type { PCProjectDetail } from '@/lib/types/operations';

export async function GET(req: Request, { params }: { params: { id: string } }) {
  const startedAt = Date.now();
  const projectId = params.id;
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', `/api/operations/projects/${projectId}`, undefined, reqId);

  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  // Check user role is operations_coordinator, operations_manager, office_leader, regional, or super_admin
  const userRole = auth.session.user.role;
  const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
  if (!userRole || !allowedRoles.includes(userRole)) {
    return NextResponse.json({ error: 'Unauthorized - insufficient permissions' }, { status: 403 });
  }

  const recordId = parseInt(projectId, 10);
  if (Number.isNaN(recordId)) {
    return NextResponse.json({ error: 'Invalid project ID' }, { status: 400 });
  }

  // Check project access
  const access = await requireProjectAccessById(recordId);
  if (!access.authorized) return access.response;

  try {
    // Fetch project data
    const project = await getProjectById(recordId);
    if (!project) {
      return NextResponse.json({ error: 'Project not found' }, { status: 404 });
    }

    // Get the actual project ID from the project object
    const actualProjectId = project[PROJECT_FIELDS.PROJECT_ID];
    if (!actualProjectId) {
      return NextResponse.json({ error: 'Project ID not found in project data' }, { status: 500 });
    }

    // Fetch communication history
    const communications = await getPCProjectCommunications(actualProjectId, recordId, reqId);

    // Enrich communications with project data
    const enrichedCommunications = communications.map(comm => ({
      ...comm,
      customerName: project[PROJECT_FIELDS.CUSTOMER_NAME] || 'Unknown Customer',
      customerPhone: project[PROJECT_FIELDS.CUSTOMER_PHONE] || '',
      projectStage: project[PROJECT_FIELDS.PROJECT_STAGE] || 'Unknown'
    }));

    // Fetch documents (placeholder implementation)
    const documents = await getPCProjectDocuments(actualProjectId, recordId, reqId);

    // Transform project to team members
    const teamMembers = transformProjectToTeamMembers(project);

    // Combine into PCProjectDetail object
    const projectDetail: PCProjectDetail = {
      project,
      communications: enrichedCommunications,
      documents,
      teamMembers
    };

    logApiResponse('GET', `/api/operations/projects/${recordId}`, Date.now() - startedAt, {}, reqId);
    return NextResponse.json(projectDetail, { status: 200 });

  } catch (error) {
    logError('Failed to fetch project detail', error as Error, { recordId });
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 });
  }
}
