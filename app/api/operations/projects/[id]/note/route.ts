export const runtime = 'nodejs'

// app/api/operations/projects/[id]/note/route.ts
import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { createProjectNote } from '@/lib/quickbase/queries';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';

export async function POST(request: NextRequest, { params }: { params: { id: string } }) {
  const projectId = params.id;
  const reqId = `note-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('POST', `/api/operations/projects/${projectId}/note`, {}, reqId);

    // Verify authentication and role
    const auth = await requireAuth();
    if (!auth.authorized) return auth.response;

    const session = auth.session;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(session.user.role)) {
      return NextResponse.json(
        { error: 'Insufficient permissions' },
        { status: 403 }
      );
    }

    const pcEmail = session.user.email;
    const pcName = session.user.name || session.user.email;

    // Parse request body
    const body = await request.json();
    const { noteContent } = body;

    // Validate input
    if (!noteContent || typeof noteContent !== 'string') {
      return NextResponse.json(
        { error: 'Note content is required' },
        { status: 400 }
      );
    }

    // Validate note length
    if (noteContent.length > 5000) {
      return NextResponse.json(
        { error: 'Note too long (max 5000 characters)' },
        { status: 400 }
      );
    }

    if (noteContent.trim().length === 0) {
      return NextResponse.json(
        { error: 'Note content cannot be empty' },
        { status: 400 }
      );
    }

    // Create note in QuickBase
    const success = await createProjectNote(
      projectId,
      parseInt(projectId),
      noteContent.trim(),
      pcEmail,
      reqId
    );

    if (!success) {
      return NextResponse.json(
        { error: 'Failed to create note' },
        { status: 500 }
      );
    }

    logApiResponse('POST', `/api/operations/projects/${projectId}/note`, { 
      success: true,
      noteLength: noteContent.length
    }, reqId);

    return NextResponse.json({
      success: true,
      message: 'Note created successfully'
    });

  } catch (error) {
    logError('POST /api/operations/projects/[id]/note', error, reqId);
    
    return NextResponse.json(
      { error: 'Failed to create note' },
      { status: 500 }
    );
  }
}
