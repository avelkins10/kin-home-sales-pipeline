// app/api/operations/field-tracking/crew-members/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { getUniqueCrewMembers } from '@/lib/db/arrivy';

/**
 * GET - Fetch unique crew member names for filter dropdown
 */
export async function GET(req: NextRequest) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Operations role required.' 
      }, { status: 403 });
    }

    logApiRequest('GET /api/operations/field-tracking/crew-members', {
      reqId,
    });

    const crewMembers = await getUniqueCrewMembers();

    logApiResponse('GET', '/api/operations/field-tracking/crew-members', Date.now() - startedAt, { 
      count: crewMembers.length,
    }, reqId);

    return NextResponse.json(crewMembers, { 
      status: 200,
      headers: {
        'Cache-Control': 'private, max-age=300', // 5-minute cache (crew members don't change often)
      },
    });

  } catch (error) {
    logError('Failed to fetch crew members', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to load crew members' 
    }, { status: 500 });
  }
}

