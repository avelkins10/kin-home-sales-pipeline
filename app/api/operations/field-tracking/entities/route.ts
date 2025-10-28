// app/api/operations/field-tracking/entities/route.ts
export const runtime = 'nodejs';

import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { syncEntityFromQuickBase } from '@/lib/integrations/arrivy/service';
import { listArrivyEntities } from '@/lib/db/arrivy';

/**
 * GET - List all entities (crew members)
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

    // Fetch all entities
    const entities = await listArrivyEntities();

    logApiResponse('GET', '/api/operations/field-tracking/entities', Date.now() - startedAt, { 
      count: entities.length,
    }, reqId);

    return NextResponse.json({ entities }, { status: 200 });

  } catch (error) {
    logError('Failed to fetch field tracking entities', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to load entities' 
    }, { status: 500 });
  }
}

/**
 * POST - Create/sync entity (crew member)
 */
export async function POST(req: NextRequest) {
  const startedAt = Date.now();
  const reqId = req.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  
  const auth = await requireAuth();
  if (!auth.authorized) return auth.response;

  try {
    const { role } = auth.session.user as any;
    const allowedRoles = ['operations_manager', 'office_leader', 'regional', 'super_admin'];
    
    if (!allowedRoles.includes(role)) {
      return NextResponse.json({ 
        error: 'Access denied. Manager role required.' 
      }, { status: 403 });
    }

    // Parse request body
    const body = await req.json();
    const { email, name, phone, quickbaseUserId } = body;

    if (!email || !name) {
      return NextResponse.json({ 
        error: 'Missing required fields: email, name' 
      }, { status: 400 });
    }

    // Sync entity from QuickBase
    const result = await syncEntityFromQuickBase(email, name, phone, quickbaseUserId);

    logApiResponse('POST', '/api/operations/field-tracking/entities', Date.now() - startedAt, { 
      email,
      isNew: result.isNew,
    }, reqId);

    return NextResponse.json({ 
      success: true,
      entity: result.entity,
      isNew: result.isNew,
    }, { status: 201 });

  } catch (error) {
    logError('Failed to create/sync field tracking entity', error as Error, { reqId });
    return NextResponse.json({ 
      error: 'Failed to create/sync entity',
      message: error instanceof Error ? error.message : 'Unknown error',
    }, { status: 500 });
  }
}

