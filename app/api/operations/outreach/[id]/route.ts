import { NextRequest, NextResponse } from 'next/server';
import { requireAuth } from '@/lib/auth/guards';
import { qbClient } from '@/lib/quickbase/client';
import { logApiRequest, logApiResponse, logError } from '@/lib/logging/logger';
import { OUTREACH_RECORD_FIELDS, QB_TABLE_OUTREACH_RECORDS } from '@/lib/constants/fieldIds';

export const runtime = 'nodejs';

/**
 * PUT /api/operations/outreach/[id]
 * Update a single outreach record
 */
export async function PUT(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('PUT /api/operations/outreach/[id]', {
      reqId,
      recordId: params.id
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

    const recordId = parseInt(params.id);
    if (isNaN(recordId)) {
      return NextResponse.json(
        { error: 'Invalid record ID' },
        { status: 400 }
      );
    }

    const body = await request.json();
    const { outreachStatus, outreachCompletedDate, note, attemptNote } = body;

    // Build update data
    const updateData: any = {
      [OUTREACH_RECORD_FIELDS.RECORD_ID]: { value: recordId }
    };

    if (outreachStatus !== undefined) {
      updateData[OUTREACH_RECORD_FIELDS.OUTREACH_STATUS] = { value: outreachStatus };
    }
    if (outreachCompletedDate !== undefined) {
      updateData[OUTREACH_RECORD_FIELDS.OUTREACH_COMPLETED_DATE] = { value: outreachCompletedDate };
    }
    if (note !== undefined) {
      updateData[OUTREACH_RECORD_FIELDS.NOTE] = { value: note };
    }
    if (attemptNote !== undefined) {
      updateData[OUTREACH_RECORD_FIELDS.ATTEMPT_NOTE] = { value: attemptNote };
    }

    // Update the record
    const response = await qbClient.updateRecord({
      to: QB_TABLE_OUTREACH_RECORDS,
      data: [updateData]
    });

    logApiResponse('PUT /api/operations/outreach/[id]', {
      reqId,
      recordId: params.id,
      success: true
    });

    return NextResponse.json({
      success: true,
      recordId,
      message: 'Outreach record updated successfully'
    });

  } catch (error) {
    logError('PUT /api/operations/outreach/[id] failed', error, { reqId, recordId: params.id });
    
    return NextResponse.json(
      { error: 'Failed to update outreach record' },
      { status: 500 }
    );
  }
}

/**
 * DELETE /api/operations/outreach/[id]
 * Delete a single outreach record
 */
export async function DELETE(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  const reqId = `req_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;
  
  try {
    logApiRequest('DELETE /api/operations/outreach/[id]', {
      reqId,
      recordId: params.id
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

    const recordId = parseInt(params.id);
    if (isNaN(recordId)) {
      return NextResponse.json(
        { error: 'Invalid record ID' },
        { status: 400 }
      );
    }

    // Delete the record
    const response = await qbClient.deleteRecord({
      from: QB_TABLE_OUTREACH_RECORDS,
      where: `{${OUTREACH_RECORD_FIELDS.RECORD_ID}.EX.${recordId}}`
    });

    logApiResponse('DELETE /api/operations/outreach/[id]', {
      reqId,
      recordId: params.id,
      success: true
    });

    return NextResponse.json({
      success: true,
      recordId,
      message: 'Outreach record deleted successfully'
    });

  } catch (error) {
    logError('DELETE /api/operations/outreach/[id] failed', error, { reqId, recordId: params.id });
    
    return NextResponse.json(
      { error: 'Failed to delete outreach record' },
      { status: 500 }
    );
  }
}
