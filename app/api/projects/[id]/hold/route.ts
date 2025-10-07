'use server'

import { NextRequest, NextResponse } from 'next/server'
import { requireAuth, requireProjectAccessById } from '@/lib/auth/guards'
import { logInfo, logError, logWarn } from '@/lib/logging/logger'
import { updateProject } from '@/lib/quickbase/queries'
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds'

export async function POST(
  request: NextRequest,
  { params }: { params: { id: string } }
) {
  let session: any = null;
  let onHold: boolean | undefined = undefined;

  try {
    // Check authentication
    const auth = await requireAuth()
    if (!auth.authorized) return auth.response
    session = auth.session

    // Parse request body
    const body = await request.json()
    const { onHold: parsedOnHold, holdReason, blockReason } = body
    onHold = parsedOnHold

    // Log request
    logInfo('Hold update request', { 
      projectId: params.id, 
      userId: session.user.id, 
      onHold, 
      action: onHold ? 'place' : 'release' 
    })

    // Validate input
    if (typeof onHold !== 'boolean') {
      logWarn('Invalid hold update request', { 
        projectId: params.id, 
        userId: session.user.id, 
        error: 'Invalid onHold value' 
      });
      return NextResponse.json({ error: 'Invalid onHold value' }, { status: 400 })
    }

    // Parse record ID
    const recordId = parseInt(params.id)
    if (isNaN(recordId)) {
      return NextResponse.json({ error: 'Invalid project ID' }, { status: 400 })
    }

    // Project-level authorization
    const access = await requireProjectAccessById(params.id)
    if (!access.authorized) return access.response

    // Build update payload
    const updates: Record<number, { value: string }> = {
      [PROJECT_FIELDS.ON_HOLD]: { value: onHold ? 'Yes' : 'No' },
      [PROJECT_FIELDS.HOLD_REASON]: { value: (holdReason || '').trim() },
      [PROJECT_FIELDS.BLOCK_REASON]: { value: (blockReason || '').trim() },
    }

    // Add date field based on hold status
    if (onHold) {
      updates[PROJECT_FIELDS.DATE_ON_HOLD] = { value: new Date().toISOString() }
    } else {
      updates[PROJECT_FIELDS.DATE_ON_HOLD] = { value: '' } // Clear date when releasing hold
    }

    // Update project in Quickbase
    await updateProject(recordId, updates)

    // Log success
    logInfo('Hold status updated successfully', { 
      projectId: recordId, 
      userId: session.user.id, 
      onHold, 
      synced: true 
    });

    return NextResponse.json({
      success: true,
      message: onHold ? 'Project placed on hold' : 'Project hold released'
    })

  } catch (error) {
    logError('Error updating hold status', error as Error, { 
      projectId: params.id, 
      userId: session?.user?.id, 
      onHold 
    });
    return NextResponse.json(
      { error: error instanceof Error ? error.message : 'Internal server error' },
      { status: 500 }
    )
  }
}
