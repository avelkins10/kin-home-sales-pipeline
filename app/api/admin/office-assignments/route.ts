export const runtime = 'nodejs'
export const dynamic = 'force-dynamic';
import { NextRequest, NextResponse } from 'next/server'
import { requireAuth } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logWarn, logError } from '@/lib/logging/logger'

/**
 * GET /api/admin/office-assignments
 * Fetch office assignments for specific users
 * Query params: ?userIds=id1,id2,id3
 */
export async function GET(req: NextRequest) {
  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      return auth.response
    }

    const { searchParams } = new URL(req.url)
    const userIds = searchParams.get('userIds')?.split(',') || []

    if (userIds.length === 0) {
      return NextResponse.json([])
    }

    const result = await sql.query(
      `SELECT
        user_id as "userId",
        office_name as "officeName",
        access_level as "accessLevel"
       FROM office_assignments
       WHERE user_id = ANY($1)
       ORDER BY office_name`,
      [userIds]
    )

    return NextResponse.json(result.rows)
  } catch (error) {
    logError('[OFFICE_ASSIGNMENTS_GET] Error fetching assignments', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to fetch office assignments' },
      { status: 500 }
    )
  }
}

/**
 * DELETE /api/admin/office-assignments
 * Remove a manager from an office
 * Body: { userId: string, officeName: string }
 */
export async function DELETE(req: NextRequest) {
  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      return auth.response
    }

    // Check if user has permission to manage office assignments
    const userRole = auth.session.user?.role
    if (!['super_admin', 'regional'].includes(userRole || '')) {
      logWarn('[OFFICE_ASSIGNMENTS_DELETE] User lacks permission', {
        userId: auth.session.user?.id,
        role: userRole,
      })
      return NextResponse.json(
        { error: 'You do not have permission to manage office assignments' },
        { status: 403 }
      )
    }

    const body = await req.json()
    const { userId, officeName } = body

    if (!userId || !officeName) {
      return NextResponse.json(
        { error: 'userId and officeName are required' },
        { status: 400 }
      )
    }

    // Delete the assignment
    const result = await sql.query(
      `DELETE FROM office_assignments
       WHERE user_id = $1 AND office_name = $2
       RETURNING *`,
      [userId, officeName]
    )

    if (result.rowCount === 0) {
      logWarn('[OFFICE_ASSIGNMENTS_DELETE] Assignment not found', {
        userId,
        officeName,
      })
      return NextResponse.json(
        { error: 'Assignment not found' },
        { status: 404 }
      )
    }

    logInfo('[OFFICE_ASSIGNMENTS_DELETE] Assignment removed successfully', {
      userId,
      officeName,
      removedBy: auth.session.user?.id,
    })

    return NextResponse.json({
      success: true,
      message: 'Manager removed from office successfully',
    })
  } catch (error) {
    logError('[OFFICE_ASSIGNMENTS_DELETE] Error removing assignment', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to remove office assignment' },
      { status: 500 }
    )
  }
}
