import { NextRequest, NextResponse } from 'next/server'
import { getServerSession } from 'next-auth'
import { authOptions } from '@/lib/auth/authOptions'
import { pool } from '@/lib/db/pool'
import { logInfo, logWarn, logError } from '@/lib/logging/logger'

/**
 * GET /api/admin/office-assignments
 * Fetch office assignments for specific users
 * Query params: ?userIds=id1,id2,id3
 */
export async function GET(req: NextRequest) {
  try {
    const session = await getServerSession(authOptions)
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 })
    }

    const { searchParams } = new URL(req.url)
    const userIds = searchParams.get('userIds')?.split(',') || []

    if (userIds.length === 0) {
      return NextResponse.json([])
    }

    const result = await pool.query(
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
    logError('[OFFICE_ASSIGNMENTS_GET] Error fetching assignments', { error })
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
    const session = await getServerSession(authOptions)
    if (!session) {
      return NextResponse.json({ error: 'Unauthorized' }, { status: 401 })
    }

    // Check if user has permission to manage office assignments
    const userRole = session.user?.role
    if (!['super_admin', 'regional'].includes(userRole || '')) {
      logWarn('[OFFICE_ASSIGNMENTS_DELETE] User lacks permission', {
        userId: session.user?.id,
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
    const result = await pool.query(
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
      removedBy: session.user?.id,
    })

    return NextResponse.json({
      success: true,
      message: 'Manager removed from office successfully',
    })
  } catch (error) {
    logError('[OFFICE_ASSIGNMENTS_DELETE] Error removing assignment', { error })
    return NextResponse.json(
      { error: 'Failed to remove office assignment' },
      { status: 500 }
    )
  }
}
