// app/api/admin/users/deactivate-inactive/route.ts
export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'

/**
 * POST /api/admin/users/deactivate-inactive
 * Deactivate inactive users based on project activity
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const { 
      monthsInactive = 12, 
      dryRun = true 
    } = body

    if (monthsInactive < 6 || monthsInactive > 36) {
      return NextResponse.json(
        { error: 'monthsInactive must be between 6 and 36' },
        { status: 400 }
      )
    }

    logInfo('Starting inactive user deactivation', { monthsInactive, dryRun })

    // Calculate threshold date
    const thresholdDate = new Date()
    thresholdDate.setMonth(thresholdDate.getMonth() - monthsInactive)

    // Query users with no recent project activity
    const inactiveUsersResult = await sql.query(`
      SELECT 
        id, email, name, role, last_project_date, created_at
      FROM users 
      WHERE 
        (last_project_date < $1 OR last_project_date IS NULL)
        AND role NOT IN ('super_admin', 'office_leader', 'area_director', 'divisional', 'regional')
        AND is_active = true
      ORDER BY last_project_date ASC NULLS LAST
    `, [thresholdDate.toISOString()])

    const inactiveUsers = inactiveUsersResult.rows

    logInfo('Found inactive users', { 
      count: inactiveUsers.length,
      thresholdDate: thresholdDate.toISOString()
    })

    if (dryRun) {
      // Preview mode - return list of users that would be deactivated
      return NextResponse.json({
        success: true,
        dryRun: true,
        usersToDeactivate: inactiveUsers,
        count: inactiveUsers.length,
        thresholdDate: thresholdDate.toISOString(),
        message: `Would deactivate ${inactiveUsers.length} users with no activity in the last ${monthsInactive} months`
      })
    }

    // Actual deactivation mode
    if (inactiveUsers.length === 0) {
      return NextResponse.json({
        success: true,
        dryRun: false,
        deactivatedCount: 0,
        message: 'No inactive users found to deactivate'
      })
    }

    await sql.query('BEGIN')

    try {
      const deactivatedUsers = []

      for (const user of inactiveUsers) {
        // Deactivate user
        await sql.query(`
          UPDATE users 
          SET is_active = false, updated_at = NOW()
          WHERE id = $1
        `, [user.id])

        deactivatedUsers.push({
          id: user.id,
          email: user.email,
          name: user.name,
          role: user.role,
          lastProjectDate: user.lastProjectDate
        })

        // Log audit event
        await logAudit(
          'deactivate_inactive',
          'user',
          user.id,
          auth.session.user.id,
          {
            is_active: { old: true, new: false },
            reason: { old: null, new: `No project activity in ${monthsInactive} months` },
            lastProjectDate: { old: user.last_project_date, new: user.last_project_date }
          }
        )
      }

      await sql.query('COMMIT')

      logInfo('Inactive users deactivated', { 
        count: deactivatedUsers.length,
        monthsInactive
      })

      return NextResponse.json({
        success: true,
        dryRun: false,
        deactivatedUsers,
        deactivatedCount: deactivatedUsers.length,
        message: `Successfully deactivated ${deactivatedUsers.length} inactive users`
      })
    } catch (error) {
      await sql.query('ROLLBACK')
      throw error
    }
  } catch (error) {
    logError('Failed to deactivate inactive users', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to deactivate inactive users' },
      { status: 500 }
    )
  }
}

/**
 * GET /api/admin/users/deactivate-inactive?monthsInactive=12
 * Preview inactive users that would be deactivated
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const monthsInactive = parseInt(searchParams.get('monthsInactive') || '12')

    if (monthsInactive < 6 || monthsInactive > 36) {
      return NextResponse.json(
        { error: 'monthsInactive must be between 6 and 36' },
        { status: 400 }
      )
    }

    // Calculate threshold date
    const thresholdDate = new Date()
    thresholdDate.setMonth(thresholdDate.getMonth() - monthsInactive)

    // Query users with no recent project activity
    const inactiveUsersResult = await sql.query(`
      SELECT 
        id, email, name, role, last_project_date, created_at,
        EXTRACT(DAYS FROM (NOW() - COALESCE(last_project_date, created_at))) as days_since_last_activity
      FROM users 
      WHERE 
        (last_project_date < $1 OR last_project_date IS NULL)
        AND role NOT IN ('super_admin', 'office_leader', 'area_director', 'divisional', 'regional')
        AND is_active = true
      ORDER BY last_project_date ASC NULLS LAST
    `, [thresholdDate.toISOString()])

    const inactiveUsers = inactiveUsersResult.rows

    return NextResponse.json({
      inactiveUsers,
      count: inactiveUsers.length,
      thresholdDate: thresholdDate.toISOString(),
      monthsInactive
    })
  } catch (error) {
    logError('Failed to preview inactive users', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to preview inactive users' },
      { status: 500 }
    )
  }
}

/**
 * POST /api/admin/users/deactivate-inactive/reactivate
 * Reactivate a specific user
 */
export async function PUT(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const { userId } = body

    if (!userId) {
      return NextResponse.json(
        { error: 'User ID is required' },
        { status: 400 }
      )
    }

    // Check if user exists and is deactivated
    const userResult = await sql.query(`
      SELECT id, email, name, is_active, last_project_date
      FROM users 
      WHERE id = $1
    `, [userId])

    if (userResult.rows.length === 0) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    }

    const user = userResult.rows[0]

    if (user.is_active) {
      return NextResponse.json(
        { error: 'User is already active' },
        { status: 400 }
      )
    }

    // Reactivate user and update last project date
    await sql.query(`
      UPDATE users 
      SET 
        is_active = true, 
        last_project_date = NOW(),
        updated_at = NOW()
      WHERE id = $1
    `, [userId])

    // Log audit event
    await logAudit(
      'reactivate',
      'user',
      userId,
      auth.session.user.id,
      {
        is_active: { old: false, new: true },
        last_project_date: { old: user.last_project_date, new: new Date().toISOString() }
      }
    )

    logInfo('User reactivated', { 
      userId, 
      email: user.email,
      name: user.name
    })

    return NextResponse.json({
      success: true,
      message: 'User reactivated successfully',
      user: {
        id: user.id,
        email: user.email,
        name: user.name,
        is_active: true
      }
    })
  } catch (error) {
    logError('Failed to reactivate user', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to reactivate user' },
      { status: 500 }
    )
  }
}


