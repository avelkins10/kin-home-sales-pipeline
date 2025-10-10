// app/api/admin/hierarchies/route.ts
export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit, logWarn } from '@/lib/logging/logger'
import { z } from 'zod'
import { assignTeamLeadSchema } from '@/lib/validation/admin'

/**
 * Check if assigning a user to a manager would create a circular hierarchy
 * 
 * @param managerId - The ID of the proposed manager
 * @param userId - The ID of the user being assigned
 * @returns true if circular hierarchy detected, false otherwise
 * 
 * @example
 * If A manages B, and B manages C, prevent C from managing A
 * createsCycle('A', 'C') would return true
 */
async function createsCycle(managerId: string, userId: string): Promise<boolean> {
  let current: string | null = managerId
  const seen = new Set<string>()
  
  while (current) {
    if (current === userId) return true
    if (seen.has(current)) return true
    seen.add(current)

    const result: { rows: Array<{ manager_id: string | null }> } = await sql.query('SELECT manager_id FROM user_hierarchies WHERE user_id = $1', [current])
    current = result.rows[0]?.manager_id ?? null
  }
  
  return false
}

/**
 * GET /api/admin/hierarchies?managerId=xxx&userId=xxx
 * Fetch user hierarchies
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin', 'office_leader', 'area_director', 'divisional', 'regional'])
    if (!auth.authorized) {
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const managerId = searchParams.get('managerId')
    const userId = searchParams.get('userId')

    let query = `
      SELECT 
        h.id,
        h.manager_id,
        h.user_id,
        h.created_at,
        h.updated_at,
        -- Manager details
        m.name as manager_name,
        m.email as manager_email,
        m.role as manager_role,
        -- User details
        u.name as user_name,
        u.email as user_email,
        u.role as user_role,
        u.is_active as user_is_active
      FROM user_hierarchies h
      JOIN users m ON m.id = h.manager_id
      JOIN users u ON u.id = h.user_id
    `
    const conditions = []
    const params = []

    if (managerId) {
      conditions.push(`h.manager_id = $${params.length + 1}`)
      params.push(managerId)
    }

    if (userId) {
      conditions.push(`h.user_id = $${params.length + 1}`)
      params.push(userId)
    }

    if (conditions.length > 0) {
      query += ` WHERE ${conditions.join(' AND ')}`
    }

    query += ` ORDER BY h.created_at DESC`

    const result = await sql.query(query, params)
    const hierarchies = result.rows

    logInfo('Hierarchies fetched', { 
      count: hierarchies.length,
      filters: { managerId, userId }
    })

    return NextResponse.json(hierarchies)
  } catch (error) {
    logError('Failed to fetch hierarchies', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to fetch hierarchies' },
      { status: 500 }
    )
  }
}

/**
 * POST /api/admin/hierarchies
 * Create hierarchy assignments (team lead assignments)
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const validatedData = assignTeamLeadSchema.parse(body)

    const { managerId, userIds } = validatedData

    // Validate that manager has appropriate role
    const managerResult = await sql.query(
      'SELECT id, name, role FROM users WHERE id = $1',
      [managerId]
    )

    if (managerResult.rows.length === 0) {
      return NextResponse.json(
        { error: 'Manager not found' },
        { status: 404 }
      )
    }

    const manager = managerResult.rows[0]
    const validManagerRoles = ['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin']
    
    if (!validManagerRoles.includes(manager.role)) {
      return NextResponse.json(
        { error: 'Manager role is not appropriate for managing users' },
        { status: 400 }
      )
    }

    // Validate that all users exist and are not already managed
    const usersResult = await sql.query(
      'SELECT id, name, email FROM users WHERE id = ANY($1)',
      [userIds]
    )

    if (usersResult.rows.length !== userIds.length) {
      return NextResponse.json(
        { error: 'One or more users not found' },
        { status: 404 }
      )
    }

    // Check for existing management relationships
    const existingResult = await sql.query(
      'SELECT user_id FROM user_hierarchies WHERE user_id = ANY($1)',
      [userIds]
    )

    if (existingResult.rows.length > 0) {
      const existingUserIds = existingResult.rows.map(row => row.user_id)
      return NextResponse.json(
        { 
          error: 'Some users are already managed by someone else',
          existingUserIds 
        },
        { status: 400 }
      )
    }

    // Check for circular hierarchy for each user being assigned
    for (const userId of userIds) {
      if (await createsCycle(managerId, userId)) {
        logWarn('Circular hierarchy prevented', { managerId, userId })
        return NextResponse.json(
          { 
            error: 'Circular hierarchy detected',
            message: `Cannot assign user ${userId} to manager ${managerId} as it would create a circular management chain`,
            userId 
          },
          { status: 400 }
        )
      }
    }

    // Start transaction
    await sql.query('BEGIN')

    try {
      const createdHierarchies = []

      for (const userId of userIds) {
        // Insert hierarchy relationship
        const hierarchyResult = await sql.query(`
          INSERT INTO user_hierarchies (manager_id, user_id)
          VALUES ($1, $2)
          RETURNING id, manager_id, user_id, created_at, updated_at
        `, [managerId, userId])

        const hierarchy = hierarchyResult.rows[0]
        const user = usersResult.rows.find(u => u.id === userId)

        createdHierarchies.push({
          ...hierarchy,
          manager_name: manager.name,
          user_name: user?.name,
          user_email: user?.email
        })

        // Log audit event
        await logAudit(
          'assign_team_lead',
          'user_hierarchy',
          hierarchy.id,
          auth.session.user.id,
          {
            manager_id: { old: null, new: managerId },
            user_id: { old: null, new: userId },
            manager_name: { old: null, new: manager.name },
            user_name: { old: null, new: user?.name }
          }
        )
      }

      await sql.query('COMMIT')

      logInfo('Team lead assignments created', { 
        managerId,
        managerName: manager.name,
        assignedCount: userIds.length
      })

      return NextResponse.json({
        success: true,
        hierarchies: createdHierarchies,
        message: `Successfully assigned ${userIds.length} users to ${manager.name}`
      })
    } catch (error) {
      await sql.query('ROLLBACK')
      throw error
    }
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Validation failed', message: error.errors[0].message },
        { status: 400 }
      )
    }

    if (error && typeof error === 'object' && 'code' in error && error.code === '23505') {
      return NextResponse.json(
        { error: 'User is already managed by this manager' },
        { status: 400 }
      )
    }

    logError('Failed to create hierarchy assignments', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to create hierarchy assignments' },
      { status: 500 }
    )
  }
}

/**
 * DELETE /api/admin/hierarchies?managerId=xxx&userId=xxx
 * Remove hierarchy assignment
 */
export async function DELETE(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const managerId = searchParams.get('managerId')
    const userId = searchParams.get('userId')

    if (!managerId || !userId) {
      return NextResponse.json(
        { error: 'Both managerId and userId are required' },
        { status: 400 }
      )
    }

    // Get hierarchy details for audit log
    const hierarchyResult = await sql.query(`
      SELECT 
        h.id,
        m.name as manager_name,
        u.name as user_name,
        u.email as user_email
      FROM user_hierarchies h
      JOIN users m ON m.id = h.manager_id
      JOIN users u ON u.id = h.user_id
      WHERE h.manager_id = $1 AND h.user_id = $2
    `, [managerId, userId])

    if (hierarchyResult.rows.length === 0) {
      return NextResponse.json(
        { error: 'Hierarchy relationship not found' },
        { status: 404 }
      )
    }

    const hierarchy = hierarchyResult.rows[0]

    // Delete hierarchy relationship
    await sql.query(
      'DELETE FROM user_hierarchies WHERE manager_id = $1 AND user_id = $2',
      [managerId, userId]
    )

    // Log audit event
    await logAudit(
      'remove_team_lead',
      'user_hierarchy',
      hierarchy.id,
      auth.session.user.id,
      {
        manager_id: { old: managerId, new: null },
        user_id: { old: userId, new: null },
        manager_name: { old: hierarchy.manager_name, new: null },
        user_name: { old: hierarchy.user_name, new: null }
      }
    )

    logInfo('Hierarchy assignment removed', { 
      managerId,
      userId,
      managerName: hierarchy.manager_name,
      userName: hierarchy.user_name
    })

    return NextResponse.json({
      success: true,
      message: `Removed ${hierarchy.user_name} from ${hierarchy.manager_name}'s team`
    })
  } catch (error) {
    logError('Failed to remove hierarchy assignment', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to remove hierarchy assignment' },
      { status: 500 }
    )
  }
}


