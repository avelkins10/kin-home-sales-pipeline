export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logWarn, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'
import { updateUserSchema } from '@/lib/validation/admin'
import { validateOffices } from '@/lib/db/offices'
import { normalizeOfficeName } from '@/lib/constants/offices'

/**
 * GET /api/admin/users/[userId]
 * Fetch a single user by ID with hierarchy and office access data
 */
export async function GET(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const auth = await requireRole(['super_admin', 'regional', 'divisional', 'area_director', 'office_leader'])
    if (!auth.authorized) {
      return auth.response
    }

    const { userId } = params

    const result = await sql.query(`
      SELECT
        u.id, u.email, u.name, u.phone, u.role, u.quickbase_user_id, u.sales_office,
        u.sales_office[1] AS office, u.region, u.is_active, u.created_at, u.updated_at, u.last_login_at,
        uh.manager_id as managed_by,
        COALESCE(
          (SELECT array_agg(uh2.user_id)
           FROM user_hierarchies uh2
           WHERE uh2.manager_id = u.id),
          ARRAY[]::text[]
        ) as manages,
        COALESCE(
          (SELECT array_agg(
            json_build_object(
              'officeName', oa.office_name,
              'accessLevel', oa.access_level,
              'assignedAt', oa.assigned_at
            )
          )
           FROM office_assignments oa
           WHERE oa.user_id = u.id),
          ARRAY[]::json[]
        ) as office_access
      FROM users u
      LEFT JOIN user_hierarchies uh ON uh.user_id = u.id
      WHERE u.id = $1
    `, [userId])

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    }

    return NextResponse.json(result.rows[0])
  } catch (error) {
    logError('Failed to fetch user', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to fetch user' },
      { status: 500 }
    )
  }
}

export async function DELETE(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { userId } = params

    // Prevent self-deletion
    if (userId === auth.session.user.id) {
      logWarn('Admin attempted self-deletion', { userId })
      return NextResponse.json(
        { error: 'Cannot delete your own account' },
        { status: 400 }
      )
    }

    // Get user data for audit log before deletion
    const userResult = await sql.query(`
      SELECT
        u.id, u.name, u.email, u.role, u.sales_office, u.is_active
      FROM users u
      WHERE u.id = $1
    `, [userId])

    if (userResult.rows.length === 0) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    }

    const user = userResult.rows[0]

    // Start transaction
    await sql.query('BEGIN')

    try {
      // Delete related data first (foreign key constraints)

      // Delete notifications
      await sql.query('DELETE FROM notifications WHERE user_id = $1', [userId])

      // Delete notification settings
      await sql.query('DELETE FROM notification_settings WHERE user_id = $1', [userId])

      // Delete user hierarchies (both as manager and managed user)
      await sql.query('DELETE FROM user_hierarchies WHERE user_id = $1 OR manager_id = $1', [userId])

      // Delete office assignments
      await sql.query('DELETE FROM office_assignments WHERE user_id = $1', [userId])

      // Note: audit_logs are kept for historical records (don't delete)

      // Delete the user
      await sql.query('DELETE FROM users WHERE id = $1', [userId])

      // Commit transaction
      await sql.query('COMMIT')

      // Log audit event
      await logAudit(
        'delete',
        'user',
        userId,
        auth.session.user.id,
        { deletedUser: user }
      )

      logInfo('User deleted', { userId, userName: user.name, userEmail: user.email })

      return NextResponse.json({ success: true, message: 'User deleted successfully' })
    } catch (error) {
      await sql.query('ROLLBACK')
      throw error
    }
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : String(error)
    const errorStack = error instanceof Error ? error.stack : undefined

    logError('Failed to delete user', error instanceof Error ? error : new Error(String(error)), {
      userId: params.userId,
      errorMessage,
      errorStack
    })

    return NextResponse.json(
      {
        error: 'Failed to delete user',
        message: errorMessage,
        ...(process.env.NODE_ENV === 'development' && { stack: errorStack })
      },
      { status: 500 }
    )
  }
}

export async function PATCH(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { userId } = params
    const body = await request.json()
    const validatedData = updateUserSchema.parse(body)

    // Extract hierarchy and office access fields
    const {
      managedBy,
      manages,
      ...basicFields
    } = validatedData

    // Extract officeAccess separately to allow reassignment after validation
    let officeAccess = validatedData.officeAccess

    // Create canonical office access for mapped values
    let canonicalOfficeAccess = officeAccess

    // Get old user data for audit log
    const oldUserResult = await sql.query(`
      SELECT 
        u.name, u.email, u.phone, u.role, u.sales_office, u.region, u.is_active,
        uh.manager_id as managed_by,
        COALESCE(
          (SELECT array_agg(uh2.user_id) 
           FROM user_hierarchies uh2 
           WHERE uh2.manager_id = u.id), 
          ARRAY[]::text[]
        ) as manages,
        COALESCE(
          (SELECT array_agg(
            json_build_object(
              'officeName', oa.office_name,
              'accessLevel', oa.access_level
            )
          )
           FROM office_assignments oa 
           WHERE oa.user_id = u.id), 
          ARRAY[]::json[]
        ) as office_access
      FROM users u
      LEFT JOIN user_hierarchies uh ON uh.user_id = u.id
      WHERE u.id = $1
    `, [userId])
    
    // Early 404 check - user doesn't exist
    if (oldUserResult.rows.length === 0) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    }
    
    const oldUser = oldUserResult.rows[0]

    // Check if admin is trying to deactivate themselves
    if (basicFields.isActive === false && userId === auth.session.user.id) {
      logWarn('Admin attempted self-deactivation', { userId })
      return NextResponse.json(
        { error: 'Cannot deactivate your own account' },
        { status: 400 }
      )
    }

    // Prevent self-management
    if (managedBy === userId) {
      return NextResponse.json(
        { error: 'User cannot manage themselves' },
        { status: 400 }
      )
    }
    if (manages && manages.includes(userId)) {
      return NextResponse.json(
        { error: 'User cannot manage themselves' },
        { status: 400 }
      )
    }

    // Prevent two-way cycle within single request
    if (managedBy && manages && manages.includes(managedBy)) {
      return NextResponse.json(
        { error: 'Cannot create two-way management relationship in single request' },
        { status: 400 }
      )
    }

    // Validate manager exists and has appropriate role (if managedBy is being updated)
    if (managedBy !== undefined) {
      if (managedBy !== null) {
        const managerResult = await sql.query(
          'SELECT id, role FROM users WHERE id = $1',
          [managedBy]
        )
        if (managerResult.rows.length === 0) {
          return NextResponse.json(
            { error: 'Manager not found' },
            { status: 400 }
          )
        }
        const managerRole = managerResult.rows[0].role
        if (!['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(managerRole)) {
          return NextResponse.json(
            { error: 'Manager role is not appropriate for managing users' },
            { status: 400 }
          )
        }
        
        // Prevent circular hierarchy: check if managedBy is managed by this user
        const circularCheck = await sql.query(
          'SELECT 1 FROM user_hierarchies WHERE manager_id = $1 AND user_id = $2',
          [userId, managedBy]
        )
        if (circularCheck.rows.length > 0) {
          return NextResponse.json(
            { error: 'Circular hierarchy detected: manager cannot be managed by this user' },
            { status: 400 }
          )
        }
      }
      // managedBy === null is valid - it means clearing the manager
    }

    // Validate managed users exist (if manages is being updated)
    if (manages !== undefined && manages.length > 0) {
      // Check if user has manager role (considering potential role update)
      const effectiveRole = basicFields.role || oldUser.role
      const managerRoles = ['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin']
      if (!managerRoles.includes(effectiveRole)) {
        return NextResponse.json(
          { error: 'Only users with manager roles can manage others' },
          { status: 400 }
        )
      }

      const managedUsersResult = await sql.query(
        'SELECT id FROM users WHERE id = ANY($1)',
        [manages]
      )
      if (managedUsersResult.rows.length !== manages.length) {
        return NextResponse.json(
          { error: 'One or more managed users not found' },
          { status: 400 }
        )
      }
      
      // Prevent circular hierarchy: check if any managed user is a manager of this user
      const circularCheck = await sql.query(
        'SELECT 1 FROM user_hierarchies WHERE manager_id = ANY($1) AND user_id = $2',
        [manages, userId]
      )
      if (circularCheck.rows.length > 0) {
        return NextResponse.json(
          { error: 'Circular hierarchy detected: cannot manage a user who manages you' },
          { status: 400 }
        )
      }
    }

    // Validate office names against canonical list
    const officesToValidate: string[] = []
    if (basicFields.office) officesToValidate.push(basicFields.office)
    if (officeAccess && officeAccess.length > 0) {
      officesToValidate.push(...officeAccess.map(access => access.officeName))
    }

    if (officesToValidate.length > 0) {
      try {
        const validatedOffices = await validateOffices(officesToValidate)
        // Use validated office names
        if (basicFields.office) {
          const officeToValidate = basicFields.office
          const validatedOffice = validatedOffices.find(o => o === officeToValidate || o === normalizeOfficeName(officeToValidate))
          if (validatedOffice) basicFields.office = validatedOffice
        }
        if (canonicalOfficeAccess && canonicalOfficeAccess.length > 0) {
          canonicalOfficeAccess = canonicalOfficeAccess.map(access => ({
            ...access,
            officeName: validatedOffices.find(o => o === access.officeName || o === normalizeOfficeName(access.officeName)) || access.officeName
          })) as typeof canonicalOfficeAccess
        }
      } catch (error) {
        return NextResponse.json(
          { error: error instanceof Error ? error.message : 'Invalid office names' },
          { status: 400 }
        )
      }
    }

    // Check email uniqueness if email is being updated
    if (basicFields.email) {
      const existingUser = await sql.query(
        'SELECT id FROM users WHERE email = $1 AND id != $2',
        [basicFields.email, userId]
      )
      if (existingUser.rows.length > 0) {
        return NextResponse.json(
          { error: 'Email already in use' },
          { status: 400 }
        )
      }
    }

    // Deduplicate manages and officeAccess to prevent unique constraint violations
    const deduplicatedManages = manages ? Array.from(new Set(manages)) : manages
    const deduplicatedOfficeAccess = canonicalOfficeAccess ? canonicalOfficeAccess.reduce((acc, access) => {
      const key = `${access.officeName}-${access.accessLevel}`
      if (!acc.some(item => `${item.officeName}-${item.accessLevel}` === key)) {
        acc.push(access)
      }
      return acc
    }, [] as typeof canonicalOfficeAccess) : canonicalOfficeAccess

    // Start transaction
    await sql.query('BEGIN')

    try {
      // Update basic user fields if any are provided
      if (Object.keys(basicFields).length > 0) {
        const updateFields = []
        const values = []
        let paramIndex = 1

        Object.entries(basicFields).forEach(([key, value]) => {
          if (value !== undefined) {
            if (key === 'office') {
              updateFields.push(`sales_office = ARRAY[$${paramIndex}]::text[]`)
              values.push(value)
              paramIndex++
              return
            }
            const dbKey = key === 'isActive' ? 'is_active' : key
            updateFields.push(`${dbKey} = $${paramIndex}`)
            values.push(value)
            paramIndex++
          }
        })

        if (updateFields.length > 0) {
          updateFields.push(`updated_at = NOW()`)
          values.push(userId)

          const query = `
            UPDATE users 
            SET ${updateFields.join(', ')}
            WHERE id = $${paramIndex}
          `
          await sql.query(query, values)
        }
      }

      // Update user_hierarchies: managedBy relationship
      if (managedBy !== undefined) {
        // Delete existing managedBy relationship
        await sql.query(
          'DELETE FROM user_hierarchies WHERE user_id = $1',
          [userId]
        )
        
        // Insert new managedBy relationship if provided (null means clear manager)
        if (managedBy !== null) {
          await sql.query(
            'INSERT INTO user_hierarchies (manager_id, user_id) VALUES ($1, $2)',
            [managedBy, userId]
          )
        }
      }

      // Update user_hierarchies: manages relationships
      if (deduplicatedManages !== undefined) {
        // Delete existing manages relationships
        await sql.query(
          'DELETE FROM user_hierarchies WHERE manager_id = $1',
          [userId]
        )
        
        // Insert new manages relationships
        if (deduplicatedManages.length > 0) {
          for (const managedUserId of deduplicatedManages) {
            await sql.query(
              'INSERT INTO user_hierarchies (manager_id, user_id) VALUES ($1, $2)',
              [userId, managedUserId]
            )
          }
        }
      }

      // Update office_assignments
      if (deduplicatedOfficeAccess !== undefined) {
        // Delete existing office assignments
        await sql.query(
          'DELETE FROM office_assignments WHERE user_id = $1',
          [userId]
        )
        
        // Insert new office assignments
        if (deduplicatedOfficeAccess.length > 0) {
          for (const access of deduplicatedOfficeAccess) {
            await sql.query(
              'INSERT INTO office_assignments (user_id, office_name, access_level) VALUES ($1, $2, $3)',
              [userId, access.officeName, access.accessLevel]
            )
          }
        }
      }

      // Fetch updated user with hierarchy and office access data
      const result = await sql.query(`
        SELECT 
          u.id, u.email, u.name, u.phone, u.role, u.quickbase_user_id, u.sales_office, 
          u.sales_office[1] AS office, u.region, u.is_active, u.created_at, u.updated_at, u.last_login_at,
          uh.manager_id as managed_by,
          COALESCE(
            (SELECT array_agg(uh2.user_id) 
             FROM user_hierarchies uh2 
             WHERE uh2.manager_id = u.id), 
            ARRAY[]::text[]
          ) as manages,
          COALESCE(
            (SELECT array_agg(
              json_build_object(
                'officeName', oa.office_name,
                'accessLevel', oa.access_level,
                'assignedAt', oa.assigned_at
              )
            )
             FROM office_assignments oa 
             WHERE oa.user_id = u.id), 
            ARRAY[]::json[]
          ) as office_access
        FROM users u
        LEFT JOIN user_hierarchies uh ON uh.user_id = u.id
        WHERE u.id = $1
      `, [userId])

      if (result.rows.length === 0) {
        await sql.query('ROLLBACK')
        return NextResponse.json(
          { error: 'User not found' },
          { status: 404 }
        )
      }

      const user = result.rows[0]

      // Commit transaction
      await sql.query('COMMIT')

      // Log audit event with field-level changes (including hierarchy and office access)
      const changes: Record<string, { old: any; new: any }> = {}
      
      // Helper function to get old value with proper key mapping
      const getOldValue = (key: string) => {
        if (key === 'office') {
          return oldUser.sales_office?.[0] || null
        }
        if (key === 'isActive') {
          return oldUser.is_active
        }
        return oldUser[key]
      }
      
      // Track basic field changes
      Object.keys(basicFields).forEach(key => {
        const typedKey = key as keyof typeof basicFields
        const oldValue = getOldValue(key)
        if (oldValue !== basicFields[typedKey]) {
          changes[key] = { old: oldValue, new: basicFields[typedKey] }
        }
      })
      
      // Track hierarchy changes
      if (managedBy !== undefined && oldUser.managed_by !== managedBy) {
        changes.managedBy = { old: oldUser.managed_by, new: managedBy }
      }
      if (deduplicatedManages !== undefined) {
        const oldManages = oldUser.manages || []
        const newManages = deduplicatedManages || []
        if (JSON.stringify(oldManages.sort()) !== JSON.stringify(newManages.sort())) {
          changes.manages = { old: oldManages, new: newManages }
        }
      }
      
      // Track office access changes
      if (deduplicatedOfficeAccess !== undefined) {
        const oldOfficeAccess = oldUser.office_access || []
        if (JSON.stringify(oldOfficeAccess) !== JSON.stringify(deduplicatedOfficeAccess)) {
          changes.officeAccess = { old: oldOfficeAccess, new: deduplicatedOfficeAccess }
        }
      }
      
      await logAudit(
        'update',
        'user',
        userId,
        auth.session.user.id,
        changes
      )

      logInfo('User updated with hierarchy and office access', { 
        userId, 
        updatedFields: Object.keys(validatedData),
        hierarchyUpdated: managedBy !== undefined || deduplicatedManages !== undefined,
        officeAccessUpdated: deduplicatedOfficeAccess !== undefined
      })

      return NextResponse.json(user)
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

    if (error && typeof error === 'object' && 'code' in error && error.code === '23505') { // Unique constraint violation
      return NextResponse.json(
        { error: 'Email already in use' },
        { status: 400 }
      )
    }

    logError('Failed to update user', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to update user' },
      { status: 500 }
    )
  }
}
