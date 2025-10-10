export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { hash } from 'bcryptjs'
import { logInfo, logError, logAudit, logApiRequest, logApiResponse } from '@/lib/logging/logger'
import { z } from 'zod'
import { createUserSchema } from '@/lib/validation/admin'
import { validateOffices } from '@/lib/db/offices'
import { normalizeOfficeName } from '@/lib/constants/offices'

export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/admin/users', undefined, reqId);

  try {
    const auth = await requireRole(['super_admin', 'office_leader', 'area_director', 'divisional', 'regional'])
    if (!auth.authorized) {
      logApiResponse('GET', '/api/admin/users', Date.now() - startedAt, { status: 403 }, reqId);
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const search = searchParams.get('search')?.trim().slice(0, 100)
    const role = searchParams.get('role')
    const office = searchParams.get('office')?.trim().slice(0, 100)
    const managedBy = searchParams.get('managedBy') // Filter users by their manager
    const manages = searchParams.get('manages') // Get users managed by a specific user
    const activeOnly = searchParams.get('activeOnly') === 'true' // Filter users with recent activity
    const monthsBack = parseInt(searchParams.get('monthsBack') || '12') // Activity timeframe

    // Validate role parameter against allowed values (including new manager roles)
    const allowedRoles = ['closer', 'setter', 'team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin', 'all']
    if (role && !allowedRoles.includes(role)) {
      return NextResponse.json(
        { error: 'Invalid role parameter' },
        { status: 400 }
      )
    }

    let query = `
      SELECT 
        u.id, u.email, u.name, u.phone, u.role, u.quickbase_user_id, u.sales_office, 
        u.sales_office[1] AS office,
        u.region, u.is_active, u.created_at, u.updated_at, u.last_login_at,
        u.last_project_date, u.invited_at, u.invite_token, u.invite_accepted_at,
        -- Hierarchy data
        uh.manager_id as managed_by,
        COALESCE(
          (SELECT array_agg(uh2.user_id)
           FROM user_hierarchies uh2
           WHERE uh2.manager_id = u.id),
          ARRAY[]::text[]
        ) as manages,
        -- Office access data
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
    `
    const conditions = []
    const params = []

    if (search) {
      conditions.push(`(u.name ILIKE $${params.length + 1} OR u.email ILIKE $${params.length + 1})`)
      params.push(`%${search}%`)
    }

    if (role && role !== 'all') {
      conditions.push(`u.role = $${params.length + 1}`)
      params.push(role)
    }

    if (office && office !== 'all') {
      conditions.push(`$${params.length + 1} = ANY(u.sales_office)`)
      params.push(office)
    }

    if (managedBy) {
      conditions.push(`uh.manager_id = $${params.length + 1}`)
      params.push(managedBy)
    }

    if (manages) {
      conditions.push(`$${params.length + 1} = ANY(
        SELECT array_agg(uh2.user_id) 
        FROM user_hierarchies uh2 
        WHERE uh2.manager_id = u.id
      )`)
      params.push(manages)
    }

    if (activeOnly) {
      const thresholdDate = new Date();
      thresholdDate.setMonth(thresholdDate.getMonth() - monthsBack);
      conditions.push(`u.last_project_date >= $${params.length + 1}`)
      params.push(thresholdDate.toISOString())
    }

    if (conditions.length > 0) {
      query += ` WHERE ${conditions.join(' AND ')}`
    }

    query += ` ORDER BY u.created_at DESC`

    const result = await sql.query(query, params)
    const users = result.rows

    logInfo('Users fetched', { 
      count: users.length, 
      filters: { search, role, office, managedBy, manages, activeOnly, monthsBack } 
    })

    logApiResponse('GET', '/api/admin/users', Date.now() - startedAt, { count: users.length }, reqId);
    return NextResponse.json(users)
  } catch (error) {
    logError('Failed to fetch users', error instanceof Error ? error : new Error(String(error)))
    logApiResponse('GET', '/api/admin/users', Date.now() - startedAt, { error: true }, reqId);
    return NextResponse.json(
      { error: 'Failed to fetch users' },
      { status: 500 }
    )
  }
}

export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const validatedData = createUserSchema.parse(body)

    const {
      name, email, phone, role, quickbaseUserId, region, temporaryPassword,
      managedBy, manages
    } = validatedData

    // Extract office and officeAccess separately to allow reassignment after validation
    let { office, officeAccess } = validatedData

    // Check email uniqueness
    const existingUser = await sql.query('SELECT id FROM users WHERE email = $1', [email])
    if (existingUser.rows.length > 0) {
      return NextResponse.json(
        { error: 'Email already in use' },
        { status: 400 }
      )
    }

    // Validate manager exists and has appropriate role
    if (managedBy) {
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
    }

    // Validate managed users exist
    if (manages && manages.length > 0) {
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
    }

    // Validate office names
    const officesToValidate: string[] = []
    if (office) officesToValidate.push(office)
    if (officeAccess && officeAccess.length > 0) {
      officesToValidate.push(...officeAccess.map(access => access.officeName))
    }

    if (officesToValidate.length > 0) {
      try {
        const validatedOffices = await validateOffices(officesToValidate)
        // Use validated office names
        if (office) {
          const officeToValidate = office
          const validatedOffice = validatedOffices.find(o => o === officeToValidate || o === normalizeOfficeName(officeToValidate))
          if (validatedOffice) office = validatedOffice
        }
        if (officeAccess && officeAccess.length > 0) {
          officeAccess = officeAccess.map(access => ({
            ...access,
            officeName: validatedOffices.find(o => o === access.officeName || o === normalizeOfficeName(access.officeName)) || access.officeName
          })) as typeof officeAccess
        }
      } catch (error) {
        return NextResponse.json(
          { error: error instanceof Error ? error.message : 'Invalid office names' },
          { status: 400 }
        )
      }
    }

    // Hash password
    const hashedPassword = await hash(temporaryPassword, 10)

    // Start transaction
    await sql.query('BEGIN')

    try {
      // Insert user
      const userResult = await sql.query(`
        INSERT INTO users (email, name, phone, role, quickbase_user_id, sales_office, region, password_hash, is_active)
        VALUES ($1, $2, $3, $4, $5, ARRAY[$6]::text[], $7, $8, $9)
        RETURNING id, email, name, phone, role, quickbase_user_id, sales_office, sales_office[1] AS office, region, is_active, created_at, updated_at, last_login_at
      `, [email, name, phone, role, quickbaseUserId || null, office ?? null, region, hashedPassword, true])

      const user = userResult.rows[0]

      // Insert hierarchy relationships
      if (managedBy) {
        await sql.query(`
          INSERT INTO user_hierarchies (manager_id, user_id)
          VALUES ($1, $2)
        `, [managedBy, user.id])
      }

      if (manages && manages.length > 0) {
        for (const managedUserId of manages) {
          await sql.query(`
            INSERT INTO user_hierarchies (manager_id, user_id)
            VALUES ($1, $2)
          `, [user.id, managedUserId])
        }
      }

      // Insert office access assignments
      if (officeAccess && officeAccess.length > 0) {
        for (const access of officeAccess) {
          await sql.query(`
            INSERT INTO office_assignments (user_id, office_name, access_level)
            VALUES ($1, $2, $3)
          `, [user.id, access.officeName, access.accessLevel])
        }
      }

      // Insert default notification settings
      await sql.query(`
        INSERT INTO notification_settings (user_id)
        VALUES ($1)
      `, [user.id])

      // Commit transaction
      await sql.query('COMMIT')

      // Log audit event for creation
      const changes: Record<string, { old: any; new: any }> = {
        name: { old: null, new: name },
        email: { old: null, new: email },
        role: { old: null, new: role },
        office: { old: null, new: office },
        quickbaseUserId: { old: null, new: quickbaseUserId },
        managedBy: { old: null, new: managedBy },
        manages: { old: null, new: manages },
        officeAccess: { old: null, new: officeAccess }
      }
      await logAudit(
        'create',
        'user',
        user.id,
        auth.session.user.id,
        changes
      )

      logInfo('User created with hierarchy and office access', { 
        userId: user.id, 
        email, 
        role, 
        managedBy, 
        managesCount: manages?.length || 0,
        officeAccessCount: officeAccess?.length || 0
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

    logError('Failed to create user', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to create user' },
      { status: 500 }
    )
  }
}
