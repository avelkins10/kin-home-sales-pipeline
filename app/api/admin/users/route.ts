export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { hash } from 'bcryptjs'
import { logInfo, logError, logAudit, logApiRequest, logApiResponse } from '@/lib/logging/logger'
import { z } from 'zod'
import { createUserSchema } from '@/lib/validation/admin'

export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/admin/users', undefined, reqId);

  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      logApiResponse('GET', '/api/admin/users', Date.now() - startedAt, { status: 403 }, reqId);
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const search = searchParams.get('search')?.trim().slice(0, 100) // Clamp to 100 chars and normalize whitespace
    const role = searchParams.get('role')
    const office = searchParams.get('office')?.trim().slice(0, 100) // Clamp to 100 chars and normalize whitespace

    // Validate role parameter against allowed values
    const allowedRoles = ['closer', 'setter', 'office_leader', 'regional', 'super_admin', 'all']
    if (role && !allowedRoles.includes(role)) {
      return NextResponse.json(
        { error: 'Invalid role parameter' },
        { status: 400 }
      )
    }

    let query = `
      SELECT 
        id, email, name, phone, role, quickbase_user_id, sales_office, 
        sales_office[1] AS office,
        region, is_active, created_at, updated_at, last_login_at 
      FROM users
    `
    const conditions = []
    const params = []

    if (search) {
      conditions.push(`(name ILIKE $${params.length + 1} OR email ILIKE $${params.length + 1})`)
      params.push(`%${search}%`)
    }

    if (role && role !== 'all') {
      conditions.push(`role = $${params.length + 1}`)
      params.push(role)
    }

    if (office && office !== 'all') {
      conditions.push(`$${params.length + 1} = ANY(sales_office)`)
      params.push(office)
    }

    if (conditions.length > 0) {
      query += ` WHERE ${conditions.join(' AND ')}`
    }

    query += ` ORDER BY created_at DESC`

    const result = await sql.query(query, params)
    const users = result.rows

    logInfo('Users fetched', { count: users.length, filters: { search, role, office } })

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

    const { name, email, phone, role, quickbaseUserId, office, region, temporaryPassword } = validatedData

    // Check email uniqueness
    const existingUser = await sql.query('SELECT id FROM users WHERE email = $1', [email])
    if (existingUser.rows.length > 0) {
      return NextResponse.json(
        { error: 'Email already in use' },
        { status: 400 }
      )
    }

    // Hash password
    const hashedPassword = await hash(temporaryPassword, 10)

    // Insert user
    const userResult = await sql.query(`
      INSERT INTO users (email, name, phone, role, quickbase_user_id, sales_office, region, password_hash, is_active)
      VALUES ($1, $2, $3, $4, $5, ARRAY[$6]::text[], $7, $8, $9)
      RETURNING id, email, name, phone, role, quickbase_user_id, sales_office, sales_office[1] AS office, region, is_active, created_at, updated_at, last_login_at
    `, [email, name, phone, role, quickbaseUserId, office ?? null, region, hashedPassword, true])

    const user = userResult.rows[0]

    // Insert default notification settings
    await sql.query(`
      INSERT INTO notification_settings (user_id)
      VALUES ($1)
    `, [user.id])

    // Log audit event for creation
    const changes: Record<string, { old: any; new: any }> = {
      name: { old: null, new: name },
      email: { old: null, new: email },
      role: { old: null, new: role },
      office: { old: null, new: office },
      quickbaseUserId: { old: null, new: quickbaseUserId }
    }
    await logAudit(
      'create',
      'user',
      user.id,
      auth.session.user.id,
      changes
    )

    logInfo('User created', { userId: user.id, email, role })

    return NextResponse.json(user)
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
