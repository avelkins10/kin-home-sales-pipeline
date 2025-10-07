export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireAuth } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit, logApiRequest, logApiResponse } from '@/lib/logging/logger'
import { z } from 'zod'

const updateProfileSchema = z.object({
  name: z.string().min(1, 'Name is required').max(100),
  email: z.string().email('Invalid email address'),
  phone: z.string().optional(),
})

export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/user/profile', undefined, reqId);

  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      logApiResponse('GET', '/api/user/profile', Date.now() - startedAt, { status: 401 }, reqId);
      return auth.response
    }

    const userId = auth.session.user.id

    const userResult = await sql`
      SELECT id, email, name, phone, role, quickbase_user_id, sales_office, is_active, created_at, updated_at
      FROM users
      WHERE id = ${userId}
    `
    const user = userResult.rows[0]

    if (!user) {
      return NextResponse.json({ error: 'User not found' }, { status: 404 })
    }

    logInfo('Profile fetched', { userId })

    logApiResponse('GET', '/api/user/profile', Date.now() - startedAt, {}, reqId);
    return NextResponse.json({
      id: user.id,
      email: user.email,
      name: user.name,
      phone: user.phone || '',
      role: user.role,
      quickbaseUserId: user.quickbase_user_id,
      office: user.sales_office?.[0] || '',
      salesOffice: user.sales_office || [],
      isActive: user.is_active,
      createdAt: user.created_at,
      updatedAt: user.updated_at
    })
  } catch (error) {
    logError('Error fetching profile', error instanceof Error ? error : new Error(String(error)))
    logApiResponse('GET', '/api/user/profile', Date.now() - startedAt, { error: true }, reqId);
    return NextResponse.json({ error: 'Internal server error' }, { status: 500 })
  }
}

export async function PUT(request: NextRequest) {
  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      return auth.response
    }

    const userId = auth.session.user.id
    const body = await request.json()

    // Validate input
    const validatedData = updateProfileSchema.parse(body)
    const { name, email, phone } = validatedData

    // Get old user data for audit log
    const oldUserResult = await sql`
      SELECT name, email, phone FROM users WHERE id = ${userId}
    `
    const oldUser = oldUserResult.rows[0]

    // Check email uniqueness
    const existingUserResult = await sql`
      SELECT id FROM users
      WHERE email = ${email} AND id != ${userId}
    `
    const existingUser = existingUserResult.rows[0]

    if (existingUser) {
      return NextResponse.json({ 
        error: 'Email already in use',
        message: 'This email address is already associated with another account'
      }, { status: 400 })
    }

    // Update user
    const updatedUserResult = await sql`
      UPDATE users
      SET name = ${name}, email = ${email}, phone = ${phone || null}, updated_at = NOW()
      WHERE id = ${userId}
      RETURNING id, email, name, phone, role, quickbase_user_id, sales_office, is_active, created_at, updated_at
    `
    const updatedUser = updatedUserResult.rows[0]

    if (!updatedUser) {
      return NextResponse.json({ error: 'User not found' }, { status: 404 })
    }

    // Log audit event
    const changes: Record<string, { old: any; new: any }> = {}
    if (oldUser.name !== name) changes.name = { old: oldUser.name, new: name }
    if (oldUser.email !== email) changes.email = { old: oldUser.email, new: email }
    if (oldUser.phone !== phone) changes.phone = { old: oldUser.phone, new: phone }
    
    await logAudit(
      'update',
      'user_profile',
      userId,
      userId,
      changes
    )

    logInfo('Profile updated', { userId, fields: ['name', 'email', 'phone'] })

    return NextResponse.json({
      id: updatedUser.id,
      email: updatedUser.email,
      name: updatedUser.name,
      phone: updatedUser.phone || '',
      role: updatedUser.role,
      quickbaseUserId: updatedUser.quickbase_user_id,
      office: updatedUser.sales_office?.[0] || '',
      salesOffice: updatedUser.sales_office || [],
      isActive: updatedUser.is_active,
      createdAt: updatedUser.created_at,
      updatedAt: updatedUser.updated_at
    })
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ 
        error: 'Validation failed',
        details: error.errors
      }, { status: 400 })
    }

    logError('Error updating profile', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ error: 'Internal server error' }, { status: 500 })
  }
}
