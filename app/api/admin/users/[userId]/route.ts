export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logWarn, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'
import { updateUserSchema } from '@/lib/validation/admin'

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

    // Get old user data for audit log
    const oldUserResult = await sql`
      SELECT name, email, phone, role, sales_office, region, is_active
      FROM users WHERE id = ${userId}
    `
    const oldUser = oldUserResult.rows[0]

    // Check if admin is trying to deactivate themselves
    if (validatedData.isActive === false && userId === auth.session.user.id) {
      logWarn('Admin attempted self-deactivation', { userId })
      return NextResponse.json(
        { error: 'Cannot deactivate your own account' },
        { status: 400 }
      )
    }

    // Check email uniqueness if email is being updated
    if (validatedData.email) {
      const existingUser = await sql.query(
        'SELECT id FROM users WHERE email = $1 AND id != $2',
        [validatedData.email, userId]
      )
      if (existingUser.rows.length > 0) {
        return NextResponse.json(
          { error: 'Email already in use' },
          { status: 400 }
        )
      }
    }

    // Build dynamic UPDATE query
    const updateFields = []
    const values = []
    let paramIndex = 1

    Object.entries(validatedData).forEach(([key, value]) => {
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

    if (updateFields.length === 0) {
      return NextResponse.json(
        { error: 'No fields to update' },
        { status: 400 }
      )
    }

    // Always update updated_at
    updateFields.push(`updated_at = NOW()`)
    values.push(userId)

    const query = `
      UPDATE users 
      SET ${updateFields.join(', ')}
      WHERE id = $${paramIndex}
      RETURNING id, email, name, phone, role, quickbase_user_id, sales_office, sales_office[1] AS office, region, is_active, created_at, updated_at, last_login_at
    `

    const result = await sql.query(query, values)

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    }

    const user = result.rows[0]

    // Log audit event with field-level changes
    const changes: Record<string, { old: any; new: any }> = {}
    Object.keys(validatedData).forEach(key => {
      const typedKey = key as keyof typeof validatedData
      if (oldUser[key] !== validatedData[typedKey]) {
        changes[key] = { old: oldUser[key], new: validatedData[typedKey] }
      }
    })
    await logAudit(
      'update',
      'user',
      userId,
      auth.session.user.id,
      changes
    )

    logInfo('User updated', { userId, updatedFields: Object.keys(validatedData) })

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

    logError('Failed to update user', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to update user' },
      { status: 500 }
    )
  }
}
