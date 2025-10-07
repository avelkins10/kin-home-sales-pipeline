export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireAuth } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { compare, hash } from 'bcryptjs'
import { logInfo, logError, logWarn, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'

const changePasswordSchema = z.object({
  currentPassword: z.string().min(1, 'Current password is required'),
  newPassword: z.string().min(8, 'Password must be at least 8 characters'),
})

export async function PUT(request: NextRequest) {
  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      return auth.response
    }

    const userId = auth.session.user.id
    const body = await request.json()

    // Validate input
    const validatedData = changePasswordSchema.parse(body)
    const { currentPassword, newPassword } = validatedData

    // Get current user's password hash
    const userResult = await sql`
      SELECT password_hash FROM users
      WHERE id = ${userId}
    `
    const user = userResult.rows[0]

    if (!user) {
      return NextResponse.json({ error: 'User not found' }, { status: 404 })
    }

    // Verify current password
    const isCurrentPasswordValid = await compare(currentPassword, user.password_hash)
    
    if (!isCurrentPasswordValid) {
      logWarn('Password change failed - incorrect current password', { userId })
      return NextResponse.json({ 
        error: 'Current password is incorrect',
        message: 'The current password you entered is incorrect'
      }, { status: 400 })
    }

    // Hash new password
    const newPasswordHash = await hash(newPassword, 10)

    // Update password
    await sql`
      UPDATE users 
      SET password_hash = ${newPasswordHash}, updated_at = NOW()
      WHERE id = ${userId}
    `

    // Log audit event (don't log actual passwords)
    const changes: Record<string, { old: any; new: any }> = {
      password: { old: '***', new: '***' }
    }
    await logAudit(
      'update',
      'user_password',
      userId,
      userId,
      changes
    )

    logInfo('Password changed', { userId })

    return NextResponse.json({ 
      success: true, 
      message: 'Password changed successfully' 
    })
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ 
        error: 'Validation failed',
        details: error.errors
      }, { status: 400 })
    }

    logError('Error changing password', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ error: 'Internal server error' }, { status: 500 })
  }
}
