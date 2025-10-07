import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { hash } from 'bcryptjs'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import { sendMail } from '@/lib/utils/mailer'
import { rateLimit } from '@/lib/auth/rateLimit'

export async function POST(
  request: NextRequest,
  { params }: { params: { userId: string } }
) {
  try {
    const ip = request.headers.get('x-forwarded-for') || ''
    if (!rateLimit(['reset-password', ip, params.userId], 10, 60_000)) {
      return NextResponse.json({ error: 'Too Many Requests' }, { status: 429 })
    }
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { userId } = params

    // Generate temporary password
    const randomString = Math.random().toString(36).slice(2, 10)
    const timestamp = Date.now().toString(36)
    const temporaryPassword = `TempPass${randomString}${timestamp}`

    // Hash password
    const hashedPassword = await hash(temporaryPassword, 10)

    // Update user password
    const result = await sql.query(`
      UPDATE users 
      SET password_hash = $1, updated_at = NOW()
      WHERE id = $2
      RETURNING email, name
    `, [hashedPassword, userId])

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    }

    const user = result.rows[0]

    // Log audit event (don't log actual password)
    const changes: Record<string, { old: any; new: any }> = {
      password: { old: '***', new: '*** (reset by admin)' }
    }
    await logAudit(
      'update',
      'user_password_reset',
      userId,
      auth.session.user.id,
      changes,
      {
        ipAddress: request.headers.get('x-forwarded-for') || undefined,
        userAgent: request.headers.get('user-agent') || undefined,
      }
    )

    // Log password reset (for audit trail)
    logInfo('Password reset', { userId, email: user.email })

    // Send email if enabled
    if (process.env.EMAIL_ENABLED === 'true') {
      await sendMail({
        to: user.email,
        subject: 'Your Kin Solar temporary password',
        html: `<p>Hello ${user.name || ''},</p><p>Your temporary password has been generated. Please use it to log in and change it immediately.</p>`,
      })
    }

    return NextResponse.json({
      success: true,
      message: 'Password reset successfully. Email sent if enabled.'
    })
  } catch (error) {
    logError('Failed to reset password', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to reset password' },
      { status: 500 }
    )
  }
}
