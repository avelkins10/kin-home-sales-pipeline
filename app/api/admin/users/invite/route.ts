// app/api/admin/users/invite/route.ts
export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { hash } from 'bcryptjs'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'
import { inviteUserSchema } from '@/lib/validation/admin'
import { v4 as uuidv4 } from 'uuid'

/**
 * POST /api/admin/users/invite
 * Send user invite (recommended method to avoid thousands of users)
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin', 'office_leader'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const validatedData = inviteUserSchema.parse(body)

    const { email, name, role, office, offices, sendEmail } = validatedData

    // Check if user already exists
    const existingUser = await sql.query('SELECT id FROM users WHERE email = $1', [email])
    if (existingUser.rows.length > 0) {
      return NextResponse.json(
        { error: 'User already exists with this email' },
        { status: 400 }
      )
    }

    // Generate unique invite token
    const inviteToken = uuidv4()
    const invitedAt = new Date().toISOString()

    // Start transaction
    await sql.query('BEGIN')

    try {
      // Create user with is_active=false and invite token
      const userResult = await sql.query(`
        INSERT INTO users (
          email, name, role, quickbase_user_id, sales_office, 
          is_active, invite_token, invited_at
        )
        VALUES ($1, $2, $3, $4, ARRAY[$5]::text[], $6, $7, $8)
        RETURNING id, email, name, role, sales_office, invite_token, invited_at
      `, [
        email, 
        name, 
        role, 
        '', // quickbase_user_id will be filled when user accepts invite
        office || null, 
        false, // is_active
        inviteToken, 
        invitedAt
      ])

      const user = userResult.rows[0]

      // Insert office access assignments for area directors/divisionals
      if (offices && offices.length > 0) {
        for (const officeName of offices) {
          await sql.query(`
            INSERT INTO office_assignments (user_id, office_name, access_level)
            VALUES ($1, $2, $3)
          `, [user.id, officeName, 'manage'])
        }
      }

      // Insert default notification settings
      await sql.query(`
        INSERT INTO notification_settings (user_id)
        VALUES ($1)
      `, [user.id])

      // Commit transaction
      await sql.query('COMMIT')

      // Send invite email if requested
      if (sendEmail) {
        const inviteLink = `${process.env.NEXT_PUBLIC_APP_URL}/accept-invite?token=${inviteToken}`
        
        // TODO: Implement email sending
        // await sendInviteEmail(email, name, role, inviteLink)
        
        logInfo('Invite email would be sent', { 
          email, 
          inviteLink,
          note: 'Email sending not implemented yet'
        })
      }

      // Log audit event
      await logAudit(
        'invite',
        'user',
        user.id,
        auth.session.user.id,
        {
          email: { old: null, new: email },
          name: { old: null, new: name },
          role: { old: null, new: role },
          office: { old: null, new: office },
          offices: { old: null, new: offices },
          sendEmail: { old: null, new: sendEmail }
        }
      )

      logInfo('User invite created', { 
        userId: user.id, 
        email, 
        role, 
        inviteToken: inviteToken.substring(0, 8) + '...'
      })

      return NextResponse.json({
        success: true,
        user: {
          id: user.id,
          email: user.email,
          name: user.name,
          role: user.role,
          inviteToken: user.invite_token,
          inviteLink: `${process.env.NEXT_PUBLIC_APP_URL}/accept-invite?token=${inviteToken}`,
          invitedAt: user.invited_at
        }
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
        { error: 'Email already in use' },
        { status: 400 }
      )
    }

    logError('Failed to create user invite', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to create user invite' },
      { status: 500 }
    )
  }
}

/**
 * GET /api/admin/users/invite?token=xxx
 * Check invite status
 */
export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url)
    const token = searchParams.get('token')

    if (!token) {
      return NextResponse.json(
        { error: 'Token is required' },
        { status: 400 }
      )
    }

    // Look up invite by token
    const result = await sql.query(`
      SELECT id, email, name, role, sales_office, invited_at, invite_accepted_at
      FROM users 
      WHERE invite_token = $1
    `, [token])

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'Invalid or expired invite token' },
        { status: 404 }
      )
    }

    const user = result.rows[0]
    const invitedAt = new Date(user.invited_at)
    const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000)

    // Check if invite is expired (7 days)
    if (invitedAt < sevenDaysAgo) {
      return NextResponse.json(
        { error: 'Invite has expired' },
        { status: 410 }
      )
    }

    // Check if already accepted
    if (user.invite_accepted_at) {
      return NextResponse.json(
        { error: 'Invite has already been used' },
        { status: 410 }
      )
    }

    return NextResponse.json({
      email: user.email,
      name: user.name,
      role: user.role,
      office: user.sales_office?.[0],
      invitedAt: user.invited_at,
      accepted: !!user.invite_accepted_at
    })
  } catch (error) {
    logError('Failed to check invite status', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to check invite status' },
      { status: 500 }
    )
  }
}

/**
 * POST /api/admin/users/invite/accept
 * Accept invite and set password
 */
export async function PUT(request: NextRequest) {
  try {
    const body = await request.json()
    const { token, password } = body

    if (!token || !password) {
      return NextResponse.json(
        { error: 'Token and password are required' },
        { status: 400 }
      )
    }

    if (password.length < 8) {
      return NextResponse.json(
        { error: 'Password must be at least 8 characters' },
        { status: 400 }
      )
    }

    // Look up invite by token
    const result = await sql.query(`
      SELECT id, email, name, role, sales_office, invited_at, invite_accepted_at
      FROM users 
      WHERE invite_token = $1
    `, [token])

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'Invalid or expired invite token' },
        { status: 404 }
      )
    }

    const user = result.rows[0]
    const invitedAt = new Date(user.invited_at)
    const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000)

    // Check if invite is expired (7 days)
    if (invitedAt < sevenDaysAgo) {
      return NextResponse.json(
        { error: 'Invite has expired' },
        { status: 410 }
      )
    }

    // Check if already accepted
    if (user.invite_accepted_at) {
      return NextResponse.json(
        { error: 'Invite has already been used' },
        { status: 410 }
      )
    }

    // Hash password
    const hashedPassword = await hash(password, 10)
    const acceptedAt = new Date().toISOString()

    // Update user: set password, activate account, clear invite token
    await sql.query(`
      UPDATE users 
      SET 
        password_hash = $1,
        is_active = true,
        invite_accepted_at = $2,
        invite_token = NULL
      WHERE id = $3
    `, [hashedPassword, acceptedAt, user.id])

    // TODO: Look up user's QuickBase data using email and update user record
    // This would call getUserByQuickbaseId and update quickbase_user_id, phone, etc.

    logInfo('User accepted invite', { 
      userId: user.id, 
      email: user.email,
      role: user.role
    })

    return NextResponse.json({
      success: true,
      message: 'Account created successfully',
      user: {
        id: user.id,
        email: user.email,
        name: user.name,
        role: user.role
      }
    })
  } catch (error) {
    logError('Failed to accept invite', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to accept invite' },
      { status: 500 }
    )
  }
}

/**
 * DELETE /api/admin/users/invite?userId=xxx
 * Revoke invite (super admin only)
 */
export async function DELETE(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const userId = searchParams.get('userId')

    if (!userId) {
      return NextResponse.json(
        { error: 'User ID is required' },
        { status: 400 }
      )
    }

    // Check if user exists and has pending invite
    const result = await sql.query(`
      SELECT id, email, invite_accepted_at
      FROM users 
      WHERE id = $1
    `, [userId])

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'User not found' },
        { status: 404 }
      )
    }

    const user = result.rows[0]

    // Check if invite was already accepted
    if (user.invite_accepted_at) {
      return NextResponse.json(
        { error: 'Cannot revoke accepted invite' },
        { status: 400 }
      )
    }

    // Delete user (since invite wasn't accepted yet)
    await sql.query('DELETE FROM users WHERE id = $1', [userId])

    // Log audit event
    await logAudit(
      'revoke_invite',
      'user',
      userId,
      auth.session.user.id,
      {
        email: { old: user.email, new: null }
      }
    )

    logInfo('User invite revoked', { 
      userId, 
      email: user.email
    })

    return NextResponse.json({
      success: true,
      message: 'Invite revoked successfully'
    })
  } catch (error) {
    logError('Failed to revoke invite', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to revoke invite' },
      { status: 500 }
    )
  }
}
