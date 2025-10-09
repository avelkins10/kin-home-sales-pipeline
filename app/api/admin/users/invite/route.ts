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
import { searchQuickbaseUsers } from '@/lib/quickbase/userQueries'
import { validateOffices } from '@/lib/db/offices'
import { normalizeOfficeName } from '@/lib/constants/offices'
import { sendInviteEmail, sendWelcomeEmail, validateEmailConfig } from '@/lib/utils/email-helpers'

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

    // RBAC guard: prevent privilege escalation
    const inviterRole = auth.session.user.role
    const canInviteRole = (inviterRole: string, targetRole: string): boolean => {
      // Super admin can invite anyone
      if (inviterRole === 'super_admin') return true
      
      // Office leaders can only invite closers and setters
      if (inviterRole === 'office_leader') {
        return ['closer', 'setter'].includes(targetRole)
      }
      
      // Other roles cannot invite anyone
      return false
    }

    if (!canInviteRole(inviterRole, role)) {
      return NextResponse.json(
        { error: `Insufficient privileges to invite users with role: ${role}` },
        { status: 403 }
      )
    }

    // Validate email configuration if sending email
    if (sendEmail) {
      const emailConfig = validateEmailConfig()
      if (!emailConfig.valid) {
        logInfo('Email not configured, invite will be created without sending email', { 
          missingVars: emailConfig.missingVars,
          email,
          name,
          role
        })
      }
    }

    // Validate office names
    const officesToValidate: string[] = []
    if (office) officesToValidate.push(office)
    if (offices && offices.length > 0) {
      officesToValidate.push(...offices)
    }
    
    let validatedOffice = office
    let validatedOffices = offices
    
    if (officesToValidate.length > 0) {
      try {
        const validated = await validateOffices(officesToValidate)
        if (office) {
          validatedOffice = validated.find(o => o === office || o === normalizeOfficeName(office)) || office
        }
        if (offices && offices.length > 0) {
          validatedOffices = offices.map(origOffice =>
            validated.find(o => o === origOffice || o === normalizeOfficeName(origOffice)) || origOffice
          ) as typeof offices
        }
      } catch (error) {
        return NextResponse.json(
          { error: error instanceof Error ? error.message : 'Invalid office names' },
          { status: 400 }
        )
      }
    }

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
          is_active, invite_token, invited_at, invited_by
        )
        VALUES ($1, $2, $3, $4, CASE WHEN $5 IS NOT NULL THEN ARRAY[$5]::text[] ELSE NULL END, $6, $7, $8, $9)
        RETURNING id, email, name, role, sales_office, invite_token, invited_at, invited_by
      `, [
        email, 
        name, 
        role, 
        '', // quickbase_user_id will be filled when user accepts invite
        validatedOffice || null, 
        false, // is_active
        inviteToken, 
        invitedAt,
        auth.session.user.id // invited_by
      ])

      const user = userResult.rows[0]

      // Insert office access assignments for area directors/divisionals
      if (validatedOffices && validatedOffices.length > 0) {
        for (const officeName of validatedOffices) {
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
      let emailSent = false
      let emailSkipped = false
      if (sendEmail) {
        const inviteLink = `${process.env.NEXT_PUBLIC_APP_URL}/accept-invite?token=${inviteToken}`
        
        const emailResult = await sendInviteEmail(
          email, 
          name, 
          role, 
          inviteLink, 
          auth.session.user.name,
          validatedOffice,
          validatedOffices
        )
        
        emailSent = emailResult.success
        emailSkipped = !!emailResult.skipped
        
        if (emailResult.success) {
          logInfo('Invite email sent successfully', { 
            userId: user.id, 
            email 
          })
        } else if (emailResult.skipped) {
          logInfo('Invite created but email skipped (disabled/not configured)', { 
            userId: user.id, 
            email,
            error: emailResult.error
          })
        } else {
          logInfo('Invite created but email failed to send', { 
            userId: user.id, 
            email,
            error: emailResult.error 
          })
        }
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
          office: { old: null, new: validatedOffice },
          offices: { old: null, new: validatedOffices },
          sendEmail: { old: null, new: sendEmail },
          emailSent: { old: null, new: emailSent },
          emailSkipped: { old: null, new: emailSkipped }
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
          invitedAt: user.invited_at,
          emailSent,
          emailSkipped
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
      SELECT id, email, name, role, sales_office, invited_at, invite_accepted_at, invited_by
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

    // Query office assignments for the user
    const officeResult = await sql.query(`
      SELECT office_name
      FROM office_assignments
      WHERE user_id = $1
      ORDER BY office_name
    `, [user.id])

    const offices = officeResult.rows.map(row => row.office_name)

    return NextResponse.json({
      email: user.email,
      name: user.name,
      role: user.role,
      office: user.sales_office?.[0], // Backward compatibility
      offices: offices, // New field with all assigned offices
      invitedAt: user.invited_at,
      invitedBy: user.invited_by,
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
