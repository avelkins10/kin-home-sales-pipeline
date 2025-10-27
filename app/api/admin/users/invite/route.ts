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
 * Send user invite OR accept invite based on request body
 * - If body contains { email, name, role, ... } → create invite
 * - If body contains { token, password } → accept invite
 */
export async function POST(request: NextRequest) {
  let requestBody: any
  try {
    requestBody = await request.json()
    const body = requestBody
    
    // Check if this is an invite acceptance (has token and password)
    if (body.token && body.password) {
      return await handleInviteAcceptance(body)
    }
    
    // Otherwise, this is an invite creation (requires auth)
    const auth = await requireRole(['super_admin', 'office_leader'])
    if (!auth.authorized) {
      return auth.response
    }

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
      // Prepare sales_office array
      const salesOfficeArray = validatedOffice ? [validatedOffice] : null

      // Create user with is_active=false and invite token
      const userResult = await sql.query(`
        INSERT INTO users (
          email, name, role, quickbase_user_id, sales_office,
          is_active, invite_token, invited_at
        )
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
        RETURNING id, email, name, role, sales_office, invite_token, invited_at
      `, [
        email,
        name,
        role,
        null, // quickbase_user_id will be filled when user accepts invite (if they have one)
        salesOfficeArray,
        false, // is_active
        inviteToken,
        invitedAt
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

    const errorMessage = error instanceof Error ? error.message : String(error)
    const errorStack = error instanceof Error ? error.stack : undefined

    logError('Failed to create user invite', error instanceof Error ? error : new Error(String(error)), {
      errorMessage,
      errorStack,
      requestBody: requestBody ? JSON.stringify(requestBody) : 'N/A'
    })

    return NextResponse.json(
      {
        error: 'Failed to create user invite',
        message: errorMessage,
        ...(process.env.NODE_ENV === 'development' && { stack: errorStack })
      },
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
      SELECT
        u.id, u.email, u.name, u.role, u.sales_office,
        u.invited_at, u.invite_accepted_at
      FROM users u
      WHERE u.invite_token = $1
      LIMIT 1
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

    // Get inviter info from audit log (optional)
    let invitedBy = 'Administrator'
    let invitedByName = 'Administrator'
    try {
      const inviterResult = await sql.query(`
        SELECT u.name, u.email
        FROM audit_logs al
        JOIN users u ON u.id = al.user_id
        WHERE al.record_id = $1::text
          AND al.action = 'invite'
          AND al.entity_type = 'user'
        ORDER BY al.created_at DESC
        LIMIT 1
      `, [user.id])

      if (inviterResult.rows.length > 0) {
        invitedByName = inviterResult.rows[0].name
        invitedBy = inviterResult.rows[0].email
      }
    } catch (error) {
      // If audit log query fails, just use default values
      logInfo('Could not fetch inviter info from audit log', { userId: user.id })
    }

    return NextResponse.json({
      email: user.email,
      name: user.name,
      role: user.role,
      office: user.sales_office?.[0] || null, // Backward compatibility
      offices: offices, // New field with all assigned offices
      invitedAt: user.invited_at,
      invitedBy: invitedBy,
      invitedByName: invitedByName,
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

/**
 * Handle invite acceptance logic
 * Migrated from /api/admin/users/invite/accept/route.ts
 */
async function handleInviteAcceptance(body: { token: string; password: string }) {
  try {
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

    // Look up user's QuickBase data using email and update user record
    let quickbaseUserId: string | null = null;
    let phone = user.phone || '';
    let defaultOffice = user.sales_office?.[0] || '';

    try {
      const quickbaseUsers = await searchQuickbaseUsers(user.email);
      if (quickbaseUsers.length > 0) {
        // Find best match by email
        const bestMatch = quickbaseUsers.find(qbUser =>
          qbUser.email?.toLowerCase() === user.email.toLowerCase()
        ) || quickbaseUsers[0]; // Fall back to first result

        quickbaseUserId = bestMatch.quickbaseUserId || null;
        phone = bestMatch.phone || phone;

        // Set default office if not already set and QuickBase user has office
        if (!defaultOffice && bestMatch.office) {
          defaultOffice = bestMatch.office;
        }

        logInfo('QuickBase user data found for invite acceptance', {
          userId: user.id,
          email: user.email,
          quickbaseUserId: quickbaseUserId || 'none',
          phone: phone ? '***' : 'none',
          office: defaultOffice
        });
      } else {
        logInfo('No QuickBase user found for invite acceptance - continuing without QB data', {
          userId: user.id,
          email: user.email
        });
      }
    } catch (error) {
      logError('Failed to lookup QuickBase user data during invite acceptance', error as Error, {
        userId: user.id,
        email: user.email
      });
      // Continue with activation even if QuickBase lookup fails
    }

    // Update user: set password, activate account, clear invite token, and back-fill QuickBase data
    await sql.query(`
      UPDATE users 
      SET 
        password_hash = $1,
        is_active = true,
        invite_accepted_at = $2,
        invite_token = NULL,
        quickbase_user_id = $4,
        phone = $5,
        sales_office = CASE 
          WHEN $6 IS NOT NULL AND $6 != '' THEN ARRAY[$6]::text[]
          ELSE sales_office
        END
      WHERE id = $3
    `, [hashedPassword, acceptedAt, user.id, quickbaseUserId, phone, defaultOffice])

    // Log audit event for QuickBase data back-fill (only if data was found)
    if (quickbaseUserId || phone || defaultOffice) {
      await logAudit(
        'backfill_quickbase_data',
        'user',
        user.id,
        'system', // System action
        {
          quickbase_user_id: { old: null, new: quickbaseUserId },
          phone: { old: user.phone || '', new: phone },
          default_office: { old: user.sales_office?.[0] || '', new: defaultOffice }
        }
      );
    }

    logInfo('User accepted invite', { 
      userId: user.id, 
      email: user.email,
      role: user.role
    })

    // Send welcome email (fire and forget)
    sendWelcomeEmail(user.email, user.name, user.role)
      .then((result) => {
        if (result.success) {
          logInfo('Welcome email sent', { userId: user.id })
        } else {
          logError('Failed to send welcome email', new Error(result.error || 'Unknown error'), { userId: user.id })
        }
      })
      .catch((error) => {
        logError('Failed to send welcome email', error as Error, { userId: user.id })
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


