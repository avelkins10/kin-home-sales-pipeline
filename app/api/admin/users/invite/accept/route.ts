// app/api/admin/users/invite/accept/route.ts
export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { sql } from '@/lib/db/client'
import { hash } from 'bcryptjs'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import { searchQuickbaseUsers } from '@/lib/quickbase/userQueries'
import { sendWelcomeEmail } from '@/lib/utils/email-helpers'

/**
 * POST /api/admin/users/invite/accept
 * Accept invite and set password
 */
export async function POST(request: NextRequest) {
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

    // Look up user's QuickBase data using email and update user record
    let quickbaseUserId = '';
    let phone = user.phone || '';
    let defaultOffice = user.sales_office?.[0] || '';
    
    try {
      const quickbaseUsers = await searchQuickbaseUsers(user.email);
      if (quickbaseUsers.length > 0) {
        // Find best match by email
        const bestMatch = quickbaseUsers.find(qbUser => 
          qbUser.email?.toLowerCase() === user.email.toLowerCase()
        ) || quickbaseUsers[0]; // Fall back to first result
        
        quickbaseUserId = bestMatch.quickbaseUserId;
        phone = bestMatch.phone || phone;
        
        // Set default office if not already set and QuickBase user has office
        if (!defaultOffice && bestMatch.office) {
          defaultOffice = bestMatch.office;
        }
        
        logInfo('QuickBase user data found for invite acceptance', {
          userId: user.id,
          email: user.email,
          quickbaseUserId,
          phone: phone ? '***' : 'none',
          office: defaultOffice
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

    // Log audit event for QuickBase data back-fill
    if (quickbaseUserId || phone || defaultOffice) {
      await logAudit(
        'backfill_quickbase_data',
        'user',
        user.id,
        'system', // System action
        {
          quickbase_user_id: { old: '', new: quickbaseUserId },
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
