// app/api/admin/users/sync/route.ts
export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { hash } from 'bcryptjs'
import { randomBytes } from 'crypto'
import { logInfo, logError, logAudit, logWarn } from '@/lib/logging/logger'
import { getActiveUsersFromQuickbase } from '@/lib/quickbase/userQueries'

/**
 * POST /api/admin/users/sync
 * Smart QuickBase user synchronization (only active users)
 */
export async function POST(request: NextRequest) {
  const lockKey = 'user_sync_lock'

  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    // Rate limiting: Check if sync is already running or was executed recently
    const lockResult = await sql.query(`
      SELECT pg_try_advisory_lock(hashtext($1)) as acquired
    `, [lockKey])
    
    if (!lockResult.rows[0].acquired) {
      return NextResponse.json(
        { error: 'Sync is already running or was executed recently. Please wait before trying again.' },
        { status: 429 }
      )
    }

    // Check for recent sync (within last minute)
    const recentSyncResult = await sql.query(`
      SELECT created_at 
      FROM sync_logs 
      WHERE sync_type = 'user_sync' 
      AND created_at > NOW() - INTERVAL '1 minute'
      ORDER BY created_at DESC 
      LIMIT 1
    `)
    
    if (recentSyncResult.rows.length > 0) {
      // Release the lock
      await sql.query(`SELECT pg_advisory_unlock(hashtext($1))`, [lockKey])
      return NextResponse.json(
        { error: 'Sync was executed recently. Please wait at least 1 minute before trying again.' },
        { status: 429 }
      )
    }

    const body = await request.json()
    const { 
      dryRun = true, 
      monthsBack = 6, 
      role = 'both' 
    } = body

    // Validate role parameter
    const allowedRoles = new Set(['both', 'closer', 'setter'])
    if (!allowedRoles.has(role)) {
      return NextResponse.json({ error: 'role must be closer|setter|both' }, { status: 400 })
    }

    if (monthsBack < 1 || monthsBack > 24) {
      return NextResponse.json(
        { error: 'monthsBack must be between 1 and 24' },
        { status: 400 }
      )
    }

    logInfo('Starting smart user sync', { dryRun, monthsBack, role })

    // Get active users from QuickBase
    const activeUsers = await getActiveUsersFromQuickbase(monthsBack)
    
    // Filter by role if specified
    let usersToSync = activeUsers
    if (role !== 'both') {
      usersToSync = activeUsers.filter(user => user.role === role)
    }

    logInfo('Found active users in QuickBase', { 
      total: activeUsers.length,
      filtered: usersToSync.length,
      role
    })

    const results = {
      created: 0,
      updated: 0,
      skipped: 0,
      errors: 0,
      conflicts: [] as any[]
    }

    if (dryRun) {
      // Preview mode - just return what would happen
      for (const userData of usersToSync) {
        const existingUser = await sql.query(
          'SELECT id, email, quickbase_user_id FROM users WHERE email = $1 OR quickbase_user_id = $2',
          [userData.email, userData.quickbaseUserId]
        )

        if (existingUser.rows.length === 0) {
          results.created++
        } else {
          results.updated++
        }
      }

      // Release the lock for dry run
      await sql.query(`SELECT pg_advisory_unlock(hashtext($1))`, [lockKey])
      
      return NextResponse.json({
        success: true,
        dryRun: true,
        results,
        usersToSync: usersToSync.slice(0, 10), // Preview first 10 users
        message: `Would create ${results.created} users, update ${results.updated} users, skip ${results.skipped} skipped (inactive), ${results.errors} errors`
      })
    }

    // Actual sync mode
    await sql.query('BEGIN')

    try {
      for (const userData of usersToSync) {
        try {
          // Check if user already exists (by email or QuickBase ID)
          const existingUser = await sql.query(
            'SELECT id, email, quickbase_user_id, name, phone FROM users WHERE email = $1 OR quickbase_user_id = $2',
            [userData.email, userData.quickbaseUserId]
          )

          if (existingUser.rows.length === 0) {
            // Create new user
            const tempPassword = randomBytes(12).toString('base64url')
            const hashedPassword = await hash(tempPassword, 10)

            const inserted = await sql.query(`
              INSERT INTO users (
                email, name, phone, role, quickbase_user_id, sales_office, 
                password_hash, is_active, last_project_date
              )
              VALUES ($1, $2, $3, $4, $5, ARRAY[$6]::text[], $7, $8, $9)
              RETURNING id
            `, [
              userData.email,
              userData.name,
              userData.phone,
              userData.role,
              userData.quickbaseUserId,
              userData.office,
              hashedPassword,
              true,
              userData.lastProjectDate
            ])

            const newUserId = inserted.rows[0].id

            // Insert default notification settings (schema from migration 002)
            // The notification_settings table uses user_id as PRIMARY KEY
            // and has default values for all notification preferences
            try {
              await sql.query(`
                INSERT INTO notification_settings (user_id)
                VALUES ($1)
                ON CONFLICT (user_id) DO NOTHING
              `, [newUserId])
            } catch (notifError) {
              // Log but don't fail the sync if notification settings insert fails
              logWarn('Failed to create notification settings for user', {
                userId: newUserId,
                error: notifError instanceof Error ? notifError.message : String(notifError)
              })
            }

            results.created++

            // Log audit event
            await logAudit(
              'sync_create',
              'user',
              newUserId,
              auth.session.user.id,
              {
                email: { old: null, new: userData.email },
                name: { old: null, new: userData.name },
                role: { old: null, new: userData.role },
                quickbaseUserId: { old: null, new: userData.quickbaseUserId },
                lastProjectDate: { old: null, new: userData.lastProjectDate }
              }
            )
          } else {
            // Update existing user
            const existing = existingUser.rows[0]
            
            await sql.query(`
              UPDATE users 
              SET 
                name = $1,
                phone = $2,
                quickbase_user_id = $3,
                sales_office = ARRAY[$4]::text[],
                last_project_date = $5,
                updated_at = NOW()
              WHERE id = $6
            `, [
              userData.name,
              userData.phone,
              userData.quickbaseUserId,
              userData.office,
              userData.lastProjectDate,
              existing.id
            ])

            results.updated++

            // Log audit event
            await logAudit(
              'sync_update',
              'user',
              existing.id,
              auth.session.user.id,
              {
                name: { old: existing.name, new: userData.name },
                phone: { old: existing.phone, new: userData.phone },
                quickbaseUserId: { old: existing.quickbase_user_id, new: userData.quickbaseUserId },
                lastProjectDate: { old: null, new: userData.lastProjectDate }
              }
            )
          }
        } catch (error) {
          results.conflicts.push({
            user: userData,
            error: error instanceof Error ? error.message : String(error)
          })
          results.errors++
        }
      }

      // Store sync results in database for status checking
      await sql.query(`
        INSERT INTO sync_logs (sync_type, results, created_by, created_at)
        VALUES ($1, $2, $3, NOW())
      `, ['user_sync', JSON.stringify(results), auth.session.user.id])

      await sql.query('COMMIT')

      logInfo('Smart user sync completed', results)

      return NextResponse.json({
        success: true,
        dryRun: false,
        results,
        message: `Sync completed: ${results.created} created, ${results.updated} updated, ${results.skipped} skipped (inactive), ${results.errors} errors`
      })
    } catch (error) {
      await sql.query('ROLLBACK')
      throw error
    } finally {
      // Always release the advisory lock
      await sql.query(`SELECT pg_advisory_unlock(hashtext($1))`, [lockKey])
    }
  } catch (error) {
    // Release the lock in case of error
    try {
      await sql.query(`SELECT pg_advisory_unlock(hashtext($1))`, [lockKey])
    } catch (lockError) {
      // Ignore lock release errors
    }
    
    logError('Failed to sync users', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to sync users' },
      { status: 500 }
    )
  }
}

/**
 * GET /api/admin/users/sync/status
 * Check sync status and get last sync results
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    // Get last sync timestamp and results
    const lastSyncResult = await sql.query(`
      SELECT results, created_at, created_by
      FROM sync_logs 
      WHERE sync_type = 'user_sync'
      ORDER BY created_at DESC 
      LIMIT 1
    `)

    // Get user activity statistics
    const activityStats = await sql.query(`
      SELECT 
        COUNT(*) as total_users,
        COUNT(CASE WHEN last_project_date >= NOW() - INTERVAL '6 months' THEN 1 END) as active_6_months,
        COUNT(CASE WHEN last_project_date >= NOW() - INTERVAL '12 months' AND last_project_date < NOW() - INTERVAL '6 months' THEN 1 END) as inactive_6_12_months,
        COUNT(CASE WHEN last_project_date < NOW() - INTERVAL '12 months' OR last_project_date IS NULL THEN 1 END) as dormant_12_months
      FROM users 
      WHERE role IN ('closer', 'setter')
    `)

    const stats = activityStats.rows[0]

    return NextResponse.json({
      lastSync: lastSyncResult.rows[0] || null,
      activityStats: {
        total: parseInt(stats.total_users),
        active: parseInt(stats.active_6_months),
        inactive: parseInt(stats.inactive_6_12_months),
        dormant: parseInt(stats.dormant_12_months)
      }
    })
  } catch (error) {
    logError('Failed to get sync status', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to get sync status' },
      { status: 500 }
    )
  }
}


