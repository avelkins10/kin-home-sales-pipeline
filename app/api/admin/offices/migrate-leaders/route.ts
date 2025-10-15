export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'

/**
 * POST /api/admin/offices/migrate-leaders
 * Migrate legacy office leaders (offices.leader_id) to new multi-manager system (office_assignments)
 * This ensures users assigned via the old system can access their office projects
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    logInfo('[MIGRATE_LEADERS] Starting migration of legacy office leaders to office_assignments')

    // Start transaction
    await sql.query('BEGIN')

    try {
      // Find all offices with leader_id set
      const officesWithLeaders = await sql.query(`
        SELECT
          o.id,
          o.name,
          o.leader_id,
          u.name as leader_name,
          u.role as leader_role,
          u.email as leader_email
        FROM offices o
        INNER JOIN users u ON o.leader_id = u.id
        WHERE o.leader_id IS NOT NULL
          AND o.is_active = true
          AND u.is_active = true
      `)

      logInfo('[MIGRATE_LEADERS] Found offices with leaders', { count: officesWithLeaders.rows.length })

      let migratedCount = 0
      let skippedCount = 0
      let errorCount = 0
      const migratedOffices: Array<{ officeName: string; leaderName: string; leaderEmail: string }> = []
      const skippedOffices: Array<{ officeName: string; leaderName: string; reason: string }> = []

      for (const office of officesWithLeaders.rows) {
        try {
          // Check if assignment already exists
          const existingAssignment = await sql.query(
            `SELECT user_id FROM office_assignments WHERE user_id = $1 AND office_name = $2`,
            [office.leader_id, office.name]
          )

          if (existingAssignment.rows.length > 0) {
            skippedCount++
            skippedOffices.push({
              officeName: office.name,
              leaderName: office.leader_name,
              reason: 'Assignment already exists'
            })
            logInfo('[MIGRATE_LEADERS] Skipping - assignment already exists', {
              officeName: office.name,
              leaderId: office.leader_id,
              leaderName: office.leader_name
            })
            continue
          }

          // Create office assignment with 'admin' access level
          await sql.query(
            `INSERT INTO office_assignments (user_id, office_name, access_level, assigned_at)
             VALUES ($1, $2, $3, NOW())`,
            [office.leader_id, office.name, 'admin']
          )

          // Update user's sales_office array to include this office if not already present
          await sql.query(
            `UPDATE users
             SET sales_office = ARRAY(
               SELECT DISTINCT unnest(COALESCE(sales_office, ARRAY[]::text[]) || $1)
             )
             WHERE id = $2`,
            [[office.name], office.leader_id]
          )

          migratedCount++
          migratedOffices.push({
            officeName: office.name,
            leaderName: office.leader_name,
            leaderEmail: office.leader_email
          })

          logInfo('[MIGRATE_LEADERS] Migrated office leader', {
            officeName: office.name,
            leaderId: office.leader_id,
            leaderName: office.leader_name,
            leaderRole: office.leader_role
          })

          // Log audit event
          await logAudit(
            'migrate',
            'office_assignment',
            `${office.leader_id}-${office.name}`,
            auth.session.user.id,
            {
              action: { old: null, new: 'migrate_legacy_leader' },
              officeName: { old: null, new: office.name },
              userId: { old: null, new: office.leader_id }
            }
          )
        } catch (error) {
          errorCount++
          logError('[MIGRATE_LEADERS] Error migrating office', error instanceof Error ? error : new Error(String(error)), {
            officeName: office.name,
            leaderId: office.leader_id
          })
        }
      }

      // Commit transaction
      await sql.query('COMMIT')

      logInfo('[MIGRATE_LEADERS] Migration completed', {
        total: officesWithLeaders.rows.length,
        migrated: migratedCount,
        skipped: skippedCount,
        errors: errorCount
      })

      return NextResponse.json({
        success: true,
        message: `Migration completed: ${migratedCount} leaders migrated, ${skippedCount} skipped, ${errorCount} errors`,
        stats: {
          total: officesWithLeaders.rows.length,
          migrated: migratedCount,
          skipped: skippedCount,
          errors: errorCount
        },
        migratedOffices,
        skippedOffices
      })
    } catch (error) {
      // Rollback on error
      await sql.query('ROLLBACK')
      throw error
    }
  } catch (error) {
    logError('[MIGRATE_LEADERS] Migration failed', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to migrate office leaders', message: error instanceof Error ? error.message : 'Unknown error' },
      { status: 500 }
    )
  }
}
