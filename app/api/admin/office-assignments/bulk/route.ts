export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit, logWarn } from '@/lib/logging/logger'
import { z } from 'zod'
import { bulkAssignOfficesSchema } from '@/lib/validation/admin'

export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const validatedData = bulkAssignOfficesSchema.parse(body)

    const { userIds, officeNames, accessLevel } = validatedData

    // Start a transaction
    await sql.query('BEGIN')
    logInfo('[OFFICE_ASSIGNMENT] Starting transaction', { userCount: userIds.length, officeCount: officeNames.length });

    try {
      // Validate all users exist and have appropriate roles
      const userValidationQuery = `
        SELECT id, name, role 
        FROM users 
        WHERE id = ANY($1) 
          AND is_active = true
          AND role IN ('office_leader', 'area_director', 'divisional', 'regional', 'super_admin')
      `
      const userResult = await sql.query(userValidationQuery, [userIds])
      
      if (userResult.rows.length !== userIds.length) {
        const foundIds = userResult.rows.map(row => row.id)
        const missingIds = userIds.filter(id => !foundIds.includes(id))
        logWarn('[OFFICE_ASSIGNMENT] Some users not found or invalid', { requestedCount: userIds.length, foundCount: userResult.rows.length, missingIds });
        throw new Error(`Invalid or inactive users: ${missingIds.join(', ')}`)
      }
      
      logInfo('[OFFICE_ASSIGNMENT] Validated users', { requestedCount: userIds.length, foundCount: userResult.rows.length });

      // Validate all offices exist
      const officeValidationQuery = `
        SELECT name 
        FROM offices 
        WHERE name = ANY($1) AND is_active = true
      `
      const officeResult = await sql.query(officeValidationQuery, [officeNames])
      
      if (officeResult.rows.length !== officeNames.length) {
        const foundNames = officeResult.rows.map(row => row.name)
        const missingNames = officeNames.filter(name => !foundNames.includes(name))
        logWarn('[OFFICE_ASSIGNMENT] Some offices not found or invalid', { requestedCount: officeNames.length, foundCount: officeResult.rows.length, missingNames });
        throw new Error(`Invalid or inactive offices: ${missingNames.join(', ')}`)
      }
      
      logInfo('[OFFICE_ASSIGNMENT] Validated offices', { requestedCount: officeNames.length, foundCount: officeResult.rows.length });

      // Bulk insert/update office assignments
      let createdCount = 0
      let updatedCount = 0
      const assignments: Array<{ userId: string; userName: string; officeName: string; accessLevel: string }> = []

      for (const userId of userIds) {
        const user = userResult.rows.find(u => u.id === userId)
        
        for (const officeName of officeNames) {
          // Insert or update office assignment
          const assignmentQuery = `
            INSERT INTO office_assignments (user_id, office_name, access_level, assigned_at)
            VALUES ($1, $2, $3, NOW())
            ON CONFLICT (user_id, office_name) 
            DO UPDATE SET 
              access_level = EXCLUDED.access_level,
              assigned_at = NOW()
            RETURNING (xmax = 0) AS inserted
          `
          
          const assignmentResult = await sql.query(assignmentQuery, [userId, officeName, accessLevel])
          const wasInserted = assignmentResult.rows[0].inserted
          
          if (wasInserted) {
            createdCount++
          } else {
            updatedCount++
          }

          assignments.push({
            userId,
            userName: user.name,
            officeName,
            accessLevel
          })

          logInfo('[OFFICE_ASSIGNMENT] Created/updated assignment', { userId, userName: user.name, officeName, accessLevel, wasInserted });

          // Log audit event for each assignment
          await logAudit(
            'assign',
            'office_assignment',
            `${userId}-${officeName}`,
            auth.session.user.id,
            {
              officeNames: { old: [], new: [officeName] }
            }
          )
        }

        // Update user's sales_office array to include all assigned offices
        const updateUserOfficesQuery = `
          UPDATE users 
          SET sales_office = ARRAY(
            SELECT DISTINCT unnest(COALESCE(sales_office, ARRAY[]::text[]) || $1)
          )
          WHERE id = $2
        `
        await sql.query(updateUserOfficesQuery, [officeNames, userId])
        logInfo('[OFFICE_ASSIGNMENT] Updated user sales_office array', { userId, officeCount: officeNames.length });
      }

      // Commit transaction
      await sql.query('COMMIT')
      logInfo('[OFFICE_ASSIGNMENT] Transaction committed successfully', { created: createdCount, updated: updatedCount });

      logInfo('Bulk office assignments completed', {
        userIds: userIds.length,
        officeNames: officeNames.length,
        created: createdCount,
        updated: updatedCount,
        accessLevel
      })

      return NextResponse.json({
        success: true,
        created: createdCount,
        updated: updatedCount,
        total: createdCount + updatedCount,
        assignments
      })

    } catch (error) {
      // Rollback transaction on error
      await sql.query('ROLLBACK')
      logError('[OFFICE_ASSIGNMENT] Transaction rolled back', error as Error, { userIds, officeNames });
      throw error
    }

  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Validation failed', message: error.errors[0].message },
        { status: 400 }
      )
    }

    if (error instanceof Error) {
      return NextResponse.json(
        { error: error.message },
        { status: 400 }
      )
    }

    logError('Failed to bulk assign offices', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to bulk assign offices' },
      { status: 500 }
    )
  }
}
