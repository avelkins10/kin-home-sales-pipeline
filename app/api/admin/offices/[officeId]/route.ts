export const runtime = 'nodejs'

import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logWarn, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'

const updateOfficeSchema = z.object({
  name: z.string().min(1).max(100).optional(),
  region: z.enum(['southwest', 'southeast', 'midwest', 'northeast', 'west']).optional(),
  leaderId: z.string().optional(),
})

export async function PATCH(
  request: NextRequest,
  { params }: { params: { officeId: string } }
) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { officeId } = params
    const body = await request.json()
    const validatedData = updateOfficeSchema.parse(body)

    // Get old office data for audit log
    const oldOfficeResult = await sql`
      SELECT name, region, leader_id FROM offices WHERE id = ${officeId}
    `
    const oldOffice = oldOfficeResult.rows[0]

    // Check name uniqueness if name is being updated
    if (validatedData.name) {
      const existingOffice = await sql.query(
        'SELECT id FROM offices WHERE name = $1 AND id != $2',
        [validatedData.name, officeId]
      )
      if (existingOffice.rows.length > 0) {
        return NextResponse.json(
          { error: 'Office name already exists' },
          { status: 400 }
        )
      }
    }

    // Verify leader if leaderId is being updated
    if (validatedData.leaderId) {
      const leaderResult = await sql.query('SELECT id, role FROM users WHERE id = $1', [validatedData.leaderId])
      if (leaderResult.rows.length === 0) {
        return NextResponse.json(
          { error: 'Invalid office leader' },
          { status: 400 }
        )
      }

      const leader = leaderResult.rows[0]
      if (!['office_leader', 'regional', 'super_admin'].includes(leader.role)) {
        return NextResponse.json(
          { error: 'User must be office leader or above' },
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
        const dbKey = key === 'leaderId' ? 'leader_id' : key
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
    values.push(officeId)

    const query = `
      UPDATE offices 
      SET ${updateFields.join(', ')}
      WHERE id = $${paramIndex}
      RETURNING id, name, region, leader_id, created_at, updated_at
    `

    const result = await sql.query(query, values)

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'Office not found' },
        { status: 404 }
      )
    }

    const office = result.rows[0]

    // Fetch office with stats
    const statsResult = await sql.query(`
      SELECT 
        o.id, o.name, o.region, o.leader_id, o.created_at, o.updated_at,
        u.name as leader_name,
        (
          SELECT COUNT(*) FROM users u2 
          WHERE o.name = ANY(u2.sales_office) AND u2.is_active = true
        ) as user_count,
        0 as active_projects
      FROM offices o
      LEFT JOIN users u ON o.leader_id = u.id
      WHERE o.id = $1
    `, [office.id])

    const officeWithStats = statsResult.rows[0]
    const resultWithStats = {
      id: officeWithStats.id,
      name: officeWithStats.name,
      region: officeWithStats.region,
      leaderId: officeWithStats.leader_id,
      leaderName: officeWithStats.leader_name || 'Unassigned',
      userCount: parseInt(officeWithStats.user_count) || 0,
      activeProjects: parseInt(officeWithStats.active_projects) || 0,
      createdAt: officeWithStats.created_at,
      updatedAt: officeWithStats.updated_at,
    }

    // Log audit event with field-level changes
    const changes: Record<string, { old: any; new: any }> = {}
    Object.keys(validatedData).forEach(key => {
      const typedKey = key as keyof typeof validatedData
      if (oldOffice[key] !== validatedData[typedKey]) {
        changes[key] = { old: oldOffice[key], new: validatedData[typedKey] }
      }
    })
    await logAudit(
      'update',
      'office',
      officeId,
      auth.session.user.id,
      changes
    )

    logInfo('Office updated', { officeId, updatedFields: Object.keys(validatedData) })

    return NextResponse.json(resultWithStats)
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json(
        { error: 'Validation failed', message: error.errors[0].message },
        { status: 400 }
      )
    }

    if (error && typeof error === 'object' && 'code' in error && error.code === '23505') { // Unique constraint violation
      return NextResponse.json(
        { error: 'Office name already exists' },
        { status: 400 }
      )
    }

    if (error && typeof error === 'object' && 'code' in error && error.code === '23503') { // Foreign key violation
      return NextResponse.json(
        { error: 'Invalid office leader' },
        { status: 400 }
      )
    }

    logError('Failed to update office', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to update office' },
      { status: 500 }
    )
  }
}

export async function DELETE(
  request: NextRequest,
  { params }: { params: { officeId: string } }
) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { officeId } = params

    // Get office data for audit log
    const officeResult = await sql`
      SELECT name, region, leader_id FROM offices WHERE id = ${officeId}
    `
    const office = officeResult.rows[0]

    // Check for assigned users
    const userCountResult = await sql.query(`
      SELECT COUNT(*) FROM users u 
      WHERE (SELECT name FROM offices WHERE id = $1) = ANY(u.sales_office)
    `, [officeId])

    const userCount = parseInt(userCountResult.rows[0].count)
    if (userCount > 0) {
      logWarn('Attempted to delete office with assigned users', { officeId, userCount })
      return NextResponse.json(
        { error: 'Cannot delete office with assigned users. Reassign users first.' },
        { status: 400 }
      )
    }

    // Delete office
    const result = await sql.query(`
      DELETE FROM offices 
      WHERE id = $1
      RETURNING name
    `, [officeId])

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'Office not found' },
        { status: 404 }
      )
    }

    const officeName = result.rows[0].name

    // Log audit event for deletion
    const changes: Record<string, { old: any; new: any }> = {}
    Object.keys(office).forEach(key => {
      changes[key] = { old: office[key], new: null }
    })
    await logAudit(
      'delete',
      'office',
      officeId,
      auth.session.user.id,
      changes
    )

    logInfo('Office deleted', { officeId, name: officeName })

    return NextResponse.json({
      success: true,
      message: 'Office deleted successfully'
    })
  } catch (error) {
    logError('Failed to delete office', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to delete office' },
      { status: 500 }
    )
  }
}
