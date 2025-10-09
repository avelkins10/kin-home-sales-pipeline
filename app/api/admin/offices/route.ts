export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'
import { createOfficeSchema } from '@/lib/validation/admin'
import { normalizeOfficeName } from '@/lib/constants/offices'

export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const { searchParams } = new URL(request.url)
    const search = searchParams.get('search')

    let query = `
      SELECT 
        o.id, o.name, o.is_active, o.region, o.leader_id, o.created_at, o.updated_at,
        u.name as leader_name,
        (
          SELECT COUNT(*) 
          FROM users u2 
          WHERE o.name = ANY(u2.sales_office) 
            AND u2.is_active = true
            AND u2.role IN ('office_leader','area_director','divisional','regional','super_admin')
        ) as manager_count,
        0 as project_count
      FROM offices o
      LEFT JOIN users u ON o.leader_id = u.id
    `

    const params: any[] = []

    query += ` ORDER BY o.name ASC`

    const result = await sql.query(query, params)
    const offices = result.rows.map(row => ({
      id: row.id,
      name: row.name,
      is_active: row.is_active,
      manager_count: Number(row.manager_count) || 0,
      project_count: Number(row.project_count) || 0,
      region: row.region,
      leaderId: row.leader_id,
      leaderName: row.leader_name || 'Unassigned',
      createdAt: row.created_at,
      updatedAt: row.updated_at,
    }))

    logInfo('Offices fetched', { count: offices.length })

    return NextResponse.json(offices)
  } catch (error) {
    logError('Failed to fetch offices', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to fetch offices' },
      { status: 500 }
    )
  }
}

export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const validatedData = createOfficeSchema.parse(body)

    const { name, region, leaderId } = validatedData
    const normalizedName = normalizeOfficeName(name)

    // Check name uniqueness
    const existingOffice = await sql.query('SELECT id FROM offices WHERE name = $1', [normalizedName])
    if (existingOffice.rows.length > 0) {
      return NextResponse.json(
        { error: 'Office name already exists' },
        { status: 400 }
      )
    }

    // Handle optional leaderId
    let leaderIdToUse: string | null = null
    if (leaderId) {
      // Verify leader exists and has appropriate role
      const leaderResult = await sql.query('SELECT id, role FROM users WHERE id = $1', [leaderId])
      if (leaderResult.rows.length === 0) {
        return NextResponse.json(
          { error: 'Invalid office leader' },
          { status: 400 }
        )
      }

      const leader = leaderResult.rows[0]
      if (!['office_leader', 'area_director', 'divisional', 'regional', 'super_admin'].includes(leader.role)) {
        return NextResponse.json(
          { error: 'User must be office leader or above' },
          { status: 400 }
        )
      }
      leaderIdToUse = leaderId
    }

    // Insert office
    const insertSql = leaderIdToUse
      ? `INSERT INTO offices (name, region, leader_id, is_active) VALUES ($1, $2, $3, $4) RETURNING id, name, region, leader_id, is_active, created_at, updated_at`
      : `INSERT INTO offices (name, region, is_active) VALUES ($1, $2, $3) RETURNING id, name, region, leader_id, is_active, created_at, updated_at`
    
    const params = leaderIdToUse ? [normalizedName, region, leaderIdToUse, true] : [normalizedName, region, true]
    const officeResult = await sql.query(insertSql, params)

    const office = officeResult.rows[0]

    // Fetch office with stats
    const statsResult = await sql.query(`
      SELECT 
        o.id, o.name, o.is_active, o.region, o.leader_id, o.created_at, o.updated_at,
        u.name as leader_name,
        (
          SELECT COUNT(*) FROM users u2 
          WHERE o.name = ANY(u2.sales_office) 
            AND u2.is_active = true
            AND u2.role IN ('office_leader','area_director','divisional','regional','super_admin')
        ) as manager_count,
        0 as project_count
      FROM offices o
      LEFT JOIN users u ON o.leader_id = u.id
      WHERE o.id = $1
    `, [office.id])

    const officeWithStats = statsResult.rows[0]
    const result = {
      id: officeWithStats.id,
      name: officeWithStats.name,
      is_active: officeWithStats.is_active,
      manager_count: Number(officeWithStats.manager_count) || 0,
      project_count: Number(officeWithStats.project_count) || 0,
      region: officeWithStats.region,
      leaderId: officeWithStats.leader_id,
      leaderName: officeWithStats.leader_name || 'Unassigned',
      createdAt: officeWithStats.created_at,
      updatedAt: officeWithStats.updated_at,
    }

    // Log audit event for creation
    const changes: Record<string, { old: any; new: any }> = {
      name: { old: null, new: normalizedName },
      region: { old: null, new: region },
      leaderId: { old: null, new: leaderIdToUse }
    }
    await logAudit(
      'create',
      'office',
      office.id,
      auth.session.user.id,
      changes
    )

    logInfo('Office created', { officeId: office.id, name: normalizedName, leaderId: leaderIdToUse })

    return NextResponse.json(result)
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

    logError('Failed to create office', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to create office' },
      { status: 500 }
    )
  }
}
