export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'

const createOfficeSchema = z.object({
  name: z.string().min(1, 'Office name is required').max(100),
  region: z.enum(['southwest', 'southeast', 'midwest', 'northeast', 'west']),
  leaderId: z.string().min(1, 'Office leader is required'),
})

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
        o.id, o.name, o.region, o.leader_id, o.created_at, o.updated_at,
        u.name as leader_name,
        (
          SELECT COUNT(*) 
          FROM users u2 
          WHERE o.name = ANY(u2.sales_office) AND u2.is_active = true
        ) as user_count,
        0 as active_projects
      FROM offices o
      LEFT JOIN users u ON o.leader_id = u.id
    `

    const params: any[] = []

    query += ` ORDER BY o.name ASC`

    const result = await sql.query(query, params)
    const offices = result.rows.map(row => ({
      id: row.id,
      name: row.name,
      region: row.region,
      leaderId: row.leader_id,
      leaderName: row.leader_name || 'Unassigned',
      userCount: parseInt(row.user_count) || 0,
      activeProjects: parseInt(row.active_projects) || 0,
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

    // Check name uniqueness
    const existingOffice = await sql.query('SELECT id FROM offices WHERE name = $1', [name])
    if (existingOffice.rows.length > 0) {
      return NextResponse.json(
        { error: 'Office name already exists' },
        { status: 400 }
      )
    }

    // Verify leader exists and has appropriate role
    const leaderResult = await sql.query('SELECT id, role FROM users WHERE id = $1', [leaderId])
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

    // Insert office
    const officeResult = await sql.query(`
      INSERT INTO offices (name, region, leader_id)
      VALUES ($1, $2, $3)
      RETURNING id, name, region, leader_id, created_at, updated_at
    `, [name, region, leaderId])

    const office = officeResult.rows[0]

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
    const result = {
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

    // Log audit event for creation
    const changes: Record<string, { old: any; new: any }> = {
      name: { old: null, new: name },
      region: { old: null, new: region },
      leaderId: { old: null, new: leaderId }
    }
    await logAudit(
      'create',
      'office',
      office.id,
      auth.session.user.id,
      changes
    )

    logInfo('Office created', { officeId: office.id, name, leaderId })

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
