export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import milestonesConfig from '@/lib/config/milestones.json'

/**
 * GET /api/admin/milestones/config
 *
 * Returns milestone configuration - DB overrides first, fallback to JSON defaults
 */
export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    // Fetch custom configurations from database
    const result = await sql.query(`
      SELECT milestone_id, configuration, is_active, updated_at, version
      FROM milestone_configurations
      WHERE is_active = true
      ORDER BY milestone_id
    `)

    const customConfigs = result.rows.reduce((acc, row) => {
      acc[row.milestone_id] = {
        ...row.configuration,
        _metadata: {
          isCustom: true,
          updatedAt: row.updated_at,
          version: row.version
        }
      }
      return acc
    }, {} as Record<string, any>)

    // Merge with defaults from JSON
    const mergedConfig = {
      ...milestonesConfig,
      milestones: milestonesConfig.milestones.map(milestone => {
        if (customConfigs[milestone.id]) {
          return {
            ...milestone,
            ...customConfigs[milestone.id]
          }
        }
        return {
          ...milestone,
          _metadata: {
            isCustom: false
          }
        }
      })
    }

    logInfo('Fetched milestone configurations', {
      customCount: Object.keys(customConfigs).length,
      totalCount: milestonesConfig.milestones.length
    })

    return NextResponse.json(mergedConfig)
  } catch (error) {
    logError('Failed to fetch milestone configurations', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to fetch milestone configurations' },
      { status: 500 }
    )
  }
}

/**
 * PATCH /api/admin/milestones/config
 *
 * Updates a specific milestone configuration
 */
export async function PATCH(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const { milestoneId, configuration, notes } = body

    if (!milestoneId || !configuration) {
      return NextResponse.json(
        { error: 'milestoneId and configuration are required' },
        { status: 400 }
      )
    }

    // Validate milestoneId exists in default config
    const validMilestone = milestonesConfig.milestones.find(m => m.id === milestoneId)
    if (!validMilestone) {
      return NextResponse.json(
        { error: 'Invalid milestone ID' },
        { status: 400 }
      )
    }

    // Check if configuration already exists
    const existing = await sql.query(
      'SELECT id FROM milestone_configurations WHERE milestone_id = $1',
      [milestoneId]
    )

    if (existing.rows.length > 0) {
      // Update existing configuration
      await sql.query(`
        UPDATE milestone_configurations
        SET configuration = $1,
            updated_by = $2,
            notes = $3
        WHERE milestone_id = $4
      `, [JSON.stringify(configuration), auth.session.user.id, notes || null, milestoneId])

      logInfo('Updated milestone configuration', { milestoneId })
    } else {
      // Insert new configuration
      await sql.query(`
        INSERT INTO milestone_configurations
          (milestone_id, configuration, created_by, updated_by, notes)
        VALUES ($1, $2, $3, $3, $4)
      `, [milestoneId, JSON.stringify(configuration), auth.session.user.id, notes || null])

      logInfo('Created milestone configuration', { milestoneId })
    }

    // Log audit event
    await logAudit(
      existing.rows.length > 0 ? 'update' : 'create',
      'milestone_configuration',
      milestoneId,
      auth.session.user.id,
      {
        milestoneId: { old: null, new: milestoneId },
        configuration: { old: null, new: configuration }
      }
    )

    return NextResponse.json({ success: true, milestoneId })
  } catch (error) {
    logError('Failed to update milestone configuration', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to update milestone configuration' },
      { status: 500 }
    )
  }
}

/**
 * POST /api/admin/milestones/config/reset
 *
 * Resets a milestone configuration to default (deletes custom override)
 */
export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    const body = await request.json()
    const { milestoneId } = body

    if (!milestoneId) {
      return NextResponse.json(
        { error: 'milestoneId is required' },
        { status: 400 }
      )
    }

    // Delete custom configuration (will fallback to JSON default)
    const result = await sql.query(
      'DELETE FROM milestone_configurations WHERE milestone_id = $1 RETURNING id',
      [milestoneId]
    )

    if (result.rows.length === 0) {
      return NextResponse.json(
        { error: 'No custom configuration found for this milestone' },
        { status: 404 }
      )
    }

    // Log audit event
    await logAudit(
      'delete',
      'milestone_configuration',
      milestoneId,
      auth.session.user.id,
      {
        action: { old: 'custom', new: 'default' }
      }
    )

    logInfo('Reset milestone configuration to default', { milestoneId })

    return NextResponse.json({ success: true, milestoneId })
  } catch (error) {
    logError('Failed to reset milestone configuration', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json(
      { error: 'Failed to reset milestone configuration' },
      { status: 500 }
    )
  }
}
