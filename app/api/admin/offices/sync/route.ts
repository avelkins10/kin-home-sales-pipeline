export const runtime = 'nodejs'
// Increase timeout to 5 minutes for large QuickBase queries
export const maxDuration = 300

import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { syncOfficesFromQuickBase } from '@/lib/db/offices'
import { logInfo, logError } from '@/lib/logging/logger'

/**
 * POST /api/admin/offices/sync
 * Sync offices from QuickBase projects table
 * Queries for unique SALES_OFFICE values and creates office records
 */
export async function POST(request: NextRequest) {
  try {
    // Require super_admin role
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }

    logInfo('Starting office sync from QuickBase', { userId: auth.session.user.id })

    // Trigger sync
    const results = await syncOfficesFromQuickBase()

    logInfo('Office sync completed', {
      userId: auth.session.user.id,
      created: results.created,
      skipped: results.skipped,
      errors: results.errors.length
    })

    return NextResponse.json({
      success: true,
      message: `Sync completed: ${results.created} created, ${results.skipped} skipped`,
      results: {
        created: results.created,
        skipped: results.skipped,
        errors: results.errors
      }
    })
  } catch (error) {
    logError('Failed to sync offices from QuickBase', error as Error)
    return NextResponse.json(
      {
        error: 'Failed to sync offices from QuickBase',
        message: error instanceof Error ? error.message : String(error)
      },
      { status: 500 }
    )
  }
}
