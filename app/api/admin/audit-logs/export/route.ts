export const runtime = 'nodejs'

import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import { format } from 'date-fns'

export async function GET(request: NextRequest) {
  try {
    // Authorization: Only super_admin can export audit logs
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      return auth.response
    }
    const { session } = auth

    // Parse query parameters
    const { searchParams } = new URL(request.url)
    const from = searchParams.get('from')
    const to = searchParams.get('to')
    const action = searchParams.get('action')
    const search = searchParams.get('search')

    // Validate required parameters
    if (!from || !to) {
      return NextResponse.json(
        { error: 'Missing required parameters: from and to dates are required' },
        { status: 400 }
      )
    }

    // Validate date range
    const fromDate = new Date(from)
    const toDate = new Date(to)
    if (fromDate > toDate) {
      return NextResponse.json(
        { error: 'Invalid date range: from date must be before to date' },
        { status: 400 }
      )
    }

    // Build query (same as GET /api/admin/audit-logs but without pagination)
    const searchPattern = search ? `%${search}%` : null

    // Execute query using conditional logic
    const logsResult = action && action !== 'all' && search
      ? await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND al.action = ${action}
            AND (al.user_name ILIKE ${searchPattern} OR al.resource ILIKE ${searchPattern} OR al.ip_address ILIKE ${searchPattern})
          ORDER BY al.timestamp DESC
        `
      : action && action !== 'all'
      ? await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND al.action = ${action}
          ORDER BY al.timestamp DESC
        `
      : search
      ? await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND (al.user_name ILIKE ${searchPattern} OR al.resource ILIKE ${searchPattern} OR al.ip_address ILIKE ${searchPattern})
          ORDER BY al.timestamp DESC
        `
      : await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
          ORDER BY al.timestamp DESC
        `
    const logs = logsResult.rows

    // Generate CSV
    const csvHeader = 'Timestamp,User,Action,Resource,Resource ID,Changes,IP Address,User Agent\n'
    
    const csvRows = logs.map((log: any) => {
      const timestamp = format(new Date(log.timestamp), 'yyyy-MM-dd HH:mm:ss')
      const userName = escapeCsvField(log.user_name || '')
      const action = escapeCsvField(log.action || '')
      const resource = escapeCsvField(log.resource || '')
      const resourceId = escapeCsvField(log.resource_id || '')
      const changes = escapeCsvField(JSON.stringify(log.changes || {}))
      const ipAddress = escapeCsvField(log.ip_address || 'N/A')
      const userAgent = escapeCsvField(log.user_agent || 'N/A')
      
      return `${timestamp},${userName},${action},${resource},${resourceId},${changes},${ipAddress},${userAgent}`
    })

    const csvContent = csvHeader + csvRows.join('\n')

    // Log the export event
    const changes: Record<string, { old: any; new: any }> = {
      count: { old: null, new: logs.length },
      filters: { old: null, new: { from, to, action, search } }
    }
    await logAudit('export', 'audit_logs', 'all', session.user.id, changes)

    logInfo('Audit logs exported', {
      userId: session.user.id,
      count: logs.length,
      filters: { from, to, action, search },
    })

    // Return CSV file
    const filename = `audit-logs-${format(new Date(), 'yyyy-MM-dd')}.csv`
    
    return new NextResponse(csvContent, {
      status: 200,
      headers: {
        'Content-Type': 'text/csv',
        'Content-Disposition': `attachment; filename="${filename}"`,
      },
    })
  } catch (error) {
    logError('Failed to export audit logs', error as Error, { url: request.url })
    return NextResponse.json(
      { error: 'Failed to export audit logs' },
      { status: 500 }
    )
  }
}

/**
 * Escape CSV field by wrapping in quotes and escaping internal quotes
 */
function escapeCsvField(field: string): string {
  // If field contains comma, quote, or newline, wrap in quotes and escape quotes
  if (field.includes(',') || field.includes('"') || field.includes('\n')) {
    return `"${field.replace(/"/g, '""')}"`
  }
  return field
}