export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logApiRequest, logApiResponse } from '@/lib/logging/logger'
import type { AuditLog } from '@/lib/types/audit'

export async function GET(request: NextRequest) {
  const startedAt = Date.now();
  const reqId = request.headers.get('x-request-id') || Math.random().toString(36).slice(2, 10);
  logApiRequest('GET', '/api/admin/audit-logs', undefined, reqId);

  try {
    // Authorization: Only super_admin can access audit logs
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) {
      logApiResponse('GET', '/api/admin/audit-logs', Date.now() - startedAt, { status: 403 }, reqId);
      return auth.response
    }
    const { session } = auth

    // Parse query parameters with input hardening
    const { searchParams } = new URL(request.url)
    const from = searchParams.get('from')
    const to = searchParams.get('to')
    const action = searchParams.get('action')
    const search = searchParams.get('search')?.trim().slice(0, 100) // Clamp to 100 chars and normalize whitespace
    const page = Math.max(1, parseInt(searchParams.get('page') || '1'))
    const limit = Math.min(Math.max(1, parseInt(searchParams.get('limit') || '50')), 100)

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

    // Validate pagination
    if (page < 1 || limit < 1) {
      return NextResponse.json(
        { error: 'Invalid pagination parameters' },
        { status: 400 }
      )
    }

    // Validate action parameter against allowed values
    const allowedActions = ['create', 'update', 'delete', 'login', 'logout', 'export', 'all']
    if (action && !allowedActions.includes(action)) {
      return NextResponse.json(
        { error: 'Invalid action parameter' },
        { status: 400 }
      )
    }

    // Build query with filters (Vercel Postgres doesn't support .unsafe())
    const offset = (page - 1) * limit
    const searchPattern = search ? `%${search}%` : null

    // Execute main query using conditional logic
    const logsResult = action && action !== 'all' && search
      ? await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND al.action = ${action}
            AND (al.user_name ILIKE ${searchPattern} OR al.resource ILIKE ${searchPattern} OR al.ip_address ILIKE ${searchPattern})
          ORDER BY al.timestamp DESC
          LIMIT ${limit} OFFSET ${offset}
        `
      : action && action !== 'all'
      ? await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND al.action = ${action}
          ORDER BY al.timestamp DESC
          LIMIT ${limit} OFFSET ${offset}
        `
      : search
      ? await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND (al.user_name ILIKE ${searchPattern} OR al.resource ILIKE ${searchPattern} OR al.ip_address ILIKE ${searchPattern})
          ORDER BY al.timestamp DESC
          LIMIT ${limit} OFFSET ${offset}
        `
      : await sql`
          SELECT al.id, al.timestamp, al.user_id, al.user_name, al.action,
                 al.resource, al.resource_id, al.changes, al.ip_address, al.user_agent
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
          ORDER BY al.timestamp DESC
          LIMIT ${limit} OFFSET ${offset}
        `
    const logs: AuditLog[] = logsResult.rows.map((row: any) => ({
      id: row.id,
      timestamp: row.timestamp.toISOString(),
      userId: row.user_id,
      userName: row.user_name,
      action: row.action,
      resource: row.resource,
      resourceId: row.resource_id,
      changes: row.changes || {},
      ipAddress: row.ip_address || 'N/A',
      userAgent: row.user_agent || 'N/A',
    }))

    // Get total count for pagination
    const countResult = action && action !== 'all' && search
      ? await sql`
          SELECT COUNT(*) as total
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND al.action = ${action}
            AND (al.user_name ILIKE ${searchPattern} OR al.resource ILIKE ${searchPattern} OR al.ip_address ILIKE ${searchPattern})
        `
      : action && action !== 'all'
      ? await sql`
          SELECT COUNT(*) as total
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND al.action = ${action}
        `
      : search
      ? await sql`
          SELECT COUNT(*) as total
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
            AND (al.user_name ILIKE ${searchPattern} OR al.resource ILIKE ${searchPattern} OR al.ip_address ILIKE ${searchPattern})
        `
      : await sql`
          SELECT COUNT(*) as total
          FROM audit_logs al
          WHERE al.timestamp >= ${from} AND al.timestamp <= ${to}
        `
    const total = parseInt(countResult.rows[0].total)
    const pages = Math.ceil(total / limit)

    // Log the request
    logInfo('Audit logs fetched', {
      userId: session.user.id,
      count: logs.length,
      filters: { from, to, action, search, page, limit },
    })

    logApiResponse('GET', '/api/admin/audit-logs', Date.now() - startedAt, { count: logs.length }, reqId);
    return NextResponse.json({
      logs,
      total,
      pages,
    })
  } catch (error) {
    logError('Failed to fetch audit logs', error as Error, { url: request.url })
    logApiResponse('GET', '/api/admin/audit-logs', Date.now() - startedAt, { error: true }, reqId);
    return NextResponse.json(
      { error: 'Failed to fetch audit logs' },
      { status: 500 }
    )
  }
}