export const runtime = 'nodejs'
import { NextRequest, NextResponse } from 'next/server'
import { sql } from '@/lib/db/client'
import { logError, logInfo } from '@/lib/logging/logger'
import { z } from 'zod'

const auditLogSchema = z.object({
  action: z.string(),
  resource: z.string(),
  resourceId: z.string(),
  userId: z.string(),
  changes: z.record(z.object({ old: z.any(), new: z.any() })).optional(),
  ipAddress: z.string().optional(),
  userAgent: z.string().optional(),
})

export async function POST(req: NextRequest) {
  const secret = req.headers.get('x-internal-secret') || ''
  const clientIP = req.headers.get('x-forwarded-for') || req.headers.get('x-real-ip') || 'unknown'
  const userAgent = req.headers.get('user-agent') || 'unknown'
  
  if (!process.env.INTERNAL_API_SECRET || secret !== process.env.INTERNAL_API_SECRET) {
    // Log unauthorized attempts for security monitoring
    logError('[AUDIT][SECURITY] Unauthorized audit API access attempt', new Error('Missing or invalid secret'), {
      clientIP,
      userAgent,
      hasSecret: !!secret,
      secretLength: secret.length
    })
    return NextResponse.json({ error: 'Forbidden' }, { status: 403 })
  }

  let body: unknown
  try {
    body = await req.json()
  } catch (error) {
    return NextResponse.json({ error: 'Invalid JSON' }, { status: 400 })
  }

  const parse = auditLogSchema.safeParse(body)
  if (!parse.success) {
    return NextResponse.json({ error: 'Validation failed', details: parse.error.flatten() }, { status: 400 })
  }

  const { action, resource, resourceId, userId, changes, ipAddress, userAgent } = parse.data

  try {
    const userResult = await sql`SELECT name FROM users WHERE id = ${userId}`
    const userName = userResult.rows[0]?.name || 'Unknown User'

    await sql`
      INSERT INTO audit_logs (
        user_id, user_name, action, resource, resource_id,
        changes, ip_address, user_agent
      ) VALUES (
        ${userId}, ${userName}, ${action}, ${resource}, ${resourceId},
        ${JSON.stringify(changes || {})}::jsonb, ${ipAddress || 'N/A'}, ${userAgent || 'N/A'}
      )
    `

    if (process.env.NODE_ENV === 'development') {
      logInfo('[AUDIT][API] Inserted audit log', { action, resource, resourceId, userId })
    }

    return NextResponse.json({ success: true })
  } catch (error) {
    logError('[AUDIT][API] Failed to insert audit log', error as Error, { action, resource, resourceId, userId })
    return NextResponse.json({ error: 'Internal Server Error' }, { status: 500 })
  }
}


