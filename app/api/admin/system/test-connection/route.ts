export const runtime = 'nodejs'

import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { logInfo, logError } from '@/lib/logging/logger'
import { z } from 'zod'

const testConnectionSchema = z.object({
  realm: z.string().min(1, 'Realm is required'),
  token: z.string().min(1, 'Token is required'),
})

export async function POST(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) return auth.response

    const body = await request.json()
    const parsed = testConnectionSchema.parse(body)
    const { realm, token } = parsed

    const controller = new AbortController()
    const timeout = setTimeout(() => controller.abort(), 5000)

    try {
      const res = await fetch('https://api.quickbase.com/v1/records/query', {
        method: 'POST',
        headers: {
          'QB-Realm-Hostname': realm,
          'Authorization': `QB-USER-TOKEN ${token}`,
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ from: process.env.QUICKBASE_TABLE_PROJECTS, select: [3], options: { top: 1 } }),
        signal: controller.signal,
      })
      clearTimeout(timeout)

      if (res.ok) {
        logInfo('Quickbase connection test', { userId: auth.session.user.id, realm, success: true })
        return NextResponse.json({ success: true, message: 'Connection successful' })
      }

      if (res.status === 401) {
        return NextResponse.json({ success: false, message: 'Invalid token or realm' })
      }
      if (res.status === 403) {
        return NextResponse.json({ success: false, message: 'Token lacks permissions for table' })
      }

      const text = await res.text().catch(() => '')
      return NextResponse.json({ success: false, message: `Connection failed: ${text || res.statusText}` })
    } catch (err: any) {
      const msg = err?.name === 'AbortError' ? 'Connection timeout or network error' : 'Connection timeout or network error'
      logError('Quickbase connection test failed', err, { realm })
      return NextResponse.json({ success: false, message: msg })
    }
  } catch (error: any) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ error: 'Validation failed', message: error.errors[0]?.message }, { status: 400 })
    }
    logError('Quickbase connection test endpoint error', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ success: false, message: 'Connection failed' })
  }
}


