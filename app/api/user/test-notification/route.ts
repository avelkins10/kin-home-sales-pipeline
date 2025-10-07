import { NextRequest, NextResponse } from 'next/server'
import { requireAuth } from '@/lib/auth/guards'
import { logInfo, logError } from '@/lib/logging/logger'
import { sendMail } from '@/lib/utils/mailer'
import { rateLimit } from '@/lib/auth/rateLimit'

export async function POST(request: NextRequest) {
  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      return auth.response
    }

    const userId = auth.session.user.id
    const userEmail = auth.session.user.email

    const ip = request.headers.get('x-forwarded-for') || ''
    if (!rateLimit(['test-notification', ip, userId], 30, 60_000)) {
      return NextResponse.json({ error: 'Too Many Requests' }, { status: 429 })
    }

    logInfo('Test notification requested', { userId, email: userEmail })
    if (process.env.EMAIL_ENABLED === 'true') {
      await sendMail({
        to: userEmail,
        subject: 'Test Notification - Kin Solar Pipeline',
        html: '<p>This is a test notification from your Kin Solar Pipeline dashboard.</p>'
      })
    }

    return NextResponse.json({ 
      success: true, 
      message: `Test notification sent to ${userEmail}` 
    })
  } catch (error) {
    logError('Error sending test notification', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ error: 'Internal server error' }, { status: 500 })
  }
}
