export const runtime = 'nodejs'
export const dynamic = 'force-dynamic';
import { NextRequest, NextResponse } from 'next/server'
import { requireAuth } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError } from '@/lib/logging/logger'
import { z } from 'zod'

const notificationSettingsSchema = z.object({
  emailEnabled: z.boolean(),
  urgentAlerts: z.boolean(),
  dailyDigest: z.boolean(),
  weeklySummary: z.boolean(),
  holdThreshold: z.number().int().min(1).max(30),
  ageWarningThreshold: z.number().int().min(30).max(180),
  installOverdueThreshold: z.number().int().min(7).max(60),
})

export async function GET(request: NextRequest) {
  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      return auth.response
    }

    const userId = auth.session.user.id

    const settingsResult = await sql`
      SELECT * FROM notification_settings
      WHERE user_id = ${userId}
    `
    const settings = settingsResult.rows[0]

    if (!settings) {
      // Return default settings if none exist
      const defaultSettings = {
        userId,
        emailEnabled: true,
        urgentAlerts: true,
        dailyDigest: false,
        weeklySummary: false,
        holdThreshold: 7,
        ageWarningThreshold: 90,
        installOverdueThreshold: 14
      }
      
      logInfo('Notification settings fetched (defaults)', { userId })
      return NextResponse.json(defaultSettings)
    }

    logInfo('Notification settings fetched', { userId })

    return NextResponse.json({
      userId: settings.user_id,
      emailEnabled: settings.email_enabled,
      urgentAlerts: settings.urgent_alerts,
      dailyDigest: settings.daily_digest,
      weeklySummary: settings.weekly_summary,
      holdThreshold: settings.hold_threshold,
      ageWarningThreshold: settings.age_warning_threshold,
      installOverdueThreshold: settings.install_overdue_threshold
    })
  } catch (error) {
    logError('Error fetching notification settings', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ error: 'Internal server error' }, { status: 500 })
  }
}

export async function PUT(request: NextRequest) {
  try {
    const auth = await requireAuth()
    if (!auth.authorized) {
      return auth.response
    }

    const userId = auth.session.user.id
    const body = await request.json()

    // Validate input
    const validatedData = notificationSettingsSchema.parse(body)
    const {
      emailEnabled,
      urgentAlerts,
      dailyDigest,
      weeklySummary,
      holdThreshold,
      ageWarningThreshold,
      installOverdueThreshold
    } = validatedData

    // Upsert notification settings
    const settingsResult = await sql`
      INSERT INTO notification_settings (
        user_id, email_enabled, urgent_alerts, daily_digest, weekly_summary,
        hold_threshold, age_warning_threshold, install_overdue_threshold, updated_at
      ) VALUES (
        ${userId}, ${emailEnabled}, ${urgentAlerts}, ${dailyDigest}, ${weeklySummary},
        ${holdThreshold}, ${ageWarningThreshold}, ${installOverdueThreshold}, NOW()
      )
      ON CONFLICT (user_id) DO UPDATE SET
        email_enabled = EXCLUDED.email_enabled,
        urgent_alerts = EXCLUDED.urgent_alerts,
        daily_digest = EXCLUDED.daily_digest,
        weekly_summary = EXCLUDED.weekly_summary,
        hold_threshold = EXCLUDED.hold_threshold,
        age_warning_threshold = EXCLUDED.age_warning_threshold,
        install_overdue_threshold = EXCLUDED.install_overdue_threshold,
        updated_at = NOW()
      RETURNING *
    `
    const settings = settingsResult.rows[0]

    logInfo('Notification settings updated', {
      userId,
      changes: Object.keys(body)
    })

    return NextResponse.json({
      userId: settings.user_id,
      emailEnabled: settings.email_enabled,
      urgentAlerts: settings.urgent_alerts,
      dailyDigest: settings.daily_digest,
      weeklySummary: settings.weekly_summary,
      holdThreshold: settings.hold_threshold,
      ageWarningThreshold: settings.age_warning_threshold,
      installOverdueThreshold: settings.install_overdue_threshold
    })
  } catch (error) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ 
        error: 'Validation failed',
        details: error.errors
      }, { status: 400 })
    }

    logError('Error updating notification settings', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ error: 'Internal server error' }, { status: 500 })
  }
}
