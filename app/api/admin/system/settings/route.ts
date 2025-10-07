export const runtime = 'nodejs'

import { NextRequest, NextResponse } from 'next/server'
import { requireRole } from '@/lib/auth/guards'
import { sql } from '@/lib/db/client'
import { logInfo, logError, logAudit } from '@/lib/logging/logger'
import { z } from 'zod'
import type { SystemSettings } from '@/lib/types/settings'

const systemSettingsSchema = z.object({
  quickbaseRealm: z.string().min(1).optional(),
  quickbaseToken: z.string().min(1).optional(),
  milestoneSLA: z.object({
    survey: z.number().int().min(1).max(60),
    design: z.number().int().min(1).max(60),
    permit: z.number().int().min(1).max(90),
    nem: z.number().int().min(1).max(60),
    install: z.number().int().min(1).max(30),
    inspection: z.number().int().min(1).max(30),
    pto: z.number().int().min(1).max(60),
  }).optional(),
  warningThreshold: z.number().int().min(50).max(100).optional(),
  criticalThreshold: z.number().int().min(50).max(150).optional(),
  holdReasons: z.array(z.string()).optional(),
  dateFormat: z.enum(['MM/DD/YYYY', 'DD/MM/YYYY', 'YYYY-MM-DD']).optional(),
  timezone: z.string().optional(),
  sessionTimeout: z.number().int().min(15).max(480).optional(),
})

// Extended type for API responses that includes the hasQuickbaseToken flag
type SystemSettingsResponse = SystemSettings & {
  hasQuickbaseToken: boolean
}

export async function GET(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) return auth.response

    const result = await sql`SELECT settings FROM system_settings WHERE id = 1`
    let settings: SystemSettings

    if (result.rows.length === 0) {
      settings = {
        quickbaseRealm: 'kin.quickbase.com',
        quickbaseToken: '',
        milestoneSLA: {
          survey: 7,
          design: 10,
          permit: 21,
          nem: 14,
          install: 7,
          inspection: 5,
          pto: 10,
        },
        warningThreshold: 75,
        criticalThreshold: 100,
        holdReasons: ['Finance Hold', 'Roof Hold', 'Customer Hold', 'Permit Hold', 'HOA Hold'],
        dateFormat: 'MM/DD/YYYY',
        timezone: 'America/Phoenix',
        sessionTimeout: 60,
      }
    } else {
      settings = result.rows[0].settings as SystemSettings
    }

    const hasQuickbaseToken = Boolean(settings.quickbaseToken && settings.quickbaseToken.length > 0)
    const response: SystemSettingsResponse = { 
      ...settings, 
      quickbaseToken: '', // Always return empty token to prevent masking issues
      hasQuickbaseToken 
    }

    logInfo('System settings fetched', { userId: auth.session.user.id })
    return NextResponse.json(response)
  } catch (error: any) {
    logError('Failed to fetch system settings', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ error: 'Failed to fetch system settings' }, { status: 500 })
  }
}

export async function PUT(request: NextRequest) {
  try {
    const auth = await requireRole(['super_admin'])
    if (!auth.authorized) return auth.response

    const body = await request.json()
    const parsed = systemSettingsSchema.parse(body)

    const currentRes = await sql`SELECT settings FROM system_settings WHERE id = 1`
    const currentSettings: SystemSettings = currentRes.rows[0]?.settings || {
      quickbaseRealm: 'kin.quickbase.com',
      quickbaseToken: '',
      milestoneSLA: {
        survey: 7,
        design: 10,
        permit: 21,
        nem: 14,
        install: 7,
        inspection: 5,
        pto: 10,
      },
      warningThreshold: 75,
      criticalThreshold: 100,
      holdReasons: ['Finance Hold', 'Roof Hold', 'Customer Hold', 'Permit Hold', 'HOA Hold'],
      dateFormat: 'MM/DD/YYYY',
      timezone: 'America/Phoenix',
      sessionTimeout: 60,
    }

    const mergedSettings: SystemSettings = {
      ...currentSettings,
      ...parsed,
      milestoneSLA: parsed.milestoneSLA ? { ...currentSettings.milestoneSLA, ...parsed.milestoneSLA } : currentSettings.milestoneSLA,
    }

    const updateRes = await sql`
      UPDATE system_settings 
      SET settings = ${JSON.stringify(mergedSettings)}::jsonb, updated_at = NOW()
      WHERE id = 1
      RETURNING settings
    `

    if (updateRes.rows.length === 0) {
      await sql`
        INSERT INTO system_settings (id, settings)
        VALUES (1, ${JSON.stringify(mergedSettings)}::jsonb)
      `
    }

    // Log audit event with field-level changes
    const changes: Record<string, { old: any; new: any }> = {}
    Object.keys(parsed).forEach(key => {
      const typedKey = key as keyof typeof currentSettings
      if (currentSettings[typedKey] !== mergedSettings[typedKey]) {
        changes[key] = { old: currentSettings[typedKey], new: mergedSettings[typedKey] }
      }
    })
    logAudit('update', 'system_settings', '1', auth.session.user.id, changes)
    logInfo('System settings updated', { userId: auth.session.user.id, updatedFields: Object.keys(parsed) })

    const hasQuickbaseToken = Boolean(mergedSettings.quickbaseToken && mergedSettings.quickbaseToken.length > 0)
    const response: SystemSettingsResponse = { 
      ...mergedSettings, 
      quickbaseToken: '', // Always return empty token to prevent masking issues
      hasQuickbaseToken 
    }

    return NextResponse.json(response)
  } catch (error: any) {
    if (error instanceof z.ZodError) {
      return NextResponse.json({ error: 'Validation failed', message: error.errors[0]?.message }, { status: 400 })
    }
    logError('Failed to update system settings', error instanceof Error ? error : new Error(String(error)))
    return NextResponse.json({ error: 'Failed to update system settings' }, { status: 500 })
  }
}


