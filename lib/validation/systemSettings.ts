import { z } from 'zod'

export const systemSettingsSchema = z.object({
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


