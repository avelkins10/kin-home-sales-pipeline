import { describe, it, expect } from 'vitest'
import { z } from 'zod'

// Validation schemas (copied from API routes for testing)
const updateProfileSchema = z.object({
  name: z.string().min(1, 'Name is required').max(100),
  email: z.string().email('Invalid email address'),
  phone: z.string().optional(),
})

const changePasswordSchema = z.object({
  currentPassword: z.string().min(1, 'Current password is required'),
  newPassword: z.string().min(8, 'Password must be at least 8 characters'),
})

const notificationSettingsSchema = z.object({
  emailEnabled: z.boolean(),
  urgentAlerts: z.boolean(),
  dailyDigest: z.boolean(),
  weeklySummary: z.boolean(),
  holdThreshold: z.number().int().min(1).max(30),
  ageWarningThreshold: z.number().int().min(30).max(180),
  installOverdueThreshold: z.number().int().min(7).max(60),
})

describe('Settings Validation', () => {
  describe('updateProfileSchema', () => {
    it('accepts valid profile data', () => {
      const validData = {
        name: 'John Smith',
        email: 'john@kinhome.com',
        phone: '555-1234'
      }
      
      const result = updateProfileSchema.safeParse(validData)
      expect(result.success).toBe(true)
    })

    it('rejects empty name', () => {
      const invalidData = {
        name: '',
        email: 'john@kinhome.com',
        phone: ''
      }
      
      const result = updateProfileSchema.safeParse(invalidData)
      expect(result.success).toBe(false)
      if (!result.success) {
        expect(result.error.errors[0].message).toBe('Name is required')
      }
    })

    it('rejects invalid email', () => {
      const invalidData = {
        name: 'John',
        email: 'invalid-email',
        phone: ''
      }
      
      const result = updateProfileSchema.safeParse(invalidData)
      expect(result.success).toBe(false)
      if (!result.success) {
        expect(result.error.errors[0].message).toBe('Invalid email address')
      }
    })

    it('accepts missing phone', () => {
      const validData = {
        name: 'John',
        email: 'john@kinhome.com'
      }
      
      const result = updateProfileSchema.safeParse(validData)
      expect(result.success).toBe(true)
    })

    it('rejects name longer than 100 chars', () => {
      const invalidData = {
        name: 'a'.repeat(101),
        email: 'john@kinhome.com',
        phone: ''
      }
      
      const result = updateProfileSchema.safeParse(invalidData)
      expect(result.success).toBe(false)
    })
  })

  describe('changePasswordSchema', () => {
    it('accepts valid password data', () => {
      const validData = {
        currentPassword: 'oldpass123',
        newPassword: 'newpass123'
      }
      
      const result = changePasswordSchema.safeParse(validData)
      expect(result.success).toBe(true)
    })

    it('rejects empty current password', () => {
      const invalidData = {
        currentPassword: '',
        newPassword: 'newpass123'
      }
      
      const result = changePasswordSchema.safeParse(invalidData)
      expect(result.success).toBe(false)
      if (!result.success) {
        expect(result.error.errors[0].message).toBe('Current password is required')
      }
    })

    it('rejects short new password', () => {
      const invalidData = {
        currentPassword: 'oldpass',
        newPassword: 'short'
      }
      
      const result = changePasswordSchema.safeParse(invalidData)
      expect(result.success).toBe(false)
      if (!result.success) {
        expect(result.error.errors[0].message).toBe('Password must be at least 8 characters')
      }
    })

    it('accepts exactly 8 character password', () => {
      const validData = {
        currentPassword: 'oldpass',
        newPassword: '12345678'
      }
      
      const result = changePasswordSchema.safeParse(validData)
      expect(result.success).toBe(true)
    })
  })

  describe('notificationSettingsSchema', () => {
    const validSettings = {
      emailEnabled: true,
      urgentAlerts: true,
      dailyDigest: false,
      weeklySummary: false,
      holdThreshold: 7,
      ageWarningThreshold: 90,
      installOverdueThreshold: 14
    }

    it('accepts valid notification settings', () => {
      const result = notificationSettingsSchema.safeParse(validSettings)
      expect(result.success).toBe(true)
    })

    it('rejects hold threshold < 1', () => {
      const invalidSettings = { ...validSettings, holdThreshold: 0 }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })

    it('rejects hold threshold > 30', () => {
      const invalidSettings = { ...validSettings, holdThreshold: 31 }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })

    it('rejects age warning < 30', () => {
      const invalidSettings = { ...validSettings, ageWarningThreshold: 29 }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })

    it('rejects age warning > 180', () => {
      const invalidSettings = { ...validSettings, ageWarningThreshold: 181 }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })

    it('rejects non-boolean email enabled', () => {
      const invalidSettings = { ...validSettings, emailEnabled: 'true' as any }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })

    it('rejects non-integer thresholds', () => {
      const invalidSettings = { ...validSettings, holdThreshold: 7.5 }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })

    it('rejects install overdue threshold < 7', () => {
      const invalidSettings = { ...validSettings, installOverdueThreshold: 6 }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })

    it('rejects install overdue threshold > 60', () => {
      const invalidSettings = { ...validSettings, installOverdueThreshold: 61 }
      const result = notificationSettingsSchema.safeParse(invalidSettings)
      expect(result.success).toBe(false)
    })
  })
})
