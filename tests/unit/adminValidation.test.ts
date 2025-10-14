import { describe, it, expect } from 'vitest'
import { createUserSchema, updateUserSchema, createOfficeSchema, updateOfficeSchema } from '@/lib/validation/admin'

describe('Admin API Validation', () => {
  describe('createUserSchema', () => {
    it('accepts valid user data', () => {
      const input = {
        name: 'John Doe',
        email: 'john@kinhome.com',
        phone: '555-1234',
        role: 'closer',
        quickbaseUserId: 'qb-123',
        office: 'Phoenix',
        temporaryPassword: 'temppass123',
      }
      expect(() => createUserSchema.parse(input)).not.toThrow()
    })

    it('rejects empty name', () => {
      const input = {
        name: '',
        email: 'john@kinhome.com',
        role: 'closer',
        quickbaseUserId: 'qb-123',
        temporaryPassword: 'temppass123',
      }
      expect(() => createUserSchema.parse(input)).toThrow()
    })

    it('rejects invalid email', () => {
      const input = {
        name: 'John',
        email: 'invalid-email',
        role: 'closer',
        quickbaseUserId: 'qb-123',
        temporaryPassword: 'temppass123',
      }
      expect(() => createUserSchema.parse(input)).toThrow()
    })

    it('rejects invalid role', () => {
      const input: any = {
        name: 'John',
        email: 'john@kinhome.com',
        role: 'invalid_role',
        quickbaseUserId: 'qb-123',
        temporaryPassword: 'temppass123',
      }
      expect(() => createUserSchema.parse(input)).toThrow()
    })

    it('rejects short password', () => {
      const input = {
        name: 'John',
        email: 'john@kinhome.com',
        role: 'closer',
        quickbaseUserId: 'qb-123',
        temporaryPassword: 'short',
      }
      expect(() => createUserSchema.parse(input)).toThrow()
    })

    it('accepts missing phone', () => {
      const input = {
        name: 'John',
        email: 'john@kinhome.com',
        role: 'closer',
        quickbaseUserId: 'qb-123',
        temporaryPassword: 'temppass123',
      }
      expect(() => createUserSchema.parse(input)).not.toThrow()
    })

    it('accepts missing office', () => {
      const input = {
        name: 'John',
        email: 'john@kinhome.com',
        role: 'closer',
        quickbaseUserId: 'qb-123',
        temporaryPassword: 'temppass123',
      }
      expect(() => createUserSchema.parse(input)).not.toThrow()
    })
  })

  describe('updateUserSchema', () => {
    it('accepts partial updates', () => {
      const input = { name: 'New Name' }
      expect(() => updateUserSchema.parse(input)).not.toThrow()
    })

    it('accepts empty object', () => {
      const input = {}
      expect(() => updateUserSchema.parse(input)).not.toThrow()
    })

    it('rejects invalid email in update', () => {
      const input = { email: 'invalid' }
      expect(() => updateUserSchema.parse(input)).toThrow()
    })

    it('accepts isActive boolean', () => {
      const input = { isActive: false }
      expect(() => updateUserSchema.parse(input)).not.toThrow()
    })

    it('rejects non-boolean isActive', () => {
      const input: any = { isActive: 'false' }
      expect(() => updateUserSchema.parse(input)).toThrow()
    })
  })

  describe('createOfficeSchema', () => {
    it('accepts valid office data', () => {
      const input = { name: 'Phoenix', region: 'southwest', leaderId: 'user-123' }
      expect(() => createOfficeSchema.parse(input)).not.toThrow()
    })

    it('rejects empty name', () => {
      const input = { name: '', region: 'southwest', leaderId: 'user-123' }
      expect(() => createOfficeSchema.parse(input)).toThrow()
    })

    it('rejects invalid region', () => {
      const input: any = { name: 'Phoenix', region: 'invalid-region', leaderId: 'user-123' }
      expect(() => createOfficeSchema.parse(input)).toThrow()
    })

    it('accepts all valid regions', () => {
      const regions = ['southwest', 'southeast', 'midwest', 'northeast', 'west']
      for (const region of regions) {
        const input = { name: 'Phoenix', region, leaderId: 'user-123' }
        expect(() => createOfficeSchema.parse(input)).not.toThrow()
      }
    })

    it('accepts optional leaderId', () => {
      // leaderId is now optional in the schema
      const input = { name: 'Phoenix', region: 'southwest' }
      expect(() => createOfficeSchema.parse(input)).not.toThrow()
    })

    it('rejects invalid office name', () => {
      const input = { name: 'Invalid Office', region: 'southwest', leaderId: 'user-123' }
      expect(() => createOfficeSchema.parse(input)).toThrow()
    })
  })

  describe('updateOfficeSchema', () => {
    it('accepts partial updates with valid office name', () => {
      const input = { name: 'Dallas' }
      expect(() => updateOfficeSchema.parse(input)).not.toThrow()
    })

    it('accepts empty object', () => {
      const input = {}
      expect(() => updateOfficeSchema.parse(input)).not.toThrow()
    })

    it('rejects invalid region in update', () => {
      const input: any = { region: 'invalid' }
      expect(() => updateOfficeSchema.parse(input)).toThrow()
    })

    it('rejects invalid office name in update', () => {
      const input = { name: 'Invalid Office Name' }
      expect(() => updateOfficeSchema.parse(input)).toThrow()
    })
  })
})
