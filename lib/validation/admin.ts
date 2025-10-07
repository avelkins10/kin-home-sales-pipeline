import { z } from 'zod'
import { roleEnum, regionEnum } from '@/lib/validation/constants'

export const createUserSchema = z.object({
  name: z.string().min(1, 'Name is required').max(100),
  email: z.string().email('Invalid email address'),
  phone: z.string().optional(),
  role: roleEnum,
  quickbaseUserId: z.string().min(1, 'Quickbase User ID is required'),
  office: z.string().optional(),
  region: z.string().optional(),
  temporaryPassword: z.string().min(8, 'Password must be at least 8 characters'),
})

export const updateUserSchema = z.object({
  name: z.string().min(1).max(100).optional(),
  email: z.string().email().optional(),
  phone: z.string().optional(),
  role: roleEnum.optional(),
  office: z.string().optional(),
  region: z.string().optional(),
  isActive: z.boolean().optional(),
})

export const createOfficeSchema = z.object({
  name: z.string().min(1, 'Office name is required').max(100),
  region: regionEnum,
  leaderId: z.string().min(1, 'Office leader is required'),
})

export const updateOfficeSchema = z.object({
  name: z.string().min(1).max(100).optional(),
  region: regionEnum.optional(),
  leaderId: z.string().optional(),
})


