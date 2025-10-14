import { z } from 'zod'
import { roleEnum, regionEnum } from '@/lib/validation/constants'
import { CANONICAL_OFFICES, isValidOffice } from '@/lib/constants/offices'

export const createUserSchema = z.object({
  name: z.string().min(1, 'Name is required').max(100),
  email: z.string().email('Invalid email address'),
  phone: z.string().optional(),
  role: roleEnum,
  quickbaseUserId: z.string().optional(),
  office: z.string().optional(), // Allow any office name from QuickBase
  region: z.string().optional(),
  temporaryPassword: z.string().min(8, 'Password must be at least 8 characters'),
  // Hierarchy and management fields
  managedBy: z.string().uuid().optional(),
  manages: z.array(z.string().uuid()).optional(),
  officeAccess: z.array(z.object({
    officeName: z.string(), // Allow any office name from QuickBase
    accessLevel: z.enum(['view', 'manage', 'admin']),
  })).optional(),
})

export const updateUserSchema = z.object({
  name: z.string().min(1).max(100).optional(),
  email: z.string().email().optional(),
  phone: z.string().optional(),
  role: roleEnum.optional(),
  office: z.string().optional(), // Allow any office name from QuickBase
  region: z.string().optional(),
  isActive: z.boolean().optional(),
  // Hierarchy and management fields
  managedBy: z.string().uuid().nullable().optional(),
  manages: z.array(z.string().uuid()).optional(),
  officeAccess: z.array(z.object({
    officeName: z.string(), // Allow any office name from QuickBase
    accessLevel: z.enum(['view', 'manage', 'admin']),
  })).optional(),
})

export const inviteUserSchema = z.object({
  email: z.string().email('Invalid email address'),
  name: z.string().min(2, 'Name must be at least 2 characters').max(100),
  role: roleEnum,
  office: z.string().optional(), // Allow any office name from QuickBase
  offices: z.array(z.string()).optional(), // For area directors/divisionals - allow any office names
  sendEmail: z.boolean(),
})

export const assignTeamLeadSchema = z.object({
  managerId: z.string().uuid('Invalid manager ID'),
  userIds: z.array(z.string().uuid('Invalid user ID')).min(1, 'At least one user is required').max(50, 'Maximum 50 users allowed'),
}).refine((data) => !data.userIds.includes(data.managerId), {
  message: 'Manager cannot manage themselves',
  path: ['userIds'],
})

export const officeAccessSchema = z.object({
  userId: z.string().uuid('Invalid user ID'),
  officeNames: z.array(z.string()).min(1, 'At least one office is required'), // Allow any office names from QuickBase
  accessLevel: z.enum(['view', 'manage', 'admin']),
})

export const quickbaseLookupSchema = z.object({
  searchTerm: z.string().min(2, 'Search term must be at least 2 characters').max(100),
  searchType: z.enum(['name', 'email', 'quickbaseId']),
})

export const createOfficeSchema = z.object({
  name: z.string().min(1, 'Office name is required').max(100), // Allow any office name from QuickBase
  region: regionEnum,
  leaderId: z.string().optional(),
})

export const updateOfficeSchema = z.object({
  name: z.string().min(1).max(100).optional(), // Allow any office name from QuickBase
  region: regionEnum.optional(),
  leaderId: z.string().optional(),
  isActive: z.boolean().optional(),
})

/**
 * Schema for bulk assigning offices to multiple managers
 * Used by the bulk office assignment API endpoint
 */
export const bulkAssignOfficesSchema = z.object({
  userIds: z.array(z.string().uuid()).min(1, 'At least one user is required').max(50, 'Maximum 50 users allowed'),
  officeNames: z.array(z.string().min(1)).min(1, 'At least one office is required').max(100, 'Maximum 100 offices allowed'),
  accessLevel: z.enum(['view', 'manage', 'admin']).default('view')
})


