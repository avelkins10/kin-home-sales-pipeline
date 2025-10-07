import { UserRole } from './project'

export interface User {
  id: string
  email: string
  name: string
  phone?: string
  role: UserRole
  quickbaseUserId: string
  office?: string
  salesOffice?: string[]
  region?: string
  isActive: boolean
  lastLoginAt?: string
  createdAt: string
  updatedAt: string
}

export interface NotificationSettings {
  userId: string
  emailEnabled: boolean
  urgentAlerts: boolean
  dailyDigest: boolean
  weeklySummary: boolean
  holdThreshold: number
  ageWarningThreshold: number
  installOverdueThreshold: number
}

export interface UpdateProfileInput {
  name: string
  email: string
  phone: string
}

export interface ChangePasswordInput {
  currentPassword: string
  newPassword: string
}

export interface CreateUserInput {
  name: string;
  email: string;
  phone?: string;
  role: string;
  quickbaseUserId: string;
  office?: string;
  region?: string;
  temporaryPassword: string;
}

export interface UpdateUserInput {
  name?: string;
  email?: string;
  phone?: string;
  role?: string;
  office?: string;
  region?: string;
  isActive?: boolean;
}
