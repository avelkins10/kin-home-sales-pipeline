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
  
  // Hierarchy and management fields
  managedBy?: string // ID of team lead who manages this user
  manages?: string[] // Array of user IDs this team lead manages
  officeAccess?: OfficeAccess[] // Complex office/region assignments for managers
  
  // QuickBase contact data (multiple emails/phones possible)
  quickbaseEmails?: string[]
  quickbasePhones?: string[]
  
  // Activity tracking for smart user provisioning
  lastProjectDate?: string // Date of user's most recent project
  invitedAt?: string // When user was invited
  inviteToken?: string // Token for invite link
  inviteAcceptedAt?: string // When user accepted invite
}

export interface OfficeAccess {
  officeName: string
  accessLevel: 'view' | 'manage' | 'admin'
  assignedAt: string
}

export interface UserHierarchy {
  id: string
  managerId: string
  userId: string
  createdAt: string
  updatedAt: string
}

// Database hierarchy representation (snake_case from SQL query joins)
export interface Hierarchy {
  id: string
  manager_id: string
  user_id: string
  created_at: string
  updated_at: string
  manager_name: string
  manager_email: string
  manager_role: string
  user_name: string
  user_email: string
  user_role: string
  user_is_active: boolean
}

export interface QuickBaseUserData {
  quickbaseUserId: string
  name: string
  email?: string
  phone?: string
  role: 'closer' | 'setter'
  office?: string
  offices?: string[] // Multiple offices for users with projects across offices
  lastProjectDate?: string
  projectCount?: number
  activeProjectCount?: number // Projects in last 6 months
}

export interface InviteUserInput {
  email: string
  name: string
  role: string
  office?: string
  offices?: string[] // For area directors/divisionals
  sendEmail: boolean
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
  offices?: string[]; // Array of office names for multi-select
  region?: string;
  temporaryPassword: string;
  
  // Hierarchy fields
  managedBy?: string; // ID of team lead who manages this user
  manages?: string[]; // Array of user IDs this team lead manages
  
  // Office access for managers
  officeAccess?: Array<{
    officeName: string;
    accessLevel: 'view' | 'manage' | 'admin';
  }>;
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
