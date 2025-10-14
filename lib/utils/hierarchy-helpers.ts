import { User, Hierarchy } from '@/lib/types/user';
import { UserRole } from '@/lib/types/project';
import { isManagerRole } from './role-helpers';

export interface ValidationResult {
  valid: boolean;
  error?: string;
  errors?: string[];
  warnings?: string[];
  circularUsers?: string[];
  invalidUsers?: string[];
}

export interface ManagerRoleConstraints {
  canManage: UserRole[];
  cannotManage: UserRole[];
}

/**
 * Validates if a manager has the appropriate role to manage users
 */
export function validateManagerRole(
  manager: User | undefined,
  users: User[]
): ValidationResult {
  if (!manager) {
    return { valid: false, error: 'Manager not found' };
  }

  if (!isManagerRole(manager.role)) {
    return { valid: false, error: `${manager.role} cannot manage users` };
  }

  // Check if manager can manage all selected users
  const invalidUsers: string[] = [];
  for (const user of users) {
    const roleValidation = validateRoleHierarchy(manager.role, user.role);
    if (!roleValidation.valid) {
      invalidUsers.push(user.name || user.email);
    }
  }

  if (invalidUsers.length > 0) {
    return {
      valid: false,
      error: `${manager.role} cannot manage: ${invalidUsers.join(', ')}`,
      invalidUsers
    };
  }

  return { valid: true };
}

/**
 * Validates if assignment would create a circular hierarchy
 */
export function validateCircularHierarchy(
  managerId: string,
  userIds: string[],
  hierarchies: Hierarchy[]
): ValidationResult {
  const circularUsers: string[] = [];

  for (const userId of userIds) {
    // Check if assigning this user would create a cycle
    if (wouldCreateCircularDependency(managerId, userId, hierarchies)) {
      circularUsers.push(userId);
    }
  }

  if (circularUsers.length > 0) {
    return {
      valid: false,
      error: `Cannot assign users - would create circular hierarchy`,
      circularUsers
    };
  }

  return { valid: true };
}

/**
 * Validates if a manager role can manage a user role
 */
export function validateRoleHierarchy(
  managerRole: UserRole,
  userRole: UserRole
): ValidationResult {
  const constraints = getManagerRoleConstraints(managerRole);
  
  if (constraints.cannotManage.includes(userRole)) {
    return {
      valid: false,
      error: `${managerRole} cannot manage ${userRole}`
    };
  }

  return { valid: true };
}

/**
 * Gets users who don't have a manager assigned
 */
export function getUnassignedUsers(
  users: User[],
  hierarchies: Hierarchy[]
): User[] {
  const managedUserIds = new Set(hierarchies.map(h => h.user_id));
  
  return users.filter(user => 
    !managedUserIds.has(user.id) && 
    !['super_admin', 'regional'].includes(user.role)
  );
}

/**
 * Gets the full hierarchy path from a user to the root
 */
export function getUserHierarchyPath(
  userId: string,
  hierarchies: Hierarchy[],
  users: User[]
): User[] {
  const path: User[] = [];
  const user = users.find(u => u.id === userId);
  
  if (!user) return path;
  
  path.push(user);
  
  let currentUserId = userId;
  const visited = new Set<string>();
  
  while (currentUserId && !visited.has(currentUserId)) {
    visited.add(currentUserId);
    
    const hierarchy = hierarchies.find(h => h.user_id === currentUserId);
    if (!hierarchy) break;
    
    const manager = users.find(u => u.id === hierarchy.manager_id);
    if (!manager) break;
    
    path.push(manager);
    currentUserId = hierarchy.manager_id;
  }
  
  return path;
}

/**
 * Gets all users managed by a given user (recursively)
 */
export function getDescendants(
  userId: string,
  hierarchies: Hierarchy[]
): string[] {
  const descendants: string[] = [];
  const directReports = hierarchies.filter(h => h.manager_id === userId);
  
  for (const report of directReports) {
    descendants.push(report.user_id);
    // Recursively get descendants of this user
    descendants.push(...getDescendants(report.user_id, hierarchies));
  }
  
  return descendants;
}

/**
 * Checks if a manager role can manage a user role
 */
export function canManageRole(managerRole: UserRole, userRole: UserRole): boolean {
  return validateRoleHierarchy(managerRole, userRole).valid;
}

/**
 * Gets role constraints for a manager role
 */
export function getManagerRoleConstraints(role: UserRole): ManagerRoleConstraints {
  const constraints: Record<UserRole, ManagerRoleConstraints> = {
    super_admin: {
      canManage: ['regional', 'divisional', 'area_director', 'office_leader', 'coordinator', 'team_lead', 'closer', 'setter'],
      cannotManage: []
    },
    regional: {
      canManage: ['divisional', 'area_director', 'office_leader', 'coordinator', 'team_lead', 'closer', 'setter'],
      cannotManage: ['super_admin']
    },
    divisional: {
      canManage: ['area_director', 'office_leader', 'coordinator', 'team_lead', 'closer', 'setter'],
      cannotManage: ['super_admin', 'regional']
    },
    area_director: {
      canManage: ['office_leader', 'coordinator', 'team_lead', 'closer', 'setter'],
      cannotManage: ['super_admin', 'regional', 'divisional']
    },
    office_leader: {
      canManage: ['coordinator', 'team_lead', 'closer', 'setter'],
      cannotManage: ['super_admin', 'regional', 'divisional', 'area_director']
    },
    coordinator: {
      canManage: [],
      cannotManage: ['super_admin', 'regional', 'divisional', 'area_director', 'office_leader', 'team_lead', 'closer', 'setter']
    },
    team_lead: {
      canManage: ['closer', 'setter'],
      cannotManage: ['super_admin', 'regional', 'divisional', 'area_director', 'office_leader', 'coordinator']
    },
    closer: {
      canManage: [],
      cannotManage: ['super_admin', 'regional', 'divisional', 'area_director', 'office_leader', 'coordinator', 'team_lead', 'setter']
    },
    setter: {
      canManage: [],
      cannotManage: ['super_admin', 'regional', 'divisional', 'area_director', 'office_leader', 'coordinator', 'team_lead', 'closer']
    }
  };
  
  return constraints[role] || { canManage: [], cannotManage: [] };
}

/**
 * Formats hierarchy error messages with context
 */
export function formatHierarchyError(
  errorType: 'circular' | 'role_mismatch' | 'already_managed' | 'self_assignment',
  context: {
    managerName?: string;
    userName?: string;
    managerRole?: UserRole;
    userRole?: UserRole;
    circularPath?: string[];
  }
): string {
  switch (errorType) {
    case 'circular':
      return `Cannot create circular hierarchy: ${context.circularPath?.join(' → ') || 'circular dependency detected'}`;
    
    case 'role_mismatch':
      return `${context.managerRole} cannot manage ${context.userRole}`;
    
    case 'already_managed':
      return `${context.userName} is already managed by someone else`;
    
    case 'self_assignment':
      return 'Cannot assign user to themselves';
    
    default:
      return 'Invalid hierarchy assignment';
  }
}

/**
 * Gets the total team size for a manager (direct + indirect reports)
 */
export function getTeamSize(managerId: string, hierarchies: Hierarchy[]): number {
  return getDescendants(managerId, hierarchies).length;
}

/**
 * Validates user assignments for bulk operations
 */
export function validateUserAssignments(
  userIds: string[],
  users: User[],
  hierarchies: Hierarchy[]
): ValidationResult {
  const errors: string[] = [];
  const warnings: string[] = [];
  
  for (const userId of userIds) {
    const user = users.find(u => u.id === userId);
    if (!user) {
      errors.push(`User ${userId} not found`);
      continue;
    }
    
    // Check if user is already managed
    const existingHierarchy = hierarchies.find(h => h.user_id === userId);
    if (existingHierarchy) {
      const manager = users.find(u => u.id === existingHierarchy.manager_id);
      errors.push(`${user.name || user.email} is already managed by ${manager?.name || 'someone else'}`);
    }
    
    // Check if user is super_admin (cannot be managed)
    if (user.role === 'super_admin') {
      errors.push(`${user.name || user.email} is a super admin and cannot be managed`);
    }
    
    // Check if user is inactive
    if (!user.isActive) {
      warnings.push(`${user.name || user.email} is inactive and may not be able to log in`);
    }
  }
  
  return {
    valid: errors.length === 0,
    errors: errors.length > 0 ? errors : undefined,
    warnings: warnings.length > 0 ? warnings : undefined
  };
}

/**
 * Validates bulk assignment size limits
 */
export function validateBulkAssignmentSize(userCount: number): ValidationResult {
  if (userCount > 50) {
    return {
      valid: false,
      error: 'Cannot assign more than 50 users at once'
    };
  }
  
  if (userCount > 20) {
    return {
      valid: true,
      warnings: ['Assigning more than 20 users may take a moment']
    };
  }
  
  return { valid: true };
}

/**
 * Helper function to check if assignment would create circular dependency
 */
function wouldCreateCircularDependency(
  managerId: string,
  userId: string,
  hierarchies: Hierarchy[]
): boolean {
  // Direct self-assignment
  if (managerId === userId) {
    return true;
  }
  
  // Check if the user to be assigned is already a manager of the target manager
  const userDescendants = getDescendants(userId, hierarchies);
  return userDescendants.includes(managerId);
}
