// lib/auth/roles.ts
/**
 * Centralized role definitions and permission constants
 * Used across API endpoints for authorization checks
 */

import { UserRole } from './guards';

/**
 * Roles allowed to read field tracking tasks
 * Includes coordinators, managers, and leadership
 */
export const ALLOWED_ROLES_TASK_READ: UserRole[] = [
  'operations_coordinator',
  'operations_manager',
  'office_leader',
  'regional',
  'super_admin',
];

/**
 * Roles allowed to write/update field tracking tasks
 * Same as read permissions for field tracking operations
 */
export const ALLOWED_ROLES_TASK_WRITE: UserRole[] = [
  'operations_coordinator',
  'operations_manager',
  'office_leader',
  'regional',
  'super_admin',
];

/**
 * Roles allowed to delete field tracking tasks
 * More restricted - requires manager level or above
 */
export const ALLOWED_ROLES_TASK_DELETE: UserRole[] = [
  'operations_manager',
  'office_leader',
  'regional',
  'super_admin',
];

/**
 * Check if a role has read access to field tracking tasks
 */
export function canReadTasks(role: UserRole): boolean {
  return ALLOWED_ROLES_TASK_READ.includes(role);
}

/**
 * Check if a role has write access to field tracking tasks
 */
export function canWriteTasks(role: UserRole): boolean {
  return ALLOWED_ROLES_TASK_WRITE.includes(role);
}

/**
 * Check if a role has delete access to field tracking tasks
 */
export function canDeleteTasks(role: UserRole): boolean {
  return ALLOWED_ROLES_TASK_DELETE.includes(role);
}

