/**
 * App-related helper functions for multi-app architecture
 * These functions determine which apps a user can access based on their role
 */

/**
 * Check if a user role can access the Sales app
 * @param role - User role string
 * @returns True if user can access Sales app
 */
export function canAccessSalesApp(role: string): boolean {
  const salesRoles = [
    'closer', 'setter', 'coordinator', 'team_lead',
    'office_leader', 'area_director', 'divisional',
    'regional', 'super_admin'
  ];
  return salesRoles.includes(role);
}

/**
 * Check if a user role can access the Operations app
 * @param role - User role string
 * @returns True if user can access Operations app
 */
export function canAccessOperationsApp(role: string): boolean {
  const operationsRoles = [
    'operations_coordinator', 'operations_manager',
    'office_leader', 'regional', 'super_admin'
  ];
  return operationsRoles.includes(role);
}

/**
 * Check if a user role can access both Sales and Operations apps
 * @param role - User role string
 * @returns True if user can access both apps
 */
export function canAccessBothApps(role: string): boolean {
  const managerRoles = [
    'office_leader', 'regional', 'super_admin', 'operations_manager'
  ];
  return managerRoles.includes(role);
}

/**
 * Get the default app for a user role
 * @param role - User role string
 * @returns Default app ('sales' or 'operations')
 */
export function getDefaultAppForRole(role: string): 'sales' | 'operations' {
  const operationsRoles = ['operations_coordinator', 'operations_manager'];
  return operationsRoles.includes(role) ? 'operations' : 'sales';
}

/**
 * Get all roles that can access a specific app
 * @param app - App type ('sales' or 'operations')
 * @returns Array of roles that can access the app
 */
export function getRolesForApp(app: 'sales' | 'operations'): string[] {
  if (app === 'sales') {
    return [
      'closer', 'setter', 'coordinator', 'team_lead',
      'office_leader', 'area_director', 'divisional',
      'regional', 'super_admin'
    ];
  } else {
    return [
      'operations_coordinator', 'operations_manager',
      'office_leader', 'regional', 'super_admin'
    ];
  }
}

/**
 * Check if a user should see the app switcher
 * @param role - User role string
 * @returns True if user should see app switcher
 */
export function shouldShowAppSwitcher(role: string): boolean {
  return canAccessBothApps(role);
}
