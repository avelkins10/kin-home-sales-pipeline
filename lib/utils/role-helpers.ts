/**
 * Role-related helper functions
 */

/**
 * Convert role enum to human-readable display name
 */
export function getRoleDisplayName(role: string): string {
  const roleMap: Record<string, string> = {
    'closer': 'Closer',
    'setter': 'Setter',
    'team_lead': 'Team Lead',
    'office_leader': 'Office Leader',
    'area_director': 'Area Director',
    'divisional': 'Divisional Manager',
    'regional': 'Regional Manager',
    'super_admin': 'Super Admin',
    'operations_coordinator': 'Operations Coordinator',
    'operations_manager': 'Operations Manager'
  };
  
  return roleMap[role] || role;
}

/**
 * Get detailed description of what the role can see/do
 */
export function getRoleDescription(role: string, offices?: string[]): string {
  const officeList = offices && offices.length > 0 ? offices.join(', ') : '';
  
  switch (role) {
    case 'closer':
      return 'You will see your own projects where you are the closer.';
    
    case 'setter':
      return 'You will see your own projects where you are the setter.';
    
    case 'team_lead':
      return 'You will see projects for the sales reps you manage (both closer and setter projects).';
    
    case 'office_leader':
      return officeList 
        ? `You will see ALL projects in your assigned office(s): ${officeList}. This includes projects from reps who don't have accounts in the app.`
        : 'You will see ALL projects in your assigned office(s). This includes projects from reps who don\'t have accounts in the app.';
    
    case 'area_director':
      return officeList
        ? `You will see ALL projects in your assigned offices: ${officeList}. This includes projects from reps who don't have accounts in the app.`
        : 'You will see ALL projects in your assigned offices. This includes projects from reps who don\'t have accounts in the app.';
    
    case 'divisional':
      return officeList
        ? `You will see ALL projects in your assigned offices/regions: ${officeList}. This includes projects from reps who don't have accounts in the app.`
        : 'You will see ALL projects in your assigned offices/regions. This includes projects from reps who don\'t have accounts in the app.';
    
    case 'regional':
      return 'You will see ALL projects across all offices in your region.';
    
    case 'super_admin':
      return 'You have full access to all projects and administrative features.';
    
    case 'operations_coordinator':
      return 'You will see operations work orders and tasks assigned to you.';
    
    case 'operations_manager':
      return 'You will see team-wide operations metrics and can manage operations workflows.';
    
    default:
      return 'You will have access to projects based on your assigned role.';
  }
}

/**
 * Get Tailwind color class for role badge
 */
export function getRoleBadgeColor(role: string): string {
  const colorMap: Record<string, string> = {
    'closer': 'bg-blue-100 text-blue-800',
    'setter': 'bg-purple-100 text-purple-800',
    'team_lead': 'bg-cyan-100 text-cyan-800',
    'office_leader': 'bg-green-100 text-green-800',
    'area_director': 'bg-emerald-100 text-emerald-800',
    'divisional': 'bg-teal-100 text-teal-800',
    'regional': 'bg-indigo-100 text-indigo-800',
    'super_admin': 'bg-red-100 text-red-800',
    'operations_coordinator': 'bg-orange-100 text-orange-800',
    'operations_manager': 'bg-amber-100 text-amber-800'
  };
  
  return colorMap[role] || 'bg-gray-100 text-gray-800';
}

/**
 * Check if role uses office-based visibility
 */
export function isOfficeBasedRole(role: string): boolean {
  const officeBasedRoles = ['office_leader', 'area_director', 'divisional', 'regional'];
  return officeBasedRoles.includes(role);
}

/**
 * Check if role is a manager role that can see team-wide metrics
 * Manager roles: team_lead, office_leader, area_director, divisional, regional, super_admin
 * These roles have access to the personal vs team scope toggle in the dashboard
 * @param role - User role string
 * @returns True if role is a manager role
 */
export function isManagerRole(role: string): boolean {
  const managerRoles = ['team_lead', 'office_leader', 'area_director', 'divisional', 'regional', 'super_admin', 'operations_manager'];
  return managerRoles.includes(role);
}

/**
 * Check if role is operations_manager
 * Operations managers can see all operations projects (any project with a PC assigned)
 * @param role - User role string
 * @returns True if role is operations_manager
 */
export function isOperationsManager(role: string): boolean {
  return role === 'operations_manager';
}

/**
 * Check if role has unrestricted access to operations data
 * Unrestricted roles: super_admin, regional, office_leader, area_director, divisional
 * These roles can see ALL projects regardless of PC assignment
 * @param role - User role string
 * @returns True if role has unrestricted operations access
 */
export function hasOperationsUnrestrictedAccess(role: string): boolean {
  return ['super_admin', 'regional', 'office_leader', 'area_director', 'divisional'].includes(role);
}
