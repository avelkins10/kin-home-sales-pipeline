import type { UserRole } from '@/lib/types/project';
import type { BadgeProps } from '@/components/ui/badge';

/**
 * Get the appropriate Badge variant for a user role
 */
export function getRoleBadgeVariant(role: UserRole): BadgeProps['variant'] {
  switch (role) {
    case 'super_admin':
      return 'destructive';
    case 'regional':
    case 'team_lead':
    case 'area_director':
    case 'divisional':
      return 'default';
    case 'office_leader':
      return 'secondary';
    default:
      return 'outline';
  }
}

/**
 * Get color classes for role badges (for custom styling)
 */
export function getRoleColorClasses(role: UserRole): string {
  switch (role) {
    case 'super_admin':
    case 'regional':
    case 'divisional':
      return 'bg-purple-100 text-purple-700';
    case 'area_director':
      return 'bg-blue-100 text-blue-700';
    case 'team_lead':
      return 'bg-cyan-100 text-cyan-700';
    case 'office_leader':
      return 'bg-blue-100 text-blue-700';
    case 'coordinator':
      return 'bg-green-100 text-green-700';
    case 'closer':
      return 'bg-indigo-100 text-indigo-700';
    case 'setter':
      return 'bg-orange-100 text-orange-700';
    default:
      return 'bg-slate-100 text-slate-700';
  }
}

/**
 * Get the display name for a user role
 */
export function getRoleDisplayName(role: UserRole): string {
  switch (role) {
    case 'super_admin':
      return 'Super Admin';
    case 'regional':
      return 'Regional Manager';
    case 'team_lead':
      return 'Team Lead';
    case 'area_director':
      return 'Area Director';
    case 'divisional':
      return 'Divisional Manager';
    case 'office_leader':
      return 'Office Leader';
    case 'closer':
      return 'Closer';
    case 'setter':
      return 'Setter';
    default:
      return role;
  }
}
