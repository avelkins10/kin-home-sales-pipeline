import { describe, it, expect } from 'vitest';
import {
  validateManagerRole,
  validateCircularHierarchy,
  validateRoleHierarchy,
  getUnassignedUsers,
  getUserHierarchyPath,
  getDescendants,
  canManageRole,
  getManagerRoleConstraints,
  formatHierarchyError,
  getTeamSize,
  validateUserAssignments,
  validateBulkAssignmentSize
} from '@/lib/utils/hierarchy-helpers';
import type { User, UserRole, Hierarchy } from '@/lib/types';

// Mock data
const mockUsers: User[] = [
  {
    id: '1',
    name: 'Super Admin',
    email: 'super@example.com',
    role: 'super_admin',
    is_active: true
  },
  {
    id: '2',
    name: 'Regional Manager',
    email: 'regional@example.com',
    role: 'regional',
    is_active: true
  },
  {
    id: '3',
    name: 'Office Leader',
    email: 'office@example.com',
    role: 'office_leader',
    is_active: true
  },
  {
    id: '4',
    name: 'Team Lead',
    email: 'team@example.com',
    role: 'team_lead',
    is_active: true
  },
  {
    id: '5',
    name: 'Closer',
    email: 'closer@example.com',
    role: 'closer',
    is_active: true
  },
  {
    id: '6',
    name: 'Setter',
    email: 'setter@example.com',
    role: 'setter',
    is_active: true
  },
  {
    id: '7',
    name: 'Inactive User',
    email: 'inactive@example.com',
    role: 'closer',
    is_active: false
  }
];

const mockHierarchies: Hierarchy[] = [
  {
    id: '1',
    manager_id: '1',
    user_id: '2',
    created_at: '2024-01-01',
    updated_at: '2024-01-01',
    manager_name: 'Super Admin',
    manager_email: 'super@example.com',
    manager_role: 'super_admin',
    user_name: 'Regional Manager',
    user_email: 'regional@example.com',
    user_role: 'regional',
    user_is_active: true
  },
  {
    id: '2',
    manager_id: '2',
    user_id: '3',
    created_at: '2024-01-01',
    updated_at: '2024-01-01',
    manager_name: 'Regional Manager',
    manager_email: 'regional@example.com',
    manager_role: 'regional',
    user_name: 'Office Leader',
    user_email: 'office@example.com',
    user_role: 'office_leader',
    user_is_active: true
  },
  {
    id: '3',
    manager_id: '3',
    user_id: '4',
    created_at: '2024-01-01',
    updated_at: '2024-01-01',
    manager_name: 'Office Leader',
    manager_email: 'office@example.com',
    manager_role: 'office_leader',
    user_name: 'Team Lead',
    user_email: 'team@example.com',
    user_role: 'team_lead',
    user_is_active: true
  },
  {
    id: '4',
    manager_id: '4',
    user_id: '5',
    created_at: '2024-01-01',
    updated_at: '2024-01-01',
    manager_name: 'Team Lead',
    manager_email: 'team@example.com',
    manager_role: 'team_lead',
    user_name: 'Closer',
    user_email: 'closer@example.com',
    user_role: 'closer',
    user_is_active: true
  }
];

describe('validateManagerRole', () => {
  it('should validate team_lead can manage closer', () => {
    const manager = mockUsers.find(u => u.id === '4'); // Team Lead
    const users = [mockUsers.find(u => u.id === '5')!]; // Closer
    
    const result = validateManagerRole(manager, users);
    expect(result.valid).toBe(true);
  });

  it('should invalidate closer managing setter', () => {
    const manager = mockUsers.find(u => u.id === '5'); // Closer
    const users = [mockUsers.find(u => u.id === '6')!]; // Setter
    
    const result = validateManagerRole(manager, users);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('closer cannot manage users');
  });

  it('should handle manager not found', () => {
    const manager = undefined;
    const users = [mockUsers.find(u => u.id === '5')!];
    
    const result = validateManagerRole(manager, users);
    expect(result.valid).toBe(false);
    expect(result.error).toBe('Manager not found');
  });

  it('should validate office_leader can manage team_lead', () => {
    const manager = mockUsers.find(u => u.id === '3'); // Office Leader
    const users = [mockUsers.find(u => u.id === '4')!]; // Team Lead
    
    const result = validateManagerRole(manager, users);
    expect(result.valid).toBe(true);
  });
});

describe('validateCircularHierarchy', () => {
  it('should allow valid assignment', () => {
    const result = validateCircularHierarchy('3', ['6'], mockHierarchies);
    expect(result.valid).toBe(true);
  });

  it('should prevent direct self-assignment', () => {
    const result = validateCircularHierarchy('3', ['3'], mockHierarchies);
    expect(result.valid).toBe(false);
    expect(result.circularUsers).toContain('3');
  });

  it('should prevent indirect circular dependency', () => {
    // Try to assign Super Admin (1) to Closer (5), but Closer is managed by Team Lead (4) 
    // who is managed by Office Leader (3) who is managed by Regional (2) who is managed by Super Admin (1)
    const result = validateCircularHierarchy('5', ['1'], mockHierarchies);
    expect(result.valid).toBe(false);
    expect(result.circularUsers).toContain('1');
  });
});

describe('validateRoleHierarchy', () => {
  it('should allow team_lead to manage closer', () => {
    const result = validateRoleHierarchy('team_lead', 'closer');
    expect(result.valid).toBe(true);
  });

  it('should prevent team_lead from managing office_leader', () => {
    const result = validateRoleHierarchy('team_lead', 'office_leader');
    expect(result.valid).toBe(false);
    expect(result.error).toContain('team_lead cannot manage office_leader');
  });

  it('should allow office_leader to manage team_lead', () => {
    const result = validateRoleHierarchy('office_leader', 'team_lead');
    expect(result.valid).toBe(true);
  });

  it('should allow super_admin to manage all roles', () => {
    const roles: UserRole[] = ['regional', 'divisional', 'area_director', 'office_leader', 'team_lead', 'closer', 'setter'];
    
    roles.forEach(role => {
      const result = validateRoleHierarchy('super_admin', role);
      expect(result.valid).toBe(true);
    });
  });
});

describe('getUnassignedUsers', () => {
  it('should return users without managers', () => {
    const result = getUnassignedUsers(mockUsers, mockHierarchies);
    const unassignedIds = result.map(u => u.id);
    
    expect(unassignedIds).toContain('6'); // Setter
    expect(unassignedIds).toContain('7'); // Inactive User
    expect(unassignedIds).not.toContain('1'); // Super Admin (excluded)
    expect(unassignedIds).not.toContain('2'); // Has manager
    expect(unassignedIds).not.toContain('3'); // Has manager
    expect(unassignedIds).not.toContain('4'); // Has manager
    expect(unassignedIds).not.toContain('5'); // Has manager
  });

  it('should exclude super_admin and regional from unassigned', () => {
    const result = getUnassignedUsers(mockUsers, []);
    const unassignedIds = result.map(u => u.id);
    
    expect(unassignedIds).not.toContain('1'); // Super Admin
    expect(unassignedIds).not.toContain('2'); // Regional
  });
});

describe('getUserHierarchyPath', () => {
  it('should return path from user to root', () => {
    const result = getUserHierarchyPath('5', mockHierarchies, mockUsers);
    const pathIds = result.map(u => u.id);
    
    expect(pathIds).toEqual(['5', '4', '3', '2', '1']); // Closer -> Team Lead -> Office Leader -> Regional -> Super Admin
  });

  it('should return single user for root node', () => {
    const result = getUserHierarchyPath('1', mockHierarchies, mockUsers);
    const pathIds = result.map(u => u.id);
    
    expect(pathIds).toEqual(['1']);
  });

  it('should handle user not found', () => {
    const result = getUserHierarchyPath('999', mockHierarchies, mockUsers);
    expect(result).toEqual([]);
  });
});

describe('getDescendants', () => {
  it('should return all descendants recursively', () => {
    const result = getDescendants('1', mockHierarchies);
    expect(result).toEqual(['2', '3', '4', '5']); // All users managed by Super Admin
  });

  it('should return direct descendants only', () => {
    const result = getDescendants('3', mockHierarchies);
    expect(result).toEqual(['4', '5']); // Team Lead and Closer managed by Office Leader
  });

  it('should return empty array for leaf node', () => {
    const result = getDescendants('5', mockHierarchies);
    expect(result).toEqual([]);
  });
});

describe('canManageRole', () => {
  it('should return true for valid role combinations', () => {
    expect(canManageRole('team_lead', 'closer')).toBe(true);
    expect(canManageRole('team_lead', 'setter')).toBe(true);
    expect(canManageRole('office_leader', 'team_lead')).toBe(true);
    expect(canManageRole('super_admin', 'regional')).toBe(true);
  });

  it('should return false for invalid role combinations', () => {
    expect(canManageRole('closer', 'setter')).toBe(false);
    expect(canManageRole('team_lead', 'office_leader')).toBe(false);
    expect(canManageRole('setter', 'closer')).toBe(false);
  });
});

describe('getManagerRoleConstraints', () => {
  it('should return correct constraints for team_lead', () => {
    const constraints = getManagerRoleConstraints('team_lead');
    expect(constraints.canManage).toEqual(['closer', 'setter']);
    expect(constraints.cannotManage).toContain('office_leader');
    expect(constraints.cannotManage).toContain('super_admin');
  });

  it('should return correct constraints for super_admin', () => {
    const constraints = getManagerRoleConstraints('super_admin');
    expect(constraints.canManage).toEqual(['regional', 'divisional', 'area_director', 'office_leader', 'team_lead', 'closer', 'setter']);
    expect(constraints.cannotManage).toEqual([]);
  });

  it('should return correct constraints for closer', () => {
    const constraints = getManagerRoleConstraints('closer');
    expect(constraints.canManage).toEqual([]);
    expect(constraints.cannotManage).toContain('setter');
  });
});

describe('formatHierarchyError', () => {
  it('should format circular hierarchy error', () => {
    const result = formatHierarchyError('circular', {
      circularPath: ['A', 'B', 'C', 'A']
    });
    expect(result).toContain('Cannot create circular hierarchy');
    expect(result).toContain('A → B → C → A');
  });

  it('should format role mismatch error', () => {
    const result = formatHierarchyError('role_mismatch', {
      managerRole: 'closer',
      userRole: 'setter'
    });
    expect(result).toBe('closer cannot manage setter');
  });

  it('should format already managed error', () => {
    const result = formatHierarchyError('already_managed', {
      userName: 'John Doe'
    });
    expect(result).toBe('John Doe is already managed by someone else');
  });

  it('should format self assignment error', () => {
    const result = formatHierarchyError('self_assignment', {});
    expect(result).toBe('Cannot assign user to themselves');
  });
});

describe('getTeamSize', () => {
  it('should count direct and indirect reports', () => {
    const result = getTeamSize('1', mockHierarchies);
    expect(result).toBe(4); // Regional, Office Leader, Team Lead, Closer
  });

  it('should count direct reports only', () => {
    const result = getTeamSize('3', mockHierarchies);
    expect(result).toBe(2); // Team Lead, Closer
  });

  it('should return 0 for non-manager', () => {
    const result = getTeamSize('5', mockHierarchies);
    expect(result).toBe(0);
  });
});

describe('validateUserAssignments', () => {
  it('should validate unassigned users', () => {
    const result = validateUserAssignments(['6'], mockUsers, mockHierarchies);
    expect(result.valid).toBe(true);
  });

  it('should detect already managed users', () => {
    const result = validateUserAssignments(['5'], mockUsers, mockHierarchies);
    expect(result.valid).toBe(false);
    expect(result.errors).toContain('Closer is already managed by Team Lead');
  });

  it('should detect super_admin users', () => {
    const result = validateUserAssignments(['1'], mockUsers, mockHierarchies);
    expect(result.valid).toBe(false);
    expect(result.errors).toContain('Super Admin is a super admin and cannot be managed');
  });

  it('should warn about inactive users', () => {
    const result = validateUserAssignments(['7'], mockUsers, []);
    expect(result.valid).toBe(true);
    expect(result.warnings).toContain('Inactive User is inactive and may not be able to log in');
  });
});

describe('validateBulkAssignmentSize', () => {
  it('should allow assignments within limit', () => {
    const result = validateBulkAssignmentSize(10);
    expect(result.valid).toBe(true);
  });

  it('should warn for large assignments', () => {
    const result = validateBulkAssignmentSize(25);
    expect(result.valid).toBe(true);
    expect(result.warning).toContain('Assigning more than 20 users may take a moment');
  });

  it('should reject assignments over limit', () => {
    const result = validateBulkAssignmentSize(51);
    expect(result.valid).toBe(false);
    expect(result.error).toContain('Cannot assign more than 50 users at once');
  });
});
