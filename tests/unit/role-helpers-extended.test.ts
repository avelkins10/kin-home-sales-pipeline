import { describe, it, expect } from 'vitest';
import {
  getRoleDisplayName,
  getRoleDescription,
  isManagerRole,
} from '@/lib/utils/role-helpers';

describe('Role Helpers Extended', () => {
  describe('getRoleDisplayName', () => {
    it('should return display names for sales roles', () => {
      expect(getRoleDisplayName('closer')).toBe('Closer');
      expect(getRoleDisplayName('setter')).toBe('Setter');
      expect(getRoleDisplayName('team_lead')).toBe('Team Lead');
      expect(getRoleDisplayName('office_leader')).toBe('Office Leader');
      expect(getRoleDisplayName('regional')).toBe('Regional Manager');
      expect(getRoleDisplayName('super_admin')).toBe('Super Admin');
    });

    it('should return display names for operations roles', () => {
      expect(getRoleDisplayName('operations_coordinator')).toBe('Operations Coordinator');
      expect(getRoleDisplayName('operations_manager')).toBe('Operations Manager');
    });

    it('should return formatted role for unknown roles', () => {
      expect(getRoleDisplayName('unknown_role')).toBe('Unknown Role');
    });
  });

  describe('getRoleDescription', () => {
    it('should return descriptions for sales roles', () => {
      expect(getRoleDescription('closer')).toContain('closes deals');
      expect(getRoleDescription('setter')).toContain('sets appointments');
      expect(getRoleDescription('office_leader')).toContain('manages office');
      expect(getRoleDescription('super_admin')).toContain('full system access');
    });

    it('should return descriptions for operations roles', () => {
      expect(getRoleDescription('operations_coordinator')).toContain('coordinates operations');
      expect(getRoleDescription('operations_manager')).toContain('manages operations');
    });

    it('should return default description for unknown roles', () => {
      expect(getRoleDescription('unknown_role')).toContain('User role');
    });
  });

  describe('isManagerRole', () => {
    it('should return true for manager roles', () => {
      expect(isManagerRole('team_lead')).toBe(true);
      expect(isManagerRole('office_leader')).toBe(true);
      expect(isManagerRole('area_director')).toBe(true);
      expect(isManagerRole('divisional')).toBe(true);
      expect(isManagerRole('regional')).toBe(true);
      expect(isManagerRole('super_admin')).toBe(true);
      expect(isManagerRole('operations_manager')).toBe(true);
    });

    it('should return false for non-manager roles', () => {
      expect(isManagerRole('closer')).toBe(false);
      expect(isManagerRole('setter')).toBe(false);
      expect(isManagerRole('operations_coordinator')).toBe(false);
    });

    it('should return false for invalid roles', () => {
      expect(isManagerRole('invalid_role')).toBe(false);
      expect(isManagerRole('')).toBe(false);
    });
  });
});
