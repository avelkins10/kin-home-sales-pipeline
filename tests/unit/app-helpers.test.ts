import { describe, it, expect } from 'vitest';
import {
  canAccessSalesApp,
  canAccessOperationsApp,
  canAccessBothApps,
  getDefaultAppForRole,
  getRolesForApp,
  shouldShowAppSwitcher,
} from '@/lib/utils/app-helpers';

describe('App Helpers', () => {
  describe('canAccessSalesApp', () => {
    it('should return true for sales roles', () => {
      expect(canAccessSalesApp('closer')).toBe(true);
      expect(canAccessSalesApp('setter')).toBe(true);
      expect(canAccessSalesApp('coordinator')).toBe(true);
      expect(canAccessSalesApp('team_lead')).toBe(true);
      expect(canAccessSalesApp('office_leader')).toBe(true);
      expect(canAccessSalesApp('area_director')).toBe(true);
      expect(canAccessSalesApp('divisional')).toBe(true);
      expect(canAccessSalesApp('regional')).toBe(true);
      expect(canAccessSalesApp('super_admin')).toBe(true);
    });

    it('should return false for operations-only roles', () => {
      expect(canAccessSalesApp('operations_coordinator')).toBe(false);
      expect(canAccessSalesApp('operations_manager')).toBe(false);
    });

    it('should return false for invalid roles', () => {
      expect(canAccessSalesApp('invalid_role')).toBe(false);
      expect(canAccessSalesApp('')).toBe(false);
    });
  });

  describe('canAccessOperationsApp', () => {
    it('should return true for operations roles', () => {
      expect(canAccessOperationsApp('operations_coordinator')).toBe(true);
      expect(canAccessOperationsApp('operations_manager')).toBe(true);
    });

    it('should return true for cross-app roles', () => {
      expect(canAccessOperationsApp('office_leader')).toBe(true);
      expect(canAccessOperationsApp('regional')).toBe(true);
      expect(canAccessOperationsApp('super_admin')).toBe(true);
    });

    it('should return false for sales-only roles', () => {
      expect(canAccessOperationsApp('closer')).toBe(false);
      expect(canAccessOperationsApp('setter')).toBe(false);
      expect(canAccessOperationsApp('coordinator')).toBe(false);
      expect(canAccessOperationsApp('team_lead')).toBe(false);
      expect(canAccessOperationsApp('area_director')).toBe(false);
      expect(canAccessOperationsApp('divisional')).toBe(false);
    });

    it('should return false for invalid roles', () => {
      expect(canAccessOperationsApp('invalid_role')).toBe(false);
      expect(canAccessOperationsApp('')).toBe(false);
    });
  });

  describe('canAccessBothApps', () => {
    it('should return true for manager roles', () => {
      expect(canAccessBothApps('office_leader')).toBe(true);
      expect(canAccessBothApps('regional')).toBe(true);
      expect(canAccessBothApps('super_admin')).toBe(true);
      expect(canAccessBothApps('operations_manager')).toBe(true);
    });

    it('should return false for single-app roles', () => {
      expect(canAccessBothApps('closer')).toBe(false);
      expect(canAccessBothApps('setter')).toBe(false);
      expect(canAccessBothApps('operations_coordinator')).toBe(false);
    });

    it('should return false for invalid roles', () => {
      expect(canAccessBothApps('invalid_role')).toBe(false);
      expect(canAccessBothApps('')).toBe(false);
    });
  });

  describe('getDefaultAppForRole', () => {
    it('should return operations for operations roles', () => {
      expect(getDefaultAppForRole('operations_coordinator')).toBe('operations');
      expect(getDefaultAppForRole('operations_manager')).toBe('operations');
    });

    it('should return sales for all other roles', () => {
      expect(getDefaultAppForRole('closer')).toBe('sales');
      expect(getDefaultAppForRole('setter')).toBe('sales');
      expect(getDefaultAppForRole('office_leader')).toBe('sales');
      expect(getDefaultAppForRole('regional')).toBe('sales');
      expect(getDefaultAppForRole('super_admin')).toBe('sales');
    });
  });

  describe('getRolesForApp', () => {
    it('should return sales roles for sales app', () => {
      const salesRoles = getRolesForApp('sales');
      expect(salesRoles).toContain('closer');
      expect(salesRoles).toContain('setter');
      expect(salesRoles).toContain('office_leader');
      expect(salesRoles).toContain('super_admin');
      expect(salesRoles).not.toContain('operations_coordinator');
    });

    it('should return operations roles for operations app', () => {
      const operationsRoles = getRolesForApp('operations');
      expect(operationsRoles).toContain('operations_coordinator');
      expect(operationsRoles).toContain('operations_manager');
      expect(operationsRoles).toContain('office_leader');
      expect(operationsRoles).toContain('super_admin');
      expect(operationsRoles).not.toContain('closer');
    });
  });

  describe('shouldShowAppSwitcher', () => {
    it('should return true for roles that can access both apps', () => {
      expect(shouldShowAppSwitcher('office_leader')).toBe(true);
      expect(shouldShowAppSwitcher('regional')).toBe(true);
      expect(shouldShowAppSwitcher('super_admin')).toBe(true);
      expect(shouldShowAppSwitcher('operations_manager')).toBe(true);
    });

    it('should return false for single-app roles', () => {
      expect(shouldShowAppSwitcher('closer')).toBe(false);
      expect(shouldShowAppSwitcher('operations_coordinator')).toBe(false);
    });

    it('should return false for invalid roles', () => {
      expect(shouldShowAppSwitcher('invalid_role')).toBe(false);
      expect(shouldShowAppSwitcher('')).toBe(false);
    });
  });
});
