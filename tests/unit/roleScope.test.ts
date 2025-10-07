import { describe, it, expect } from 'vitest';
import { buildRoleClause } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

describe('Role Scoping', () => {
  describe('buildRoleClause', () => {
    it('should return all projects clause for super_admin role', () => {
      const clause = buildRoleClause('user123', 'super_admin');
      expect(clause).toBe('{3.GT.0}');
    });

    it('should return all projects clause for regional role', () => {
      const clause = buildRoleClause('user123', 'regional');
      expect(clause).toBe('{3.GT.0}');
    });

    it('should filter by closer ID for closer role', () => {
      const clause = buildRoleClause('user123', 'closer');
      expect(clause).toBe(`{${PROJECT_FIELDS.CLOSER_ID}.EX.'user123'}`);
    });

    it('should filter by setter ID for setter role', () => {
      const clause = buildRoleClause('user123', 'setter');
      expect(clause).toBe(`{${PROJECT_FIELDS.SETTER_ID}.EX.'user123'}`);
    });

    it('should handle multiple user IDs for closer role', () => {
      const clause = buildRoleClause('user123,user456', 'closer');
      expect(clause).toBe(`{${PROJECT_FIELDS.CLOSER_ID}.EX.'user123'} OR {${PROJECT_FIELDS.CLOSER_ID}.EX.'user456'}`);
    });

    it('should handle multiple user IDs for setter role', () => {
      const clause = buildRoleClause('user123,user456', 'setter');
      expect(clause).toBe(`{${PROJECT_FIELDS.SETTER_ID}.EX.'user123'} OR {${PROJECT_FIELDS.SETTER_ID}.EX.'user456'}`);
    });

    it('should filter by office for office_leader role with assigned offices', () => {
      const clause = buildRoleClause('user123', 'office_leader', ['Office A']);
      expect(clause).toBe(`{${PROJECT_FIELDS.SALES_OFFICE}.EX.'Office A'}`);
    });

    it('should filter by multiple offices for office_leader role', () => {
      const clause = buildRoleClause('user123', 'office_leader', ['Office A', 'Office B']);
      expect(clause).toBe(`{${PROJECT_FIELDS.SALES_OFFICE}.EX.'Office A'} OR {${PROJECT_FIELDS.SALES_OFFICE}.EX.'Office B'}`);
    });

    it('should return no projects clause for office_leader role with no offices assigned', () => {
      const clause = buildRoleClause('user123', 'office_leader');
      expect(clause).toBe('{3.EQ.0}');
    });

    it('should return no projects clause for office_leader role with empty offices array', () => {
      const clause = buildRoleClause('user123', 'office_leader', []);
      expect(clause).toBe('{3.EQ.0}');
    });

    it('should handle whitespace in user IDs', () => {
      const clause = buildRoleClause(' user123 , user456 ', 'closer');
      expect(clause).toBe(`{${PROJECT_FIELDS.CLOSER_ID}.EX.'user123'} OR {${PROJECT_FIELDS.CLOSER_ID}.EX.'user456'}`);
    });

    it('should handle unknown role gracefully', () => {
      const clause = buildRoleClause('user123', 'unknown_role' as any);
      expect(clause).toBe('{3.GT.0}'); // Default fallback
    });
  });

  describe('office leader scoping fallback', () => {
    it('should enforce least-privilege when no offices are assigned', () => {
      // This is the critical test case mentioned in the comment
      const clause = buildRoleClause('user123', 'office_leader');
      
      // Should return a clause that matches no records (least privilege)
      expect(clause).toBe('{3.EQ.0}');
      
      // Should NOT return the old fallback that showed all projects
      expect(clause).not.toBe('{3.GT.0}');
    });

    it('should still allow access when offices are properly assigned', () => {
      const clause = buildRoleClause('user123', 'office_leader', ['Office A']);
      
      // Should return a clause that matches projects in the assigned office
      expect(clause).toBe(`{${PROJECT_FIELDS.SALES_OFFICE}.EX.'Office A'}`);
    });

    it('should handle multiple offices correctly', () => {
      const clause = buildRoleClause('user123', 'office_leader', ['Office A', 'Office B', 'Office C']);
      
      // Should return a clause that matches projects in any of the assigned offices
      expect(clause).toBe(`{${PROJECT_FIELDS.SALES_OFFICE}.EX.'Office A'} OR {${PROJECT_FIELDS.SALES_OFFICE}.EX.'Office B'} OR {${PROJECT_FIELDS.SALES_OFFICE}.EX.'Office C'}`);
    });
  });

  describe('security implications', () => {
    it('should prevent office leaders from seeing all projects when misconfigured', () => {
      // This test ensures the security fix is working
      const clause = buildRoleClause('user123', 'office_leader');
      
      // The clause should be restrictive, not permissive
      expect(clause).toBe('{3.EQ.0}');
      
      // This ensures that if an office leader is not properly configured with offices,
      // they see no projects rather than all projects (which was the previous behavior)
    });

    it('should maintain proper scoping for other roles', () => {
      // Ensure other roles are not affected by the office leader fix
      expect(buildRoleClause('user123', 'super_admin')).toBe('{3.GT.0}');
      expect(buildRoleClause('user123', 'regional')).toBe('{3.GT.0}');
      expect(buildRoleClause('user123', 'closer')).toBe(`{${PROJECT_FIELDS.CLOSER_ID}.EX.'user123'}`);
      expect(buildRoleClause('user123', 'setter')).toBe(`{${PROJECT_FIELDS.SETTER_ID}.EX.'user123'}`);
    });
  });
});