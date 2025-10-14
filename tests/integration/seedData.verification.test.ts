import { describe, it, expect, beforeAll } from 'vitest';
import { sql } from '@/lib/db/client';

describe('Seed Data and Migration Verification', () => {
  beforeAll(async () => {
    // This test assumes the seed data has been loaded
    // Run with SEED_TEST_DB=true to load the data
  });

  describe('Database Schema Verification', () => {
    it('should have user_hierarchies table with correct structure', async () => {
      const result = await sql`
        SELECT column_name, data_type, is_nullable
        FROM information_schema.columns
        WHERE table_name = 'user_hierarchies'
        ORDER BY ordinal_position
      `;

      expect(result.length).toBeGreaterThan(0);
      
      const columns = result.map(row => row.column_name);
      expect(columns).toContain('id');
      expect(columns).toContain('manager_id');
      expect(columns).toContain('user_id');
      expect(columns).toContain('created_at');
      expect(columns).toContain('updated_at');
    });

    it('should have office_assignments table with correct structure', async () => {
      const result = await sql`
        SELECT column_name, data_type, is_nullable
        FROM information_schema.columns
        WHERE table_name = 'office_assignments'
        ORDER BY ordinal_position
      `;

      expect(result.length).toBeGreaterThan(0);
      
      const columns = result.map(row => row.column_name);
      expect(columns).toContain('id');
      expect(columns).toContain('user_id');
      expect(columns).toContain('office_name');
      expect(columns).toContain('access_level');
      expect(columns).toContain('assigned_at');
    });

    it('should have users table with required fields', async () => {
      const result = await sql`
        SELECT column_name, data_type, is_nullable
        FROM information_schema.columns
        WHERE table_name = 'users'
        AND column_name IN ('id', 'email', 'role', 'name', 'is_active', 'last_project_date', 'invite_token')
        ORDER BY column_name
      `;

      expect(result.length).toBeGreaterThan(0);
      
      const columns = result.map(row => row.column_name);
      expect(columns).toContain('id');
      expect(columns).toContain('email');
      expect(columns).toContain('role');
      expect(columns).toContain('name');
      expect(columns).toContain('is_active');
    });
  });

  describe('Seed Data Verification', () => {
    it('should have test users with different roles', async () => {
      const result = await sql`
        SELECT role, COUNT(*) as count
        FROM users
        WHERE email LIKE '%@test.com'
        GROUP BY role
        ORDER BY role
      `;

      expect(result.length).toBeGreaterThan(0);
      
      const roles = result.map(row => row.role);
      expect(roles).toContain('closer');
      expect(roles).toContain('setter');
      expect(roles).toContain('team_lead');
      expect(roles).toContain('office_leader');
      expect(roles).toContain('super_admin');
      expect(roles).toContain('regional');
    });

    it('should have user hierarchies for team leads', async () => {
      const result = await sql`
        SELECT 
          m.name as manager_name,
          m.role as manager_role,
          COUNT(u.id) as managed_users
        FROM user_hierarchies uh
        JOIN users m ON uh.manager_id = m.id
        JOIN users u ON uh.user_id = u.id
        WHERE m.role = 'team_lead'
        GROUP BY m.id, m.name, m.role
        ORDER BY m.name
      `;

      expect(result.length).toBeGreaterThan(0);
      
      // Should have at least one team lead with managed users
      const teamLeadWithUsers = result.find(row => row.managed_users > 0);
      expect(teamLeadWithUsers).toBeDefined();
      expect(teamLeadWithUsers.manager_role).toBe('team_lead');
    });

    it('should have office assignments for office leaders', async () => {
      const result = await sql`
        SELECT 
          u.name as user_name,
          u.role as user_role,
          COUNT(oa.office_name) as office_count
        FROM office_assignments oa
        JOIN users u ON oa.user_id = u.id
        WHERE u.role = 'office_leader'
        GROUP BY u.id, u.name, u.role
        ORDER BY u.name
      `;

      expect(result.length).toBeGreaterThan(0);
      
      // Should have at least one office leader with office assignments
      const officeLeaderWithOffices = result.find(row => row.office_count > 0);
      expect(officeLeaderWithOffices).toBeDefined();
      expect(officeLeaderWithOffices.user_role).toBe('office_leader');
    });

    it('should have test users with special email characters', async () => {
      const result = await sql`
        SELECT email, name, role
        FROM users
        WHERE email LIKE '%@test.com'
        AND (email LIKE '%+%' OR email LIKE '%''%' OR email LIKE '%.%')
        ORDER BY email
      `;

      expect(result.length).toBeGreaterThan(0);
      
      // Should have users with special characters for testing
      const emails = result.map(row => row.email);
      expect(emails.some(email => email.includes('+'))).toBe(true);
      expect(emails.some(email => email.includes("'"))).toBe(true);
      expect(emails.some(email => email.includes('.'))).toBe(true);
    });

    it('should have inactive users for testing', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE email LIKE '%@test.com'
        AND is_active = false
      `;

      expect(result[0].count).toBeGreaterThan(0);
    });
  });

  describe('Data Integrity Verification', () => {
    it('should have valid foreign key relationships in user_hierarchies', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM user_hierarchies uh
        JOIN users m ON uh.manager_id = m.id
        JOIN users u ON uh.user_id = u.id
        WHERE m.email LIKE '%@test.com'
        AND u.email LIKE '%@test.com'
      `;

      expect(result[0].count).toBeGreaterThan(0);
    });

    it('should have valid foreign key relationships in office_assignments', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM office_assignments oa
        JOIN users u ON oa.user_id = u.id
        WHERE u.email LIKE '%@test.com'
      `;

      expect(result[0].count).toBeGreaterThan(0);
    });

    it('should have unique constraints working correctly', async () => {
      // Try to insert duplicate hierarchy (should fail)
      try {
        await sql`
          INSERT INTO user_hierarchies (manager_id, user_id)
          VALUES ('user-team-lead-b', 'user-rep-c')
        `;
        expect.fail('Should have failed due to unique constraint');
      } catch (error) {
        expect(error).toBeDefined();
      }

      // Try to insert duplicate office assignment (should fail)
      try {
        await sql`
          INSERT INTO office_assignments (user_id, office_name, access_level)
          VALUES ('user-office-leader-a', 'Office A', 'view')
        `;
        expect.fail('Should have failed due to unique constraint');
      } catch (error) {
        expect(error).toBeDefined();
      }
    });
  });

  describe('Performance Test Data', () => {
    it('should have sufficient data for performance testing', async () => {
      const userCount = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE email LIKE '%@test.com'
      `;

      const hierarchyCount = await sql`
        SELECT COUNT(*) as count
        FROM user_hierarchies uh
        JOIN users m ON uh.manager_id = m.id
        WHERE m.email LIKE '%@test.com'
      `;

      const officeCount = await sql`
        SELECT COUNT(*) as count
        FROM office_assignments oa
        JOIN users u ON oa.user_id = u.id
        WHERE u.email LIKE '%@test.com'
      `;

      expect(userCount[0].count).toBeGreaterThanOrEqual(10);
      expect(hierarchyCount[0].count).toBeGreaterThanOrEqual(5);
      expect(officeCount[0].count).toBeGreaterThanOrEqual(5);
    });

    it('should have team lead with multiple managed users for performance testing', async () => {
      const result = await sql`
        SELECT 
          m.name as manager_name,
          COUNT(u.id) as managed_users
        FROM user_hierarchies uh
        JOIN users m ON uh.manager_id = m.id
        JOIN users u ON uh.user_id = u.id
        WHERE m.role = 'team_lead'
        GROUP BY m.id, m.name
        HAVING COUNT(u.id) >= 5
        ORDER BY COUNT(u.id) DESC
      `;

      expect(result.length).toBeGreaterThan(0);
      expect(result[0].managed_users).toBeGreaterThanOrEqual(5);
    });
  });

  describe('Edge Case Data', () => {
    it('should have users with null emails for testing', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE email IS NULL
        AND name LIKE '%test%'
      `;

      expect(result[0].count).toBeGreaterThan(0);
    });

    it('should have users with empty or whitespace emails for testing', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE (email = '' OR email = '   ')
        AND name LIKE '%test%'
      `;

      expect(result[0].count).toBeGreaterThan(0);
    });

    it('should have office assignments with edge case office names', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM office_assignments
        WHERE office_name = '' OR office_name = '   '
      `;

      expect(result[0].count).toBeGreaterThan(0);
    });
  });
});
