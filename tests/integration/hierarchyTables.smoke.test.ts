import { describe, it, expect, beforeAll } from 'vitest';
import { sql } from '@/lib/db/client';

describe('Hierarchy Tables Smoke Test', () => {
  beforeAll(async () => {
    // This test verifies that the hierarchy tables are populated
    // Run with SEED_TEST_DB=true to ensure data is loaded
  });

  describe('Database Schema Verification', () => {
    it('should have user_hierarchies table with data', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM user_hierarchies
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ user_hierarchies table has ${result[0].count} records`);
    });

    it('should have office_assignments table with data', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM office_assignments
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ office_assignments table has ${result[0].count} records`);
    });

    it('should have users table with test data', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE email LIKE '%@test.com'
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ users table has ${result[0].count} test records`);
    });
  });

  describe('Data Relationships Verification', () => {
    it('should have valid user hierarchies with existing users', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM user_hierarchies uh
        JOIN users m ON uh.manager_id = m.id
        JOIN users u ON uh.user_id = u.id
        WHERE m.email LIKE '%@test.com'
        AND u.email LIKE '%@test.com'
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ Found ${result[0].count} valid user hierarchies`);
    });

    it('should have valid office assignments with existing users', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM office_assignments oa
        JOIN users u ON oa.user_id = u.id
        WHERE u.email LIKE '%@test.com'
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ Found ${result[0].count} valid office assignments`);
    });

    it('should have team leads with managed users', async () => {
      const result = await sql`
        SELECT 
          m.name as manager_name,
          m.email as manager_email,
          COUNT(u.id) as managed_count
        FROM user_hierarchies uh
        JOIN users m ON uh.manager_id = m.id
        JOIN users u ON uh.user_id = u.id
        WHERE m.role = 'team_lead'
        AND m.email LIKE '%@test.com'
        GROUP BY m.id, m.name, m.email
        HAVING COUNT(u.id) > 0
      `;

      expect(result.length).toBeGreaterThan(0);
      console.log(`✅ Found ${result.length} team leads with managed users`);
      
      result.forEach(teamLead => {
        console.log(`   - ${teamLead.manager_name} (${teamLead.manager_email}) manages ${teamLead.managed_count} users`);
      });
    });

    it('should have office leaders with office assignments', async () => {
      const result = await sql`
        SELECT 
          u.name as user_name,
          u.email as user_email,
          COUNT(oa.office_name) as office_count
        FROM office_assignments oa
        JOIN users u ON oa.user_id = u.id
        WHERE u.role = 'office_leader'
        AND u.email LIKE '%@test.com'
        GROUP BY u.id, u.name, u.email
        HAVING COUNT(oa.office_name) > 0
      `;

      expect(result.length).toBeGreaterThan(0);
      console.log(`✅ Found ${result.length} office leaders with office assignments`);
      
      result.forEach(officeLeader => {
        console.log(`   - ${officeLeader.user_name} (${officeLeader.user_email}) has access to ${officeLeader.office_count} offices`);
      });
    });
  });

  describe('Data Quality Verification', () => {
    it('should have users with valid email addresses', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE email LIKE '%@test.com'
        AND email IS NOT NULL
        AND email != ''
        AND email != '   '
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ Found ${result[0].count} users with valid email addresses`);
    });

    it('should have office assignments with valid office names', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM office_assignments
        WHERE office_name IS NOT NULL
        AND office_name != ''
        AND office_name != '   '
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ Found ${result[0].count} office assignments with valid office names`);
    });

    it('should have proper role distribution', async () => {
      const result = await sql`
        SELECT role, COUNT(*) as count
        FROM users
        WHERE email LIKE '%@test.com'
        GROUP BY role
        ORDER BY count DESC
      `;

      expect(result.length).toBeGreaterThan(0);
      console.log(`✅ Role distribution:`);
      
      result.forEach(role => {
        console.log(`   - ${role.role}: ${role.count} users`);
      });

      // Should have at least one of each key role
      const roles = result.map(r => r.role);
      expect(roles).toContain('team_lead');
      expect(roles).toContain('office_leader');
      expect(roles).toContain('closer');
      expect(roles).toContain('setter');
    });
  });

  describe('Performance Verification', () => {
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

      console.log(`✅ Performance test data:`);
      console.log(`   - Users: ${userCount[0].count}`);
      console.log(`   - Hierarchies: ${hierarchyCount[0].count}`);
      console.log(`   - Office assignments: ${officeCount[0].count}`);
    });

    it('should have team lead with multiple managed users for performance testing', async () => {
      const result = await sql`
        SELECT 
          m.name as manager_name,
          COUNT(u.id) as managed_count
        FROM user_hierarchies uh
        JOIN users m ON uh.manager_id = m.id
        JOIN users u ON uh.user_id = u.id
        WHERE m.role = 'team_lead'
        AND m.email LIKE '%@test.com'
        GROUP BY m.id, m.name
        HAVING COUNT(u.id) >= 5
        ORDER BY COUNT(u.id) DESC
        LIMIT 1
      `;

      expect(result.length).toBeGreaterThan(0);
      expect(result[0].managed_count).toBeGreaterThanOrEqual(5);
      console.log(`✅ Team lead ${result[0].manager_name} manages ${result[0].managed_count} users (good for performance testing)`);
    });
  });

  describe('Edge Case Data Verification', () => {
    it('should have users with special email characters for testing', async () => {
      const result = await sql`
        SELECT email, name
        FROM users
        WHERE email LIKE '%@test.com'
        AND (email LIKE '%+%' OR email LIKE '%''%' OR email LIKE '%.%')
        LIMIT 5
      `;

      expect(result.length).toBeGreaterThan(0);
      console.log(`✅ Found ${result.length} users with special email characters:`);
      
      result.forEach(user => {
        console.log(`   - ${user.name}: ${user.email}`);
      });
    });

    it('should have inactive users for testing', async () => {
      const result = await sql`
        SELECT COUNT(*) as count
        FROM users
        WHERE email LIKE '%@test.com'
        AND is_active = false
      `;

      expect(result[0].count).toBeGreaterThan(0);
      console.log(`✅ Found ${result[0].count} inactive users for testing`);
    });

    it('should have edge case office names for testing', async () => {
      const result = await sql`
        SELECT office_name, COUNT(*) as count
        FROM office_assignments
        WHERE office_name = '' OR office_name = '   '
        GROUP BY office_name
      `;

      if (result.length > 0) {
        console.log(`✅ Found edge case office names for testing:`);
        result.forEach(office => {
          console.log(`   - Empty/whitespace office names: ${office.count}`);
        });
      }
    });
  });
});
