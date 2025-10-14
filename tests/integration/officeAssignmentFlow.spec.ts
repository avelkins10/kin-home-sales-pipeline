/**
 * End-to-End Office Assignment Flow Test
 * 
 * Verifies that when an admin assigns offices to a manager, the manager
 * immediately sees projects from those offices without needing to log out/in.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { sql } from '@/lib/db/client';
import { getProjectsForUserList } from '@/lib/quickbase/queries';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';

// Mock QuickBase client
const mockQuickBaseResponse = {
  data: [
    {
      [3]: { value: 1 }, // RECORD_ID
      [339]: { value: 'Office A' }, // SALES_OFFICE
      [518]: { value: 'rep1@example.com' }, // CLOSER_EMAIL
      [331]: { value: 'setter1@example.com' }, // SETTER_EMAIL
    },
    {
      [3]: { value: 2 }, // RECORD_ID
      [339]: { value: 'Office B' }, // SALES_OFFICE
      [518]: { value: 'rep2@example.com' }, // CLOSER_EMAIL
      [331]: { value: 'setter2@example.com' }, // SETTER_EMAIL
    },
  ],
  metadata: { totalRecords: 2 }
};

// Mock the QuickBase client with WHERE clause filtering
vi.mock('@/lib/quickbase/client', () => ({
  qbClient: {
    queryRecords: vi.fn().mockImplementation(({ where }) => {
      // Filter mock data based on WHERE clause
      let filteredData = mockQuickBaseResponse.data;
      
      if (where) {
        // Extract all office names with regex: /\{339\.EX\.'((?:[^']|'')*)'\}/g
        const officeMatches = where.match(/\{339\.EX\.'((?:[^']|'')*)'\}/g);
        
        if (officeMatches && officeMatches.length > 0) {
          // Extract office names and unescape doubled quotes
          const officeNames = officeMatches.map(match => {
            const officeName = match.match(/\{339\.EX\.'((?:[^']|'')*)'\}/)?.[1];
            return officeName ? officeName.replace(/''/g, "'") : null;
          }).filter(Boolean);
          
          // Filter by any of the found offices
          filteredData = mockQuickBaseResponse.data.filter(project => 
            officeNames.includes(project[339]?.value)
          );
        } else if (where.includes("3.EQ.0")) {
          // No access clause
          filteredData = [];
        } else if (where.includes("3.GT.0")) {
          // All projects clause
          filteredData = mockQuickBaseResponse.data;
        } else {
          // If WHERE clause contains office field but no matches found, return empty array
          if (where.includes("339.EX")) {
            filteredData = [];
          }
        }
      }
      
      return Promise.resolve({
        data: filteredData,
        metadata: { totalRecords: filteredData.length }
      });
    })
  }
}));

describe('Office Assignment Flow', () => {
  let adminUserId: string;
  let managerUserId: string;
  let repUserId: string;

  beforeEach(async () => {
    // Clean up any existing test data
    await sql`DELETE FROM office_assignments WHERE user_id LIKE 'test-%'`;
    await sql`DELETE FROM users WHERE id LIKE 'test-%'`;

    // Create test users
    const adminResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-admin-1', 'admin@example.com', 'Test Admin', 'super_admin', 'hashed', true)
      RETURNING id
    `;
    adminUserId = adminResult.rows[0].id;

    const managerResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-manager-1', 'manager@example.com', 'Test Manager', 'office_leader', 'hashed', true)
      RETURNING id
    `;
    managerUserId = managerResult.rows[0].id;

    const repResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-rep-1', 'rep1@example.com', 'Test Rep', 'closer', 'hashed', true)
      RETURNING id
    `;
    repUserId = repResult.rows[0].id;
  });

  afterEach(async () => {
    // Clean up test data
    await sql`DELETE FROM office_assignments WHERE user_id LIKE 'test-%'`;
    await sql`DELETE FROM users WHERE id LIKE 'test-%'`;
  });

  it('should show no projects when manager has no office assignments', async () => {
    // Manager should see no projects initially
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toEqual([]);
  });

  it('should show projects from assigned office immediately after assignment', async () => {
    // Admin assigns Office A to manager
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'Office A', 'full')
    `;

    // Update user's sales_office array to match
    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A']
      WHERE id = ${managerUserId}
    `;

    // Manager should immediately see projects from Office A
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toHaveLength(1);
    expect(projects[0][3].value).toBe(1); // Project 1 from Office A
    expect(projects[0][339].value).toBe('Office A');
    
    // Verify the WHERE clause was correctly generated and used
    const { qbClient } = await import('@/lib/quickbase/client');
    const lastCall = (qbClient.queryRecords as any).mock.calls.slice(-1)[0];
    expect(lastCall[0].where).toBe("{339.EX.'Office A'}");
  });

  it('should show projects from multiple offices when assigned', async () => {
    // Admin assigns both Office A and Office B to manager
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES 
        (${managerUserId}, 'Office A', 'full'),
        (${managerUserId}, 'Office B', 'full')
    `;

    // Update user's sales_office array to match
    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A', 'Office B']
      WHERE id = ${managerUserId}
    `;

    // Manager should see projects from both offices
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toHaveLength(2);
    expect(projects.map(p => p[339].value)).toContain('Office A');
    expect(projects.map(p => p[339].value)).toContain('Office B');
  });

  it('should generate correct WHERE clause for office-based filtering', () => {
    // Test the authorization clause generation
    const clause = buildProjectAccessClause(null, 'office_leader', ['Office A', 'Office B']);
    
    expect(clause).toBe("{339.EX.'Office A'} OR {339.EX.'Office B'}");
  });

  it('should handle office removal correctly', async () => {
    // First assign both offices
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES 
        (${managerUserId}, 'Office A', 'full'),
        (${managerUserId}, 'Office B', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A', 'Office B']
      WHERE id = ${managerUserId}
    `;

    // Verify manager sees both projects
    let projects = await getProjectsForUserList(managerUserId, 'office_leader');
    expect(projects).toHaveLength(2);

    // Remove Office A assignment
    await sql`
      DELETE FROM office_assignments 
      WHERE user_id = ${managerUserId} AND office_name = 'Office A'
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office B']
      WHERE id = ${managerUserId}
    `;

    // Manager should now only see projects from Office B
    projects = await getProjectsForUserList(managerUserId, 'office_leader');
    expect(projects).toHaveLength(1);
    expect(projects[0][339].value).toBe('Office B');
  });

  it('should handle office names with special characters', async () => {
    // Test with office name containing single quote
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'O''Brien Office', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['O''Brien Office']
      WHERE id = ${managerUserId}
    `;

    // Should generate properly escaped WHERE clause
    const clause = buildProjectAccessClause(null, 'office_leader', ["O'Brien Office"]);
    expect(clause).toBe("{339.EX.'O''Brien Office'}");
  });

  it('should handle empty office assignments gracefully', async () => {
    // Manager with no office assignments should see no projects
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    expect(projects).toEqual([]);

    // Should generate no-access clause
    const clause = buildProjectAccessClause(null, 'office_leader', []);
    expect(clause).toBe('{3.EQ.0}');
  });

  it('should verify database state after office assignment', async () => {
    // Assign office
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'Office A', 'full')
    `;

    // Verify office_assignments table has the assignment
    const assignmentResult = await sql`
      SELECT * FROM office_assignments 
      WHERE user_id = ${managerUserId} AND office_name = 'Office A'
    `;
    expect(assignmentResult.rows).toHaveLength(1);
    expect(assignmentResult.rows[0].access_level).toBe('full');

    // Verify users.sales_office array is updated
    const userResult = await sql`
      SELECT sales_office FROM users WHERE id = ${managerUserId}
    `;
    expect(userResult.rows[0].sales_office).toContain('Office A');
  });
});
