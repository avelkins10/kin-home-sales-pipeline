/**
 * Office-Based Visibility Verification Test
 * 
 * Verifies that office-based roles (office_leader, area_director, divisional) 
 * see ALL projects in their assigned offices, including projects from inactive users.
 */

import { describe, it, expect, beforeEach, afterEach } from 'vitest';
import { sql } from '@/lib/db/client';
import { getProjectsForUserList } from '@/lib/quickbase/queries';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';

// Mock QuickBase client with test projects
const mockQuickBaseResponse = {
  data: [
    {
      [3]: { value: 1 }, // RECORD_ID
      [2087]: { value: 'Office A' }, // SALES_OFFICE
      [518]: { value: 'active-rep@example.com' }, // CLOSER_EMAIL
      [331]: { value: 'active-setter@example.com' }, // SETTER_EMAIL
    },
    {
      [3]: { value: 2 }, // RECORD_ID
      [2087]: { value: 'Office A' }, // SALES_OFFICE
      [518]: { value: 'inactive-rep@example.com' }, // CLOSER_EMAIL
      [331]: { value: 'inactive-setter@example.com' }, // SETTER_EMAIL
    },
    {
      [3]: { value: 3 }, // RECORD_ID
      [2087]: { value: 'Office B' }, // SALES_OFFICE
      [518]: { value: 'other-rep@example.com' }, // CLOSER_EMAIL
      [331]: { value: 'other-setter@example.com' }, // SETTER_EMAIL
    },
    {
      [3]: { value: 4 }, // RECORD_ID
      [2087]: { value: 'Office C' }, // SALES_OFFICE
      [518]: { value: 'third-rep@example.com' }, // CLOSER_EMAIL
      [331]: { value: 'third-setter@example.com' }, // SETTER_EMAIL
    },
  ],
  metadata: { totalRecords: 4 }
};

// Mock the QuickBase client with WHERE clause filtering
vi.mock('@/lib/quickbase/client', () => ({
  qbClient: {
    queryRecords: vi.fn().mockImplementation(({ where }) => {
      // Filter mock data based on WHERE clause
      let filteredData = mockQuickBaseResponse.data;
      
      if (where) {
        // Extract all office names with regex: /\{2087\.EX\.'((?:[^']|'')*)'\}/g
        const officeMatches = where.match(/\{2087\.EX\.'((?:[^']|'')*)'\}/g);
        
        if (officeMatches && officeMatches.length > 0) {
          // Extract office names and unescape doubled quotes
          const officeNames = officeMatches.map(match => {
            const officeName = match.match(/\{2087\.EX\.'((?:[^']|'')*)'\}/)?.[1];
            return officeName ? officeName.replace(/''/g, "'") : null;
          }).filter(Boolean);
          
          // Filter by any of the found offices
          filteredData = mockQuickBaseResponse.data.filter(project => 
            officeNames.includes(project[2087]?.value)
          );
        } else if (where.includes("3.EQ.0")) {
          // No access clause
          filteredData = [];
        } else if (where.includes("3.GT.0")) {
          // All projects clause
          filteredData = mockQuickBaseResponse.data;
        } else {
          // If WHERE clause contains office field but no matches found, return empty array
          if (where.includes("2087.EX")) {
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

describe('Office-Based Visibility Verification', () => {
  let managerUserId: string;
  let areaDirectorUserId: string;
  let divisionalUserId: string;
  let activeRepUserId: string;
  let inactiveRepUserId: string;

  beforeEach(async () => {
    // Clean up any existing test data
    await sql`DELETE FROM office_assignments WHERE user_id LIKE 'test-%'`;
    await sql`DELETE FROM users WHERE id LIKE 'test-%'`;

    // Create test users
    const managerResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-manager-1', 'manager@example.com', 'Test Manager', 'office_leader', 'hashed', true)
      RETURNING id
    `;
    managerUserId = managerResult.rows[0].id;

    const areaDirectorResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-area-director-1', 'area-director@example.com', 'Test Area Director', 'area_director', 'hashed', true)
      RETURNING id
    `;
    areaDirectorUserId = areaDirectorResult.rows[0].id;

    const divisionalResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-divisional-1', 'divisional@example.com', 'Test Divisional', 'divisional', 'hashed', true)
      RETURNING id
    `;
    divisionalUserId = divisionalResult.rows[0].id;

    const activeRepResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-active-rep-1', 'active-rep@example.com', 'Active Rep', 'closer', 'hashed', true)
      RETURNING id
    `;
    activeRepUserId = activeRepResult.rows[0].id;

    const inactiveRepResult = await sql`
      INSERT INTO users (id, email, name, role, password_hash, is_active)
      VALUES ('test-inactive-rep-1', 'inactive-rep@example.com', 'Inactive Rep', 'closer', 'hashed', false)
      RETURNING id
    `;
    inactiveRepUserId = inactiveRepResult.rows[0].id;
  });

  afterEach(async () => {
    // Clean up test data
    await sql`DELETE FROM office_assignments WHERE user_id LIKE 'test-%'`;
    await sql`DELETE FROM users WHERE id LIKE 'test-%'`;
  });

  it('should show projects from active rep to office manager', async () => {
    // Assign Office A to manager
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'Office A', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A']
      WHERE id = ${managerUserId}
    `;

    // Manager should see projects from Office A (including from active rep)
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toHaveLength(2); // Both projects from Office A
    expect(projects.map(p => p[2087].value)).toEqual(['Office A', 'Office A']);
    
    // Verify the WHERE clause was correctly generated and used
    const { qbClient } = await import('@/lib/quickbase/client');
    const lastCall = (qbClient.queryRecords as any).mock.calls.slice(-1)[0];
    expect(lastCall[0].where).toBe("{2087.EX.'Office A'}");
  });

  it('should show projects from inactive rep to office manager', async () => {
    // Assign Office A to manager
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'Office A', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A']
      WHERE id = ${managerUserId}
    `;

    // Manager should see projects from Office A (including from inactive rep)
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toHaveLength(2); // Both projects from Office A
    // Should include project from inactive rep (project 2)
    expect(projects.find(p => p[3].value === 2)).toBeDefined();
    expect(projects.find(p => p[518].value === 'inactive-rep@example.com')).toBeDefined();
  });

  it('should NOT show projects from other offices to office manager', async () => {
    // Assign only Office A to manager
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'Office A', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A']
      WHERE id = ${managerUserId}
    `;

    // Manager should NOT see projects from Office B or Office C
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toHaveLength(2); // Only Office A projects
    expect(projects.map(p => p[2087].value)).toEqual(['Office A', 'Office A']);
    expect(projects.find(p => p[2087].value === 'Office B')).toBeUndefined();
    expect(projects.find(p => p[2087].value === 'Office C')).toBeUndefined();
  });

  it('should verify office-based filtering uses SALES_OFFICE field', () => {
    // Test that the WHERE clause filters by SALES_OFFICE field (2087)
    const clause = buildProjectAccessClause(null, 'office_leader', ['Office A']);
    
    expect(clause).toBe("{2087.EX.'Office A'}");
    expect(clause).toContain('2087'); // SALES_OFFICE field ID
  });

  it('should verify office-based filtering does NOT filter by user.is_active', async () => {
    // Assign Office A to manager
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'Office A', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A']
      WHERE id = ${managerUserId}
    `;

    // Manager should see projects from both active and inactive users
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toHaveLength(2);
    // Should include project from active rep
    expect(projects.find(p => p[518].value === 'active-rep@example.com')).toBeDefined();
    // Should include project from inactive rep
    expect(projects.find(p => p[518].value === 'inactive-rep@example.com')).toBeDefined();
  });

  it('should verify office-based filtering does NOT filter by CLOSER_EMAIL or SETTER_EMAIL', () => {
    // Test that office-based roles don't filter by email fields
    const clause = buildProjectAccessClause(null, 'office_leader', ['Office A']);
    
    expect(clause).toBe("{2087.EX.'Office A'}");
    expect(clause).not.toContain('518'); // CLOSER_EMAIL field ID
    expect(clause).not.toContain('331'); // SETTER_EMAIL field ID
  });

  it('should show projects from multiple offices for area director', async () => {
    // Assign Office A and Office B to area director
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES 
        (${areaDirectorUserId}, 'Office A', 'full'),
        (${areaDirectorUserId}, 'Office B', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A', 'Office B']
      WHERE id = ${areaDirectorUserId}
    `;

    // Area director should see projects from both offices
    const projects = await getProjectsForUserList(areaDirectorUserId, 'area_director');
    
    expect(projects).toHaveLength(3); // Projects from Office A and Office B
    expect(projects.map(p => p[2087].value)).toContain('Office A');
    expect(projects.map(p => p[2087].value)).toContain('Office B');
    expect(projects.find(p => p[2087].value === 'Office C')).toBeUndefined();
  });

  it('should show projects from all assigned offices for divisional', async () => {
    // Assign all three offices to divisional
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES 
        (${divisionalUserId}, 'Office A', 'full'),
        (${divisionalUserId}, 'Office B', 'full'),
        (${divisionalUserId}, 'Office C', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A', 'Office B', 'Office C']
      WHERE id = ${divisionalUserId}
    `;

    // Divisional should see projects from all three offices
    const projects = await getProjectsForUserList(divisionalUserId, 'divisional');
    
    expect(projects).toHaveLength(4); // All projects
    expect(projects.map(p => p[2087].value)).toContain('Office A');
    expect(projects.map(p => p[2087].value)).toContain('Office B');
    expect(projects.map(p => p[2087].value)).toContain('Office C');
  });

  it('should generate correct WHERE clause for multiple offices', () => {
    // Test WHERE clause generation for multiple offices
    const clause = buildProjectAccessClause(null, 'area_director', ['Office A', 'Office B']);
    
    expect(clause).toBe("{2087.EX.'Office A'} OR {2087.EX.'Office B'}");
  });

  it('should handle office names with special characters in WHERE clause', () => {
    // Test office names with single quotes
    const clause = buildProjectAccessClause(null, 'office_leader', ["O'Brien Office"]);
    
    expect(clause).toBe("{2087.EX.'O''Brien Office'}");
  });

  it('should return no projects when no offices assigned', async () => {
    // Manager with no office assignments
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toEqual([]);
  });

  it('should generate no-access clause when no offices assigned', () => {
    // Test clause generation for no offices
    const clause = buildProjectAccessClause(null, 'office_leader', []);
    
    expect(clause).toBe('{3.EQ.0}');
  });

  it('should verify inactive users cannot log in but their projects remain visible', async () => {
    // Verify inactive user exists but is inactive
    const inactiveUserResult = await sql`
      SELECT is_active FROM users WHERE id = ${inactiveRepUserId}
    `;
    expect(inactiveUserResult.rows[0].is_active).toBe(false);

    // Assign Office A to manager
    await sql`
      INSERT INTO office_assignments (user_id, office_name, access_level)
      VALUES (${managerUserId}, 'Office A', 'full')
    `;

    await sql`
      UPDATE users 
      SET sales_office = ARRAY['Office A']
      WHERE id = ${managerUserId}
    `;

    // Manager should still see projects from inactive user
    const projects = await getProjectsForUserList(managerUserId, 'office_leader');
    
    expect(projects).toHaveLength(2);
    expect(projects.find(p => p[518].value === 'inactive-rep@example.com')).toBeDefined();
  });
});
