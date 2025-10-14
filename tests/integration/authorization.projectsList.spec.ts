import { describe, it, expect, vi, beforeEach } from 'vitest';
import { getProjectsForUserList } from '@/lib/quickbase/queries';
import { qbClient } from '@/lib/quickbase/client';
import { __test__ } from '@/lib/quickbase/queries';

// Mock the QuickBase client
const mockQbClient = {
  queryRecords: vi.fn(),
};

vi.mock('@/lib/quickbase/client', () => ({
  qbClient: mockQbClient,
}));

// Mock the SQL helper functions
const mockGetAssignedOffices = vi.fn();
const mockGetManagedUserEmails = vi.fn();
const mockGetUserEmail = vi.fn();

vi.mock('@/lib/quickbase/queries', async () => {
  const actual = await vi.importActual('@/lib/quickbase/queries');
  return {
    ...actual,
    __test__: {
      ...actual.__test__,
      getAssignedOffices: mockGetAssignedOffices,
      getManagedUserEmails: mockGetManagedUserEmails,
    },
  };
});

// Mock the database client
vi.mock('@/lib/db/client', () => ({
  sql: vi.fn(),
}));

// Mock the logger
vi.mock('@/lib/logging/logger', () => ({
  logError: vi.fn(),
}));

describe('Authorization Integration Tests', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    
    // Default mock responses
    mockQbClient.queryRecords.mockResolvedValue({
      data: [
        {
          [3]: { value: 1 }, // RECORD_ID
          [11]: { value: 'KIN-12345' }, // PROJECT_ID
          [145]: { value: 'John Smith' }, // CUSTOMER_NAME
          [2087]: { value: 'Office A' }, // SALES_OFFICE
          [518]: { value: 'closer@example.com' }, // CLOSER_EMAIL
          [331]: { value: 'setter@example.com' }, // SETTER_EMAIL
        },
      ],
      metadata: { totalRecords: 1 },
    });
  });

  describe('Office Leader Authorization', () => {
    it('should filter by provided offices', async () => {
      const result = await getProjectsForUserList(
        'leader123',
        'office_leader',
        undefined, // view
        undefined, // search
        undefined, // sort
        ['Office A', 'Office B'] // salesOffice
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringContaining('{2087.EX.\'Office A\'} OR {2087.EX.\'Office B\'}'),
        })
      );
      expect(result).toBeDefined();
    });

    it('should fetch offices from database when none provided', async () => {
      mockGetAssignedOffices.mockResolvedValue(['Office C', 'Office D']);

      const result = await getProjectsForUserList(
        'leader123',
        'office_leader'
      );

      expect(mockGetAssignedOffices).toHaveBeenCalledWith('leader123');
      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringContaining('{2087.EX.\'Office C\'} OR {2087.EX.\'Office D\'}'),
        })
      );
      expect(result).toBeDefined();
    });

    it('should return no projects when no offices assigned', async () => {
      mockGetAssignedOffices.mockResolvedValue([]);

      const result = await getProjectsForUserList(
        'leader123',
        'office_leader'
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.EQ.0}', // No projects clause
        })
      );
      expect(result).toBeDefined();
    });
  });

  describe('Team Lead Authorization', () => {
    it('should filter by managed user emails', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com', 'user2@example.com']);

      const result = await getProjectsForUserList(
        'teamlead123',
        'team_lead'
      );

      expect(mockGetManagedUserEmails).toHaveBeenCalledWith('teamlead123');
      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /\(\{518\.EX\.'user1@example\.com'\} OR \{518\.EX\.'user2@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\} OR \{331\.EX\.'user2@example\.com'\}\)/
          ),
        })
      );
      expect(result).toBeDefined();
    });

    it('should return no projects when no managed users', async () => {
      mockGetManagedUserEmails.mockResolvedValue([]);

      const result = await getProjectsForUserList(
        'teamlead123',
        'team_lead'
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.EQ.0}', // No projects clause
        })
      );
      expect(result).toBeDefined();
    });
  });

  describe('Rep Authorization (Closer/Setter)', () => {
    it('should filter by user email for closer role', async () => {
      mockGetUserEmail.mockResolvedValue('closer@example.com');

      const result = await getProjectsForUserList(
        'closer123',
        'closer'
      );

      expect(mockGetUserEmail).toHaveBeenCalledWith('closer123');
      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /\(\{518\.EX\.'closer@example\.com'\}\) OR \(\{331\.EX\.'closer@example\.com'\}\)/
          ),
        })
      );
      expect(result).toBeDefined();
    });

    it('should filter by user email for setter role', async () => {
      mockGetUserEmail.mockResolvedValue('setter@example.com');

      const result = await getProjectsForUserList(
        'setter123',
        'setter'
      );

      expect(mockGetUserEmail).toHaveBeenCalledWith('setter123');
      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /\(\{518\.EX\.'setter@example\.com'\}\) OR \(\{331\.EX\.'setter@example\.com'\}\)/
          ),
        })
      );
      expect(result).toBeDefined();
    });

    it('should return no projects when no email found', async () => {
      mockGetUserEmail.mockResolvedValue(null);

      const result = await getProjectsForUserList(
        'closer123',
        'closer'
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.EQ.0}', // No projects clause
        })
      );
      expect(result).toBeDefined();
    });
  });

  describe('Admin Authorization', () => {
    it('should return all projects for super_admin role', async () => {
      const result = await getProjectsForUserList(
        'admin123',
        'super_admin'
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.GT.0}', // All projects clause
        })
      );
      expect(result).toBeDefined();
    });

    it('should return all projects for regional role', async () => {
      const result = await getProjectsForUserList(
        'regional123',
        'regional'
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.GT.0}', // All projects clause
        })
      );
      expect(result).toBeDefined();
    });
  });

  describe('Combined Filters', () => {
    it('should combine role clause with view filter', async () => {
      const result = await getProjectsForUserList(
        'leader123',
        'office_leader',
        'active', // view filter
        undefined, // search
        undefined, // sort
        ['Office A'] // salesOffice
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /\(\{2087\.EX\.'Office A'\}\) AND \(\{255\.EX\.'Active'\} AND \(\{1548\.EX\.'Yes'\} OR \{603\.EX\.'Yes'\}\)\)/
          ),
        })
      );
      expect(result).toBeDefined();
    });

    it('should combine role clause with search filter', async () => {
      const result = await getProjectsForUserList(
        'closer123',
        'closer',
        undefined, // view
        'Smith', // search
        undefined, // sort
        undefined // salesOffice
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /\(\{3\.EQ\.0\}\) AND \(\{145\.CT\.'Smith'\} OR \{11\.CT\.'Smith'\}\)/
          ),
        })
      );
      expect(result).toBeDefined();
    });

    it('should combine role clause with both view and search filters', async () => {
      mockGetUserEmail.mockResolvedValue('closer@example.com');

      const result = await getProjectsForUserList(
        'closer123',
        'closer',
        'on-hold', // view filter
        'Smith', // search filter
        undefined, // sort
        undefined // salesOffice
      );

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /\(\{518\.EX\.'closer@example\.com'\}\) OR \(\{331\.EX\.'closer@example\.com'\}\)\) AND \(\{255\.CT\.'On Hold'\}\) AND \(\{145\.CT\.'Smith'\} OR \{11\.CT\.'Smith'\}\)/
          ),
        })
      );
      expect(result).toBeDefined();
    });
  });

  describe('Parentheses and Precedence', () => {
    it('should properly group OR clauses with parentheses for team lead', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com', 'user2@example.com']);

      const result = await getProjectsForUserList(
        'teamlead123',
        'team_lead',
        'active' // view filter
      );

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have proper parentheses grouping
      expect(whereClause).toMatch(
        /^\(\(\{518\.EX\.'user1@example\.com'\} OR \{518\.EX\.'user2@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\} OR \{331\.EX\.'user2@example\.com'\}\)\) AND/
      );
      expect(result).toBeDefined();
    });

    it('should properly group OR clauses with parentheses for rep roles', async () => {
      mockGetUserEmail.mockResolvedValue('closer@example.com');

      const result = await getProjectsForUserList(
        'closer123',
        'closer',
        'active' // view filter
      );

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have proper parentheses grouping
      expect(whereClause).toMatch(
        /^\(\{518\.EX\.'closer@example\.com'\}\) OR \(\{331\.EX\.'closer@example\.com'\}\)\) AND/
      );
      expect(result).toBeDefined();
    });

    it('should properly group complex filters with parentheses for team lead with ownership and view', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com', 'user2@example.com']);
      mockGetUserEmail.mockResolvedValue('teamlead@example.com');

      const result = await getProjectsForUserList(
        'teamlead123',
        'team_lead',
        'on-hold', // view filter
        undefined, // search
        undefined, // sort
        undefined, // salesOffice
        undefined, // memberEmail
        'team-projects' // ownership filter
      );

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have proper parentheses grouping: ((A OR B)) AND (C) AND (D)
      // Where A = managed emails closer clause, B = managed emails setter clause
      // C = ownership filter (team-projects), D = view filter (on-hold)
      expect(whereClause).toMatch(
        /^\(\(\{518\.EX\.'user1@example\.com'\} OR \{518\.EX\.'user2@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\} OR \{331\.EX\.'user2@example\.com'\}\)\) AND \(\(\{518\.XEX\.'teamlead@example\.com'\} OR \{518\.EX\.''\}\) AND \(\{331\.XEX\.'teamlead@example\.com'\} OR \{331\.EX\.''\}\)\)\) AND \(\{255\.CT\.'On Hold'\}\)/
      );
      expect(result).toBeDefined();
    });
  });

  describe('Error Handling', () => {
    it('should handle database errors gracefully', async () => {
      mockGetAssignedOffices.mockRejectedValue(new Error('Database connection failed'));

      const result = await getProjectsForUserList(
        'leader123',
        'office_leader'
      );

      // Should still call QuickBase with no-projects clause
      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.EQ.0}', // No projects clause due to error
        })
      );
      expect(result).toBeDefined();
    });

    it('should handle QuickBase errors gracefully', async () => {
      mockQbClient.queryRecords.mockRejectedValue(new Error('QuickBase API error'));

      const result = await getProjectsForUserList(
        'admin123',
        'super_admin'
      );

      // Should return empty array on error
      expect(result).toEqual([]);
    });
  });
});
