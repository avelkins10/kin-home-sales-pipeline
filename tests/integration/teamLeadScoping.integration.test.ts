import { describe, it, expect, vi, beforeEach } from 'vitest';
import { getProjectsForUserList } from '@/lib/quickbase/queries';
import { qbClient } from '@/lib/quickbase/client';

// Mock the QuickBase client
const mockQbClient = {
  queryRecords: vi.fn(),
};

vi.mock('@/lib/quickbase/client', () => ({
  qbClient: mockQbClient,
}));

// Mock the SQL helper functions
const mockGetManagedUserEmails = vi.fn();

vi.mock('@/lib/quickbase/queries', async () => {
  const actual = await vi.importActual('@/lib/quickbase/queries');
  return {
    ...actual,
    __test__: {
      ...actual.__test__,
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

describe('Team Lead Scoping Integration Tests', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    
    // Default mock responses
    mockQbClient.queryRecords.mockResolvedValue({
      data: [
        {
          [3]: { value: 1 }, // RECORD_ID
          [11]: { value: 'KIN-12345' }, // PROJECT_ID
          [145]: { value: 'John Smith' }, // CUSTOMER_NAME
          [518]: { value: 'user1@example.com' }, // CLOSER_EMAIL
          [331]: { value: 'user2@example.com' }, // SETTER_EMAIL
        },
      ],
      metadata: { totalRecords: 1 },
    });
  });

  describe('Team Lead Authorization with Correct Parentheses', () => {
    it('should generate correct WHERE clause with proper parentheses for team lead', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com', 'user2@example.com']);

      await getProjectsForUserList('teamlead123', 'team_lead');

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /^\(\(\{518\.EX\.'user1@example\.com'\} OR \{518\.EX\.'user2@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\} OR \{331\.EX\.'user2@example\.com'\}\)\)$/
          ),
        })
      );
    });

    it('should combine team lead clause with view filter maintaining correct precedence', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com', 'user2@example.com']);

      await getProjectsForUserList('teamlead123', 'team_lead', 'active');

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have proper parentheses grouping: ((roleClause)) AND (viewFilter)
      expect(whereClause).toMatch(
        /^\(\(\{518\.EX\.'user1@example\.com'\} OR \{518\.EX\.'user2@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\} OR \{331\.EX\.'user2@example\.com'\}\)\) AND/
      );
      
      // Should include the view filter
      expect(whereClause).toContain('{255.EX.\'Active\'}');
    });

    it('should combine team lead clause with search filter maintaining correct precedence', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com']);

      await getProjectsForUserList('teamlead123', 'team_lead', undefined, 'Smith');

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have proper parentheses grouping: ((roleClause)) AND (searchFilter)
      expect(whereClause).toMatch(
        /^\(\(\{518\.EX\.'user1@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\}\)\) AND/
      );
      
      // Should include the search filter
      expect(whereClause).toContain('{145.CT.\'Smith\'}');
      expect(whereClause).toContain('{11.CT.\'Smith\'}');
    });

    it('should combine team lead clause with both view and search filters maintaining correct precedence', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com', 'user2@example.com']);

      await getProjectsForUserList('teamlead123', 'team_lead', 'on-hold', 'Smith');

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have proper parentheses grouping: ((roleClause)) AND (viewFilter) AND (searchFilter)
      expect(whereClause).toMatch(
        /^\(\(\{518\.EX\.'user1@example\.com'\} OR \{518\.EX\.'user2@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\} OR \{331\.EX\.'user2@example\.com'\}\)\) AND/
      );
      
      // Should include both filters
      expect(whereClause).toContain('{255.CT.\'On Hold\'}');
      expect(whereClause).toContain('{145.CT.\'Smith\'}');
      expect(whereClause).toContain('{11.CT.\'Smith\'}');
    });
  });

  describe('Edge Cases and Error Handling', () => {
    it('should handle team lead with no managed users', async () => {
      mockGetManagedUserEmails.mockResolvedValue([]);

      await getProjectsForUserList('teamlead123', 'team_lead');

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.EQ.0}', // No projects clause
        })
      );
    });

    it('should handle team lead with single managed user', async () => {
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com']);

      await getProjectsForUserList('teamlead123', 'team_lead');

      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: expect.stringMatching(
            /^\(\(\{518\.EX\.'user1@example\.com'\}\) OR \(\{331\.EX\.'user1@example\.com'\}\)\)$/
          ),
        })
      );
    });

    it('should handle team lead with many managed users', async () => {
      const manyEmails = Array.from({ length: 10 }, (_, i) => `user${i}@example.com`);
      mockGetManagedUserEmails.mockResolvedValue(manyEmails);

      await getProjectsForUserList('teamlead123', 'team_lead');

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have proper parentheses grouping
      expect(whereClause).toMatch(/^\(\(.*\) OR \(.*\)\)$/);
      
      // Should include all emails
      expect(whereClause).toContain('user0@example.com');
      expect(whereClause).toContain('user9@example.com');
      
      // Should have correct field IDs
      expect(whereClause).toContain('518'); // CLOSER_EMAIL
      expect(whereClause).toContain('331'); // SETTER_EMAIL
    });

    it('should handle database errors gracefully', async () => {
      mockGetManagedUserEmails.mockRejectedValue(new Error('Database connection failed'));

      await getProjectsForUserList('teamlead123', 'team_lead');

      // Should still call QuickBase with no-projects clause
      expect(mockQbClient.queryRecords).toHaveBeenCalledWith(
        expect.objectContaining({
          where: '{3.EQ.0}', // No projects clause due to error
        })
      );
    });
  });

  describe('SQL Injection Safety', () => {
    it('should properly escape single quotes in managed user emails', async () => {
      mockGetManagedUserEmails.mockResolvedValue(["user'name@example.com", "another'user@example.com"]);

      await getProjectsForUserList('teamlead123', 'team_lead');

      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      
      // Should have escaped single quotes (doubled)
      expect(whereClause).toContain("user''name@example.com");
      expect(whereClause).toContain("another''user@example.com");
      
      // Should not contain unescaped single quotes
      expect(whereClause).not.toContain("user'name@example.com");
      expect(whereClause).not.toContain("another'user@example.com");
    });
  });

  describe('Performance with Large Datasets', () => {
    it('should handle large number of managed users efficiently', async () => {
      const startTime = Date.now();
      const manyEmails = Array.from({ length: 50 }, (_, i) => `user${i}@example.com`);
      mockGetManagedUserEmails.mockResolvedValue(manyEmails);

      await getProjectsForUserList('teamlead123', 'team_lead');

      const endTime = Date.now();
      const duration = endTime - startTime;
      
      // Should complete quickly (less than 100ms)
      expect(duration).toBeLessThan(100);
      
      // Should still produce valid clause
      const whereClause = mockQbClient.queryRecords.mock.calls[0][0].where;
      expect(whereClause).toMatch(/^\(\(.*\) OR \(.*\)\)$/);
    });
  });
});
