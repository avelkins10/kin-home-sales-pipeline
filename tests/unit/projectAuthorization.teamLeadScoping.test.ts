import { describe, it, expect } from 'vitest';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

describe('Team Lead Scoping - OR Grouping and Parentheses', () => {
  describe('Correct OR grouping for team lead with managed users', () => {
    it('should properly group OR clauses with parentheses for single managed user', () => {
      const managedEmails = ['user1@example.com'];
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      // Expected: (({518.EX.'user1@example.com'}) OR ({331.EX.'user1@example.com'}))
      const expectedClause = `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user1@example.com'}) OR ({${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user1@example.com'})`;
      
      expect(clause).toBe(expectedClause);
    });

    it('should properly group OR clauses with parentheses for multiple managed users', () => {
      const managedEmails = ['user1@example.com', 'user2@example.com', 'user3@example.com'];
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      // Expected: (({518.EX.'user1@example.com'} OR {518.EX.'user2@example.com'} OR {518.EX.'user3@example.com'}) OR ({331.EX.'user1@example.com'} OR {331.EX.'user2@example.com'} OR {331.EX.'user3@example.com'}))
      const expectedCloserClause = `{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user1@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user2@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user3@example.com'}`;
      const expectedSetterClause = `{${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user1@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user2@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user3@example.com'}`;
      const expectedClause = `(${expectedCloserClause}) OR (${expectedSetterClause})`;
      
      expect(clause).toBe(expectedClause);
    });

    it('should handle complex email addresses with special characters', () => {
      const managedEmails = ['user.name+tag@example.com', 'user_name@sub.example.com'];
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      // Expected: (({518.EX.'user.name+tag@example.com'} OR {518.EX.'user_name@sub.example.com'}) OR ({331.EX.'user.name+tag@example.com'} OR {331.EX.'user_name@sub.example.com'}))
      const expectedCloserClause = `{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user.name+tag@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user_name@sub.example.com'}`;
      const expectedSetterClause = `{${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user.name+tag@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user_name@sub.example.com'}`;
      const expectedClause = `(${expectedCloserClause}) OR (${expectedSetterClause})`;
      
      expect(clause).toBe(expectedClause);
    });
  });

  describe('Precedence verification with view filters', () => {
    it('should maintain correct precedence when combined with view filter', () => {
      const managedEmails = ['user1@example.com', 'user2@example.com'];
      const roleClause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      // Simulate combining with a view filter (this would be done in the calling function)
      const viewFilter = `{${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Active'}`;
      const combinedClause = `(${roleClause}) AND ${viewFilter}`;
      
      // Expected: ((({518.EX.'user1@example.com'} OR {518.EX.'user2@example.com'}) OR ({331.EX.'user1@example.com'} OR {331.EX.'user2@example.com'})) AND {255.EX.'Active'})
      const expectedCloserClause = `{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user1@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user2@example.com'}`;
      const expectedSetterClause = `{${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user1@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user2@example.com'}`;
      const expectedRoleClause = `(${expectedCloserClause}) OR (${expectedSetterClause})`;
      const expectedCombinedClause = `(${expectedRoleClause}) AND ${viewFilter}`;
      
      expect(combinedClause).toBe(expectedCombinedClause);
    });

    it('should maintain correct precedence when combined with search filter', () => {
      const managedEmails = ['user1@example.com'];
      const roleClause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      // Simulate combining with a search filter
      const searchFilter = `({${PROJECT_FIELDS.CUSTOMER_NAME}.CT.'Smith'} OR {${PROJECT_FIELDS.PROJECT_ID}.CT.'Smith'})`;
      const combinedClause = `(${roleClause}) AND ${searchFilter}`;
      
      // Expected: ((({518.EX.'user1@example.com'}) OR ({331.EX.'user1@example.com'})) AND ({145.CT.'Smith'} OR {11.CT.'Smith'}))
      const expectedCloserClause = `{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user1@example.com'}`;
      const expectedSetterClause = `{${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user1@example.com'}`;
      const expectedRoleClause = `(${expectedCloserClause}) OR (${expectedSetterClause})`;
      const expectedCombinedClause = `(${expectedRoleClause}) AND ${searchFilter}`;
      
      expect(combinedClause).toBe(expectedCombinedClause);
    });

    it('should maintain correct precedence when combined with both view and search filters', () => {
      const managedEmails = ['user1@example.com', 'user2@example.com'];
      const roleClause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      // Simulate combining with both view and search filters
      const viewFilter = `{${PROJECT_FIELDS.PROJECT_STATUS}.EX.'Active'}`;
      const searchFilter = `({${PROJECT_FIELDS.CUSTOMER_NAME}.CT.'Smith'} OR {${PROJECT_FIELDS.PROJECT_ID}.CT.'Smith'})`;
      const combinedClause = `(${roleClause}) AND ${viewFilter} AND ${searchFilter}`;
      
      // Expected: ((({518.EX.'user1@example.com'} OR {518.EX.'user2@example.com'}) OR ({331.EX.'user1@example.com'} OR {331.EX.'user2@example.com'})) AND {255.EX.'Active'} AND ({145.CT.'Smith'} OR {11.CT.'Smith'}))
      const expectedCloserClause = `{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user1@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user2@example.com'}`;
      const expectedSetterClause = `{${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user1@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user2@example.com'}`;
      const expectedRoleClause = `(${expectedCloserClause}) OR (${expectedSetterClause})`;
      const expectedCombinedClause = `(${expectedRoleClause}) AND ${viewFilter} AND ${searchFilter}`;
      
      expect(combinedClause).toBe(expectedCombinedClause);
    });
  });

  describe('Edge cases and error handling', () => {
    it('should handle empty managed emails array', () => {
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, []);
      expect(clause).toBe('{3.EQ.0}'); // No projects clause
    });

    it('should handle undefined managed emails', () => {
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, undefined);
      expect(clause).toBe('{3.EQ.0}'); // No projects clause
    });

    it('should handle null managed emails', () => {
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, null as any);
      expect(clause).toBe('{3.EQ.0}'); // No projects clause
    });

    it('should handle emails with single quotes (SQL injection safety)', () => {
      const managedEmails = ["user'name@example.com", "another'user@example.com"];
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      // Expected: (({518.EX.'user''name@example.com'} OR {518.EX.'another''user@example.com'}) OR ({331.EX.'user''name@example.com'} OR {331.EX.'another''user@example.com'}))
      const expectedCloserClause = `{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'user''name@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'another''user@example.com'}`;
      const expectedSetterClause = `{${PROJECT_FIELDS.SETTER_EMAIL}.EX.'user''name@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'another''user@example.com'}`;
      const expectedClause = `(${expectedCloserClause}) OR (${expectedSetterClause})`;
      
      expect(clause).toBe(expectedClause);
    });
  });

  describe('Comparison with other roles', () => {
    it('should have different clause structure than rep roles', () => {
      const teamLeadClause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, ['user1@example.com']);
      const closerClause = buildProjectAccessClause('closer@example.com', 'closer');
      
      // Team lead clause should have the complex OR structure
      expect(teamLeadClause).toMatch(/^\(.*\) OR \(.*\)$/);
      
      // Closer clause should also have OR structure but simpler
      expect(closerClause).toMatch(/^\(.*\) OR \(.*\)$/);
      
      // But they should be different
      expect(teamLeadClause).not.toBe(closerClause);
    });

    it('should have different clause structure than office-based roles', () => {
      const teamLeadClause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, ['user1@example.com']);
      const officeLeaderClause = buildProjectAccessClause('leader@example.com', 'office_leader', ['Office A']);
      
      // Team lead clause should have email-based OR structure
      expect(teamLeadClause).toMatch(/^\(.*\) OR \(.*\)$/);
      expect(teamLeadClause).toContain('518'); // CLOSER_EMAIL field
      expect(teamLeadClause).toContain('331'); // SETTER_EMAIL field
      
      // Office leader clause should have office-based structure
      expect(officeLeaderClause).toMatch(/^\{339\.EX\.'Office A'\}$/); // SALES_OFFICE field
    });
  });

  describe('Performance considerations', () => {
    it('should handle large number of managed users efficiently', () => {
      const managedEmails = Array.from({ length: 100 }, (_, i) => `user${i}@example.com`);
      const startTime = Date.now();
      
      const clause = buildProjectAccessClause('teamlead@example.com', 'team_lead', undefined, managedEmails);
      
      const endTime = Date.now();
      const duration = endTime - startTime;
      
      // Should complete quickly (less than 100ms)
      expect(duration).toBeLessThan(100);
      
      // Should still produce valid clause
      expect(clause).toMatch(/^\(.*\) OR \(.*\)$/);
      expect(clause).toContain('user0@example.com');
      expect(clause).toContain('user99@example.com');
    });
  });
});
