/**
 * Comprehensive Unit Tests for buildProjectAccessClause()
 * 
 * Verifies that buildProjectAccessClause() generates correct QuickBase WHERE clauses
 * for all role types and edge cases.
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

// Mock the logger to capture log calls
vi.mock('@/lib/logging/logger', () => ({
  logInfo: vi.fn(),
  logWarn: vi.fn(),
}));

describe('buildProjectAccessClause', () => {
  let mockLogInfo: any;
  let mockLogWarn: any;

  beforeEach(async () => {
    vi.clearAllMocks();
    const logger = await import('@/lib/logging/logger');
    mockLogInfo = vi.mocked(logger.logInfo);
    mockLogWarn = vi.mocked(logger.logWarn);
  });

  describe('Admin Roles', () => {
    it('should return all-projects clause for super_admin', () => {
      const clause = buildProjectAccessClause(null, 'super_admin');
      
      expect(clause).toBe('{3.GT.0}');
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Admin role detected, granting all-projects access',
        { role: 'super_admin' }
      );
    });

    it('should return all-projects clause for regional', () => {
      const clause = buildProjectAccessClause(null, 'regional');
      
      expect(clause).toBe('{3.GT.0}');
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Admin role detected, granting all-projects access',
        { role: 'regional' }
      );
    });
  });

  describe('Office-Based Roles', () => {
    it('should return office filter for office_leader with single office', () => {
      const clause = buildProjectAccessClause(null, 'office_leader', ['Office A']);

      expect(clause).toBe("{339.EX.'Office A'}");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Building access clause',
        expect.objectContaining({ role: 'office_leader', officeCount: 1 })
      );
    });

    it('should return office filter for office_leader with multiple offices', () => {
      const clause = buildProjectAccessClause(null, 'office_leader', ['Office A', 'Office B']);

      expect(clause).toBe("{339.EX.'Office A'} OR {339.EX.'Office B'}");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Building access clause',
        expect.objectContaining({ role: 'office_leader', officeCount: 2 })
      );
    });

    it('should return no-projects clause for office_leader with no offices', () => {
      const clause = buildProjectAccessClause(null, 'office_leader', []);
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Office-based role with NO assigned offices',
        { role: 'office_leader', userId: 'redacted' }
      );
    });

    it('should return no-projects clause for office_leader with undefined offices', () => {
      const clause = buildProjectAccessClause(null, 'office_leader', undefined);
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Office-based role with NO assigned offices',
        { role: 'office_leader', userId: 'redacted' }
      );
    });

    it('should return office filter for area_director with multiple offices', () => {
      const clause = buildProjectAccessClause(null, 'area_director', ['Office A', 'Office B', 'Office C']);

      expect(clause).toBe("{339.EX.'Office A'} OR {339.EX.'Office B'} OR {339.EX.'Office C'}");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Building access clause',
        expect.objectContaining({ role: 'area_director', officeCount: 3 })
      );
    });

    it('should return office filter for divisional with single office', () => {
      const clause = buildProjectAccessClause(null, 'divisional', ['Office A']);

      expect(clause).toBe("{339.EX.'Office A'}");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Building access clause',
        expect.objectContaining({ role: 'divisional', officeCount: 1 })
      );
    });
  });

  describe('Team Lead Role', () => {
    it('should return email filter for team_lead with single managed email', () => {
      const clause = buildProjectAccessClause(null, 'team_lead', undefined, ['user1@example.com']);
      
      expect(clause).toBe("({356.EX.'user1@example.com'}) OR ({334.EX.'user1@example.com'})");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Team lead with managed users',
        { managedUserCount: 1 }
      );
    });

    it('should return email filter for team_lead with multiple managed emails', () => {
      const clause = buildProjectAccessClause(null, 'team_lead', undefined, ['user1@example.com', 'user2@example.com']);
      
      expect(clause).toBe("({356.EX.'user1@example.com'} OR {356.EX.'user2@example.com'}) OR ({334.EX.'user1@example.com'} OR {334.EX.'user2@example.com'})");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Team lead with managed users',
        { managedUserCount: 2 }
      );
    });

    it('should return no-projects clause for team_lead with no managed emails', () => {
      const clause = buildProjectAccessClause(null, 'team_lead', undefined, []);
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Team lead with NO managed users',
        { role: 'team_lead' }
      );
    });

    it('should return no-projects clause for team_lead with undefined managed emails', () => {
      const clause = buildProjectAccessClause(null, 'team_lead', undefined, undefined);
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Team lead with NO managed users',
        { role: 'team_lead' }
      );
    });
  });

  describe('Rep Roles', () => {
    it('should return email filter for closer with email', () => {
      const clause = buildProjectAccessClause('closer@example.com', 'closer');
      
      expect(clause).toBe("({356.EX.'closer@example.com'}) OR ({334.EX.'closer@example.com'})");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Rep role with email, filtering by email',
        { role: 'closer' }
      );
    });

    it('should return email filter for setter with email', () => {
      const clause = buildProjectAccessClause('setter@example.com', 'setter');
      
      expect(clause).toBe("({356.EX.'setter@example.com'}) OR ({334.EX.'setter@example.com'})");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Rep role with email, filtering by email',
        { role: 'setter' }
      );
    });

    it('should return no-projects clause for closer without email', () => {
      const clause = buildProjectAccessClause(null, 'closer');
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Rep role with no email, denying access',
        { role: 'closer' }
      );
    });

    it('should return no-projects clause for setter without email', () => {
      const clause = buildProjectAccessClause(null, 'setter');
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Rep role with no email, denying access',
        { role: 'setter' }
      );
    });
  });

  describe('Edge Cases', () => {
    it('should escape single quotes in email addresses', () => {
      const clause = buildProjectAccessClause("user'@example.com", 'closer');
      
      expect(clause).toBe("({356.EX.'user''@example.com'}) OR ({334.EX.'user''@example.com'})");
    });

    it('should escape single quotes in office names', () => {
      const clause = buildProjectAccessClause(null, 'office_leader', ["O'Brien Office"]);
      
      expect(clause).toBe("{339.EX.'O''Brien Office'}");
    });

    it('should default to closer filter for unknown role', () => {
      const clause = buildProjectAccessClause('user@example.com', 'unknown_role');
      
      expect(clause).toBe("{356.EX.'user@example.com'}");
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Unknown role, defaulting to closer email filter',
        { role: 'unknown_role' }
      );
    });

    it('should return no-projects clause for coordinator role', () => {
      const clause = buildProjectAccessClause('coord@example.com', 'coordinator');
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Coordinator role has no project visibility by design',
        { role: 'coordinator' }
      );
    });

    it('should return no-projects clause for empty string email', () => {
      const clause = buildProjectAccessClause('', 'closer');
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Rep role with no email, denying access',
        { role: 'closer' }
      );
    });

    it('should return no-projects clause for whitespace-only email', () => {
      const clause = buildProjectAccessClause('   ', 'closer');
      
      expect(clause).toBe('{3.EQ.0}');
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Rep role with no email, denying access',
        { role: 'closer' }
      );
    });
  });

  describe('Logging Verification', () => {
    it('should log entry for each role type', () => {
      buildProjectAccessClause('user@example.com', 'closer');
      
      expect(mockLogInfo).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Building access clause',
        { role: 'closer', officeCount: 0, managedEmailCount: 0 }
      );
    });

    it('should log warning for office role with no offices', () => {
      buildProjectAccessClause(null, 'office_leader', []);
      
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Office-based role with NO assigned offices',
        { role: 'office_leader', userId: 'redacted' }
      );
    });

    it('should log warning for team lead with no managed users', () => {
      buildProjectAccessClause(null, 'team_lead', undefined, []);
      
      expect(mockLogWarn).toHaveBeenCalledWith(
        '[PROJECT_AUTHORIZATION] Team lead with NO managed users',
        { role: 'team_lead' }
      );
    });

    it('should not log raw emails in production logs', () => {
      buildProjectAccessClause('user@example.com', 'closer');
      
      // Verify that the log calls don't contain raw email addresses
      const logCalls = [...mockLogInfo.mock.calls, ...mockLogWarn.mock.calls];
      const hasRawEmail = logCalls.some(call => 
        JSON.stringify(call).includes('user@example.com')
      );
      
      expect(hasRawEmail).toBe(false);
    });
  });

  describe('Clause Validation', () => {
    it('should handle empty clause gracefully', () => {
      // This test would require mocking the internal logic to return empty clause
      // For now, we test that the function always returns a valid clause
      const clause = buildProjectAccessClause(null, 'office_leader', []);
      
      expect(clause).toBeTruthy();
      expect(clause.length).toBeGreaterThan(0);
    });

    it('should validate clause format', () => {
      const clause = buildProjectAccessClause('user@example.com', 'closer');
      
      // Should contain field IDs and proper QuickBase syntax
      expect(clause).toContain('{');
      expect(clause).toContain('}');
      expect(clause).toContain('356'); // CLOSER_EMAIL field
      expect(clause).toContain('334'); // SETTER_EMAIL field
    });
  });

  describe('Field ID Verification', () => {
    it('should use correct field IDs for office filtering', () => {
      const clause = buildProjectAccessClause(null, 'office_leader', ['Office A']);
      
      expect(clause).toContain('339'); // SALES_OFFICE field ID
    });

    it('should use correct field IDs for email filtering', () => {
      const clause = buildProjectAccessClause('user@example.com', 'closer');
      
      expect(clause).toContain('356'); // CLOSER_EMAIL field ID
      expect(clause).toContain('334'); // SETTER_EMAIL field ID
    });

    it('should use correct field ID for record ID filtering', () => {
      const clause = buildProjectAccessClause(null, 'super_admin');
      
      expect(clause).toContain('3'); // RECORD_ID field ID
    });
  });
});