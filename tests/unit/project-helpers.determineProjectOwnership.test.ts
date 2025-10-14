import { describe, it, expect } from 'vitest';
import { determineProjectOwnership } from '@/lib/utils/project-helpers';

describe('determineProjectOwnership', () => {
  describe('User is Owner', () => {
    it('should return "mine" when user is closer', () => {
      const result = determineProjectOwnership(
        'john@example.com',
        'john@example.com',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });

    it('should return "mine" when user is setter', () => {
      const result = determineProjectOwnership(
        'jane@example.com',
        'john@example.com',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });

    it('should return "mine" when user is both closer and setter', () => {
      const result = determineProjectOwnership(
        'john@example.com',
        'john@example.com',
        'john@example.com',
        'John Doe',
        'John Doe'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });
  });

  describe('Case Insensitivity', () => {
    it('should handle email case mismatch (uppercase user)', () => {
      const result = determineProjectOwnership(
        'JOHN@EXAMPLE.COM',
        'john@example.com',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });

    it('should handle email case mismatch (mixed case)', () => {
      const result = determineProjectOwnership(
        'John@Example.Com',
        'john@example.com',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });
  });

  describe('Team Member Projects', () => {
    it('should return team-closer when project has closer', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        'john@example.com',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'team-closer',
        displayName: 'John Doe',
      });
    });

    it('should return team-setter when project has setter only', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        null,
        'jane@example.com',
        null,
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'team-setter',
        displayName: 'Jane Smith',
      });
    });

    it('should prefer closer over setter for display', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        'john@example.com',
        'jane@example.com',
        'John',
        'Jane'
      );
      
      expect(result).toEqual({
        status: 'team-closer',
        displayName: 'John',
      });
    });
  });

  describe('Missing Names', () => {
    it('should fallback to email when name is null', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        'john@example.com',
        'jane@example.com',
        null,
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'team-closer',
        displayName: 'john@example.com',
      });
    });

    it('should fallback to email when name is undefined', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        'unknown@example.com',
        'jane@example.com',
        undefined,
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'team-closer',
        displayName: 'unknown@example.com',
      });
    });

    it('should fallback to "Team Member" when both name and email are missing', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        'unknown@example.com',
        'jane@example.com',
        null,
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'team-closer',
        displayName: 'unknown@example.com',
      });
    });
  });

  describe('Unassigned Projects', () => {
    it('should return unassigned when no closer or setter', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        null,
        null,
        null,
        null
      );
      
      expect(result).toEqual({
        status: 'unassigned',
        displayName: null,
      });
    });

    it('should return unassigned when emails are empty strings', () => {
      const result = determineProjectOwnership(
        'manager@example.com',
        '',
        '',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'unassigned',
        displayName: null,
      });
    });
  });

  describe('Whitespace Handling', () => {
    it('should handle user email with leading/trailing whitespace', () => {
      const result = determineProjectOwnership(
        '  john@example.com  ',
        'john@example.com',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });

    it('should handle closer email with whitespace', () => {
      const result = determineProjectOwnership(
        'john@example.com',
        '  john@example.com  ',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });
  });

  describe('Null/Undefined Handling', () => {
    it('should handle undefined closer email', () => {
      const result = determineProjectOwnership(
        'john@example.com',
        undefined,
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'team-setter',
        displayName: 'Jane Smith',
      });
    });

    it('should handle null setter email', () => {
      const result = determineProjectOwnership(
        'john@example.com',
        'john@example.com',
        null,
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });

    it('should handle empty string user email', () => {
      const result = determineProjectOwnership(
        '',
        'john@example.com',
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'team-closer',
        displayName: 'John Doe',
      });
    });
  });

  describe('Edge Cases', () => {
    it('should handle all null/undefined inputs', () => {
      const result = determineProjectOwnership(
        null as any,
        null,
        null,
        null,
        null
      );
      
      expect(result).toEqual({
        status: 'unassigned',
        displayName: null,
      });
    });

    it('should handle very long email addresses', () => {
      const longEmail = 'very.long.email.address.that.might.cause.issues@example.com';
      const result = determineProjectOwnership(
        longEmail,
        longEmail,
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });

    it('should handle special characters in emails', () => {
      const specialEmail = 'user+tag@example-domain.co.uk';
      const result = determineProjectOwnership(
        specialEmail,
        specialEmail,
        'jane@example.com',
        'John Doe',
        'Jane Smith'
      );
      
      expect(result).toEqual({
        status: 'mine',
        displayName: null,
      });
    });
  });
});
