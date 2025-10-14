import { describe, it, expect } from 'vitest';
import { __test__ } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

const { buildOwnershipFilter } = __test__;

describe('buildOwnershipFilter', () => {
  describe('All Projects (No Filter)', () => {
    it('should return empty string when ownership is "all"', () => {
      const result = buildOwnershipFilter('john@example.com', 'all');
      expect(result).toBe('');
    });

    it('should return empty string when ownership is undefined', () => {
      const result = buildOwnershipFilter('john@example.com', undefined);
      expect(result).toBe('');
    });

    it('should return empty string when userEmail is null', () => {
      const result = buildOwnershipFilter(null, 'my-projects');
      expect(result).toBe('');
    });

    it('should return empty string when userEmail is empty string', () => {
      const result = buildOwnershipFilter('', 'my-projects');
      expect(result).toBe('');
    });
  });

  describe('My Projects Filter', () => {
    it('should generate correct WHERE clause for my-projects', () => {
      const result = buildOwnershipFilter('john@example.com', 'my-projects');
      const expected = `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'john@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'john@example.com'})`;
      expect(result).toBe(expected);
    });

    it('should sanitize email with quotes', () => {
      const result = buildOwnershipFilter("john'test@example.com", 'my-projects');
      const expected = `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'john''test@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'john''test@example.com'})`;
      expect(result).toBe(expected);
    });

    it('should handle email with special characters', () => {
      const result = buildOwnershipFilter('john+test@example.com', 'my-projects');
      const expected = `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'john+test@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'john+test@example.com'})`;
      expect(result).toBe(expected);
    });

    it('should handle email with spaces (sanitized)', () => {
      const result = buildOwnershipFilter('john test@example.com', 'my-projects');
      const expected = `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'john test@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'john test@example.com'})`;
      expect(result).toBe(expected);
    });
  });

  describe('Team Projects Filter', () => {
    it('should generate correct WHERE clause for team-projects', () => {
      const result = buildOwnershipFilter('john@example.com', 'team-projects');
      const expected = `(({${PROJECT_FIELDS.CLOSER_EMAIL}.XEX.'john@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.''}) AND ({${PROJECT_FIELDS.SETTER_EMAIL}.XEX.'john@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.''}))`;
      expect(result).toBe(expected);
    });

    it('should include projects with no closer/setter (empty fields)', () => {
      const result = buildOwnershipFilter('john@example.com', 'team-projects');
      // Verify the WHERE clause includes conditions for empty fields
      expect(result).toContain(`{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.''}`);
      expect(result).toContain(`{${PROJECT_FIELDS.SETTER_EMAIL}.EX.''}`);
    });

    it('should sanitize email in team-projects filter', () => {
      const result = buildOwnershipFilter("john'test@example.com", 'team-projects');
      const expected = `(({${PROJECT_FIELDS.CLOSER_EMAIL}.XEX.'john''test@example.com'} OR {${PROJECT_FIELDS.CLOSER_EMAIL}.EX.''}) AND ({${PROJECT_FIELDS.SETTER_EMAIL}.XEX.'john''test@example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.''}))`;
      expect(result).toBe(expected);
    });

    it('should use XEX operator for exclusion', () => {
      const result = buildOwnershipFilter('john@example.com', 'team-projects');
      expect(result).toContain('.XEX.');
    });
  });

  describe('Edge Cases', () => {
    it('should return empty string for invalid ownership value', () => {
      const result = buildOwnershipFilter('john@example.com', 'invalid');
      expect(result).toBe('');
    });

    it('should return empty string for empty ownership value', () => {
      const result = buildOwnershipFilter('john@example.com', '');
      expect(result).toBe('');
    });

    it('should handle case sensitivity (email used as-is)', () => {
      const result = buildOwnershipFilter('John@Example.com', 'my-projects');
      const expected = `({${PROJECT_FIELDS.CLOSER_EMAIL}.EX.'John@Example.com'} OR {${PROJECT_FIELDS.SETTER_EMAIL}.EX.'John@Example.com'})`;
      expect(result).toBe(expected);
    });

    it('should handle whitespace-only email', () => {
      const result = buildOwnershipFilter('   ', 'my-projects');
      expect(result).toBe('');
    });
  });

  describe('Field ID Verification', () => {
    it('should use correct CLOSER_EMAIL field ID', () => {
      const result = buildOwnershipFilter('john@example.com', 'my-projects');
      expect(result).toContain(`{${PROJECT_FIELDS.CLOSER_EMAIL}.EX.`);
    });

    it('should use correct SETTER_EMAIL field ID', () => {
      const result = buildOwnershipFilter('john@example.com', 'my-projects');
      expect(result).toContain(`{${PROJECT_FIELDS.SETTER_EMAIL}.EX.`);
    });

    it('should use correct field IDs in team-projects filter', () => {
      const result = buildOwnershipFilter('john@example.com', 'team-projects');
      expect(result).toContain(`{${PROJECT_FIELDS.CLOSER_EMAIL}.XEX.`);
      expect(result).toContain(`{${PROJECT_FIELDS.SETTER_EMAIL}.XEX.`);
    });
  });

  describe('QuickBase Operator Usage', () => {
    it('should use EX operator for my-projects filter', () => {
      const result = buildOwnershipFilter('john@example.com', 'my-projects');
      expect(result).toContain('.EX.');
      expect(result).not.toContain('.XEX.');
    });

    it('should use XEX operator for team-projects filter', () => {
      const result = buildOwnershipFilter('john@example.com', 'team-projects');
      expect(result).toContain('.XEX.');
    });

    it('should use OR operator for my-projects filter', () => {
      const result = buildOwnershipFilter('john@example.com', 'my-projects');
      expect(result).toContain(' OR ');
    });

    it('should use AND operator for team-projects filter', () => {
      const result = buildOwnershipFilter('john@example.com', 'team-projects');
      expect(result).toContain(' AND ');
    });
  });
});
