import { describe, it, expect, vi, beforeEach } from 'vitest';
import { __test__ } from '@/lib/quickbase/queries';

// Mock the sql function to capture actual queries
const mockSql = vi.fn();
vi.mock('@/lib/db/client', () => ({
  sql: mockSql,
}));

// Mock the logger
vi.mock('@/lib/logging/logger', () => ({
  logError: vi.fn(),
}));

describe('SQL Helper Functions - Query Verification', () => {
  const { getAssignedOffices, getManagedUserEmails } = __test__;

  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('getAssignedOffices - SQL Query Verification', () => {
    it('should execute correct SQL query for office assignments', async () => {
      // Mock successful response
      mockSql.mockResolvedValue({
        rows: [
          { office_name: 'Office A' },
          { office_name: 'Office B' },
          { office_name: 'Office C' },
        ],
      });

      const result = await getAssignedOffices('user123');

      // Verify the SQL query was called with correct template
      expect(mockSql).toHaveBeenCalledTimes(1);
      const sqlCall = mockSql.mock.calls[0][0];
      
      // Verify it's a template literal with the correct structure
      expect(sqlCall.strings).toContain('SELECT office_name \n      FROM office_assignments \n      WHERE user_id = ');
      expect(sqlCall.strings).toContain('\n    ');
      
      // Verify the parameter was passed correctly
      expect(sqlCall.values).toContain('user123');
      
      // Verify result processing
      expect(result).toEqual(['Office A', 'Office B', 'Office C']);
    });

    it('should handle empty result set correctly', async () => {
      mockSql.mockResolvedValue({ rows: [] });

      const result = await getAssignedOffices('user123');

      expect(mockSql).toHaveBeenCalledTimes(1);
      expect(result).toEqual([]);
    });

    it('should process rows with null office names', async () => {
      mockSql.mockResolvedValue({
        rows: [
          { office_name: 'Office A' },
          { office_name: null },
          { office_name: 'Office C' },
        ],
      });

      const result = await getAssignedOffices('user123');

      expect(result).toEqual(['Office A', null, 'Office C']);
    });

    it('should handle database errors and return empty array', async () => {
      const dbError = new Error('Database connection failed');
      mockSql.mockRejectedValue(dbError);

      const result = await getAssignedOffices('user123');

      expect(mockSql).toHaveBeenCalledTimes(1);
      expect(result).toEqual([]);
    });

    it('should validate input and return empty array for invalid userId', async () => {
      const result = await getAssignedOffices('');

      // Should not call SQL for invalid input
      expect(mockSql).not.toHaveBeenCalled();
      expect(result).toEqual([]);
    });
  });

  describe('getManagedUserEmails - SQL Query Verification', () => {
    it('should execute correct SQL query for managed user emails', async () => {
      // Mock successful response
      mockSql.mockResolvedValue({
        rows: [
          { email: 'user1@example.com' },
          { email: 'user2@example.com' },
          { email: 'user3@example.com' },
        ],
      });

      const result = await getManagedUserEmails('manager123');

      // Verify the SQL query was called with correct template
      expect(mockSql).toHaveBeenCalledTimes(1);
      const sqlCall = mockSql.mock.calls[0][0];
      
      // Verify it's a template literal with the correct structure
      expect(sqlCall.strings).toContain('SELECT u.email\n      FROM user_hierarchies uh\n      JOIN users u ON uh.user_id = u.id\n      WHERE uh.manager_id = ');
      expect(sqlCall.strings).toContain('\n      AND u.email IS NOT NULL\n    ');
      
      // Verify the parameter was passed correctly
      expect(sqlCall.values).toContain('manager123');
      
      // Verify result processing
      expect(result).toEqual(['user1@example.com', 'user2@example.com', 'user3@example.com']);
    });

    it('should handle empty result set correctly', async () => {
      mockSql.mockResolvedValue({ rows: [] });

      const result = await getManagedUserEmails('manager123');

      expect(mockSql).toHaveBeenCalledTimes(1);
      expect(result).toEqual([]);
    });

    it('should filter out null and empty emails', async () => {
      mockSql.mockResolvedValue({
        rows: [
          { email: 'user1@example.com' },
          { email: null },
          { email: '' },
          { email: 'user4@example.com' },
        ],
      });

      const result = await getManagedUserEmails('manager123');

      // Should filter out null and empty emails
      expect(result).toEqual(['user1@example.com', 'user4@example.com']);
    });

    it('should handle database errors and return empty array', async () => {
      const dbError = new Error('Database connection failed');
      mockSql.mockRejectedValue(dbError);

      const result = await getManagedUserEmails('manager123');

      expect(mockSql).toHaveBeenCalledTimes(1);
      expect(result).toEqual([]);
    });

    it('should validate input and return empty array for invalid managerId', async () => {
      const result = await getManagedUserEmails('');

      // Should not call SQL for invalid input
      expect(mockSql).not.toHaveBeenCalled();
      expect(result).toEqual([]);
    });
  });

  describe('SQL Query Structure Verification', () => {
    it('should use parameterized queries for getAssignedOffices', async () => {
      mockSql.mockResolvedValue({ rows: [] });

      await getAssignedOffices('user123');

      const sqlCall = mockSql.mock.calls[0][0];
      
      // Verify it's using template literals (parameterized queries)
      expect(sqlCall.strings).toBeDefined();
      expect(sqlCall.values).toBeDefined();
      expect(sqlCall.values).toContain('user123');
      
      // Verify no string concatenation (SQL injection safety)
      const queryString = sqlCall.strings.join('?');
      expect(queryString).not.toContain('user123');
    });

    it('should use parameterized queries for getManagedUserEmails', async () => {
      mockSql.mockResolvedValue({ rows: [] });

      await getManagedUserEmails('manager123');

      const sqlCall = mockSql.mock.calls[0][0];
      
      // Verify it's using template literals (parameterized queries)
      expect(sqlCall.strings).toBeDefined();
      expect(sqlCall.values).toBeDefined();
      expect(sqlCall.values).toContain('manager123');
      
      // Verify no string concatenation (SQL injection safety)
      const queryString = sqlCall.strings.join('?');
      expect(queryString).not.toContain('manager123');
    });
  });

  describe('Result Processing Verification', () => {
    it('should correctly map office_name field from getAssignedOffices', async () => {
      mockSql.mockResolvedValue({
        rows: [
          { office_name: 'Office A', other_field: 'ignored' },
          { office_name: 'Office B', other_field: 'ignored' },
        ],
      });

      const result = await getAssignedOffices('user123');

      // Should only extract office_name field
      expect(result).toEqual(['Office A', 'Office B']);
    });

    it('should correctly map email field from getManagedUserEmails', async () => {
      mockSql.mockResolvedValue({
        rows: [
          { email: 'user1@example.com', name: 'User 1', id: '123' },
          { email: 'user2@example.com', name: 'User 2', id: '456' },
        ],
      });

      const result = await getManagedUserEmails('manager123');

      // Should only extract email field
      expect(result).toEqual(['user1@example.com', 'user2@example.com']);
    });

    it('should handle special characters in office names', async () => {
      mockSql.mockResolvedValue({
        rows: [
          { office_name: "O'Connor Office" },
          { office_name: 'Office "Special" Name' },
          { office_name: 'Office & Associates' },
        ],
      });

      const result = await getAssignedOffices('user123');

      expect(result).toEqual([
        "O'Connor Office",
        'Office "Special" Name',
        'Office & Associates',
      ]);
    });

    it('should handle special characters in email addresses', async () => {
      mockSql.mockResolvedValue({
        rows: [
          { email: 'user.name+tag@example.com' },
          { email: "user'name@example.com" },
          { email: 'user_name@sub.example.com' },
        ],
      });

      const result = await getManagedUserEmails('manager123');

      expect(result).toEqual([
        'user.name+tag@example.com',
        "user'name@example.com",
        'user_name@sub.example.com',
      ]);
    });
  });
});
