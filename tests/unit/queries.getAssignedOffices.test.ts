import { describe, it, expect, vi, beforeEach } from 'vitest';
import { __test__ } from '@/lib/quickbase/queries';

// Mock the sql function
const mockSql = vi.fn();
vi.mock('@/lib/db/client', () => ({
  sql: mockSql,
}));

// Mock the logger
vi.mock('@/lib/logging/logger', () => ({
  logError: vi.fn(),
}));

/**
 * @deprecated This test file provides basic mocking tests for getAssignedOffices.
 * For comprehensive SQL query verification and result processing tests,
 * see queries.sqlHelpers.verification.test.ts
 */
describe('getAssignedOffices (Basic Mocking Tests)', () => {
  const { getAssignedOffices } = __test__;

  beforeEach(() => {
    vi.clearAllMocks();
  });

  it('should return assigned offices for a user', async () => {
    // Mock successful database response
    mockSql.mockResolvedValue({
      rows: [
        { office_name: 'Office A' },
        { office_name: 'Office B' },
        { office_name: 'Office C' },
      ],
    });

    const result = await getAssignedOffices('user123');

    expect(result).toEqual(['Office A', 'Office B', 'Office C']);
    expect(mockSql).toHaveBeenCalledWith(expect.any(Object));
  });

  it('should return empty array when no offices are assigned', async () => {
    // Mock empty database response
    mockSql.mockResolvedValue({
      rows: [],
    });

    const result = await getAssignedOffices('user123');

    expect(result).toEqual([]);
    expect(mockSql).toHaveBeenCalledWith(expect.any(Object));
  });

  it('should return empty array and log error when database query fails', async () => {
    // Mock database error
    const dbError = new Error('Database connection failed');
    mockSql.mockRejectedValue(dbError);

    const result = await getAssignedOffices('user123');

    expect(result).toEqual([]);
    expect(mockSql).toHaveBeenCalledWith(expect.any(Object));
  });

  it('should handle single office assignment', async () => {
    // Mock single office response
    mockSql.mockResolvedValue({
      rows: [
        { office_name: 'Single Office' },
      ],
    });

    const result = await getAssignedOffices('user123');

    expect(result).toEqual(['Single Office']);
    expect(mockSql).toHaveBeenCalledWith(expect.any(Object));
  });

  it('should handle null office names gracefully', async () => {
    // Mock response with null office name
    mockSql.mockResolvedValue({
      rows: [
        { office_name: 'Office A' },
        { office_name: null },
        { office_name: 'Office C' },
      ],
    });

    const result = await getAssignedOffices('user123');

    expect(result).toEqual(['Office A', null, 'Office C']);
    expect(mockSql).toHaveBeenCalledWith(expect.any(Object));
  });

  it('should handle undefined office names gracefully', async () => {
    // Mock response with undefined office name
    mockSql.mockResolvedValue({
      rows: [
        { office_name: 'Office A' },
        { office_name: undefined },
        { office_name: 'Office C' },
      ],
    });

    const result = await getAssignedOffices('user123');

    expect(result).toEqual(['Office A', undefined, 'Office C']);
    expect(mockSql).toHaveBeenCalledWith(expect.any(Object));
  });
});
