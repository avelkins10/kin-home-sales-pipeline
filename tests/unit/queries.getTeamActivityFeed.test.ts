// tests/unit/queries.getTeamActivityFeed.test.ts
import { describe, it, expect, vi, beforeEach } from 'vitest';
import { getTeamActivityFeed } from '@/lib/quickbase/queries';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import type { TeamActivityItem } from '@/lib/types/dashboard';

// Mock dependencies
vi.mock('@/lib/quickbase/client', () => ({
  qbClient: {
    queryRecords: vi.fn(),
  },
}));

vi.mock('@/lib/utils/role-helpers', () => ({
  isManagerRole: vi.fn(),
}));

vi.mock('@/lib/auth/projectAuthorization', () => ({
  buildProjectAccessClause: vi.fn(),
}));

vi.mock('@/lib/quickbase/queries', async () => {
  const actual = await vi.importActual('@/lib/quickbase/queries');
  return {
    ...actual,
    getAssignedOffices: vi.fn(),
    getManagedUserEmails: vi.fn(),
    getUserEmail: vi.fn(),
  };
});

import { qbClient } from '@/lib/quickbase/client';
import { isManagerRole } from '@/lib/utils/role-helpers';
import { buildProjectAccessClause } from '@/lib/auth/projectAuthorization';
import { getAssignedOffices, getManagedUserEmails, getUserEmail } from '@/lib/quickbase/queries';

const mockQbClient = vi.mocked(qbClient);
const mockIsManagerRole = vi.mocked(isManagerRole);
const mockBuildProjectAccessClause = vi.mocked(buildProjectAccessClause);
const mockGetAssignedOffices = vi.mocked(getAssignedOffices);
const mockGetManagedUserEmails = vi.mocked(getManagedUserEmails);
const mockGetUserEmail = vi.mocked(getUserEmail);

describe('getTeamActivityFeed', () => {
  beforeEach(() => {
    vi.clearAllMocks();
  });

  describe('Manager Role Validation', () => {
    it('returns empty array for non-manager roles', async () => {
      mockIsManagerRole.mockReturnValue(false);

      const result = await getTeamActivityFeed('123', 'closer');

      expect(result).toEqual([]);
      expect(mockQbClient.queryRecords).not.toHaveBeenCalled();
    });

    it('queries data for office_leader', async () => {
      mockIsManagerRole.mockReturnValue(true);
      mockGetAssignedOffices.mockResolvedValue(['Office A']);
      mockGetUserEmail.mockResolvedValue('manager@example.com');
      mockBuildProjectAccessClause.mockReturnValue('{CLOSER_EMAIL.EX.manager@example.com}');
      mockQbClient.queryRecords.mockResolvedValue({ data: [] });

      await getTeamActivityFeed('123', 'office_leader');

      expect(mockGetAssignedOffices).toHaveBeenCalledWith('123');
      expect(mockQbClient.queryRecords).toHaveBeenCalled();
    });

    it('queries data for team_lead', async () => {
      mockIsManagerRole.mockReturnValue(true);
      mockGetManagedUserEmails.mockResolvedValue(['user1@example.com', 'user2@example.com']);
      mockGetUserEmail.mockResolvedValue('teamlead@example.com');
      mockBuildProjectAccessClause.mockReturnValue('{CLOSER_EMAIL.EX.teamlead@example.com}');
      mockQbClient.queryRecords.mockResolvedValue({ data: [] });

      await getTeamActivityFeed('123', 'team_lead');

      expect(mockGetManagedUserEmails).toHaveBeenCalledWith('123');
      expect(mockQbClient.queryRecords).toHaveBeenCalled();
    });
  });

  describe('Activity Detection', () => {
    beforeEach(() => {
      mockIsManagerRole.mockReturnValue(true);
      mockGetUserEmail.mockResolvedValue('manager@example.com');
      mockBuildProjectAccessClause.mockReturnValue('{CLOSER_EMAIL.EX.manager@example.com}');
    });

    it('detects install completion', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result).toHaveLength(1);
      expect(result[0].activityType).toBe('install_completed');
      expect(result[0].activityDescription).toBe('Install completed');
      expect(result[0].timestamp).toBe('2024-01-15T10:00:00Z');
    });

    it('detects PTO approval', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: null },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: '2024-01-15T10:00:00Z' },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result).toHaveLength(1);
      expect(result[0].activityType).toBe('pto_approved');
      expect(result[0].activityDescription).toBe('PTO approved');
      expect(result[0].timestamp).toBe('2024-01-15T10:00:00Z');
    });

    it('detects hold placement', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: null },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result).toHaveLength(1);
      expect(result[0].activityType).toBe('placed_on_hold');
      expect(result[0].activityDescription).toBe('Placed on hold');
      expect(result[0].timestamp).toBe('2024-01-15T10:00:00Z');
    });

    it('detects cancellation', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Cancelled' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: null },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result).toHaveLength(1);
      expect(result[0].activityType).toBe('cancelled');
      expect(result[0].activityDescription).toBe('Project cancelled');
    });

    it('prioritizes PTO over install when both recent', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-14T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: '2024-01-15T10:00:00Z' },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result).toHaveLength(1);
      expect(result[0].activityType).toBe('pto_approved');
      expect(result[0].timestamp).toBe('2024-01-15T10:00:00Z');
    });
  });

  describe('Team Member Attribution', () => {
    beforeEach(() => {
      mockIsManagerRole.mockReturnValue(true);
      mockGetUserEmail.mockResolvedValue('manager@example.com');
      mockBuildProjectAccessClause.mockReturnValue('{CLOSER_EMAIL.EX.manager@example.com}');
    });

    it('uses closer name when available', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'John Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: 'Jane Smith' },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result[0].teamMemberName).toBe('John Doe');
      expect(result[0].teamMemberRole).toBe('closer');
    });

    it('falls back to setter name when no closer', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: null },
        [PROJECT_FIELDS.SETTER_NAME]: { value: 'Jane Smith' },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result[0].teamMemberName).toBe('Jane Smith');
      expect(result[0].teamMemberRole).toBe('setter');
    });

    it('uses "Unassigned" when no team members', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: null },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result[0].teamMemberName).toBe('Unassigned');
      expect(result[0].teamMemberRole).toBe('closer');
    });
  });

  describe('Sorting and Limiting', () => {
    beforeEach(() => {
      mockIsManagerRole.mockReturnValue(true);
      mockGetUserEmail.mockResolvedValue('manager@example.com');
      mockBuildProjectAccessClause.mockReturnValue('{CLOSER_EMAIL.EX.manager@example.com}');
    });

    it('sorts by timestamp descending', async () => {
      const mockProjects = [
        {
          [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
          [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-1' },
          [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'Customer 1' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Closer 1' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: null },
          [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-10T10:00:00Z' }, // 5 days ago
          [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
        },
        {
          [PROJECT_FIELDS.RECORD_ID]: { value: 2 },
          [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-2' },
          [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'Customer 2' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Closer 2' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: null },
          [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-14T10:00:00Z' }, // 1 day ago
          [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
        },
        {
          [PROJECT_FIELDS.RECORD_ID]: { value: 3 },
          [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-3' },
          [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'Customer 3' },
          [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
          [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Closer 3' },
          [PROJECT_FIELDS.SETTER_NAME]: { value: null },
          [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
          [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-12T10:00:00Z' }, // 3 days ago
          [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
        },
      ];

      mockQbClient.queryRecords.mockResolvedValue({ data: mockProjects });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result).toHaveLength(3);
      expect(result[0].recordId).toBe(2); // Most recent (1 day ago)
      expect(result[1].recordId).toBe(3); // Middle (3 days ago)
      expect(result[2].recordId).toBe(1); // Oldest (5 days ago)
    });

    it('limits to specified count', async () => {
      const mockProjects = Array.from({ length: 15 }, (_, i) => ({
        [PROJECT_FIELDS.RECORD_ID]: { value: i + 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: `P-${i + 1}` },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: `Customer ${i + 1}` },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: `Closer ${i + 1}` },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      }));

      mockQbClient.queryRecords.mockResolvedValue({ data: mockProjects });

      const result = await getTeamActivityFeed('123', 'office_leader', undefined, 10);

      expect(result).toHaveLength(10);
    });

    it('returns all when fewer than limit', async () => {
      const mockProjects = Array.from({ length: 5 }, (_, i) => ({
        [PROJECT_FIELDS.RECORD_ID]: { value: i + 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: `P-${i + 1}` },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: `Customer ${i + 1}` },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: `Closer ${i + 1}` },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      }));

      mockQbClient.queryRecords.mockResolvedValue({ data: mockProjects });

      const result = await getTeamActivityFeed('123', 'office_leader', undefined, 10);

      expect(result).toHaveLength(5);
    });
  });

  describe('Edge Cases', () => {
    beforeEach(() => {
      mockIsManagerRole.mockReturnValue(true);
      mockGetUserEmail.mockResolvedValue('manager@example.com');
      mockBuildProjectAccessClause.mockReturnValue('{CLOSER_EMAIL.EX.manager@example.com}');
    });

    it('handles empty result set', async () => {
      mockQbClient.queryRecords.mockResolvedValue({ data: [] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result).toEqual([]);
    });

    it('handles missing customer name', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: null },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result[0].customerName).toBe('Unknown Customer');
    });

    it('handles missing project ID', async () => {
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: null },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: '2024-01-15T10:00:00Z' },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result[0].projectId).toBe('N/A');
    });

    it('calculates daysAgo correctly', async () => {
      const threeDaysAgo = new Date();
      threeDaysAgo.setDate(threeDaysAgo.getDate() - 3);
      
      const mockProject = {
        [PROJECT_FIELDS.RECORD_ID]: { value: 1 },
        [PROJECT_FIELDS.PROJECT_ID]: { value: 'P-12345' },
        [PROJECT_FIELDS.CUSTOMER_NAME]: { value: 'John Smith' },
        [PROJECT_FIELDS.PROJECT_STATUS]: { value: 'Active' },
        [PROJECT_FIELDS.CLOSER_NAME]: { value: 'Jane Doe' },
        [PROJECT_FIELDS.SETTER_NAME]: { value: null },
        [PROJECT_FIELDS.DATE_ON_HOLD]: { value: null },
        [PROJECT_FIELDS.INSTALL_COMPLETED_DATE]: { value: threeDaysAgo.toISOString() },
        [PROJECT_FIELDS.PTO_APPROVED]: { value: null },
      };

      mockQbClient.queryRecords.mockResolvedValue({ data: [mockProject] });

      const result = await getTeamActivityFeed('123', 'office_leader');

      expect(result[0].daysAgo).toBe(3);
    });
  });
});
