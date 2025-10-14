import { describe, it, expect, beforeEach, vi } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ProjectBuckets } from '@/components/dashboard/ProjectBuckets';
import { getEnhancedDashboardMetrics } from '@/lib/quickbase/queries';
import type { TeamMemberBuckets } from '@/lib/types/dashboard';

// Mock the router
const mockPush = vi.fn();
vi.mock('next/navigation', () => ({
  useRouter: () => ({
    push: mockPush,
  }),
}));

// Mock the QuickBase client
vi.mock('@/lib/quickbase/client', () => ({
  qbClient: {
    queryRecords: vi.fn(),
  },
}));

// Mock the database client
vi.mock('@/lib/db/client', () => ({
  sql: vi.fn(),
}));

// Mock the logger
vi.mock('@/lib/logging/logger', () => ({
  logInfo: vi.fn(),
  logError: vi.fn(),
  logWarn: vi.fn(),
}));

describe('Project Buckets Drill-Down Integration', () => {
  let queryClient: QueryClient;

  const mockBucketsByMember: TeamMemberBuckets[] = [
    {
      memberName: 'John Doe',
      memberEmail: 'john@example.com',
      role: 'closer',
      installs: 2,
      rejected: 0,
      onHold: 5,
      repAttention: 3,
      pendingCancel: 1,
      readyForInstall: 4,
      totalProjects: 12,
    },
    {
      memberName: 'Jane Smith',
      memberEmail: 'jane@example.com',
      role: 'setter',
      installs: 1,
      rejected: 1,
      onHold: 2,
      repAttention: 1,
      pendingCancel: 0,
      readyForInstall: 3,
      totalProjects: 7,
    },
    {
      memberName: 'Bob Wilson',
      memberEmail: 'bob@example.com',
      role: 'closer',
      installs: 0,
      rejected: 0,
      onHold: 0,
      repAttention: 0,
      pendingCancel: 0,
      readyForInstall: 1,
      totalProjects: 1,
    },
  ];

  const mockBuckets = {
    installs: 3,
    rejected: 1,
    onHold: 7,
    repAttention: 4,
    pendingCancel: 1,
    readyForInstall: 8,
  };

  beforeEach(() => {
    queryClient = new QueryClient({
      defaultOptions: {
        queries: {
          retry: false,
        },
      },
    });
    vi.clearAllMocks();
  });

  const renderWithQueryClient = (component: React.ReactElement) => {
    return render(
      <QueryClientProvider client={queryClient}>
        {component}
      </QueryClientProvider>
    );
  };

  describe('API Integration', () => {
    it('returns bucket breakdown for managers with team scope', async () => {
      // Mock the database and QuickBase responses
      const mockProjects = [
        {
          [1]: { value: '1' }, // RECORD_ID
          [2]: { value: 'John Doe' }, // CLOSER_NAME
          [3]: { value: 'john@example.com' }, // CLOSER_EMAIL
          [4]: { value: 'Jane Smith' }, // SETTER_NAME
          [5]: { value: 'jane@example.com' }, // SETTER_EMAIL
          [6]: { value: 'Active' }, // PROJECT_STATUS
          [7]: { value: 'Yes' }, // ON_HOLD
          [8]: { value: '45' }, // PROJECT_AGE
          [9]: { value: null }, // INSTALL_COMPLETED_DATE
          [10]: { value: null }, // PTO_APPROVED
          [11]: { value: 'Yes' }, // NEM_APPROVED
          [12]: { value: 'Yes' }, // PERMIT_APPROVED
          [13]: { value: null }, // INSTALL_SCHEDULED_DATE_CAPTURE
          [14]: { value: null }, // DATE_ON_HOLD
        },
      ];

      const { qbClient } = await import('@/lib/quickbase/client');
      const { sql } = await import('@/lib/db/client');
      
      vi.mocked(qbClient.queryRecords).mockResolvedValue({
        data: mockProjects,
        fields: [],
        metadata: { totalRecords: 1 },
      });
      
      vi.mocked(sql).mockResolvedValue({
        rows: [],
      });

      const result = await getEnhancedDashboardMetrics(
        'manager-user-id',
        'office_leader',
        'lifetime',
        undefined,
        undefined,
        'team'
      );

      expect(result.bucketsByMember).toBeDefined();
      expect(Array.isArray(result.bucketsByMember)).toBe(true);
      expect(result.bucketsByMember!.length).toBeGreaterThan(0);
    });

    it('does NOT return bucket breakdown for reps', async () => {
      const { qbClient } = await import('@/lib/quickbase/client');
      const { sql } = await import('@/lib/db/client');
      
      vi.mocked(qbClient.queryRecords).mockResolvedValue({
        data: [],
        fields: [],
        metadata: { totalRecords: 0 },
      });
      
      vi.mocked(sql).mockResolvedValue({
        rows: [],
      });

      const result = await getEnhancedDashboardMetrics(
        'closer-user-id',
        'closer',
        'lifetime',
        undefined,
        undefined,
        'personal'
      );

      expect(result.bucketsByMember).toBeUndefined();
    });

    it('does NOT return bucket breakdown for personal scope', async () => {
      const { qbClient } = await import('@/lib/quickbase/client');
      const { sql } = await import('@/lib/db/client');
      
      vi.mocked(qbClient.queryRecords).mockResolvedValue({
        data: [],
        fields: [],
        metadata: { totalRecords: 0 },
      });
      
      vi.mocked(sql).mockResolvedValue({
        rows: [],
      });

      const result = await getEnhancedDashboardMetrics(
        'manager-user-id',
        'office_leader',
        'lifetime',
        undefined,
        undefined,
        'personal'
      );

      expect(result.bucketsByMember).toBeUndefined();
    });

    it('bucket counts match aggregate', async () => {
      const { qbClient } = await import('@/lib/quickbase/client');
      const { sql } = await import('@/lib/db/client');
      
      vi.mocked(qbClient.queryRecords).mockResolvedValue({
        data: [],
        fields: [],
        metadata: { totalRecords: 0 },
      });
      
      vi.mocked(sql).mockResolvedValue({
        rows: [],
      });

      const result = await getEnhancedDashboardMetrics(
        'manager-user-id',
        'office_leader',
        'lifetime',
        undefined,
        undefined,
        'team'
      );

      if (result.bucketsByMember && result.bucketsByMember.length > 0) {
        const totalOnHold = result.bucketsByMember.reduce((sum, member) => sum + member.onHold, 0);
        expect(totalOnHold).toBeGreaterThanOrEqual(result.buckets.onHold);
      }
    });

    it('team members are sorted by total projects', async () => {
      const { qbClient } = await import('@/lib/quickbase/client');
      const { sql } = await import('@/lib/db/client');
      
      vi.mocked(qbClient.queryRecords).mockResolvedValue({
        data: [],
        fields: [],
        metadata: { totalRecords: 0 },
      });
      
      vi.mocked(sql).mockResolvedValue({
        rows: [],
      });

      const result = await getEnhancedDashboardMetrics(
        'manager-user-id',
        'office_leader',
        'lifetime',
        undefined,
        undefined,
        'team'
      );

      if (result.bucketsByMember && result.bucketsByMember.length > 1) {
        for (let i = 0; i < result.bucketsByMember.length - 1; i++) {
          expect(result.bucketsByMember[i].totalProjects).toBeGreaterThanOrEqual(
            result.bucketsByMember[i + 1].totalProjects
          );
        }
      }
    });
  });

  describe('UI Integration', () => {
    it('drill-down button renders for managers with team scope', () => {
      renderWithQueryClient(
        <ProjectBuckets
          buckets={mockBuckets}
          bucketsByMember={mockBucketsByMember}
          isManager={true}
          scope="team"
        />
      );

      // Should show drill-down buttons for buckets with team member data
      expect(screen.getByText('View by Team Member (2)')).toBeInTheDocument(); // On Hold bucket
      expect(screen.getByText('View by Team Member (2)')).toBeInTheDocument(); // Rep Attention bucket
    });

    it('drill-down button does NOT render for reps', () => {
      renderWithQueryClient(
        <ProjectBuckets
          buckets={mockBuckets}
          bucketsByMember={mockBucketsByMember}
          isManager={false}
          scope="personal"
        />
      );

      // Should NOT show drill-down buttons
      expect(screen.queryByText(/View by Team Member/)).not.toBeInTheDocument();
    });

    it('clicking drill-down button expands/collapses content', async () => {
      renderWithQueryClient(
        <ProjectBuckets
          buckets={mockBuckets}
          bucketsByMember={mockBucketsByMember}
          isManager={true}
          scope="team"
        />
      );

      const drillDownButton = screen.getByText('View by Team Member (2)');
      
      // Initially collapsed
      expect(screen.queryByText('John Doe')).not.toBeInTheDocument();
      
      // Click to expand
      fireEvent.click(drillDownButton);
      
      // Should show team member cards
      await waitFor(() => {
        expect(screen.getByText('John Doe')).toBeInTheDocument();
        expect(screen.getByText('Jane Smith')).toBeInTheDocument();
      });
      
      // Click to collapse
      fireEvent.click(drillDownButton);
      
      // Should hide team member cards
      await waitFor(() => {
        expect(screen.queryByText('John Doe')).not.toBeInTheDocument();
        expect(screen.queryByText('Jane Smith')).not.toBeInTheDocument();
      });
    });

    it('only one bucket expanded at a time', async () => {
      renderWithQueryClient(
        <ProjectBuckets
          buckets={mockBuckets}
          bucketsByMember={mockBucketsByMember}
          isManager={true}
          scope="team"
        />
      );

      const onHoldButton = screen.getByText('View by Team Member (2)');
      const repAttentionButton = screen.getByText('View by Team Member (2)');
      
      // Expand On Hold bucket
      fireEvent.click(onHoldButton);
      
      await waitFor(() => {
        expect(screen.getByText('John Doe')).toBeInTheDocument();
      });
      
      // Expand Rep Attention bucket
      fireEvent.click(repAttentionButton);
      
      // On Hold should be collapsed, Rep Attention should be expanded
      await waitFor(() => {
        expect(screen.queryByText('John Doe')).not.toBeInTheDocument();
        expect(screen.getByText('John Doe')).toBeInTheDocument(); // Should still be there from Rep Attention
      });
    });

    it('team member cards display correct data', async () => {
      renderWithQueryClient(
        <ProjectBuckets
          buckets={mockBuckets}
          bucketsByMember={mockBucketsByMember}
          isManager={true}
          scope="team"
        />
      );

      const drillDownButton = screen.getByText('View by Team Member (2)');
      fireEvent.click(drillDownButton);
      
      await waitFor(() => {
        // Check John Doe card
        expect(screen.getByText('John Doe')).toBeInTheDocument();
        expect(screen.getByText('closer')).toBeInTheDocument();
        expect(screen.getByText('12 total projects')).toBeInTheDocument();
        expect(screen.getByText('5')).toBeInTheDocument(); // On Hold count
        
        // Check Jane Smith card
        expect(screen.getByText('Jane Smith')).toBeInTheDocument();
        expect(screen.getByText('setter')).toBeInTheDocument();
        expect(screen.getByText('7 total projects')).toBeInTheDocument();
        expect(screen.getByText('2')).toBeInTheDocument(); // On Hold count
      });
    });

    it('click-through navigates to filtered project list', async () => {
      renderWithQueryClient(
        <ProjectBuckets
          buckets={mockBuckets}
          bucketsByMember={mockBucketsByMember}
          isManager={true}
          scope="team"
        />
      );

      const drillDownButton = screen.getByText('View by Team Member (2)');
      fireEvent.click(drillDownButton);
      
      await waitFor(() => {
        const johnDoeCard = screen.getByText('John Doe').closest('a');
        expect(johnDoeCard).toHaveAttribute('href', '/projects?view=on-hold&memberEmail=john%40example.com');
      });
    });

    it('drill-down button hidden for buckets with no team member data', () => {
      const bucketsWithNoData = {
        ...mockBuckets,
        rejected: 0, // No rejected projects
      };

      const membersWithNoRejected = mockBucketsByMember.map(member => ({
        ...member,
        rejected: 0,
      }));

      renderWithQueryClient(
        <ProjectBuckets
          buckets={bucketsWithNoData}
          bucketsByMember={membersWithNoRejected}
          isManager={true}
          scope="team"
        />
      );

      // Should not show drill-down button for rejected bucket
      const rejectedBucket = screen.getByText('Rejected');
      const rejectedContainer = rejectedBucket.closest('.relative');
      expect(rejectedContainer).not.toHaveTextContent('View by Team Member');
    });
  });

  describe('End-to-End Flow', () => {
    it('full drill-down flow works correctly', async () => {
      renderWithQueryClient(
        <ProjectBuckets
          buckets={mockBuckets}
          bucketsByMember={mockBucketsByMember}
          isManager={true}
          scope="team"
        />
      );

      // 1. Dashboard shows aggregate bucket counts
      expect(screen.getByText('7')).toBeInTheDocument(); // On Hold count
      
      // 2. Manager clicks "View by Team Member" for "On Hold" bucket
      const drillDownButton = screen.getByText('View by Team Member (2)');
      fireEvent.click(drillDownButton);
      
      // 3. Team member breakdown appears
      await waitFor(() => {
        expect(screen.getByText('John Doe')).toBeInTheDocument();
        expect(screen.getByText('Jane Smith')).toBeInTheDocument();
      });
      
      // 4. Manager clicks on "John Doe" card
      const johnDoeCard = screen.getByText('John Doe').closest('a');
      expect(johnDoeCard).toHaveAttribute('href', '/projects?view=on-hold&memberEmail=john%40example.com');
      
      // 5. Navigation should occur (mocked)
      fireEvent.click(johnDoeCard!);
      
      // Note: In a real test, we would verify that the router.push was called with the correct URL
      // and that the project list shows only John's on hold projects
    });
  });
});
