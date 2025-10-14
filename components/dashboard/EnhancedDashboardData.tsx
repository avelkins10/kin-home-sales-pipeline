'use client'

// components/dashboard/EnhancedDashboardData.tsx
import { useQuery } from '@tanstack/react-query';
import { PerformanceMetrics } from './PerformanceMetrics';
import { TeamPerformanceMetrics } from './TeamPerformanceMetrics';
import { CommissionSummary } from './CommissionSummary';
import { ManagerCommissionComparison } from './ManagerCommissionComparison';
import { ProjectBuckets } from './ProjectBuckets';
import { isManagerRole } from '@/lib/utils/role-helpers';
import type { EnhancedDashboardMetrics, TimeRange, MetricsScope } from '@/lib/types/dashboard';

interface EnhancedDashboardDataProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: { startDate: string; endDate: string };
  scope: 'personal' | 'team'; // NEW PROP
}

export function EnhancedDashboardData({ userId, role, timeRange, customDateRange, scope }: EnhancedDashboardDataProps) {
  const isManager = isManagerRole(role);
  
  const { data, isLoading, error } = useQuery<EnhancedDashboardMetrics>({
    queryKey: ['dashboard-metrics', userId, role, timeRange, customDateRange, scope],
    queryFn: async () => {
      let url = `/api/dashboard/metrics?timeRange=${timeRange}&scope=${scope}`;
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      const response = await fetch(url);
      if (!response.ok) {
        throw new Error('Failed to fetch dashboard metrics');
      }
      return response.json();
    },
    refetchInterval: 30000, // Poll every 30 seconds
    staleTime: 15000, // Consider stale after 15 seconds
  });

  if (error) {
    return (
      <div className="space-y-6">
        <div data-testid="error-message" className="text-center py-8 text-gray-500">
          <p className="text-sm">Unable to load dashboard data</p>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="space-y-6">
        {scope === 'team' ? (
          <TeamPerformanceMetrics
            soldAccounts={0}
            grossRevenue={0}
            installCount={0}
            installedRevenue={0}
            retentionRate={0}
            timeRange={timeRange}
            isLoading={true}
          />
        ) : (
          <PerformanceMetrics
            soldAccounts={0}
            grossRevenue={0}
            installCount={0}
            installedRevenue={0}
            retentionRate={0}
            timeRange={timeRange}
            isLoading={true}
          />
        )}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {isManager ? (
            <ManagerCommissionComparison
              userId={userId}
              role={role}
              timeRange={timeRange}
              customDateRange={customDateRange}
            />
          ) : (
            <CommissionSummary
              earnedCommission={0}
              lostCommission={0}
              onHoldCommission={0}
              pendingCommission={0}
              salesAidCommission={0}
              timeRange={timeRange}
              isLoading={true}
              isManager={false}
              scope="personal"
            />
          )}
          <ProjectBuckets
            buckets={{
              installs: 0,
              rejected: 0,
              onHold: 0,
              repAttention: 0,
              pendingCancel: 0,
              readyForInstall: 0,
            }}
            isLoading={true}
            isManager={isManager}
            scope={scope}
            bucketsByMember={undefined}
          />
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Performance Metrics - Full Width */}
      {scope === 'team' ? (
        <TeamPerformanceMetrics
          soldAccounts={data.soldAccounts}
          grossRevenue={data.grossRevenue}
          installCount={data.installCount}
          installedRevenue={data.installedRevenue}
          retentionRate={data.retentionRate}
          timeRange={data.timeRange}
          isLoading={isLoading}
        />
      ) : (
        <PerformanceMetrics
          soldAccounts={data.soldAccounts}
          grossRevenue={data.grossRevenue}
          installCount={data.installCount}
          installedRevenue={data.installedRevenue}
          retentionRate={data.retentionRate}
          timeRange={data.timeRange}
          isLoading={isLoading}
        />
      )}

      {/* Commission Summary and Project Buckets - Two Column Layout */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {isManager ? (
          <ManagerCommissionComparison
            userId={userId}
            role={role}
            timeRange={timeRange}
            customDateRange={customDateRange}
          />
        ) : (
          <CommissionSummary
            earnedCommission={data.earnedCommission}
            lostCommission={data.lostCommission}
            onHoldCommission={data.onHoldCommission}
            pendingCommission={data.pendingCommission}
            salesAidCommission={data.salesAidCommission}
            timeRange={data.timeRange}
            isLoading={isLoading}
            isManager={false}
            scope="personal"
          />
        )}
        <ProjectBuckets
          buckets={data.buckets}
          isLoading={isLoading}
          isManager={isManager}
          scope={scope}
          bucketsByMember={data.bucketsByMember}
        />
      </div>
    </div>
  );
}