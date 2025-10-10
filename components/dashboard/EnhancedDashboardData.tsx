'use client'

// components/dashboard/EnhancedDashboardData.tsx
import { useQuery } from '@tanstack/react-query';
import { PerformanceMetrics } from './PerformanceMetrics';
import { CommissionSummary } from './CommissionSummary';
import { ProjectBuckets } from './ProjectBuckets';
import type { EnhancedDashboardMetrics, TimeRange } from '@/lib/types/dashboard';

interface EnhancedDashboardDataProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
}

export function EnhancedDashboardData({ userId, role, timeRange }: EnhancedDashboardDataProps) {
  const { data, isLoading, error } = useQuery<EnhancedDashboardMetrics>({
    queryKey: ['dashboard-metrics', userId, role, timeRange],
    queryFn: async () => {
      const response = await fetch(`/api/dashboard/metrics?timeRange=${timeRange}`);
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
        <div className="text-center py-8 text-gray-500">
          <p className="text-sm">Unable to load dashboard data</p>
        </div>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="space-y-6">
        <PerformanceMetrics
          soldAccounts={0}
          grossRevenue={0}
          installCount={0}
          installedRevenue={0}
          retentionRate={0}
          timeRange={timeRange}
          isLoading={true}
        />
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          <CommissionSummary
            earnedCommission={0}
            lostCommission={0}
            onHoldCommission={0}
            pendingCommission={0}
            salesAidCommission={0}
            timeRange={timeRange}
            isLoading={true}
          />
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
          />
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Performance Metrics - Full Width */}
      <PerformanceMetrics
        soldAccounts={data.soldAccounts}
        grossRevenue={data.grossRevenue}
        installCount={data.installCount}
        installedRevenue={data.installedRevenue}
        retentionRate={data.retentionRate}
        timeRange={data.timeRange}
        isLoading={isLoading}
      />

      {/* Commission Summary and Project Buckets - Two Column Layout */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        <CommissionSummary
          earnedCommission={data.earnedCommission}
          lostCommission={data.lostCommission}
          onHoldCommission={data.onHoldCommission}
          pendingCommission={data.pendingCommission}
          salesAidCommission={data.salesAidCommission}
          timeRange={data.timeRange}
          isLoading={isLoading}
        />
        <ProjectBuckets
          buckets={data.buckets}
          isLoading={isLoading}
        />
      </div>
    </div>
  );
}