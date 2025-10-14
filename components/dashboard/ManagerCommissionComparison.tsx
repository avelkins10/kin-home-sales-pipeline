'use client'

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { CommissionSummary } from './CommissionSummary';
import type { EnhancedDashboardMetrics, TimeRange } from '@/lib/types/dashboard';

interface ManagerCommissionComparisonProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: { startDate: string; endDate: string };
}

export function ManagerCommissionComparison({
  userId,
  role,
  timeRange,
  customDateRange,
}: ManagerCommissionComparisonProps) {
  // Fetch personal metrics
  const { data: personalData, isLoading: personalLoading } = useQuery<EnhancedDashboardMetrics>({
    queryKey: ['dashboard-metrics', userId, role, timeRange, customDateRange, 'personal'],
    queryFn: async () => {
      let url = `/api/dashboard/metrics?timeRange=${timeRange}&scope=personal`;
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch personal metrics');
      return response.json();
    },
    refetchInterval: 30000, // Poll every 30 seconds
    staleTime: 15000, // Consider stale after 15 seconds
  });

  // Fetch team metrics
  const { data: teamData, isLoading: teamLoading } = useQuery<EnhancedDashboardMetrics>({
    queryKey: ['dashboard-metrics', userId, role, timeRange, customDateRange, 'team'],
    queryFn: async () => {
      let url = `/api/dashboard/metrics?timeRange=${timeRange}&scope=team`;
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch team metrics');
      return response.json();
    },
    refetchInterval: 30000, // Poll every 30 seconds
    staleTime: 15000, // Consider stale after 15 seconds
  });

  const isLoading = personalLoading || teamLoading;

  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-lg font-semibold">Commission Breakdown</CardTitle>
      </CardHeader>
      <CardContent>
        <Tabs defaultValue="team" className="w-full">
          <TabsList className="grid w-full grid-cols-2">
            <TabsTrigger data-testid="tab-personal" value="personal">My Commission</TabsTrigger>
            <TabsTrigger data-testid="tab-team" value="team">Team Commission</TabsTrigger>
          </TabsList>
          
          <TabsContent value="personal" className="mt-4">
            <div data-testid="personal-commission-summary">
              <CommissionSummary
                earnedCommission={personalData?.earnedCommission || 0}
                lostCommission={personalData?.lostCommission || 0}
                onHoldCommission={personalData?.onHoldCommission || 0}
                pendingCommission={personalData?.pendingCommission || 0}
                salesAidCommission={personalData?.salesAidCommission || 0}
                timeRange={timeRange}
                isLoading={personalLoading}
                isManager={true}
                scope="personal"
              />
            </div>
          </TabsContent>
          
          <TabsContent value="team" className="mt-4">
            <div data-testid="team-commission-summary">
              <CommissionSummary
                earnedCommission={teamData?.earnedCommission || 0}
                lostCommission={teamData?.lostCommission || 0}
                onHoldCommission={teamData?.onHoldCommission || 0}
                pendingCommission={teamData?.pendingCommission || 0}
                salesAidCommission={teamData?.salesAidCommission || 0}
                timeRange={timeRange}
                isLoading={teamLoading}
                isManager={true}
                scope="team"
                commissionByMember={teamData?.commissionByMember}
              />
            </div>
          </TabsContent>
        </Tabs>
      </CardContent>
    </Card>
  );
}
