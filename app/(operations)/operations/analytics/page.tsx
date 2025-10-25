'use client';

import { useSession } from 'next-auth/react';
import { useQuery } from '@tanstack/react-query';
import { useState, useEffect, useMemo } from 'react';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { 
  MetricCard, 
  MetricCardSkeleton,
  PCOutreachTrendChart,
  PCStageDistributionChart,
  PCResponseRatesChart,
  PCSLAComplianceGauge,
  PCCoordinatorPerformanceTable,
  PCBottlenecksCard
} from '@/components/operations';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { isManagerRole } from '@/lib/utils/role-helpers';
import { formatPercentage } from '@/lib/utils/formatters';
import { PCAnalyticsData, PCTeamMetrics } from '@/lib/types/operations';
import { 
  TrendingUp, 
  Users, 
  Clock, 
  AlertTriangle, 
  CheckCircle, 
  MessageSquare 
} from 'lucide-react';

export default function PCAnalyticsPage() {
  const { data: session } = useSession();
  const [activeTab, setActiveTab] = useState<'personal' | 'team'>('personal');
  const [timeRange, setTimeRange] = useState<'7days' | '30days' | '90days' | 'all'>('30days');
  const [selectedCoordinatorEmail, setSelectedCoordinatorEmail] = useState<string>('all');

  // Check if user is a manager
  const isManager = useMemo(() => {
    return session?.user?.role && isManagerRole(session.user.role);
  }, [session?.user?.role]);

  // Personal Analytics Query
  const { 
    data: personalData, 
    isLoading: personalLoading, 
    error: personalError 
  } = useQuery<PCAnalyticsData>({
    queryKey: ['pc-analytics-personal', session?.user?.email, timeRange],
    queryFn: async () => {
      const response = await fetch(
        `${getBaseUrl()}/api/operations/analytics/personal?timeRange=${timeRange}`
      );
      if (!response.ok) {
        throw new Error('Failed to fetch personal analytics');
      }
      return response.json();
    },
    enabled: !!session?.user?.email,
    refetchInterval: 60000, // 60 seconds
  });

  // Team Analytics Query (only for managers)
  const { 
    data: teamData, 
    isLoading: teamLoading, 
    error: teamError 
  } = useQuery<{ teamMetrics: PCTeamMetrics }>({
    queryKey: ['pc-analytics-team', timeRange, selectedCoordinatorEmail],
    queryFn: async () => {
      const response = await fetch(
        `${getBaseUrl()}/api/operations/analytics/team?timeRange=${timeRange}&coordinatorEmail=${selectedCoordinatorEmail}`
      );
      if (!response.ok) {
        throw new Error('Failed to fetch team analytics');
      }
      return response.json();
    },
    enabled: !!session?.user && isManager,
    refetchInterval: 120000, // 120 seconds
  });

  // Reset coordinator filter when switching tabs
  useEffect(() => {
    if (activeTab === 'personal') {
      setSelectedCoordinatorEmail('all');
    }
  }, [activeTab]);

  if (!session?.user) {
    return (
      <div className="flex items-center justify-center h-64">
        <p className="text-gray-500">Please log in to view analytics</p>
      </div>
    );
  }

  return (
    <div className="space-y-6 p-4 mobile:p-6 ipad:p-8">
      {/* Header */}
      <div className="space-y-2">
        <h1 className="text-2xl mobile:text-3xl ipad:text-4xl font-bold text-gray-900">
          Analytics Dashboard
        </h1>
        <p className="text-gray-600">
          Track your performance and team metrics
        </p>
      </div>

      {/* Time Range Filter */}
      <div className="flex items-center gap-4">
        <label htmlFor="timeRange" className="text-sm font-medium text-gray-700">
          Time Range:
        </label>
        <Select value={timeRange} onValueChange={(value: any) => setTimeRange(value)}>
          <SelectTrigger className="w-40">
            <SelectValue />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="7days">Last 7 Days</SelectItem>
            <SelectItem value="30days">Last 30 Days</SelectItem>
            <SelectItem value="90days">Last 90 Days</SelectItem>
            <SelectItem value="all">All Time</SelectItem>
          </SelectContent>
        </Select>
      </div>

      {/* Tabs */}
      <Tabs value={activeTab} onValueChange={(value: any) => setActiveTab(value)} className="space-y-6">
        <TabsList className="grid w-full grid-cols-2">
          <TabsTrigger value="personal">Personal</TabsTrigger>
          {isManager && <TabsTrigger value="team">Team</TabsTrigger>}
        </TabsList>

        {/* Personal Tab */}
        <TabsContent value="personal" className="space-y-6">
          {personalLoading ? (
            <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-4">
              {Array.from({ length: 6 }).map((_, i) => (
                <MetricCardSkeleton key={i} />
              ))}
            </div>
          ) : personalError ? (
            <div className="text-center py-8">
              <p className="text-red-600">Failed to load personal analytics</p>
            </div>
          ) : personalData ? (
            <>
              {/* Personal Metrics Grid */}
              <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-4">
                <MetricCard
                  label="Daily Outreach"
                  value={personalData.personalMetrics.dailyOutreach}
                  icon={MessageSquare}
                  color="text-blue-600"
                  bgColor="bg-blue-50"
                />
                <MetricCard
                  label="Response Rate"
                  value={personalData.personalMetrics.responseRate}
                  formatter={formatPercentage}
                  icon={TrendingUp}
                  color="text-green-600"
                  bgColor="bg-green-50"
                />
                <MetricCard
                  label="Avg Time to Contact"
                  value={`${personalData.personalMetrics.avgTimeToContact.toFixed(1)} days`}
                  icon={Clock}
                  color="text-orange-600"
                  bgColor="bg-orange-50"
                />
                <MetricCard
                  label="Escalation Rate"
                  value={personalData.personalMetrics.escalationRate}
                  formatter={formatPercentage}
                  icon={AlertTriangle}
                  color="text-red-600"
                  bgColor="bg-red-50"
                />
                <MetricCard
                  label="SLA Compliance"
                  value={personalData.personalMetrics.slaCompliance}
                  formatter={formatPercentage}
                  icon={CheckCircle}
                  color="text-purple-600"
                  bgColor="bg-purple-50"
                />
                <MetricCard
                  label="Total Projects"
                  value={personalData.personalMetrics.totalProjects}
                  icon={Users}
                  color="text-gray-600"
                  bgColor="bg-gray-50"
                />
              </div>

              {/* Visualizations */}
              <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
                <PCOutreachTrendChart data={personalData.outreachTrend} />
                <PCResponseRatesChart data={personalData.responseBreakdown} />
                <PCStageDistributionChart data={personalData.stageDistribution} />
                <PCSLAComplianceGauge 
                  compliance={personalData.personalMetrics.slaCompliance}
                />
              </div>
            </>
          ) : null}
        </TabsContent>

        {/* Team Tab (only for managers) */}
        {isManager && (
          <TabsContent value="team" className="space-y-6">
            {teamLoading ? (
              <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                {Array.from({ length: 4 }).map((_, i) => (
                  <MetricCardSkeleton key={i} />
                ))}
              </div>
            ) : teamError ? (
              <div className="text-center py-8">
                <p className="text-red-600">Failed to load team analytics</p>
              </div>
            ) : teamData?.teamMetrics ? (
              <>
                {/* Team Coordinator Filter */}
                {teamData.teamMetrics.coordinators.length > 1 && (
                  <div className="flex items-center gap-4">
                    <label htmlFor="coordinator" className="text-sm font-medium text-gray-700">
                      Coordinator:
                    </label>
                    <Select 
                      value={selectedCoordinatorEmail} 
                      onValueChange={setSelectedCoordinatorEmail}
                    >
                      <SelectTrigger className="w-48">
                        <SelectValue />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="all">All Coordinators</SelectItem>
                        {teamData.teamMetrics.coordinators.map((coordinator) => (
                          <SelectItem 
                            key={coordinator.coordinatorEmail} 
                            value={coordinator.coordinatorEmail}
                          >
                            {coordinator.coordinatorName}
                          </SelectItem>
                        ))}
                      </SelectContent>
                    </Select>
                  </div>
                )}

                {/* Team Overview Metrics */}
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                  <MetricCard
                    label="Team Avg Response Rate"
                    value={teamData.teamMetrics.teamAverages.responseRate}
                    formatter={formatPercentage}
                    icon={TrendingUp}
                    color="text-green-600"
                    bgColor="bg-green-50"
                  />
                  <MetricCard
                    label="Team Avg Time to Contact"
                    value={`${teamData.teamMetrics.teamAverages.avgTimeToContact.toFixed(1)} days`}
                    icon={Clock}
                    color="text-orange-600"
                    bgColor="bg-orange-50"
                  />
                  <MetricCard
                    label="Team Avg SLA Compliance"
                    value={teamData.teamMetrics.teamAverages.slaCompliance}
                    formatter={formatPercentage}
                    icon={CheckCircle}
                    color="text-purple-600"
                    bgColor="bg-purple-50"
                  />
                  <MetricCard
                    label="Total Team Projects"
                    value={teamData.teamMetrics.coordinators.reduce((sum, c) => sum + c.totalProjects, 0)}
                    icon={Users}
                    color="text-gray-600"
                    bgColor="bg-gray-50"
                  />
                </div>

                {/* Coordinator Performance Table */}
                <PCCoordinatorPerformanceTable 
                  coordinators={teamData.teamMetrics.coordinators}
                  onCoordinatorClick={(email) => setSelectedCoordinatorEmail(email)}
                />

                {/* Bottlenecks */}
                <PCBottlenecksCard 
                  bottlenecks={teamData.teamMetrics.bottlenecks}
                  onViewProjects={(stageName) => {
                    // TODO: Implement project filtering by stage
                    console.log('View projects in stage:', stageName);
                  }}
                />
              </>
            ) : null}
          </TabsContent>
        )}
      </Tabs>
    </div>
  );
}
