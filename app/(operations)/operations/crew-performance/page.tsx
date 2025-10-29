'use client';

import { useSession } from 'next-auth/react';
import { useQuery } from '@tanstack/react-query';
import { useState, useMemo } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Button } from '@/components/ui/button';
import { 
  MetricCard, 
  MetricCardSkeleton,
  CrewPerformanceTable,
  CrewComparisonChart,
  CrewTopPerformersCard,
  CrewSupportNeededCard
} from '@/components/operations';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import { CrewPerformanceDashboardData, CrewPerformanceMetrics } from '@/lib/types/operations';
import { 
  Users, 
  TrendingUp, 
  Clock, 
  Star, 
  Award, 
  AlertCircle, 
  Download, 
  BarChart3 
} from 'lucide-react';

export default function CrewPerformancePage() {
  const { data: session } = useSession();
  const [timeRange, setTimeRange] = useState<'7days' | '30days' | '90days' | 'all'>('30days');
  const [selectedCrew, setSelectedCrew] = useState<string>('all');
  const [selectedTaskType, setSelectedTaskType] = useState<string>('all');
  const [comparisonMetric, setComparisonMetric] = useState<'tasks_completed' | 'completion_time' | 'on_time' | 'ratings'>('tasks_completed');

  // Fetch crew performance data
  const { 
    data, 
    isLoading, 
    error 
  } = useQuery<CrewPerformanceDashboardData & { cached?: boolean }>({
    queryKey: ['crew-performance', timeRange, selectedCrew, selectedTaskType],
    queryFn: async () => {
      const params = new URLSearchParams({
        timeRange,
        taskType: selectedTaskType
      });
      if (selectedCrew !== 'all') {
        params.set('crewId', selectedCrew);
      }
      const response = await fetch(
        `${getBaseUrl()}/api/operations/crew-performance?${params}`
      );
      if (!response.ok) {
        throw new Error('Failed to fetch crew performance data');
      }
      return response.json();
    },
    enabled: !!session?.user,
    refetchInterval: 60000, // 60 seconds
  });

  // Helper function to format completion time
  const formatCompletionTime = (minutes: number | null): string => {
    if (!minutes) return 'N/A';
    const hours = Math.floor(minutes / 60);
    const mins = Math.round(minutes % 60);
    if (hours > 0) {
      return `${hours}h ${mins}m`;
    }
    return `${mins}m`;
  };

  // Helper function to get performance color
  const getPerformanceColor = (value: number, average: number): string => {
    if (value >= average) return 'text-green-600';
    return 'text-red-600';
  };

  // Helper function to render rating stars
  const getRatingStars = (rating: number | null): JSX.Element => {
    if (!rating) return <span className="text-gray-400">No ratings</span>;
    
    return (
      <div className="flex items-center gap-1">
        {[...Array(5)].map((_, i) => (
          <Star 
            key={i} 
            className={`h-4 w-4 ${i < Math.round(rating) ? 'fill-yellow-400 text-yellow-400' : 'text-gray-300'}`} 
          />
        ))}
        <span className="ml-1 text-sm font-medium">{rating.toFixed(1)}</span>
      </div>
    );
  };

  // CSV Export handler
  const handleExportCSV = () => {
    if (!data?.crewMetrics) return;

    const filename = `crew-performance-${timeRange}-${new Date().toISOString().split('T')[0]}.csv`;
    const fieldMapping = {
      crew_name: 'Crew Member',
      tasks_completed_today: 'Tasks Today',
      tasks_completed_week: 'Tasks This Week',
      tasks_completed_month: 'Tasks This Month',
      tasks_completed_total: 'Total Tasks',
      avg_completion_time_minutes: 'Avg Completion Time (min)',
      on_time_percentage: 'On-Time %',
      customer_rating_avg: 'Avg Rating',
      customer_rating_count: 'Rating Count',
      tasks_currently_assigned: 'Active Tasks',
      delayed_tasks_count: 'Delayed Tasks',
      exception_count: 'Exceptions',
      noshow_count: 'No-Shows'
    };

    exportAnalyticsToCSV(data.crewMetrics, filename, fieldMapping);
  };

  // Calculate active crew count
  const activeCrewCount = useMemo(() => {
    return data?.crewMetrics?.filter(crew => crew.tasks_currently_assigned > 0).length || 0;
  }, [data?.crewMetrics]);

  // Calculate total active tasks
  const totalActiveTasks = useMemo(() => {
    return data?.crewMetrics?.reduce((sum, crew) => sum + crew.tasks_currently_assigned, 0) || 0;
  }, [data?.crewMetrics]);

  if (!session?.user) {
    return (
      <div className="flex items-center justify-center h-64">
        <p className="text-gray-500">Please log in to view crew performance</p>
      </div>
    );
  }

  return (
    <div className="space-y-6 p-4 mobile:p-6 ipad:p-8">
      {/* Header */}
      <div className="flex flex-col gap-4 sm:flex-row sm:items-center sm:justify-between">
        <div>
          <h1 className="text-3xl font-bold">Crew Performance Dashboard</h1>
          <p className="text-gray-500 mt-1">Track field crew metrics and performance</p>
        </div>
        <Button onClick={handleExportCSV} disabled={!data?.crewMetrics?.length}>
          <Download className="h-4 w-4 mr-2" />
          Export to CSV
        </Button>
      </div>

      {/* Filters */}
      <div className="flex flex-col gap-4 sm:flex-row">
        <div className="flex-1">
          <label className="text-sm font-medium mb-2 block">Time Range</label>
          <Select value={timeRange} onValueChange={(value) => setTimeRange(value as typeof timeRange)}>
            <SelectTrigger>
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
        
        <div className="flex-1">
          <label className="text-sm font-medium mb-2 block">Crew Member</label>
          <Select value={selectedCrew} onValueChange={setSelectedCrew}>
            <SelectTrigger>
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Crew</SelectItem>
              {data?.crewMetrics?.map((crew) => (
                <SelectItem key={crew.entity_id} value={String(crew.entity_id)}>
                  {crew.crew_name}
                </SelectItem>
              ))}
            </SelectContent>
          </Select>
        </div>

        <div className="flex-1">
          <label className="text-sm font-medium mb-2 block">Task Type</label>
          <Select value={selectedTaskType} onValueChange={setSelectedTaskType}>
            <SelectTrigger>
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Tasks</SelectItem>
              <SelectItem value="survey">Survey</SelectItem>
              <SelectItem value="install">Install</SelectItem>
              <SelectItem value="inspection">Inspection</SelectItem>
              <SelectItem value="service">Service</SelectItem>
            </SelectContent>
          </Select>
        </div>
      </div>

      {/* Metrics Cards */}
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
        {isLoading ? (
          <>
            {[...Array(6)].map((_, i) => (
              <MetricCardSkeleton key={i} />
            ))}
          </>
        ) : data ? (
          <>
            <MetricCard
              title="Tasks This Month"
              value={data.teamAverages.tasks_completed_month.toString()}
              icon={<Award className="h-4 w-4" />}
              trend={data.teamAverages.tasks_completed_month > 0 ? 'up' : 'neutral'}
              description="Total completed this month"
            />
            <MetricCard
              title="Avg Completion Time"
              value={formatCompletionTime(data.teamAverages.avg_completion_time_minutes)}
              icon={<Clock className="h-4 w-4" />}
              trend="neutral"
              description="Average task duration"
            />
            <MetricCard
              title="On-Time Performance"
              value={`${data.teamAverages.on_time_percentage.toFixed(1)}%`}
              icon={<TrendingUp className="h-4 w-4" />}
              trend={data.teamAverages.on_time_percentage >= 80 ? 'up' : data.teamAverages.on_time_percentage >= 60 ? 'neutral' : 'down'}
              description="Tasks started on time"
            />
            <MetricCard
              title="Customer Rating"
              value={data.teamAverages.customer_rating_avg?.toFixed(1) || 'N/A'}
              icon={<Star className="h-4 w-4" />}
              trend={data.teamAverages.customer_rating_avg && data.teamAverages.customer_rating_avg >= 4.5 ? 'up' : 'neutral'}
              description="Average customer rating"
            />
            <MetricCard
              title="Active Crew Members"
              value={activeCrewCount.toString()}
              icon={<Users className="h-4 w-4" />}
              trend="neutral"
              description="Crew with active tasks"
            />
            <MetricCard
              title="Tasks In Progress"
              value={totalActiveTasks.toString()}
              icon={<BarChart3 className="h-4 w-4" />}
              trend="neutral"
              description="Currently assigned tasks"
            />
          </>
        ) : null}
      </div>

      {/* Error State */}
      {error && (
        <Card className="border-red-200 bg-red-50">
          <CardContent className="pt-6">
            <div className="flex items-center gap-2 text-red-600">
              <AlertCircle className="h-5 w-5" />
              <p>Failed to load crew performance data. Please try again.</p>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Empty State */}
      {!isLoading && !error && (!data?.crewMetrics || data.crewMetrics.length === 0) && (
        <Card>
          <CardContent className="pt-6">
            <div className="flex flex-col items-center justify-center py-12 text-center">
              <Users className="h-12 w-12 text-gray-400 mb-4" />
              <h3 className="text-lg font-semibold mb-2">No Crew Performance Data</h3>
              <p className="text-gray-500 max-w-md">
                No crew members found with performance data. Sync entities from Arrivy first.
              </p>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Tabs */}
      {!isLoading && !error && data?.crewMetrics && data.crewMetrics.length > 0 && (
        <Tabs defaultValue="overview" className="space-y-6">
          <TabsList>
            <TabsTrigger value="overview">Overview</TabsTrigger>
            <TabsTrigger value="comparison">Comparison</TabsTrigger>
            <TabsTrigger value="details">Details</TabsTrigger>
          </TabsList>

          {/* Overview Tab */}
          <TabsContent value="overview" className="space-y-6">
            <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
              <CrewTopPerformersCard leaderboard={data.leaderboard} />
              <CrewSupportNeededCard leaderboard={data.leaderboard} />
            </div>
          </TabsContent>

          {/* Comparison Tab */}
          <TabsContent value="comparison" className="space-y-6">
            <div className="flex flex-col gap-4 sm:flex-row sm:items-center sm:justify-between">
              <h2 className="text-xl font-semibold">Crew Comparison</h2>
              <div className="w-full sm:w-64">
                <Select value={comparisonMetric} onValueChange={(value) => setComparisonMetric(value as typeof comparisonMetric)}>
                  <SelectTrigger>
                    <SelectValue />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="tasks_completed">Tasks Completed</SelectItem>
                    <SelectItem value="completion_time">Completion Time</SelectItem>
                    <SelectItem value="on_time">On-Time Performance</SelectItem>
                    <SelectItem value="ratings">Customer Ratings</SelectItem>
                  </SelectContent>
                </Select>
              </div>
            </div>
            
            <CrewComparisonChart
              crewMetrics={data.crewMetrics}
              teamAverage={
                comparisonMetric === 'tasks_completed' ? data.teamAverages.tasks_completed_month :
                comparisonMetric === 'completion_time' ? (data.teamAverages.avg_completion_time_minutes || 0) :
                comparisonMetric === 'on_time' ? data.teamAverages.on_time_percentage :
                (data.teamAverages.customer_rating_avg || 0)
              }
              metric={comparisonMetric}
            />
          </TabsContent>

          {/* Details Tab */}
          <TabsContent value="details" className="space-y-6">
            <div className="flex items-center justify-between">
              <h2 className="text-xl font-semibold">Performance Details</h2>
              <Button onClick={handleExportCSV} variant="outline" size="sm">
                <Download className="h-4 w-4 mr-2" />
                Export Table
              </Button>
            </div>
            
            <CrewPerformanceTable
              crewMetrics={data.crewMetrics}
              teamAverages={data.teamAverages}
              onCrewClick={(entityId) => {
                setSelectedCrew(String(entityId));
              }}
            />
          </TabsContent>
        </Tabs>
      )}
    </div>
  );
}

