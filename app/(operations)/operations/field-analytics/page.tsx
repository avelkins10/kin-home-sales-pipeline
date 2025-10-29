'use client';

import React, { useState, useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { format, subDays } from 'date-fns';
import {
  TrendingUp,
  TrendingDown,
  Clock,
  CheckCircle,
  AlertCircle,
  Users,
  Calendar,
  Download,
  BarChart3,
  AlertTriangle,
  Target,
  Activity
} from 'lucide-react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { DateRangePicker, DateRange } from '@/components/ui/date-range-picker';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import { getBaseUrl } from '@/lib/utils/baseUrl';

// Types for analytics data
interface TaskPerformanceData {
  overview: {
    total_tasks: number;
    completion_rate: number;
    avg_duration_minutes: number;
    on_time_percentage: number;
  };
  by_task_type: Array<{
    task_type: string;
    count: number;
    completed: number;
    avg_duration_minutes: number;
  }>;
  duration_distribution: Array<{
    task_type: string;
    min: number;
    avg: number;
    max: number;
    p50: number;
    p90: number;
  }>;
  daily_trends: Array<{
    date: string;
    total: number;
    completed: number;
    completion_rate: number;
  }>;
  delay_patterns: Array<{
    hour_of_day: number;
    avg_delay_minutes: number;
    task_count: number;
  }>;
}

interface ExceptionAnalyticsData {
  overview: {
    total_exceptions: number;
    exception_rate: number;
    by_type: Record<string, number>;
  };
  exception_breakdown: Array<{
    exception_type: string;
    count: number;
    percentage: number;
    avg_per_task: number;
  }>;
  exception_trends: Array<{
    date: string;
    total_exceptions: number;
    exception_rate: number;
    most_common_type: string;
  }>;
  by_crew: Array<{
    entity_id: number;
    crew_name: string;
    exception_count: number;
    task_count: number;
    exception_rate: number;
    quality_score: number;
  }>;
  root_causes: Array<{
    reason: string;
    count: number;
  }>;
}

interface WorkloadDistributionData {
  overview: {
    total_active_tasks: number;
    avg_tasks_per_crew: number;
    max_tasks_on_crew: number;
    workload_imbalance_score: number;
  };
  by_crew: Array<{
    entity_id: number;
    crew_name: string;
    active_tasks: number;
    scheduled_tasks: number;
    completed_tasks: number;
    capacity_percentage: number;
    status: string;
  }>;
  hourly_distribution: Array<{
    hour: number;
    task_count: number;
  }>;
}

export default function FieldAnalyticsPage() {
  const { data: session, status } = useSession();

  // State
  const [dateRange, setDateRange] = useState<DateRange>({
    from: subDays(new Date(), 29),
    to: new Date(),
  });
  const [selectedTaskType, setSelectedTaskType] = useState<string>('all');
  const [selectedCrew, setSelectedCrew] = useState<string>('all');

  // Check authentication and role
  if (status === 'loading') {
    return <div className="flex items-center justify-center h-screen">Loading...</div>;
  }

  if (status === 'unauthenticated') {
    return <div className="flex items-center justify-center h-screen">Please log in to view field analytics.</div>;
  }

  const operationsRoles = ['operations_coordinator', 'operations_manager', 'office_leader', 'regional', 'super_admin'];
  if (!session?.user?.role || !operationsRoles.includes(session.user.role)) {
    return <div className="flex items-center justify-center h-screen">Access denied. Operations role required.</div>;
  }

  // Build query params
  const queryParams = useMemo(() => {
    const params = new URLSearchParams();
    if (dateRange?.from) params.append('startDate', format(dateRange.from, 'yyyy-MM-dd'));
    if (dateRange?.to) params.append('endDate', format(dateRange.to, 'yyyy-MM-dd'));
    if (selectedTaskType !== 'all') params.append('taskType', selectedTaskType);
    if (selectedCrew !== 'all') params.append('entityId', selectedCrew);
    return params.toString();
  }, [dateRange, selectedTaskType, selectedCrew]);

  // Fetch Task Performance Analytics
  const { data: taskPerformance, isLoading: loadingTaskPerf, error: taskPerfError } = useQuery<TaskPerformanceData>({
    queryKey: ['task-performance-analytics', queryParams],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/operations/analytics/task-performance?${queryParams}`);
      if (!response.ok) throw new Error('Failed to fetch task performance analytics');
      return response.json();
    },
    refetchInterval: 60000,
  });

  // Fetch Exception Analytics
  const { data: exceptionData, isLoading: loadingExceptions, error: exceptionsError } = useQuery<ExceptionAnalyticsData>({
    queryKey: ['exception-analytics', queryParams],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/operations/analytics/exceptions?${queryParams}`);
      if (!response.ok) throw new Error('Failed to fetch exception analytics');
      return response.json();
    },
    refetchInterval: 60000,
  });

  // Fetch Workload Distribution
  const { data: workloadData, isLoading: loadingWorkload, error: workloadError } = useQuery<WorkloadDistributionData>({
    queryKey: ['workload-distribution', queryParams],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/operations/analytics/workload?${queryParams}`);
      if (!response.ok) throw new Error('Failed to fetch workload distribution');
      return response.json();
    },
    refetchInterval: 30000,
  });

  // Helper functions
  const formatDuration = (minutes: number | null): string => {
    if (!minutes) return 'N/A';
    const hours = Math.floor(minutes / 60);
    const mins = Math.round(minutes % 60);
    if (hours > 0) return `${hours}h ${mins}m`;
    return `${mins}m`;
  };

  const formatPercentage = (value: number): string => {
    return `${value.toFixed(1)}%`;
  };

  // CSV Export handlers
  const handleExportTaskPerformance = () => {
    if (!taskPerformance?.by_task_type) return;
    const filename = `task-performance-${format(new Date(), 'yyyy-MM-dd')}.csv`;
    const fieldMapping = {
      task_type: 'Task Type',
      count: 'Total Tasks',
      completed: 'Completed',
      avg_duration_minutes: 'Avg Duration (min)',
    };
    exportAnalyticsToCSV(taskPerformance.by_task_type, filename, fieldMapping);
  };

  const handleExportExceptions = () => {
    if (!exceptionData?.by_crew) return;
    const filename = `exception-analytics-${format(new Date(), 'yyyy-MM-dd')}.csv`;
    const fieldMapping = {
      crew_name: 'Crew Member',
      exception_count: 'Exceptions',
      task_count: 'Tasks',
      exception_rate: 'Exception Rate (%)',
      quality_score: 'Quality Score',
    };
    exportAnalyticsToCSV(exceptionData.by_crew, filename, fieldMapping);
  };

  const handleExportWorkload = () => {
    if (!workloadData?.by_crew) return;
    const filename = `workload-distribution-${format(new Date(), 'yyyy-MM-dd')}.csv`;
    const fieldMapping = {
      crew_name: 'Crew Member',
      active_tasks: 'Active Tasks',
      scheduled_tasks: 'Scheduled Tasks',
      completed_tasks: 'Completed Tasks',
      capacity_percentage: 'Capacity %',
      status: 'Status',
    };
    exportAnalyticsToCSV(workloadData.by_crew, filename, fieldMapping);
  };

  const isLoading = loadingTaskPerf || loadingExceptions || loadingWorkload;

  return (
    <div className="container mx-auto px-4 py-8 max-w-7xl">
      {/* Header */}
      <div className="mb-8">
        <h1 className="text-3xl font-bold text-gray-900 mb-2">Field Operations Analytics</h1>
        <p className="text-gray-600">Comprehensive insights into task performance, exceptions, and crew workload</p>
      </div>

      {/* Filters */}
      <Card className="mb-6">
        <CardContent className="p-4">
          <div className="flex flex-col md:flex-row gap-4">
            {/* Date Range Picker */}
            <div className="flex-1">
              <label className="text-sm font-medium mb-2 block">Date Range</label>
              <DateRangePicker
                value={dateRange}
                onChange={(range) => setDateRange(range || { from: undefined, to: undefined })}
              />
            </div>

            {/* Task Type Filter */}
            <div className="flex-1">
              <label className="text-sm font-medium mb-2 block">Task Type</label>
              <Select value={selectedTaskType} onValueChange={setSelectedTaskType}>
                <SelectTrigger>
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">All Types</SelectItem>
                  <SelectItem value="survey">Survey</SelectItem>
                  <SelectItem value="install">Install</SelectItem>
                  <SelectItem value="inspection">Inspection</SelectItem>
                  <SelectItem value="service">Service</SelectItem>
                </SelectContent>
              </Select>
            </div>

            {/* Crew Filter */}
            <div className="flex-1">
              <label className="text-sm font-medium mb-2 block">Crew Member</label>
              <Select value={selectedCrew} onValueChange={setSelectedCrew}>
                <SelectTrigger>
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">All Crew</SelectItem>
                  {workloadData?.by_crew?.map((crew) => (
                    <SelectItem key={crew.entity_id} value={String(crew.entity_id)}>
                      {crew.crew_name}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Tabs for Different Analytics Sections */}
      <Tabs defaultValue="task-performance" className="space-y-6">
        <TabsList>
          <TabsTrigger value="task-performance">
            <Target className="h-4 w-4 mr-2" />
            Task Performance
          </TabsTrigger>
          <TabsTrigger value="exceptions">
            <AlertTriangle className="h-4 w-4 mr-2" />
            Exceptions & Quality
          </TabsTrigger>
          <TabsTrigger value="workload">
            <Activity className="h-4 w-4 mr-2" />
            Crew Workload
          </TabsTrigger>
        </TabsList>

        {/* Task Performance Tab */}
        <TabsContent value="task-performance" className="space-y-6">
          <div className="flex justify-between items-center">
            <h2 className="text-xl font-semibold">Task Performance Metrics</h2>
            <Button onClick={handleExportTaskPerformance} variant="outline" size="sm">
              <Download className="h-4 w-4 mr-2" />
              Export CSV
            </Button>
          </div>

          {/* Task Performance Overview Cards */}
          <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Total Tasks</p>
                    <p className="text-2xl font-bold">
                      {loadingTaskPerf ? '...' : taskPerformance?.overview.total_tasks || 0}
                    </p>
                  </div>
                  <BarChart3 className="h-8 w-8 text-blue-500" />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Completion Rate</p>
                    <p className="text-2xl font-bold">
                      {loadingTaskPerf ? '...' : formatPercentage(taskPerformance?.overview.completion_rate || 0)}
                    </p>
                  </div>
                  <CheckCircle className={`h-8 w-8 ${(taskPerformance?.overview.completion_rate || 0) >= 80 ? 'text-green-500' : 'text-orange-500'}`} />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Avg Duration</p>
                    <p className="text-2xl font-bold">
                      {loadingTaskPerf ? '...' : formatDuration(taskPerformance?.overview.avg_duration_minutes || 0)}
                    </p>
                  </div>
                  <Clock className="h-8 w-8 text-purple-500" />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">On-Time %</p>
                    <p className="text-2xl font-bold">
                      {loadingTaskPerf ? '...' : formatPercentage(taskPerformance?.overview.on_time_percentage || 0)}
                    </p>
                  </div>
                  <Target className={`h-8 w-8 ${(taskPerformance?.overview.on_time_percentage || 0) >= 80 ? 'text-green-500' : 'text-orange-500'}`} />
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Task Performance by Type */}
          <Card>
            <CardHeader>
              <CardTitle>Performance by Task Type</CardTitle>
            </CardHeader>
            <CardContent>
              {loadingTaskPerf ? (
                <div className="space-y-4">
                  {[...Array(4)].map((_, i) => (
                    <div key={i} className="animate-pulse h-16 bg-gray-200 rounded"></div>
                  ))}
                </div>
              ) : taskPerfError ? (
                <div className="text-center py-8 text-red-600">
                  <AlertCircle className="h-12 w-12 mx-auto mb-4" />
                  <p>Failed to load task performance data</p>
                </div>
              ) : !taskPerformance?.by_task_type?.length ? (
                <div className="text-center py-8 text-gray-500">
                  <BarChart3 className="h-12 w-12 mx-auto mb-4" />
                  <p>No task data available for selected filters</p>
                </div>
              ) : (
                <div className="space-y-4">
                  {taskPerformance.by_task_type.map((item) => (
                    <div key={item.task_type} className="border rounded-lg p-4">
                      <div className="flex items-center justify-between mb-2">
                        <h4 className="font-semibold capitalize">{item.task_type}</h4>
                        <Badge variant="outline">{item.count} tasks</Badge>
                      </div>
                      <div className="grid grid-cols-3 gap-4 text-sm">
                        <div>
                          <p className="text-gray-600">Completed</p>
                          <p className="font-medium">{item.completed} ({formatPercentage((item.completed / item.count) * 100)})</p>
                        </div>
                        <div>
                          <p className="text-gray-600">Avg Duration</p>
                          <p className="font-medium">{formatDuration(item.avg_duration_minutes)}</p>
                        </div>
                        <div>
                          <p className="text-gray-600">Completion Rate</p>
                          <div className="w-full bg-gray-200 rounded-full h-2 mt-1">
                            <div
                              className="bg-blue-500 h-2 rounded-full"
                              style={{ width: `${(item.completed / item.count) * 100}%` }}
                            />
                          </div>
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </CardContent>
          </Card>

          {/* Daily Trends */}
          {taskPerformance?.daily_trends && taskPerformance.daily_trends.length > 0 && (
            <Card>
              <CardHeader>
                <CardTitle>Daily Completion Trends</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-2">
                  {taskPerformance.daily_trends.slice(-14).map((day) => (
                    <div key={day.date} className="flex items-center justify-between py-2 border-b last:border-0">
                      <span className="text-sm font-medium">{format(new Date(day.date), 'MMM dd')}</span>
                      <div className="flex items-center space-x-4">
                        <span className="text-sm text-gray-600">{day.completed}/{day.total} tasks</span>
                        <Badge variant={day.completion_rate >= 80 ? 'default' : 'secondary'}>
                          {formatPercentage(day.completion_rate)}
                        </Badge>
                      </div>
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>
          )}
        </TabsContent>

        {/* Exceptions & Quality Tab */}
        <TabsContent value="exceptions" className="space-y-6">
          <div className="flex justify-between items-center">
            <h2 className="text-xl font-semibold">Exception & Quality Insights</h2>
            <Button onClick={handleExportExceptions} variant="outline" size="sm">
              <Download className="h-4 w-4 mr-2" />
              Export CSV
            </Button>
          </div>

          {/* Exception Overview Cards */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Total Exceptions</p>
                    <p className="text-2xl font-bold">
                      {loadingExceptions ? '...' : exceptionData?.overview.total_exceptions || 0}
                    </p>
                  </div>
                  <AlertTriangle className="h-8 w-8 text-orange-500" />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Exception Rate</p>
                    <p className="text-2xl font-bold">
                      {loadingExceptions ? '...' : formatPercentage(exceptionData?.overview.exception_rate || 0)}
                    </p>
                  </div>
                  <TrendingDown className={`h-8 w-8 ${(exceptionData?.overview.exception_rate || 0) < 10 ? 'text-green-500' : 'text-red-500'}`} />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Exception Types</p>
                    <p className="text-2xl font-bold">
                      {loadingExceptions ? '...' : Object.keys(exceptionData?.overview.by_type || {}).length}
                    </p>
                  </div>
                  <AlertCircle className="h-8 w-8 text-red-500" />
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Exception Breakdown */}
          <Card>
            <CardHeader>
              <CardTitle>Exception Breakdown by Type</CardTitle>
            </CardHeader>
            <CardContent>
              {loadingExceptions ? (
                <div className="space-y-4">
                  {[...Array(5)].map((_, i) => (
                    <div key={i} className="animate-pulse h-12 bg-gray-200 rounded"></div>
                  ))}
                </div>
              ) : exceptionsError ? (
                <div className="text-center py-8 text-red-600">
                  <AlertCircle className="h-12 w-12 mx-auto mb-4" />
                  <p>Failed to load exception data</p>
                </div>
              ) : !exceptionData?.exception_breakdown?.length ? (
                <div className="text-center py-8 text-gray-500">
                  <CheckCircle className="h-12 w-12 mx-auto mb-4 text-green-500" />
                  <p>No exceptions found - Great job!</p>
                </div>
              ) : (
                <div className="space-y-3">
                  {exceptionData.exception_breakdown.map((item) => (
                    <div key={item.exception_type} className="flex items-center justify-between p-3 border rounded-lg">
                      <div className="flex-1">
                        <h4 className="font-medium capitalize">{item.exception_type.replace(/_/g, ' ')}</h4>
                        <p className="text-sm text-gray-600">
                          {item.count} occurrences ({formatPercentage(item.percentage)})
                        </p>
                      </div>
                      <Badge variant={item.percentage > 20 ? 'destructive' : 'secondary'}>
                        {formatPercentage(item.percentage)}
                      </Badge>
                    </div>
                  ))}
                </div>
              )}
            </CardContent>
          </Card>

          {/* Crew Quality Scores */}
          <Card>
            <CardHeader>
              <CardTitle>Crew Quality Scores</CardTitle>
            </CardHeader>
            <CardContent>
              {loadingExceptions ? (
                <div className="space-y-4">
                  {[...Array(5)].map((_, i) => (
                    <div key={i} className="animate-pulse h-16 bg-gray-200 rounded"></div>
                  ))}
                </div>
              ) : !exceptionData?.by_crew?.length ? (
                <div className="text-center py-8 text-gray-500">
                  <Users className="h-12 w-12 mx-auto mb-4" />
                  <p>No crew data available</p>
                </div>
              ) : (
                <div className="space-y-3">
                  {exceptionData.by_crew
                    .sort((a, b) => b.quality_score - a.quality_score)
                    .map((crew) => (
                      <div key={crew.entity_id} className="border rounded-lg p-4">
                        <div className="flex items-center justify-between mb-2">
                          <h4 className="font-semibold">{crew.crew_name}</h4>
                          <Badge
                            variant={crew.quality_score >= 90 ? 'default' : crew.quality_score >= 70 ? 'secondary' : 'destructive'}
                          >
                            Quality: {crew.quality_score.toFixed(0)}%
                          </Badge>
                        </div>
                        <div className="grid grid-cols-3 gap-4 text-sm">
                          <div>
                            <p className="text-gray-600">Tasks</p>
                            <p className="font-medium">{crew.task_count}</p>
                          </div>
                          <div>
                            <p className="text-gray-600">Exceptions</p>
                            <p className="font-medium text-orange-600">{crew.exception_count}</p>
                          </div>
                          <div>
                            <p className="text-gray-600">Exception Rate</p>
                            <p className="font-medium">{formatPercentage(crew.exception_rate)}</p>
                          </div>
                        </div>
                      </div>
                    ))}
                </div>
              )}
            </CardContent>
          </Card>
        </TabsContent>

        {/* Crew Workload Tab */}
        <TabsContent value="workload" className="space-y-6">
          <div className="flex justify-between items-center">
            <h2 className="text-xl font-semibold">Crew Workload & Capacity</h2>
            <Button onClick={handleExportWorkload} variant="outline" size="sm">
              <Download className="h-4 w-4 mr-2" />
              Export CSV
            </Button>
          </div>

          {/* Workload Overview Cards */}
          <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Active Tasks</p>
                    <p className="text-2xl font-bold">
                      {loadingWorkload ? '...' : workloadData?.overview.total_active_tasks || 0}
                    </p>
                  </div>
                  <Activity className="h-8 w-8 text-blue-500" />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Avg per Crew</p>
                    <p className="text-2xl font-bold">
                      {loadingWorkload ? '...' : (workloadData?.overview.avg_tasks_per_crew || 0).toFixed(1)}
                    </p>
                  </div>
                  <Users className="h-8 w-8 text-green-500" />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Max Load</p>
                    <p className="text-2xl font-bold">
                      {loadingWorkload ? '...' : workloadData?.overview.max_tasks_on_crew || 0}
                    </p>
                  </div>
                  <AlertTriangle className="h-8 w-8 text-orange-500" />
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm text-gray-600">Imbalance Score</p>
                    <p className="text-2xl font-bold">
                      {loadingWorkload ? '...' : (workloadData?.overview.workload_imbalance_score || 0).toFixed(1)}%
                    </p>
                  </div>
                  <BarChart3 className={`h-8 w-8 ${(workloadData?.overview.workload_imbalance_score || 0) < 20 ? 'text-green-500' : 'text-orange-500'}`} />
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Crew Workload Distribution */}
          <Card>
            <CardHeader>
              <CardTitle>Current Workload by Crew</CardTitle>
            </CardHeader>
            <CardContent>
              {loadingWorkload ? (
                <div className="space-y-4">
                  {[...Array(5)].map((_, i) => (
                    <div key={i} className="animate-pulse h-20 bg-gray-200 rounded"></div>
                  ))}
                </div>
              ) : workloadError ? (
                <div className="text-center py-8 text-red-600">
                  <AlertCircle className="h-12 w-12 mx-auto mb-4" />
                  <p>Failed to load workload data</p>
                </div>
              ) : !workloadData?.by_crew?.length ? (
                <div className="text-center py-8 text-gray-500">
                  <Users className="h-12 w-12 mx-auto mb-4" />
                  <p>No crew workload data available</p>
                </div>
              ) : (
                <div className="space-y-4">
                  {workloadData.by_crew
                    .sort((a, b) => b.active_tasks - a.active_tasks)
                    .map((crew) => (
                      <div key={crew.entity_id} className="border rounded-lg p-4">
                        <div className="flex items-center justify-between mb-3">
                          <div className="flex items-center space-x-3">
                            <h4 className="font-semibold">{crew.crew_name}</h4>
                            <Badge
                              variant={
                                crew.status === 'overloaded' ? 'destructive' :
                                crew.status === 'optimal' ? 'default' :
                                'secondary'
                              }
                            >
                              {crew.status}
                            </Badge>
                          </div>
                          <span className="text-sm font-medium">
                            Capacity: {crew.capacity_percentage.toFixed(0)}%
                          </span>
                        </div>

                        <div className="grid grid-cols-4 gap-4 mb-2 text-sm">
                          <div>
                            <p className="text-gray-600">Active</p>
                            <p className="font-bold text-blue-600">{crew.active_tasks}</p>
                          </div>
                          <div>
                            <p className="text-gray-600">Scheduled</p>
                            <p className="font-medium">{crew.scheduled_tasks}</p>
                          </div>
                          <div>
                            <p className="text-gray-600">Completed</p>
                            <p className="font-medium text-green-600">{crew.completed_tasks}</p>
                          </div>
                          <div>
                            <p className="text-gray-600">Total</p>
                            <p className="font-medium">{crew.active_tasks + crew.scheduled_tasks + crew.completed_tasks}</p>
                          </div>
                        </div>

                        {/* Capacity Bar */}
                        <div className="w-full bg-gray-200 rounded-full h-2">
                          <div
                            className={`h-2 rounded-full ${
                              crew.capacity_percentage > 100 ? 'bg-red-500' :
                              crew.capacity_percentage > 80 ? 'bg-orange-500' :
                              'bg-green-500'
                            }`}
                            style={{ width: `${Math.min(crew.capacity_percentage, 100)}%` }}
                          />
                        </div>
                      </div>
                    ))}
                </div>
              )}
            </CardContent>
          </Card>

          {/* Hourly Distribution */}
          {workloadData?.hourly_distribution && workloadData.hourly_distribution.length > 0 && (
            <Card>
              <CardHeader>
                <CardTitle>Task Distribution by Hour</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-2">
                  {workloadData.hourly_distribution.map((hour) => (
                    <div key={hour.hour} className="flex items-center space-x-3">
                      <span className="text-sm font-medium w-20">
                        {hour.hour}:00 - {hour.hour + 1}:00
                      </span>
                      <div className="flex-1 bg-gray-200 rounded-full h-6 relative">
                        <div
                          className="bg-blue-500 h-6 rounded-full flex items-center justify-end pr-2"
                          style={{
                            width: `${Math.max((hour.task_count / Math.max(...workloadData.hourly_distribution.map(h => h.task_count))) * 100, 5)}%`
                          }}
                        >
                          <span className="text-xs text-white font-medium">{hour.task_count}</span>
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
}
