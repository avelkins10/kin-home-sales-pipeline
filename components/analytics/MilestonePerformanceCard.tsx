'use client'

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, Cell } from 'recharts';
import { BarChart3, AlertTriangle } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import type { MilestoneTimings, MilestoneTiming } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface MilestonePerformanceCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
}

interface ChartData {
  name: string;
  avgDays: number;
  medianDays: number | null;
  minDays: number | null;
  maxDays: number | null;
  projectCount: number;
  completionRate: number;
  isBottleneck: boolean;
  color: string;
}

function MilestonePerformanceSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-40" />
        </div>
        <Skeleton className="h-4 w-56" />
      </CardHeader>
      <CardContent>
        <div className="space-y-4">
          <Skeleton className="h-4 w-full" />
          <Skeleton className="h-80 w-full" />
          <Skeleton className="h-16 w-full" />
        </div>
      </CardContent>
    </Card>
  );
}

const CustomTooltip = ({ active, payload, label }: any) => {
  if (active && payload && payload.length) {
    const data = payload[0].payload;
    // Add defensive checks for undefined/null data
    if (!data || typeof data.projectCount === 'undefined' || data.projectCount === null) {
      return null;
    }
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">{label}</p>
        <div className="space-y-1 text-sm text-gray-600">
          <p>Avg: {data.avgDays ?? 0} days</p>
          {data.medianDays && <p>Median: {data.medianDays} days</p>}
          {data.minDays && data.maxDays && (
            <p>Range: {data.minDays} - {data.maxDays} days</p>
          )}
          <p>Projects: {data.projectCount.toLocaleString()}</p>
          <p>Completion: {formatPercentage(data.completionRate ?? 0)}</p>
        </div>
      </div>
    );
  }
  return null;
};

export function MilestonePerformanceCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: MilestonePerformanceCardProps) {
  const { data, isLoading, error } = useQuery<MilestoneTimings>({
    queryKey: ['milestone-timings', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/milestone-timings?timeRange=${timeRange}`;
      
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch milestone timings');
      return response.json();
    },
  });

  if (isLoading) {
    return <MilestonePerformanceSkeleton />;
  }

  if (error) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load milestone timings</p>
            <p className="text-red-500 text-sm mt-1">{error.message}</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!data || !data.milestones || data.milestones.length === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No milestone data available for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Prepare chart data and identify bottleneck
  const milestonesWithData = data.milestones.filter(m => m.avgDays !== null);
  
  if (milestonesWithData.length === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No milestone timing data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Find bottleneck (milestone with highest avgDays)
  const maxDays = Math.max(...milestonesWithData.map(m => m.avgDays!));
  const bottleneckMilestone = milestonesWithData.find(m => m.avgDays === maxDays);

  const chartData: ChartData[] = milestonesWithData.map(milestone => ({
    name: milestone.milestoneName,
    avgDays: milestone.avgDays!,
    medianDays: milestone.medianDays,
    minDays: milestone.minDays,
    maxDays: milestone.maxDays,
    projectCount: milestone.projectCount,
    completionRate: milestone.completionRate,
    isBottleneck: milestone.avgDays === maxDays,
    color: milestone.avgDays === maxDays ? '#ef4444' : '#3b82f6'
  }));

  return (
    <Card className="w-full" aria-label="Milestone performance analysis">
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <BarChart3 className="h-6 w-6 text-blue-600" />
          <span>Milestone Performance</span>
        </CardTitle>
        <p className="text-sm text-gray-600">
          Average days per milestone
        </p>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          {/* Bottleneck Alert */}
          {bottleneckMilestone && (
            <Alert className="bg-red-50 border-red-200">
              <AlertTriangle className="h-4 w-4 text-red-600" />
              <AlertDescription className="text-red-800">
                <strong>Bottleneck Identified:</strong> {bottleneckMilestone.milestoneName} takes {bottleneckMilestone.avgDays} days on average
              </AlertDescription>
            </Alert>
          )}

          {/* Bar Chart */}
          <div className="h-80 w-full">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart
                data={chartData}
                margin={{
                  top: 20,
                  right: 30,
                  left: 20,
                  bottom: 60,
                }}
              >
                <XAxis 
                  dataKey="name" 
                  angle={-45}
                  textAnchor="end"
                  height={80}
                  interval={0}
                  fontSize={12}
                />
                <YAxis 
                  label={{ value: 'Days', angle: -90, position: 'insideLeft' }}
                  allowDecimals={false}
                />
                <Tooltip content={<CustomTooltip />} />
                <Bar 
                  dataKey="avgDays" 
                  radius={[8, 8, 0, 0]}
                >
                  {chartData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.color} />
                  ))}
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          </div>

          {/* Overall Cycle Time Summary */}
          <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-blue-900">Overall Cycle Time</p>
                <p className="text-lg font-bold text-blue-900">
                  {data.overallCycleTime.avgDays ? `${Math.round(data.overallCycleTime.avgDays)} days` : 'N/A'}
                </p>
                <p className="text-sm text-blue-700">Sale to PTO</p>
              </div>
              <div className="text-right">
                <p className="text-sm text-blue-700">
                  {(data.overallCycleTime.projectCount || 0).toLocaleString()} projects
                </p>
                <p className="text-sm text-blue-700">
                  {formatPercentage(data.overallCycleTime.completionRate)} completion rate
                </p>
              </div>
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
