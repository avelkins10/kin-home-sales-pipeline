'use client'

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { PieChart, Pie, Cell, Legend, Tooltip, ResponsiveContainer } from 'recharts';
import { PieChart as PieChartIcon } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import type { OfficeMetrics } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface StatusBreakdownCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
}

interface ChartData {
  name: string;
  value: number;
  percentage: number;
  color: string;
}

function StatusBreakdownSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-36" />
        </div>
        <Skeleton className="h-4 w-48" />
      </CardHeader>
      <CardContent>
        <div className="flex flex-col items-center space-y-4">
          <Skeleton className="h-64 w-64 rounded-full" />
          <div className="space-y-2 w-full">
            <Skeleton className="h-4 w-full" />
            <Skeleton className="h-4 w-full" />
            <Skeleton className="h-4 w-full" />
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

const CustomTooltip = ({ active, payload }: any) => {
  if (active && payload && payload.length) {
    const data = payload[0].payload;
    // Add defensive checks for undefined/null data
    if (!data || typeof data.value === 'undefined' || data.value === null) {
      return null;
    }
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">{data.name}</p>
        <p className="text-sm text-gray-600">
          Count: {data.value.toLocaleString()}
        </p>
        <p className="text-sm text-gray-600">
          Percentage: {formatPercentage(data.percentage ?? 0)}
        </p>
      </div>
    );
  }
  return null;
};

const CustomLabel = ({ cx, cy, midAngle, innerRadius, outerRadius, percent, payload }: any) => {
  const pct = percent * 100;
  if (pct < 5) return null; // Don't show labels for slices < 5%
  
  const RADIAN = Math.PI / 180;
  const radius = innerRadius + (outerRadius - innerRadius) * 0.5;
  const x = cx + radius * Math.cos(-midAngle * RADIAN);
  const y = cy + radius * Math.sin(-midAngle * RADIAN);

  return (
    <text 
      x={x} 
      y={y} 
      fill="white" 
      textAnchor={x > cx ? 'start' : 'end'} 
      dominantBaseline="central"
      className="text-sm font-medium"
    >
      {formatPercentage(percent)}
    </text>
  );
};

export function StatusBreakdownCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: StatusBreakdownCardProps) {
  const { data, isLoading, error } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-metrics', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/office-metrics?timeRange=${timeRange}`;
      
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch office metrics');
      return response.json();
    },
  });

  if (isLoading) {
    return <StatusBreakdownSkeleton />;
  }

  if (error) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load status breakdown</p>
            <p className="text-red-500 text-sm mt-1">{error.message}</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!data || data.length === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No project data available for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Aggregate status counts across all offices
  const statusCounts = data.reduce((acc, office) => {
    acc.active += office.activeProjects || 0;
    acc.rejected += office.projectsRejected || 0;
    acc.pendingKca += office.pendingKcaProjects || 0;
    acc.pendingCancel += office.pendingCancelProjects || 0;
    acc.cancelled += office.cancelledProjects || 0;
    acc.generalHold += office.onHoldProjects || 0;
    acc.financeHold += office.financeHoldProjects || 0;
    acc.roofHold += office.roofHoldProjects || 0;
    return acc;
  }, {
    active: 0,
    rejected: 0,
    pendingKca: 0,
    pendingCancel: 0,
    cancelled: 0,
    generalHold: 0,
    financeHold: 0,
    roofHold: 0
  });

  // Calculate category totals matching OfficeOverviewCard structure
  const activeTotal = statusCounts.active;
  const needsAttentionTotal = statusCounts.rejected + statusCounts.pendingKca + statusCounts.pendingCancel;
  const onHoldTotal = statusCounts.generalHold + statusCounts.financeHold + statusCounts.roofHold;
  const cancelledTotal = statusCounts.cancelled;

  const totalProjects = activeTotal + needsAttentionTotal + onHoldTotal + cancelledTotal;

  if (totalProjects === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4 text-center">
            <p className="text-slate-600">No projects found for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Prepare chart data with main categories
  const chartData: ChartData[] = [
    {
      name: 'Active',
      value: activeTotal,
      percentage: activeTotal / totalProjects,
      color: '#10b981' // green
    },
    {
      name: 'Needs Attention',
      value: needsAttentionTotal,
      percentage: needsAttentionTotal / totalProjects,
      color: '#f59e0b' // yellow/orange
    },
    {
      name: 'On Hold',
      value: onHoldTotal,
      percentage: onHoldTotal / totalProjects,
      color: '#3b82f6' // blue
    },
    {
      name: 'Cancelled',
      value: cancelledTotal,
      percentage: cancelledTotal / totalProjects,
      color: '#ef4444' // red
    }
  ].filter(item => item.value > 0); // Only show statuses with projects

  return (
    <Card className="w-full" aria-label="Project status breakdown">
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <PieChartIcon className="h-6 w-6 text-blue-600" />
          <span>Status Breakdown</span>
        </CardTitle>
        <p className="text-sm text-gray-600">
          Distribution of project statuses
        </p>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          {/* Pie Chart */}
          <div className="h-64 w-full">
            <ResponsiveContainer width="100%" height="100%">
              <PieChart>
                <Pie
                  data={chartData}
                  dataKey="value"
                  nameKey="name"
                  cx="50%"
                  cy="50%"
                  outerRadius={80}
                  label={CustomLabel}
                  labelLine={false}
                >
                  {chartData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.color} />
                  ))}
                </Pie>
                <Tooltip content={<CustomTooltip />} />
                <Legend 
                  verticalAlign="bottom" 
                  height={36}
                  iconType="circle"
                  wrapperStyle={{ fontSize: '14px' }}
                />
              </PieChart>
            </ResponsiveContainer>
          </div>

          {/* Summary Stats */}
          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
            {/* Active */}
            {activeTotal > 0 && (
              <div className="bg-white rounded-lg p-4 border-l-4 border-green-500 shadow-sm">
                <div className="flex items-center justify-between mb-2">
                  <span className="text-sm font-medium text-gray-600">Active</span>
                  <span className="text-xs font-medium text-green-700 bg-green-100 px-2 py-1 rounded">
                    {formatPercentage(activeTotal / totalProjects)}
                  </span>
                </div>
                <div className="text-3xl font-bold text-green-700">{activeTotal}</div>
              </div>
            )}

            {/* Needs Attention */}
            {needsAttentionTotal > 0 && (
              <div className="bg-white rounded-lg p-4 border-l-4 border-yellow-500 shadow-sm">
                <div className="flex items-center justify-between mb-2">
                  <span className="text-sm font-medium text-gray-600">Needs Attention</span>
                  <span className="text-xs font-medium text-yellow-700 bg-yellow-100 px-2 py-1 rounded">
                    {formatPercentage(needsAttentionTotal / totalProjects)}
                  </span>
                </div>
                <div className="text-3xl font-bold text-yellow-700 mb-2">{needsAttentionTotal}</div>
                <div className="text-xs space-y-1">
                  {statusCounts.rejected > 0 && (
                    <div className="flex justify-between text-gray-600">
                      <span>Rejected</span>
                      <span className="font-medium text-red-700">{statusCounts.rejected}</span>
                    </div>
                  )}
                  {statusCounts.pendingKca > 0 && (
                    <div className="flex justify-between text-gray-600">
                      <span>Pending KCA</span>
                      <span className="font-medium">{statusCounts.pendingKca}</span>
                    </div>
                  )}
                  {statusCounts.pendingCancel > 0 && (
                    <div className="flex justify-between text-gray-600">
                      <span>Pending Cancel</span>
                      <span className="font-medium">{statusCounts.pendingCancel}</span>
                    </div>
                  )}
                </div>
              </div>
            )}

            {/* On Hold */}
            {onHoldTotal > 0 && (
              <div className="bg-white rounded-lg p-4 border-l-4 border-blue-500 shadow-sm">
                <div className="flex items-center justify-between mb-2">
                  <span className="text-sm font-medium text-gray-600">On Hold</span>
                  <span className="text-xs font-medium text-blue-700 bg-blue-100 px-2 py-1 rounded">
                    {formatPercentage(onHoldTotal / totalProjects)}
                  </span>
                </div>
                <div className="text-3xl font-bold text-blue-700 mb-2">{onHoldTotal}</div>
                <div className="text-xs space-y-1">
                  {statusCounts.generalHold > 0 && (
                    <div className="flex justify-between text-gray-600">
                      <span>General Hold</span>
                      <span className="font-medium">{statusCounts.generalHold}</span>
                    </div>
                  )}
                  {statusCounts.financeHold > 0 && (
                    <div className="flex justify-between text-gray-600">
                      <span>Finance Hold</span>
                      <span className="font-medium">{statusCounts.financeHold}</span>
                    </div>
                  )}
                  {statusCounts.roofHold > 0 && (
                    <div className="flex justify-between text-gray-600">
                      <span>Roof Hold</span>
                      <span className="font-medium">{statusCounts.roofHold}</span>
                    </div>
                  )}
                </div>
              </div>
            )}

            {/* Cancelled */}
            {cancelledTotal > 0 && (
              <div className="bg-white rounded-lg p-4 border-l-4 border-red-500 shadow-sm">
                <div className="flex items-center justify-between mb-2">
                  <span className="text-sm font-medium text-gray-600">Cancelled</span>
                  <span className="text-xs font-medium text-red-700 bg-red-100 px-2 py-1 rounded">
                    {formatPercentage(cancelledTotal / totalProjects)}
                  </span>
                </div>
                <div className="text-3xl font-bold text-red-700">{cancelledTotal}</div>
              </div>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
