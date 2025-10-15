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
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">{data.name}</p>
        <p className="text-sm text-gray-600">
          Count: {data.value.toLocaleString()}
        </p>
        <p className="text-sm text-gray-600">
          Percentage: {formatPercentage(data.percentage)}
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
    acc.active += office.activeProjects;
    acc.cancelled += office.cancelledProjects;
    acc.onHold += office.onHoldProjects;
    return acc;
  }, { active: 0, cancelled: 0, onHold: 0 });

  const totalProjects = statusCounts.active + statusCounts.cancelled + statusCounts.onHold;

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

  // Prepare chart data
  const chartData: ChartData[] = [
    {
      name: 'Active',
      value: statusCounts.active,
      percentage: statusCounts.active / totalProjects,
      color: '#10b981'
    },
    {
      name: 'Cancelled',
      value: statusCounts.cancelled,
      percentage: statusCounts.cancelled / totalProjects,
      color: '#ef4444'
    },
    {
      name: 'On Hold',
      value: statusCounts.onHold,
      percentage: statusCounts.onHold / totalProjects,
      color: '#f59e0b'
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
          <div className="grid grid-cols-1 sm:grid-cols-3 gap-4">
            {chartData.map((status) => (
              <div 
                key={status.name}
                className={`p-3 rounded-lg border-l-4 ${
                  status.name === 'Active' ? 'border-green-500 bg-green-50' :
                  status.name === 'Cancelled' ? 'border-red-500 bg-red-50' :
                  'border-orange-500 bg-orange-50'
                }`}
              >
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm font-medium text-gray-900">{status.name}</p>
                    <p className="text-2xl font-bold text-gray-900">{status.value.toLocaleString()}</p>
                  </div>
                  <div className="text-right">
                    <p className="text-sm text-gray-600">{formatPercentage(status.percentage)}</p>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
