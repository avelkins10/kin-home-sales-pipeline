'use client';

import { useQuery } from '@tanstack/react-query';
import { useState, useMemo } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Skeleton } from '@/components/ui/skeleton';
import { BarChart3 } from 'lucide-react';
import { BarChart, Bar, XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer } from 'recharts';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import type { OfficeMetrics } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface VisualComparisonsCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
}

type MetricKey = 'totalProjects' | 'avgSystemSize' | 'avgNetPpw' | 'firstTimePassRate' | 'avgCycleTime';

interface MetricConfig {
  key: MetricKey;
  label: string;
  formatter: (val: number) => string;
  color: string;
}

const METRICS: MetricConfig[] = [
  {
    key: 'totalProjects',
    label: 'Total Projects',
    formatter: (val) => val.toLocaleString(),
    color: '#3b82f6' // blue
  },
  {
    key: 'avgSystemSize',
    label: 'Avg System Size (kW)',
    formatter: formatSystemSize,
    color: '#8b5cf6' // purple
  },
  {
    key: 'avgNetPpw',
    label: 'Avg Net PPW',
    formatter: formatPPW,
    color: '#10b981' // green
  },
  {
    key: 'firstTimePassRate',
    label: 'First-Time Pass Rate (%)',
    formatter: formatPercentage,
    color: '#f59e0b' // amber
  },
  {
    key: 'avgCycleTime',
    label: 'Avg Cycle Time (days)',
    formatter: (val) => Math.round(val).toString(),
    color: '#ef4444' // red
  }
];

function VisualComparisonsSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-48" />
        </div>
      </CardHeader>
      <CardContent>
        <Skeleton className="h-96 w-full" />
      </CardContent>
    </Card>
  );
}

const CustomTooltip = ({ active, payload, label, metric }: any) => {
  if (active && payload && payload.length) {
    const metricConfig = METRICS.find(m => m.key === metric);
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900 mb-2">{label}</p>
        {payload.map((entry: any, index: number) => (
          <div key={index} className="flex items-center space-x-2">
            <div className="w-3 h-3 rounded-full" style={{ backgroundColor: entry.color }}></div>
            <span className="text-sm text-gray-600">{entry.name}:</span>
            <span className="text-sm font-semibold text-gray-900">
              {metricConfig?.formatter(entry.value) || entry.value}
            </span>
          </div>
        ))}
      </div>
    );
  }
  return null;
};

export function VisualComparisonsCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: VisualComparisonsCardProps) {
  const [selectedMetric, setSelectedMetric] = useState<MetricKey>('totalProjects');

  const { data, isLoading, error } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-metrics-visual', userId, role, timeRange, customDateRange, officeIds],
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
      const result = await response.json();
      return result.metrics || [];
    },
  });

  // Prepare chart data
  const chartData = useMemo(() => {
    if (!data || data.length === 0) return [];

    const metricConfig = METRICS.find(m => m.key === selectedMetric);
    if (!metricConfig) return [];

    // Sort offices by the selected metric (descending)
    const sorted = [...data].sort((a, b) => {
      let aVal: number;
      let bVal: number;

      if (selectedMetric === 'avgCycleTime') {
        aVal = a.avgCycleTime || 0;
        bVal = b.avgCycleTime || 0;
      } else {
        aVal = typeof a[selectedMetric] === 'number' ? a[selectedMetric] : 0;
        bVal = typeof b[selectedMetric] === 'number' ? b[selectedMetric] : 0;
      }

      return bVal - aVal;
    });

    // Take top 10 offices for cleaner visualization
    const topOffices = sorted.slice(0, 10);

    return topOffices.map(office => {
      let value: number;

      if (selectedMetric === 'avgCycleTime') {
        value = office.avgCycleTime || 0;
      } else {
        const metricValue = office[selectedMetric];
        value = typeof metricValue === 'number' ? metricValue : 0;
      }

      return {
        name: office.officeName.length > 20
          ? office.officeName.substring(0, 20) + '...'
          : office.officeName,
        fullName: office.officeName,
        value
      };
    });
  }, [data, selectedMetric]);

  // Calculate company average line
  const companyAverage = useMemo(() => {
    if (!data || data.length === 0) return 0;

    const totalProjects = data.reduce((sum, o) => sum + o.totalProjects, 0);

    switch (selectedMetric) {
      case 'totalProjects':
        return totalProjects / data.length;
      case 'avgSystemSize':
        return totalProjects > 0
          ? data.reduce((sum, o) => sum + o.avgSystemSize * o.totalProjects, 0) / totalProjects
          : 0;
      case 'avgNetPpw':
        return totalProjects > 0
          ? data.reduce((sum, o) => sum + o.avgNetPpw * o.totalProjects, 0) / totalProjects
          : 0;
      case 'firstTimePassRate':
        return totalProjects > 0
          ? data.reduce((sum, o) => sum + o.firstTimePassRate * o.totalProjects, 0) / totalProjects
          : 0;
      case 'avgCycleTime':
        const officesWithCycleTime = data.filter(o => o.avgCycleTime !== null);
        const projectsWithCycleTime = officesWithCycleTime.reduce((sum, o) => sum + o.totalProjects, 0);
        return projectsWithCycleTime > 0
          ? officesWithCycleTime.reduce((sum, o) => sum + (o.avgCycleTime || 0) * o.totalProjects, 0) / projectsWithCycleTime
          : 0;
      default:
        return 0;
    }
  }, [data, selectedMetric]);

  if (isLoading) {
    return <VisualComparisonsSkeleton />;
  }

  if (error || !data) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load visual comparisons</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (data.length === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No data available for visual comparisons</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const currentMetric = METRICS.find(m => m.key === selectedMetric);

  return (
    <Card className="w-full" aria-label="Visual office comparisons">
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <BarChart3 className="h-6 w-6 text-blue-600" />
            <CardTitle>Visual Office Comparison</CardTitle>
          </div>
          {/* Metric selector */}
          <div className="flex items-center space-x-2">
            <span className="text-sm text-gray-600">Metric:</span>
            <Select value={selectedMetric} onValueChange={(value) => setSelectedMetric(value as MetricKey)}>
              <SelectTrigger className="w-64">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {METRICS.map(metric => (
                  <SelectItem key={metric.key} value={metric.key}>
                    {metric.label}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>
        </div>
        <p className="text-sm text-gray-600">
          Comparing {data.length} office{data.length !== 1 ? 's' : ''} • Top 10 shown
        </p>
      </CardHeader>
      <CardContent>
        {/* Chart */}
        <div className="h-96 w-full">
          <ResponsiveContainer width="100%" height="100%">
            <BarChart
              data={chartData}
              margin={{ top: 20, right: 30, left: 20, bottom: 80 }}
            >
              <CartesianGrid strokeDasharray="3 3" stroke="#e5e7eb" />
              <XAxis
                dataKey="name"
                angle={-45}
                textAnchor="end"
                height={100}
                tick={{ fontSize: 12, fill: '#6b7280' }}
              />
              <YAxis
                tick={{ fontSize: 12, fill: '#6b7280' }}
                tickFormatter={(value) => currentMetric?.formatter(value) || value.toString()}
              />
              <Tooltip content={<CustomTooltip metric={selectedMetric} />} />
              <Bar
                dataKey="value"
                fill={currentMetric?.color || '#3b82f6'}
                radius={[8, 8, 0, 0]}
              />
              {/* Company average reference line */}
              {companyAverage > 0 && (
                <line
                  y1={`${companyAverage}%`}
                  y2={`${companyAverage}%`}
                  x1="0%"
                  x2="100%"
                  stroke="#ef4444"
                  strokeWidth={2}
                  strokeDasharray="5 5"
                />
              )}
            </BarChart>
          </ResponsiveContainer>
        </div>

        {/* Company average indicator */}
        {companyAverage > 0 && (
          <div className="mt-4 flex items-center justify-center space-x-2 text-sm">
            <div className="w-8 h-0.5 bg-red-500 border-dashed"></div>
            <span className="text-gray-600">
              Company Average: <span className="font-semibold">{currentMetric?.formatter(companyAverage)}</span>
            </span>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
