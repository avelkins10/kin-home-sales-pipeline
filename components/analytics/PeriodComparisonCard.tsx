'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { TrendingUp, TrendingDown, Calendar, ArrowRight } from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import type { OfficeMetrics } from '@/lib/types/analytics';
import type { TimeRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface PeriodComparisonCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  officeIds?: number[];
}

interface PeriodMetrics {
  totalProjects: number;
  avgSystemSize: number;
  avgNetPpw: number;
  firstTimePassRate: number;
  avgCycleTime: number | null;
  cancellationRate: number;
  holdRate: number;
}

interface ComparisonMetric {
  label: string;
  current: string | number;
  previous: string | number;
  delta: number;
  isPositiveChange: boolean;
  formatter: (val: number) => string;
}

function PeriodComparisonSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-48" />
        </div>
        <Skeleton className="h-4 w-64" />
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {Array.from({ length: 6 }).map((_, i) => (
            <Skeleton key={i} className="h-32 w-full" />
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

// Calculate previous period dates based on current time range
function getPreviousPeriodDates(timeRange: TimeRange): { startDate: string; endDate: string } {
  const now = new Date();
  let currentStart: Date;
  let currentEnd: Date = now;
  let periodDays: number;

  switch (timeRange) {
    case 'last_30':
      periodDays = 30;
      currentStart = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000);
      break;
    case 'last_90':
      periodDays = 90;
      currentStart = new Date(now.getTime() - 90 * 24 * 60 * 60 * 1000);
      break;
    case 'last_12_months':
    case 'ytd':
      periodDays = 365;
      currentStart = new Date(now.getFullYear(), 0, 1);
      break;
    case 'month':
      const monthStart = new Date(now.getFullYear(), now.getMonth(), 1);
      periodDays = Math.floor((now.getTime() - monthStart.getTime()) / (24 * 60 * 60 * 1000));
      currentStart = monthStart;
      break;
    default:
      periodDays = 30;
      currentStart = new Date(now.getTime() - 30 * 24 * 60 * 60 * 1000);
  }

  // Calculate previous period
  const previousEnd = new Date(currentStart.getTime() - 24 * 60 * 60 * 1000); // Day before current period
  const previousStart = new Date(previousEnd.getTime() - periodDays * 24 * 60 * 60 * 1000);

  return {
    startDate: previousStart.toISOString().split('T')[0],
    endDate: previousEnd.toISOString().split('T')[0]
  };
}

// Aggregate metrics from office data
function aggregateMetrics(data: OfficeMetrics[]): PeriodMetrics {
  if (!data || data.length === 0) {
    return {
      totalProjects: 0,
      avgSystemSize: 0,
      avgNetPpw: 0,
      firstTimePassRate: 0,
      avgCycleTime: null,
      cancellationRate: 0,
      holdRate: 0
    };
  }

  const totalProjects = data.reduce((sum, office) => sum + office.totalProjects, 0);

  // Weighted averages based on project count
  const avgSystemSize = totalProjects > 0
    ? data.reduce((sum, office) => sum + office.avgSystemSize * office.totalProjects, 0) / totalProjects
    : 0;

  const avgNetPpw = totalProjects > 0
    ? data.reduce((sum, office) => sum + office.avgNetPpw * office.totalProjects, 0) / totalProjects
    : 0;

  const firstTimePassRate = totalProjects > 0
    ? data.reduce((sum, office) => sum + office.firstTimePassRate * office.totalProjects, 0) / totalProjects
    : 0;

  // Calculate average cycle time from offices with data
  const officesWithCycleTime = data.filter(o => o.avgCycleTime !== null);
  const avgCycleTime = officesWithCycleTime.length > 0
    ? officesWithCycleTime.reduce((sum, office) => sum + (office.avgCycleTime || 0) * office.totalProjects, 0) /
      officesWithCycleTime.reduce((sum, office) => sum + office.totalProjects, 0)
    : null;

  // Calculate rates
  const totalCancelled = data.reduce((sum, office) => sum + (office.cancelledProjects || 0), 0);
  const totalOnHold = data.reduce((sum, office) => sum + (office.onHoldProjects || 0), 0);
  const cancellationRate = totalProjects > 0 ? (totalCancelled / totalProjects) * 100 : 0;
  const holdRate = totalProjects > 0 ? (totalOnHold / totalProjects) * 100 : 0;

  return {
    totalProjects,
    avgSystemSize,
    avgNetPpw,
    firstTimePassRate,
    avgCycleTime,
    cancellationRate,
    holdRate
  };
}

export function PeriodComparisonCard({
  userId,
  role,
  timeRange,
  officeIds
}: PeriodComparisonCardProps) {
  // Fetch current period data
  const { data: currentData, isLoading: isLoadingCurrent } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-metrics-current', userId, role, timeRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/office-metrics?timeRange=${timeRange}`;
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch current period metrics');
      const result = await response.json();
      return result.metrics || [];
    },
  });

  // Fetch previous period data
  const previousPeriod = getPreviousPeriodDates(timeRange);
  const { data: previousData, isLoading: isLoadingPrevious } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-metrics-previous', userId, role, previousPeriod, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/office-metrics?timeRange=custom&startDate=${previousPeriod.startDate}&endDate=${previousPeriod.endDate}`;
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch previous period metrics');
      const result = await response.json();
      return result.metrics || [];
    },
  });

  if (isLoadingCurrent || isLoadingPrevious) {
    return <PeriodComparisonSkeleton />;
  }

  if (!currentData || !previousData) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">Unable to load period comparison data</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const currentMetrics = aggregateMetrics(currentData);
  const previousMetrics = aggregateMetrics(previousData);

  // Calculate deltas
  const calculateDelta = (current: number, previous: number): number => {
    if (previous === 0) return current > 0 ? 100 : 0;
    return ((current - previous) / previous) * 100;
  };

  const metrics: ComparisonMetric[] = [
    {
      label: 'Total Projects',
      current: currentMetrics.totalProjects,
      previous: previousMetrics.totalProjects,
      delta: calculateDelta(currentMetrics.totalProjects, previousMetrics.totalProjects),
      isPositiveChange: currentMetrics.totalProjects > previousMetrics.totalProjects,
      formatter: (val: number) => val.toLocaleString()
    },
    {
      label: 'Avg System Size',
      current: currentMetrics.avgSystemSize,
      previous: previousMetrics.avgSystemSize,
      delta: calculateDelta(currentMetrics.avgSystemSize, previousMetrics.avgSystemSize),
      isPositiveChange: currentMetrics.avgSystemSize > previousMetrics.avgSystemSize,
      formatter: formatSystemSize
    },
    {
      label: 'Avg PPW',
      current: currentMetrics.avgNetPpw,
      previous: previousMetrics.avgNetPpw,
      delta: calculateDelta(currentMetrics.avgNetPpw, previousMetrics.avgNetPpw),
      isPositiveChange: currentMetrics.avgNetPpw > previousMetrics.avgNetPpw,
      formatter: formatPPW
    },
    {
      label: 'First-Time Pass Rate',
      current: currentMetrics.firstTimePassRate,
      previous: previousMetrics.firstTimePassRate,
      delta: calculateDelta(currentMetrics.firstTimePassRate, previousMetrics.firstTimePassRate),
      isPositiveChange: currentMetrics.firstTimePassRate > previousMetrics.firstTimePassRate,
      formatter: formatPercentage
    },
    {
      label: 'Avg Cycle Time',
      current: currentMetrics.avgCycleTime || 0,
      previous: previousMetrics.avgCycleTime || 0,
      delta: calculateDelta(currentMetrics.avgCycleTime || 0, previousMetrics.avgCycleTime || 0),
      isPositiveChange: (currentMetrics.avgCycleTime || 0) < (previousMetrics.avgCycleTime || 0), // Lower is better
      formatter: (val: number) => val > 0 ? `${Math.round(val)} days` : 'N/A'
    },
    {
      label: 'Cancellation Rate',
      current: currentMetrics.cancellationRate,
      previous: previousMetrics.cancellationRate,
      delta: calculateDelta(currentMetrics.cancellationRate, previousMetrics.cancellationRate),
      isPositiveChange: currentMetrics.cancellationRate < previousMetrics.cancellationRate, // Lower is better
      formatter: formatPercentage
    }
  ];

  const getTimeRangeLabel = () => {
    switch (timeRange) {
      case 'last_30': return 'Last 30 Days';
      case 'last_90': return 'Last 90 Days';
      case 'last_12_months': return 'Last 12 Months';
      case 'ytd': return 'Year to Date';
      case 'month': return 'This Month';
      default: return 'Current Period';
    }
  };

  return (
    <Card className="w-full" aria-label="Period over period comparison">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Calendar className="h-6 w-6 text-blue-600" />
          <CardTitle>Period Comparison</CardTitle>
        </div>
        <div className="flex items-center text-sm text-gray-600 space-x-2">
          <span>{getTimeRangeLabel()}</span>
          <ArrowRight className="h-4 w-4" />
          <span>vs Previous Period</span>
        </div>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {metrics.map((metric, index) => {
            const deltaAbs = Math.abs(metric.delta);
            const showDelta = deltaAbs > 0 && deltaAbs !== Infinity;

            return (
              <div
                key={index}
                className={`p-4 rounded-lg border-2 ${
                  showDelta && metric.isPositiveChange
                    ? 'bg-green-50 border-green-200'
                    : showDelta && !metric.isPositiveChange
                    ? 'bg-red-50 border-red-200'
                    : 'bg-slate-50 border-slate-200'
                }`}
              >
                <div className="text-sm font-medium text-gray-600 mb-2">{metric.label}</div>

                {/* Current Value */}
                <div className="text-2xl font-bold text-gray-900 mb-1">
                  {metric.formatter(typeof metric.current === 'number' ? metric.current : 0)}
                </div>

                {/* Previous Value & Delta */}
                <div className="flex items-center justify-between">
                  <div className="text-xs text-gray-500">
                    Was: {metric.formatter(typeof metric.previous === 'number' ? metric.previous : 0)}
                  </div>

                  {showDelta && (
                    <div className={`flex items-center space-x-1 text-sm font-semibold ${
                      metric.isPositiveChange ? 'text-green-700' : 'text-red-700'
                    }`}>
                      {metric.isPositiveChange ? (
                        <TrendingUp className="h-4 w-4" />
                      ) : (
                        <TrendingDown className="h-4 w-4" />
                      )}
                      <span>{deltaAbs.toFixed(1)}%</span>
                    </div>
                  )}
                </div>
              </div>
            );
          })}
        </div>
      </CardContent>
    </Card>
  );
}
