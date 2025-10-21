'use client'

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Building2, TrendingUp, Zap, DollarSign, Clock, CheckCircle, BarChart3 } from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import type { OfficeMetrics } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface OfficeOverviewCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
}

interface OfficeOverviewData {
  totalProjects: number;
  avgSystemSize: number;
  avgGrossPpw: number;
  avgNetPpw: number;
  avgCommissionablePpw: number;
  avgCycleTime: number | null;
  intakeApprovalRate: number;
  activeProjects: number;
  cancelledProjects: number;
  onHoldProjects: number;
  officeCount: number;
}

function OfficeOverviewSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-32" />
        </div>
        <Skeleton className="h-4 w-64" />
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {Array.from({ length: 6 }).map((_, i) => (
            <div key={i} className="flex items-center space-x-3 p-3 bg-slate-50 rounded-lg">
              <Skeleton className="h-9 w-9 rounded-lg" />
              <div className="flex-1 space-y-2">
                <Skeleton className="h-4 w-24" />
                <Skeleton className="h-6 w-16" />
              </div>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

export function OfficeOverviewCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: OfficeOverviewCardProps) {
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
      const result = await response.json();
      return result.metrics || [];
    },
  });

  if (isLoading) {
    return <OfficeOverviewSkeleton />;
  }

  if (error) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load office overview</p>
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
            <p className="text-slate-600">No office data available for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Aggregate metrics across all offices
  const aggregated: OfficeOverviewData = data.reduce((acc, office) => {
    acc.totalProjects += office.totalProjects;
    acc.avgSystemSize += office.avgSystemSize * office.totalProjects;
    acc.avgGrossPpw += office.avgGrossPpw * office.totalProjects;
    acc.avgNetPpw += office.avgNetPpw * office.totalProjects;
    acc.avgCommissionablePpw += office.avgCommissionablePpw * office.totalProjects;
    if (office.avgCycleTime !== null) {
      acc.avgCycleTime = ((acc.avgCycleTime as number) || 0) + office.avgCycleTime * office.totalProjects;
    }
    acc.intakeApprovalRate += office.intakeApprovalRate * office.totalProjects;
    acc.activeProjects += office.activeProjects;
    acc.cancelledProjects += office.cancelledProjects;
    acc.onHoldProjects += office.onHoldProjects;
    acc.officeCount = data.length;
    return acc;
  }, {
    totalProjects: 0,
    avgSystemSize: 0,
    avgGrossPpw: 0,
    avgNetPpw: 0,
    avgCommissionablePpw: 0,
    avgCycleTime: null as number | null,
    intakeApprovalRate: 0,
    activeProjects: 0,
    cancelledProjects: 0,
    onHoldProjects: 0,
    officeCount: 0
  });

  // Track projects with cycle time for weighted average
  let projectsWithCycleTime = 0;
  data.forEach(office => {
    if (office.avgCycleTime !== null) {
      projectsWithCycleTime += office.totalProjects;
    }
  });

  // Calculate weighted averages
  if (aggregated.totalProjects > 0) {
    aggregated.avgSystemSize /= aggregated.totalProjects;
    aggregated.avgGrossPpw /= aggregated.totalProjects;
    aggregated.avgNetPpw /= aggregated.totalProjects;
    aggregated.avgCommissionablePpw /= aggregated.totalProjects;
    aggregated.avgCycleTime = projectsWithCycleTime > 0 ? aggregated.avgCycleTime! / projectsWithCycleTime : null;
    aggregated.intakeApprovalRate /= aggregated.totalProjects;
  }

  const getTimeRangeLabel = () => {
    switch (timeRange) {
      case 'week': return 'This Week';
      case 'month': return 'This Month';
      case 'custom': return 'Custom Range';
      default: return 'All Time';
    }
  };

  return (
    <Card className="w-full" aria-label="Office overview metrics">
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <Building2 className="h-6 w-6 text-blue-600" />
          <span>Office Overview</span>
        </CardTitle>
        <p className="text-sm text-gray-600">
          {getTimeRangeLabel()} • {aggregated.officeCount} office{aggregated.officeCount !== 1 ? 's' : ''}
        </p>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* Total Projects */}
          <div className="flex items-center space-x-3 p-3 bg-blue-50 rounded-lg">
            <div className="p-2 bg-blue-100 rounded-lg">
              <TrendingUp className="h-5 w-5 text-blue-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Total Projects</p>
              <p className="text-2xl font-semibold text-gray-900">{(aggregated.totalProjects || 0).toLocaleString()}</p>
            </div>
          </div>

          {/* Average System Size */}
          <div className="flex items-center space-x-3 p-3 bg-purple-50 rounded-lg">
            <div className="p-2 bg-purple-100 rounded-lg">
              <Zap className="h-5 w-5 text-purple-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Avg System Size</p>
              <p className="text-2xl font-semibold text-gray-900">{formatSystemSize(aggregated.avgSystemSize)}</p>
            </div>
          </div>

          {/* Average PPW */}
          <div className="flex items-center space-x-3 p-3 bg-green-50 rounded-lg">
            <div className="p-2 bg-green-100 rounded-lg">
              <DollarSign className="h-5 w-5 text-green-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Avg PPW</p>
              <div className="text-sm font-medium text-gray-900 space-y-1">
                <div>Gross: {formatPPW(aggregated.avgGrossPpw)}</div>
                <div>Net: {formatPPW(aggregated.avgNetPpw)}</div>
                <div>Commission: {formatPPW(aggregated.avgCommissionablePpw)}</div>
              </div>
            </div>
          </div>

          {/* Average Cycle Time */}
          <div className="flex items-center space-x-3 p-3 bg-orange-50 rounded-lg">
            <div className="p-2 bg-orange-100 rounded-lg">
              <Clock className="h-5 w-5 text-orange-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Avg Cycle Time</p>
              <p className="text-2xl font-semibold text-gray-900">
                {aggregated.avgCycleTime !== null ? `${Math.round(aggregated.avgCycleTime)} days` : 'N/A'}
              </p>
            </div>
          </div>

          {/* Intake Approval Rate */}
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <div className="p-2 bg-emerald-100 rounded-lg">
              <CheckCircle className="h-5 w-5 text-emerald-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Intake Approval Rate</p>
              <p className="text-2xl font-semibold text-gray-900">{formatPercentage(aggregated.intakeApprovalRate)}</p>
            </div>
          </div>

          {/* First-Time Pass Rate (Clean Deals) */}
          <div className="flex items-center space-x-3 p-3 bg-green-50 rounded-lg">
            <div className="p-2 bg-green-100 rounded-lg">
              <CheckCircle className="h-5 w-5 text-green-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">First-Time Pass Rate</p>
              <p className={`text-2xl font-semibold ${
                (aggregated.firstTimePassRate ?? 0) >= 90 ? 'text-green-600' :
                (aggregated.firstTimePassRate ?? 0) >= 75 ? 'text-yellow-600' :
                'text-red-600'
              }`}>
                {formatPercentage(aggregated.firstTimePassRate ?? 0)}
              </p>
            </div>
          </div>

          {/* Status Summary */}
          <div className="flex items-center space-x-3 p-3 bg-slate-50 rounded-lg">
            <div className="p-2 bg-slate-100 rounded-lg">
              <BarChart3 className="h-5 w-5 text-slate-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Status Summary</p>
              <div className="text-sm font-medium text-gray-900 space-y-1">
                <div>Active: {(aggregated.activeProjects || 0).toLocaleString()}</div>
                <div>Cancelled: {(aggregated.cancelledProjects || 0).toLocaleString()}</div>
                <div>Holds: {(aggregated.onHoldProjects || 0).toLocaleString()}</div>
              </div>
            </div>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
