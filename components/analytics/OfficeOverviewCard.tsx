'use client'

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Building2, TrendingUp, Zap, DollarSign, Clock, CheckCircle, BarChart3, AlertTriangle, XCircle } from 'lucide-react';
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
  firstTimePassRate: number;
  activeProjects: number;
  cancelledProjects: number;
  onHoldProjects: number;
  projectsRejected: number;
  pendingKcaProjects: number;
  financeHoldProjects: number;
  pendingCancelProjects: number;
  roofHoldProjects: number;
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
    acc.firstTimePassRate += office.firstTimePassRate * office.totalProjects;
    acc.activeProjects += office.activeProjects;
    acc.cancelledProjects += office.cancelledProjects;
    acc.onHoldProjects += office.onHoldProjects;
    acc.projectsRejected += office.projectsRejected;
    acc.pendingKcaProjects += office.pendingKcaProjects;
    acc.financeHoldProjects += office.financeHoldProjects;
    acc.pendingCancelProjects += office.pendingCancelProjects;
    acc.roofHoldProjects += office.roofHoldProjects;
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
    firstTimePassRate: 0,
    activeProjects: 0,
    cancelledProjects: 0,
    onHoldProjects: 0,
    projectsRejected: 0,
    pendingKcaProjects: 0,
    financeHoldProjects: 0,
    pendingCancelProjects: 0,
    roofHoldProjects: 0,
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
    aggregated.firstTimePassRate /= aggregated.totalProjects;
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
    <Card className="w-full shadow-sm hover:shadow-md transition-shadow duration-200" aria-label="Office overview metrics">
      <CardHeader className="pb-4">
        <div className="flex items-center space-x-3">
          <div className="p-2.5 bg-blue-100 rounded-xl">
            <Building2 className="h-6 w-6 text-blue-600" />
          </div>
          <div>
            <CardTitle className="text-xl font-bold">Office Overview</CardTitle>
            <p className="text-sm text-gray-600 mt-0.5">
              {getTimeRangeLabel()} • {aggregated.officeCount} office{aggregated.officeCount !== 1 ? 's' : ''}
            </p>
          </div>
        </div>
      </CardHeader>
      <CardContent className="pt-0">
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

          {/* Status Summary - Redesigned */}
          <div className="col-span-1 md:col-span-2 bg-gradient-to-br from-slate-50 to-slate-100 rounded-xl p-4 border border-slate-200">
            <div className="flex items-center space-x-2 mb-4">
              <div className="p-2 bg-slate-200 rounded-lg">
                <BarChart3 className="h-5 w-5 text-slate-700" />
              </div>
              <div>
                <p className="text-base font-semibold text-gray-900">Project Status Breakdown</p>
                <p className="text-xs text-gray-600">All projects by current status</p>
              </div>
            </div>

            <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
              {/* Healthy - Active Projects */}
              <div className="bg-white rounded-lg p-3 border-l-4 border-green-500 shadow-sm">
                <div className="flex items-center justify-between mb-1">
                  <span className="text-xs font-medium text-gray-600">Active</span>
                  <CheckCircle className="h-4 w-4 text-green-600" />
                </div>
                <div className="text-2xl font-bold text-green-700">{(aggregated.activeProjects || 0).toLocaleString()}</div>
                <div className="text-xs text-gray-500 mt-1">
                  {((aggregated.activeProjects || 0) / (aggregated.totalProjects || 1) * 100).toFixed(1)}% of total
                </div>
              </div>

              {/* Needs Attention */}
              <div className="bg-white rounded-lg p-3 border-l-4 border-yellow-500 shadow-sm">
                <div className="flex items-center justify-between mb-1">
                  <span className="text-xs font-medium text-gray-600">Needs Attention</span>
                  <AlertTriangle className="h-4 w-4 text-yellow-600" />
                </div>
                <div className="text-2xl font-bold text-yellow-700">
                  {((aggregated.projectsRejected || 0) + (aggregated.pendingKcaProjects || 0) + (aggregated.pendingCancelProjects || 0)).toLocaleString()}
                </div>
                <div className="text-xs space-y-0.5 mt-2">
                  <div className="flex justify-between">
                    <span className="text-gray-600">Rejected (Awaiting Fix)</span>
                    <span className="font-medium text-red-700">{(aggregated.projectsRejected || 0).toLocaleString()}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-600">Pending KCA</span>
                    <span className="font-medium text-yellow-700">{(aggregated.pendingKcaProjects || 0).toLocaleString()}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-600">Pending Cancel</span>
                    <span className="font-medium text-orange-700">{(aggregated.pendingCancelProjects || 0).toLocaleString()}</span>
                  </div>
                </div>
              </div>

              {/* On Hold */}
              <div className="bg-white rounded-lg p-3 border-l-4 border-blue-500 shadow-sm">
                <div className="flex items-center justify-between mb-1">
                  <span className="text-xs font-medium text-gray-600">On Hold</span>
                  <Clock className="h-4 w-4 text-blue-600" />
                </div>
                <div className="text-2xl font-bold text-blue-700">
                  {((aggregated.onHoldProjects || 0) + (aggregated.financeHoldProjects || 0) + (aggregated.roofHoldProjects || 0)).toLocaleString()}
                </div>
                <div className="text-xs space-y-0.5 mt-2">
                  <div className="flex justify-between">
                    <span className="text-gray-600">General Hold</span>
                    <span className="font-medium text-blue-700">{(aggregated.onHoldProjects || 0).toLocaleString()}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-600">Finance Hold</span>
                    <span className="font-medium text-blue-700">{(aggregated.financeHoldProjects || 0).toLocaleString()}</span>
                  </div>
                  <div className="flex justify-between">
                    <span className="text-gray-600">Roof Hold</span>
                    <span className="font-medium text-blue-700">{(aggregated.roofHoldProjects || 0).toLocaleString()}</span>
                  </div>
                </div>
              </div>
            </div>

            {/* Cancelled Projects - Only show if > 0 */}
            {(aggregated.cancelledProjects || 0) > 0 && (
              <div className="mt-3 bg-white rounded-lg p-3 border-l-4 border-red-500 shadow-sm">
                <div className="flex items-center justify-between">
                  <div className="flex items-center space-x-2">
                    <XCircle className="h-4 w-4 text-red-600" />
                    <span className="text-xs font-medium text-gray-600">Cancelled</span>
                  </div>
                  <div className="text-lg font-bold text-red-700">{(aggregated.cancelledProjects || 0).toLocaleString()}</div>
                </div>
              </div>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
