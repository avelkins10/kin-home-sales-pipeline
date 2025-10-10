'use client'

// components/dashboard/PerformanceMetrics.tsx
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { formatCurrency, formatCompactCurrency, formatPercentage } from '@/lib/utils/formatters';
import { TrendingUp, DollarSign, CheckCircle, Banknote, Target } from 'lucide-react';
import type { TimeRange } from '@/lib/types/dashboard';

interface PerformanceMetricsProps {
  soldAccounts: number;
  grossRevenue: number;
  installCount: number;
  installedRevenue: number;
  retentionRate: number;
  timeRange: TimeRange;
  isLoading?: boolean;
}

export function PerformanceMetrics({
  soldAccounts,
  grossRevenue,
  installCount,
  installedRevenue,
  retentionRate,
  timeRange,
  isLoading = false,
}: PerformanceMetricsProps) {
  const getTimeRangeLabel = (range: TimeRange) => {
    switch (range) {
      case 'month': return 'This Month';
      case 'week': return 'This Week';
      case 'lifetime': return 'Lifetime';
      default: return 'Lifetime';
    }
  };

  const getRetentionColor = (rate: number) => {
    if (rate >= 80) return 'text-green-600';
    if (rate >= 60) return 'text-yellow-600';
    return 'text-red-600';
  };

  if (isLoading) {
    return <PerformanceMetricsSkeleton />;
  }

  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
        <CardTitle className="text-lg font-semibold">Performance Overview</CardTitle>
        <span className="text-xs bg-blue-100 text-blue-800 px-2 py-1 rounded-full">
          {getTimeRangeLabel(timeRange)}
        </span>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {/* Sold Accounts */}
          <div className="flex items-center space-x-3 p-3 bg-blue-50 rounded-lg">
            <div className="p-2 bg-blue-100 rounded-lg">
              <TrendingUp className="h-5 w-5 text-blue-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Sold Accounts</p>
              <p className="text-2xl font-semibold text-gray-900">{soldAccounts.toLocaleString()}</p>
            </div>
          </div>

          {/* Gross Revenue */}
          <div className="flex items-center space-x-3 p-3 bg-green-50 rounded-lg">
            <div className="p-2 bg-green-100 rounded-lg">
              <DollarSign className="h-5 w-5 text-green-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Gross Revenue</p>
              <p className="text-2xl font-semibold text-gray-900">{formatCompactCurrency(grossRevenue)}</p>
            </div>
          </div>

          {/* Installs Completed */}
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <div className="p-2 bg-emerald-100 rounded-lg">
              <CheckCircle className="h-5 w-5 text-emerald-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Installs Completed</p>
              <p className="text-2xl font-semibold text-gray-900">{installCount.toLocaleString()}</p>
            </div>
          </div>

          {/* Installed Revenue */}
          <div className="flex items-center space-x-3 p-3 bg-purple-50 rounded-lg">
            <div className="p-2 bg-purple-100 rounded-lg">
              <Banknote className="h-5 w-5 text-purple-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Installed Revenue</p>
              <p className="text-2xl font-semibold text-gray-900">{formatCompactCurrency(installedRevenue)}</p>
            </div>
          </div>
        </div>

        {/* Retention Rate - Full Width */}
        <div className="mt-4 flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
          <div className="p-2 bg-gray-100 rounded-lg">
            <Target className="h-5 w-5 text-gray-600" />
          </div>
          <div className="flex-1">
            <p className="text-sm text-gray-600">Retention Rate</p>
            <p className={`text-2xl font-semibold ${getRetentionColor(retentionRate)}`}>
              {formatPercentage(retentionRate)}
            </p>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

function PerformanceMetricsSkeleton() {
  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
        <Skeleton className="h-6 w-40" />
        <Skeleton className="h-6 w-20" />
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          {Array.from({ length: 4 }).map((_, i) => (
            <div key={i} className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
              <Skeleton className="h-9 w-9 rounded-lg" />
              <div className="flex-1">
                <Skeleton className="h-4 w-20 mb-2" />
                <Skeleton className="h-8 w-16" />
              </div>
            </div>
          ))}
        </div>
        <div className="mt-4 flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
          <Skeleton className="h-9 w-9 rounded-lg" />
          <div className="flex-1">
            <Skeleton className="h-4 w-24 mb-2" />
            <Skeleton className="h-8 w-16" />
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
