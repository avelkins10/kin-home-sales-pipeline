'use client'

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { formatCurrency, formatCompactCurrency, formatPercentage } from '@/lib/utils/formatters';
import { TrendingUp, DollarSign, CheckCircle, Banknote, Target, Users } from 'lucide-react';
import type { TimeRange } from '@/lib/types/dashboard';

interface TeamPerformanceMetricsProps {
  soldAccounts: number;
  grossRevenue: number;
  installCount: number;
  installedRevenue: number;
  retentionRate: number;
  timeRange: TimeRange;
  isLoading?: boolean;
}

export function TeamPerformanceMetrics({
  soldAccounts,
  grossRevenue,
  installCount,
  installedRevenue,
  retentionRate,
  timeRange,
  isLoading = false
}: TeamPerformanceMetricsProps) {
  const getRetentionLabel = (range: TimeRange) => {
    switch (range) {
      case 'month': return 'Team Retention Rate (This Month)';
      case 'week': return 'Team Retention Rate (This Week)';
      case 'lifetime': return 'Team Retention Rate (Lifetime)';
      default: return 'Team Retention Rate (Lifetime)';
    }
  };

  if (isLoading) {
    return <TeamPerformanceMetricsSkeleton />;
  }

  return (
    <Card className="w-full" aria-label="Team performance metrics">
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <Users className="h-6 w-6 text-emerald-600" />
          <span>Team Performance Overview</span>
        </CardTitle>
        <p className="text-sm text-gray-600">
          Aggregated metrics for your team
        </p>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Top Row - Sold Accounts and Revenue */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <div className="p-2 bg-emerald-100 rounded-lg">
              <TrendingUp className="h-5 w-5 text-emerald-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Team Sold Accounts</p>
              <p className="text-2xl font-semibold text-gray-900">{soldAccounts.toLocaleString()}</p>
            </div>
          </div>
          
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <div className="p-2 bg-emerald-100 rounded-lg">
              <DollarSign className="h-5 w-5 text-emerald-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Team Revenue</p>
              <p className="text-2xl font-semibold text-gray-900">{formatCompactCurrency(grossRevenue)}</p>
            </div>
          </div>
        </div>

        {/* Middle Row - Installs and Installed Revenue */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <div className="p-2 bg-emerald-100 rounded-lg">
              <CheckCircle className="h-5 w-5 text-emerald-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Team Installs</p>
              <p className="text-2xl font-semibold text-gray-900">{installCount.toLocaleString()}</p>
            </div>
          </div>
          
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <div className="p-2 bg-emerald-100 rounded-lg">
              <Banknote className="h-5 w-5 text-emerald-600" />
            </div>
            <div className="flex-1">
              <p className="text-sm text-gray-600">Installed Revenue</p>
              <p className="text-2xl font-semibold text-gray-900">{formatCompactCurrency(installedRevenue)}</p>
            </div>
          </div>
        </div>

        {/* Bottom Row - Retention Rate (Full Width) */}
        <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
          <div className="p-2 bg-emerald-100 rounded-lg">
            <Target className="h-5 w-5 text-emerald-600" />
          </div>
          <div className="flex-1">
            <p className="text-sm text-gray-600">{getRetentionLabel(timeRange)}</p>
            <p className="text-2xl font-semibold text-gray-900">{formatPercentage(retentionRate)}</p>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

function TeamPerformanceMetricsSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-48" />
        </div>
        <Skeleton className="h-4 w-64" />
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Top Row */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <Skeleton className="h-9 w-9 rounded-lg" />
            <div className="flex-1 space-y-2">
              <Skeleton className="h-4 w-32" />
              <Skeleton className="h-8 w-20" />
            </div>
          </div>
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <Skeleton className="h-9 w-9 rounded-lg" />
            <div className="flex-1 space-y-2">
              <Skeleton className="h-4 w-24" />
              <Skeleton className="h-8 w-24" />
            </div>
          </div>
        </div>

        {/* Middle Row */}
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <Skeleton className="h-9 w-9 rounded-lg" />
            <div className="flex-1 space-y-2">
              <Skeleton className="h-4 w-28" />
              <Skeleton className="h-8 w-16" />
            </div>
          </div>
          <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
            <Skeleton className="h-9 w-9 rounded-lg" />
            <div className="flex-1 space-y-2">
              <Skeleton className="h-4 w-32" />
              <Skeleton className="h-8 w-24" />
            </div>
          </div>
        </div>

        {/* Bottom Row */}
        <div className="flex items-center space-x-3 p-3 bg-emerald-50 rounded-lg">
          <Skeleton className="h-9 w-9 rounded-lg" />
          <div className="flex-1 space-y-2">
            <Skeleton className="h-4 w-36" />
            <Skeleton className="h-8 w-20" />
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
