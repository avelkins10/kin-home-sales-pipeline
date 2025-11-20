'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { DoorOpen, Calendar, TrendingUp, Award, Target, Zap, Trophy, User } from 'lucide-react';
import { formatLargeNumber, formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { cn } from '@/lib/utils/cn';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

interface RepCardSimpleDashboardProps {
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
}

export function RepCardSimpleDashboard({
  timeRange,
  customDateRange
}: RepCardSimpleDashboardProps) {
  // Calculate date range
  const getDateRange = () => {
    if (timeRange === 'custom' && customDateRange) {
      return {
        startDate: customDateRange.startDate,
        endDate: customDateRange.endDate
      };
    }
    
    const end = new Date();
    const start = new Date();
    
    switch (timeRange) {
      case 'today':
        start.setHours(0, 0, 0, 0);
        break;
      case 'week':
        start.setDate(start.getDate() - 7);
        break;
      case 'month':
        start.setDate(start.getDate() - 30);
        break;
      case 'quarter':
        start.setDate(start.getDate() - 90);
        break;
      case 'ytd':
        start.setMonth(0, 1);
        start.setHours(0, 0, 0, 0);
        break;
      case 'last_30':
        start.setDate(start.getDate() - 30);
        break;
      case 'last_90':
        start.setDate(start.getDate() - 90);
        break;
      case 'last_12_months':
        start.setDate(start.getDate() - 365);
        break;
      default:
        start.setDate(start.getDate() - 90);
    }
    
    return {
      startDate: start.toISOString().split('T')[0],
      endDate: end.toISOString().split('T')[0]
    };
  };

  const dateRange = getDateRange();

  const { data, isLoading, error } = useQuery({
    queryKey: ['repcard-simple-stats', dateRange.startDate, dateRange.endDate],
    queryFn: async () => {
      const params = new URLSearchParams({
        startDate: dateRange.startDate,
        endDate: dateRange.endDate
      });
      const response = await fetch(`${getBaseUrl()}/api/repcard/simple-stats?${params}`);
      if (!response.ok) throw new Error('Failed to fetch RepCard stats');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 60000
  });

  if (isLoading) {
    return (
      <div className="space-y-6">
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Zap className="h-5 w-5 text-purple-600" />
              RepCard Dashboard
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              {[...Array(4)].map((_, i) => (
                <Skeleton key={i} className="h-24 w-full" />
              ))}
            </div>
          </CardContent>
        </Card>
      </div>
    );
  }

  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="text-red-600">Error Loading RepCard Data</CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-sm text-muted-foreground">
            {(error as Error).message}
          </p>
        </CardContent>
      </Card>
    );
  }

  if (!data?.success) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>No RepCard Data</CardTitle>
        </CardHeader>
        <CardContent>
          <p className="text-sm text-muted-foreground">
            No RepCard data found for the selected date range.
          </p>
        </CardContent>
      </Card>
    );
  }

  const { overview, qualityMetrics, leaderboards } = data;

  return (
    <div className="space-y-6">
      {/* Overview Cards */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Zap className="h-5 w-5 text-purple-600" />
            RepCard Overview
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {/* Doors Knocked */}
            <div className="p-4 bg-gradient-to-br from-blue-50 to-blue-100 rounded-lg border border-blue-200">
              <div className="flex items-center justify-between mb-2">
                <DoorOpen className="h-5 w-5 text-blue-600" />
                <span className="text-xs text-blue-600 font-medium">Total</span>
              </div>
              <p className="text-2xl font-bold text-blue-900">{formatLargeNumber(overview.doorsKnocked)}</p>
              <p className="text-xs text-blue-700 mt-1">Doors Knocked</p>
            </div>

            {/* Appointments Set */}
            <div className="p-4 bg-gradient-to-br from-green-50 to-green-100 rounded-lg border border-green-200">
              <div className="flex items-center justify-between mb-2">
                <Calendar className="h-5 w-5 text-green-600" />
                <span className="text-xs text-green-600 font-medium">Total</span>
              </div>
              <p className="text-2xl font-bold text-green-900">{formatLargeNumber(overview.appointmentsSet)}</p>
              <p className="text-xs text-green-700 mt-1">Appointments Set</p>
            </div>

            {/* Conversion Rate */}
            <div className="p-4 bg-gradient-to-br from-purple-50 to-purple-100 rounded-lg border border-purple-200">
              <div className="flex items-center justify-between mb-2">
                <TrendingUp className="h-5 w-5 text-purple-600" />
                <span className="text-xs text-purple-600 font-medium">Rate</span>
              </div>
              <p className={cn(
                "text-2xl font-bold",
                overview.conversionRate >= 20 ? "text-purple-900" : overview.conversionRate >= 10 ? "text-purple-700" : "text-purple-600"
              )}>
                {formatPercentage(overview.conversionRate)}
              </p>
              <p className="text-xs text-purple-700 mt-1">Conversion Rate</p>
              <div className="mt-2 flex items-center gap-1">
                <Target className="h-3 w-3 text-purple-500" />
                <span className="text-xs text-purple-600">Target: 20%</span>
              </div>
            </div>

            {/* Quality Score */}
            <div className="p-4 bg-gradient-to-br from-amber-50 to-amber-100 rounded-lg border border-amber-200">
              <div className="flex items-center justify-between mb-2">
                <Award className="h-5 w-5 text-amber-600" />
                <span className="text-xs text-amber-600 font-medium">Average</span>
              </div>
              <p className={cn(
                "text-2xl font-bold",
                overview.qualityScore >= 75 ? "text-amber-900" : overview.qualityScore >= 50 ? "text-amber-700" : "text-amber-600"
              )}>
                {overview.qualityScore.toFixed(1)}
              </p>
              <p className="text-xs text-amber-700 mt-1">Quality Score</p>
              <p className="text-xs text-amber-600 mt-1">{overview.activeReps} active reps</p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Quality Metrics */}
      <Card>
        <CardHeader>
          <CardTitle>Quality Metrics</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="p-4 bg-blue-50 rounded-lg border border-blue-200">
              <div className="flex items-center gap-2 mb-2">
                <Calendar className="h-5 w-5 text-blue-600" />
                <span className="text-sm font-semibold text-blue-700">Appointment Speed</span>
              </div>
              <p className="text-3xl font-bold text-blue-900">{formatPercentage(qualityMetrics.appointmentSpeed)}</p>
              <p className="text-xs text-blue-600 mt-1">% scheduled within 24 hours</p>
              <p className="text-xs text-blue-500 mt-2">Target: 75%</p>
            </div>

            <div className="p-4 bg-green-50 rounded-lg border border-green-200">
              <div className="flex items-center gap-2 mb-2">
                <Award className="h-5 w-5 text-green-600" />
                <span className="text-sm font-semibold text-green-700">Power Bill Rate</span>
              </div>
              <p className="text-3xl font-bold text-green-900">{formatPercentage(qualityMetrics.attachmentRate)}</p>
              <p className="text-xs text-green-600 mt-1">% customers with attachments</p>
              <p className="text-xs text-green-500 mt-2">Target: 70%</p>
            </div>

            <div className="p-4 bg-purple-50 rounded-lg border border-purple-200">
              <div className="flex items-center gap-2 mb-2">
                <Award className="h-5 w-5 text-purple-600" />
                <span className="text-sm font-semibold text-purple-700">Quality Score</span>
              </div>
              <p className="text-3xl font-bold text-purple-900">{formatPercentage(qualityMetrics.qualityScore)}</p>
              <p className="text-xs text-purple-600 mt-1">Composite quality metric</p>
              <p className="text-xs text-purple-500 mt-2">Target: 75%</p>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Leaderboards */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Top Doors Knocked */}
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Trophy className="h-5 w-5 text-yellow-600" />
              Top Setters - Doors Knocked
            </CardTitle>
          </CardHeader>
          <CardContent>
            {leaderboards.doorsKnocked.length === 0 ? (
              <p className="text-sm text-muted-foreground text-center py-8">
                No leaderboard data available
              </p>
            ) : (
              <div className="space-y-2">
                {leaderboards.doorsKnocked.map((entry: any) => (
                  <div key={entry.repcardUserId} className="flex items-center justify-between p-3 border rounded-lg hover:bg-muted/50">
                    <div className="flex items-center gap-3">
                      <div className={cn(
                        "w-8 h-8 rounded-full flex items-center justify-center text-sm font-bold",
                        entry.rank === 1 ? "bg-yellow-100 text-yellow-800" :
                        entry.rank === 2 ? "bg-gray-100 text-gray-800" :
                        entry.rank === 3 ? "bg-orange-100 text-orange-800" :
                        "bg-muted text-muted-foreground"
                      )}>
                        {entry.rank}
                      </div>
                      <div>
                        <p className="font-medium">{entry.name}</p>
                        {entry.office && (
                          <p className="text-xs text-muted-foreground">{entry.office}</p>
                        )}
                      </div>
                    </div>
                    <div className="text-right">
                      <p className="font-bold text-lg">{formatLargeNumber(entry.value)}</p>
                      <p className="text-xs text-muted-foreground">doors</p>
                    </div>
                  </div>
                ))}
              </div>
            )}
          </CardContent>
        </Card>

        {/* Top Appointments */}
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Trophy className="h-5 w-5 text-yellow-600" />
              Top Setters - Appointments Set
            </CardTitle>
          </CardHeader>
          <CardContent>
            {leaderboards.appointments.length === 0 ? (
              <p className="text-sm text-muted-foreground text-center py-8">
                No leaderboard data available
              </p>
            ) : (
              <div className="space-y-2">
                {leaderboards.appointments.map((entry: any) => (
                  <div key={entry.repcardUserId} className="flex items-center justify-between p-3 border rounded-lg hover:bg-muted/50">
                    <div className="flex items-center gap-3">
                      <div className={cn(
                        "w-8 h-8 rounded-full flex items-center justify-center text-sm font-bold",
                        entry.rank === 1 ? "bg-yellow-100 text-yellow-800" :
                        entry.rank === 2 ? "bg-gray-100 text-gray-800" :
                        entry.rank === 3 ? "bg-orange-100 text-orange-800" :
                        "bg-muted text-muted-foreground"
                      )}>
                        {entry.rank}
                      </div>
                      <div>
                        <p className="font-medium">{entry.name}</p>
                        {entry.office && (
                          <p className="text-xs text-muted-foreground">{entry.office}</p>
                        )}
                      </div>
                    </div>
                    <div className="text-right">
                      <p className="font-bold text-lg">{formatLargeNumber(entry.value)}</p>
                      <p className="text-xs text-muted-foreground">appointments</p>
                    </div>
                  </div>
                ))}
              </div>
            )}
          </CardContent>
        </Card>
      </div>
    </div>
  );
}

