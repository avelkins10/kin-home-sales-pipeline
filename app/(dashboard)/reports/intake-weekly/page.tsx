'use client';

import { useState, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { Calendar, TrendingUp, TrendingDown, AlertCircle, Award, RefreshCw } from 'lucide-react';
import { WeeklyIntakeTable } from '@/components/reports/WeeklyIntakeTable';
import { formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import type { WeeklyIntakeReport } from '@/lib/types/reports';

// Helper to get date 7 days ago
function getLastWeekDates() {
  const today = new Date();
  const sevenDaysAgo = new Date(today);
  sevenDaysAgo.setDate(today.getDate() - 7);

  return {
    start: sevenDaysAgo.toISOString().split('T')[0],
    end: today.toISOString().split('T')[0],
  };
}

export default function WeeklyIntakeReportPage() {
  const { data: session } = useSession();
  const queryClient = useQueryClient();
  const lastWeek = getLastWeekDates();

  const [startDate, setStartDate] = useState(lastWeek.start);
  const [endDate, setEndDate] = useState(lastWeek.end);
  const [appliedStartDate, setAppliedStartDate] = useState(lastWeek.start);
  const [appliedEndDate, setAppliedEndDate] = useState(lastWeek.end);

  const { data, isLoading, error, refetch } = useQuery<WeeklyIntakeReport>({
    queryKey: ['weekly-intake-report', appliedStartDate, appliedEndDate],
    queryFn: async () => {
      const url = `${getBaseUrl()}/api/reports/intake-weekly?startDate=${appliedStartDate}&endDate=${appliedEndDate}`;
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch report');
      return response.json();
    },
    enabled: !!session?.user, // Allow all authenticated users with proper role
    staleTime: 30000, // Data is considered stale after 30 seconds
    gcTime: 60000, // Cache is garbage collected after 1 minute
    refetchOnMount: true, // Always refetch when component mounts
    refetchOnWindowFocus: true, // Refetch when user returns to the tab
  });

  const handleApplyDates = () => {
    setAppliedStartDate(startDate);
    setAppliedEndDate(endDate);
  };

  const handleRefresh = () => {
    refetch();
  };

  const handleQuickRange = (days: number) => {
    const today = new Date();
    const startDay = new Date(today);
    startDay.setDate(today.getDate() - days);

    const newStart = startDay.toISOString().split('T')[0];
    const newEnd = today.toISOString().split('T')[0];

    setStartDate(newStart);
    setEndDate(newEnd);
    setAppliedStartDate(newStart);
    setAppliedEndDate(newEnd);
  };

  if (isLoading) {
    return (
      <div className="space-y-6">
        <div className="flex items-center justify-between">
          <h1 className="text-3xl font-bold">Weekly Intake Report</h1>
        </div>
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          {[...Array(4)].map((_, i) => (
            <Card key={i}>
              <CardContent className="p-6">
                <Skeleton className="h-20 w-full" />
              </CardContent>
            </Card>
          ))}
        </div>
        <Skeleton className="h-96 w-full" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="space-y-6">
        <h1 className="text-3xl font-bold">Weekly Intake Report</h1>
        <Card>
          <CardContent className="p-6">
            <div className="bg-red-50 border border-red-200 rounded-lg p-4">
              <p className="text-red-600 font-medium">Failed to load report</p>
              <p className="text-red-500 text-sm mt-1">{error.message}</p>
            </div>
          </CardContent>
        </Card>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold">Weekly Intake Report</h1>
          <p className="text-gray-600 mt-1">
            Track first-time pass rates and rejection reasons by closer
          </p>
        </div>
        <Button
          onClick={handleRefresh}
          variant="outline"
          size="sm"
          disabled={isLoading}
          className="flex items-center gap-2"
        >
          <RefreshCw className={`h-4 w-4 ${isLoading ? 'animate-spin' : ''}`} />
          Refresh
        </Button>
      </div>

      {/* Date Range Selector */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Calendar className="h-5 w-5" />
            Date Range
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="flex flex-col md:flex-row gap-4">
            {/* Quick Range Buttons */}
            <div className="flex gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => handleQuickRange(7)}
              >
                Last 7 Days
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => handleQuickRange(30)}
              >
                Last 30 Days
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => handleQuickRange(90)}
              >
                Last 90 Days
              </Button>
            </div>

            {/* Custom Date Range */}
            <div className="flex gap-4 flex-1">
              <div className="flex-1">
                <Label htmlFor="startDate" className="text-sm">Start Date</Label>
                <Input
                  id="startDate"
                  type="date"
                  value={startDate}
                  onChange={(e) => setStartDate(e.target.value)}
                />
              </div>
              <div className="flex-1">
                <Label htmlFor="endDate" className="text-sm">End Date</Label>
                <Input
                  id="endDate"
                  type="date"
                  value={endDate}
                  onChange={(e) => setEndDate(e.target.value)}
                />
              </div>
              <div className="flex items-end">
                <Button onClick={handleApplyDates}>
                  Apply
                </Button>
              </div>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Summary Cards */}
      {data && (
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          {/* Total Projects */}
          <Card>
            <CardContent className="p-6">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Total Projects</p>
                  <p className="text-3xl font-bold text-gray-900">{data.totalProjects}</p>
                </div>
                <div className="p-3 bg-blue-100 rounded-lg">
                  <Calendar className="h-6 w-6 text-blue-600" />
                </div>
              </div>
            </CardContent>
          </Card>

          {/* Overall Pass Rate */}
          <Card>
            <CardContent className="p-6">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Overall Pass Rate</p>
                  <p className={`text-3xl font-bold ${
                    data.overallPassRate >= 80 ? 'text-green-600' :
                    data.overallPassRate >= 60 ? 'text-yellow-600' :
                    'text-red-600'
                  }`}>
                    {formatPercentage(data.overallPassRate)}
                  </p>
                </div>
                <div className="p-3 bg-emerald-100 rounded-lg">
                  <TrendingUp className="h-6 w-6 text-emerald-600" />
                </div>
              </div>
            </CardContent>
          </Card>

          {/* Top Performer */}
          {data.topPerformer && (
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div className="flex-1 min-w-0">
                    <p className="text-sm text-gray-600">Top Performer</p>
                    <p className="text-lg font-bold text-gray-900 truncate">
                      {data.topPerformer.name}
                    </p>
                    <p className="text-sm text-green-600 font-semibold">
                      {formatPercentage(data.topPerformer.rate)}
                    </p>
                  </div>
                  <div className="p-3 bg-green-100 rounded-lg">
                    <Award className="h-6 w-6 text-green-600" />
                  </div>
                </div>
              </CardContent>
            </Card>
          )}

          {/* Needs Attention */}
          {data.needsAttention && (
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div className="flex-1 min-w-0">
                    <p className="text-sm text-gray-600">Needs Attention</p>
                    <p className="text-lg font-bold text-gray-900 truncate">
                      {data.needsAttention.name}
                    </p>
                    <p className="text-sm text-red-600 font-semibold">
                      {formatPercentage(data.needsAttention.rate)}
                    </p>
                  </div>
                  <div className="p-3 bg-red-100 rounded-lg">
                    <AlertCircle className="h-6 w-6 text-red-600" />
                  </div>
                </div>
              </CardContent>
            </Card>
          )}
        </div>
      )}

      {/* Top Rejection Reason */}
      {data?.topRejectionReason && (
        <Card>
          <CardContent className="p-4">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-red-100 rounded-lg">
                <TrendingDown className="h-5 w-5 text-red-600" />
              </div>
              <div>
                <p className="text-sm text-gray-600">Most Common Rejection Reason</p>
                <p className="font-semibold text-gray-900">{data.topRejectionReason}</p>
              </div>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Closer Breakdown Table */}
      {data && (
        <WeeklyIntakeTable
          closers={data.closers}
          startDate={appliedStartDate}
          endDate={appliedEndDate}
        />
      )}
    </div>
  );
}
