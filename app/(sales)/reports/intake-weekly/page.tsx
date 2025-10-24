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
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Calendar, TrendingUp, TrendingDown, AlertCircle, Award, RefreshCw, ArrowUpDown, X, Building2 } from 'lucide-react';
import { WeeklyIntakeTable } from '@/components/reports/WeeklyIntakeTable';
import { formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import type { WeeklyIntakeReport } from '@/lib/types/reports';

// Helper to get month-to-date range
function getMonthToDateDates() {
  const today = new Date();
  const firstOfMonth = new Date(today.getFullYear(), today.getMonth(), 1);

  return {
    start: firstOfMonth.toISOString().split('T')[0],
    end: today.toISOString().split('T')[0],
  };
}

export default function WeeklyIntakeReportPage() {
  const { data: session } = useSession();
  const queryClient = useQueryClient();
  const monthToDate = getMonthToDateDates();

  const [startDate, setStartDate] = useState(monthToDate.start);
  const [endDate, setEndDate] = useState(monthToDate.end);
  const [appliedStartDate, setAppliedStartDate] = useState(monthToDate.start);
  const [appliedEndDate, setAppliedEndDate] = useState(monthToDate.end);
  const [sortBy, setSortBy] = useState<'totalSubmitted' | 'firstTimePassRate' | 'rejectionRate' | 'stillRejected'>('totalSubmitted');
  const [selectedOfficeNames, setSelectedOfficeNames] = useState<string[]>([]);

  const { data, isLoading, error, refetch } = useQuery<WeeklyIntakeReport>({
    queryKey: ['weekly-intake-report', appliedStartDate, appliedEndDate, sortBy, selectedOfficeNames],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/reports/intake-weekly?startDate=${appliedStartDate}&endDate=${appliedEndDate}&sortBy=${sortBy}`;

      // Add office filter if selected (note: we'll need to map office names to IDs)
      // For now, we'll filter client-side since the API expects office IDs not names

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

  const handleQuickRange = (range: 'mtd' | 'last_month' | 30 | 90) => {
    const today = new Date();
    let newStart: string;
    let newEnd: string;

    if (range === 'mtd') {
      // Month to date
      const firstOfMonth = new Date(today.getFullYear(), today.getMonth(), 1);
      newStart = firstOfMonth.toISOString().split('T')[0];
      newEnd = today.toISOString().split('T')[0];
    } else if (range === 'last_month') {
      // Full previous month
      const firstOfLastMonth = new Date(today.getFullYear(), today.getMonth() - 1, 1);
      const lastOfLastMonth = new Date(today.getFullYear(), today.getMonth(), 0);
      newStart = firstOfLastMonth.toISOString().split('T')[0];
      newEnd = lastOfLastMonth.toISOString().split('T')[0];
    } else {
      // Days-based range
      const startDay = new Date(today);
      startDay.setDate(today.getDate() - range);
      newStart = startDay.toISOString().split('T')[0];
      newEnd = today.toISOString().split('T')[0];
    }

    setStartDate(newStart);
    setEndDate(newEnd);
    setAppliedStartDate(newStart);
    setAppliedEndDate(newEnd);
  };

  // Filter closers by selected offices (client-side)
  const filteredData = data ? {
    ...data,
    closers: selectedOfficeNames.length > 0
      ? data.closers.filter(c => c.officeName && selectedOfficeNames.includes(c.officeName))
      : data.closers
  } : undefined;

  // Get available offices from metadata
  const availableOffices = data?.metadata?.officeBreakdown?.map(o => o.officeName) || [];

  if (isLoading) {
    return (
      <div className="space-y-6">
        <div className="flex items-center justify-between">
          <h1 className="text-3xl font-bold">Intake Quality Report</h1>
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
        <h1 className="text-3xl font-bold">Intake Quality Report</h1>
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
          <h1 className="text-3xl font-bold">Intake Quality Report</h1>
          <p className="text-gray-600 mt-1">
            Track first-time pass rates and rejection reasons by closer
          </p>
          {filteredData && (
            <p className="text-sm text-gray-500 mt-1">
              Showing {filteredData.closers.length} closers
              {selectedOfficeNames.length > 0 && ` from ${selectedOfficeNames.length} office${selectedOfficeNames.length > 1 ? 's' : ''}`}
              {' • '}Sorted by: {sortBy === 'totalSubmitted' ? 'Most Active' :
                sortBy === 'firstTimePassRate' ? 'Best Quality' :
                sortBy === 'rejectionRate' ? 'Most Rejections' : 'Needs Fixing'}
            </p>
          )}
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

      {/* Filters Card */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Calendar className="h-5 w-5" />
            Filters & Date Range
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Sort & Office Filter Row */}
          <div className="flex flex-col sm:flex-row gap-4">
            {/* Sort By */}
            <div className="flex-1">
              <Label className="text-sm mb-2 flex items-center gap-2">
                <ArrowUpDown className="h-4 w-4" />
                Sort By
              </Label>
              <Select value={sortBy} onValueChange={(value: any) => setSortBy(value)}>
                <SelectTrigger>
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="totalSubmitted">Most Active (Total Projects)</SelectItem>
                  <SelectItem value="firstTimePassRate">Best Quality (Pass Rate)</SelectItem>
                  <SelectItem value="rejectionRate">Most Rejections</SelectItem>
                  <SelectItem value="stillRejected">Needs Fixing (Still Rejected)</SelectItem>
                </SelectContent>
              </Select>
            </div>

            {/* Office Filter */}
            <div className="flex-1">
              <Label className="text-sm mb-2 flex items-center gap-2">
                <Building2 className="h-4 w-4" />
                Filter by Office
              </Label>
              <Select
                value={selectedOfficeNames.length === 0 ? 'all' : selectedOfficeNames[0]}
                onValueChange={(value) => {
                  if (value === 'all') {
                    setSelectedOfficeNames([]);
                  } else {
                    setSelectedOfficeNames([value]);
                  }
                }}
              >
                <SelectTrigger>
                  <SelectValue>
                    {selectedOfficeNames.length === 0 ? 'All Offices' : selectedOfficeNames[0]}
                  </SelectValue>
                </SelectTrigger>
                <SelectContent>
                  <SelectItem value="all">All Offices</SelectItem>
                  {availableOffices.map(office => (
                    <SelectItem key={office} value={office}>{office}</SelectItem>
                  ))}
                </SelectContent>
              </Select>
            </div>
          </div>

          {/* Date Range Controls */}
          <div className="flex flex-col md:flex-row gap-4">
            {/* Quick Range Buttons */}
            <div className="flex flex-wrap gap-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => handleQuickRange('mtd')}
              >
                Month to Date
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => handleQuickRange('last_month')}
              >
                Last Month
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

      {/* Active Filters */}
      {(selectedOfficeNames.length > 0 || sortBy !== 'totalSubmitted') && (
        <div className="flex items-center gap-2 flex-wrap">
          <span className="text-sm text-gray-600 font-medium">Active Filters:</span>
          {selectedOfficeNames.length > 0 && (
            <Badge variant="secondary" className="flex items-center gap-1">
              <Building2 className="h-3 w-3" />
              {selectedOfficeNames[0]}
              <button
                onClick={() => setSelectedOfficeNames([])}
                className="ml-1 hover:bg-gray-300 rounded-full p-0.5"
              >
                <X className="h-3 w-3" />
              </button>
            </Badge>
          )}
          {sortBy !== 'totalSubmitted' && (
            <Badge variant="secondary" className="flex items-center gap-1">
              <ArrowUpDown className="h-3 w-3" />
              {sortBy === 'firstTimePassRate' ? 'Best Quality' :
               sortBy === 'rejectionRate' ? 'Most Rejections' : 'Needs Fixing'}
              <button
                onClick={() => setSortBy('totalSubmitted')}
                className="ml-1 hover:bg-gray-300 rounded-full p-0.5"
              >
                <X className="h-3 w-3" />
              </button>
            </Badge>
          )}
          <Button
            variant="ghost"
            size="sm"
            onClick={() => {
              setSortBy('totalSubmitted');
              setSelectedOfficeNames([]);
            }}
            className="text-xs h-7"
          >
            Clear All
          </Button>
        </div>
      )}

      {/* Summary Cards */}
      {filteredData && (
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          {/* Total Projects */}
          <Card>
            <CardContent className="p-6">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Total Projects</p>
                  <p className="text-3xl font-bold text-gray-900">{filteredData.totalProjects}</p>
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
                    filteredData.overallPassRate >= 80 ? 'text-green-600' :
                    filteredData.overallPassRate >= 60 ? 'text-yellow-600' :
                    'text-red-600'
                  }`}>
                    {formatPercentage(filteredData.overallPassRate)}
                  </p>
                </div>
                <div className="p-3 bg-emerald-100 rounded-lg">
                  <TrendingUp className="h-6 w-6 text-emerald-600" />
                </div>
              </div>
            </CardContent>
          </Card>

          {/* Top Performer */}
          {filteredData.topPerformer && (
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div className="flex-1 min-w-0">
                    <p className="text-sm text-gray-600">Top Performer</p>
                    <p className="text-lg font-bold text-gray-900 truncate">
                      {filteredData.topPerformer.name}
                    </p>
                    <p className="text-sm text-green-600 font-semibold">
                      {formatPercentage(filteredData.topPerformer.rate)}
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
          {filteredData.needsAttention && (
            <Card>
              <CardContent className="p-6">
                <div className="flex items-center justify-between">
                  <div className="flex-1 min-w-0">
                    <p className="text-sm text-gray-600">Needs Attention</p>
                    <p className="text-lg font-bold text-gray-900 truncate">
                      {filteredData.needsAttention.name}
                    </p>
                    <p className="text-sm text-red-600 font-semibold">
                      {formatPercentage(filteredData.needsAttention.rate)}
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
      {filteredData?.topRejectionReason && (
        <Card>
          <CardContent className="p-4">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-red-100 rounded-lg">
                <TrendingDown className="h-5 w-5 text-red-600" />
              </div>
              <div>
                <p className="text-sm text-gray-600">Most Common Rejection Reason</p>
                <p className="font-semibold text-gray-900">{filteredData.topRejectionReason}</p>
              </div>
            </div>
          </CardContent>
        </Card>
      )}

      {/* Closer Breakdown Table */}
      {filteredData && (
        <WeeklyIntakeTable
          closers={filteredData.closers}
          startDate={appliedStartDate}
          endDate={appliedEndDate}
        />
      )}
    </div>
  );
}
