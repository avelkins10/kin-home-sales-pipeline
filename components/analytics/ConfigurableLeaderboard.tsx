'use client';

import React, { useState, useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useRouter } from 'next/navigation';
import { format, startOfWeek, startOfMonth, startOfQuarter, startOfYear } from 'date-fns';
import { toast } from 'sonner';
import { cn } from '@/lib/utils';
import { formatCurrency, formatPercentage, formatLargeNumber } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import { LeaderboardResponse, LeaderboardEntry, LeaderboardMetric, LeaderboardRole } from '@/lib/repcard/types';
import { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { OfficeMultiSelect } from './OfficeMultiSelect';
import { DateRangePicker } from '@/components/ui/date-range-picker';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible';
import { 
  Trophy, 
  Medal, 
  TrendingUp, 
  TrendingDown, 
  Minus, 
  Star, 
  FileDown, 
  RefreshCw, 
  ChevronDown, 
  ChevronUp, 
  ExternalLink 
} from 'lucide-react';

export interface ConfigurableLeaderboardProps {
  defaultRole?: LeaderboardRole;
  defaultMetric?: LeaderboardMetric;
  defaultTimeRange?: TimeRange;
  defaultOfficeIds?: number[];
  configId?: string; // Optional: use a saved configuration
  showFilters?: boolean;
  showExport?: boolean;
  showRefresh?: boolean;
  limit?: number;
  title?: string;
  description?: string;
  collapsible?: boolean;
  defaultOpen?: boolean;
  className?: string;
  onEntryClick?: (entry: LeaderboardEntry) => void;
}

export function ConfigurableLeaderboard({
  defaultRole = 'all',
  defaultMetric = 'quality_score',
  defaultTimeRange = 'month',
  defaultOfficeIds = [],
  configId,
  showFilters = true,
  showExport = true,
  showRefresh = true,
  limit = 50,
  title = 'Leaderboard',
  description,
  collapsible = false,
  defaultOpen = true,
  className,
  onEntryClick
}: ConfigurableLeaderboardProps) {
  const router = useRouter();
  
  // State
  const [role, setRole] = useState<LeaderboardRole>(defaultRole);
  const [metric, setMetric] = useState<LeaderboardMetric>(defaultMetric);
  const [timeRange, setTimeRange] = useState<TimeRange>(defaultTimeRange);
  const [customDateRange, setCustomDateRange] = useState<CustomDateRange | undefined>(undefined);
  const [selectedOfficeIds, setSelectedOfficeIds] = useState<number[]>(defaultOfficeIds);
  const [isExporting, setIsExporting] = useState(false);
  const [isOpen, setIsOpen] = useState(defaultOpen);
  const [availableOffices, setAvailableOffices] = useState<Array<{ id: number; name: string }>>([]);

  // Fetch leaderboard data
  const { data, error, isLoading, refetch } = useQuery<LeaderboardResponse>({
    queryKey: ['configurable-leaderboard', configId, role, metric, timeRange, customDateRange, selectedOfficeIds, limit],
    queryFn: async () => {
      const params = new URLSearchParams({
        role,
        metric,
        timeRange,
        limit: limit.toString()
      });

      // Add configId if provided
      if (configId) {
        params.append('configId', configId);
      }

      if (selectedOfficeIds.length > 0) {
        params.append('officeIds', selectedOfficeIds.join(','));
      }

      if (timeRange === 'custom' && customDateRange) {
        params.append('startDate', customDateRange.startDate);
        params.append('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) {
        throw new Error(`Failed to fetch leaderboard: ${response.statusText}`);
      }
      return response.json();
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds for near real-time updates
    staleTime: 60000, // Consider data stale after 1 minute
    enabled: true
  });

  // Fetch available offices
  const { data: officesData } = useQuery({
    queryKey: ['leaderboard-offices'],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/analytics/office-metrics`);
      if (!response.ok) {
        throw new Error(`Failed to fetch offices: ${response.statusText}`);
      }
      return response.json();
    },
    staleTime: 300000, // 5 minutes
    enabled: showFilters
  });

  // Update available offices when data loads
  useEffect(() => {
    if (officesData?.metrics) {
      // Derive offices from metrics array and de-duplicate by id
      const officeMap = new Map();
      officesData.metrics.forEach((metric: any) => {
        if (metric.officeId && metric.officeName) {
          officeMap.set(metric.officeId, {
            id: metric.officeId,
            name: metric.officeName
          });
        }
      });
      setAvailableOffices(Array.from(officeMap.values()));
    }
  }, [officesData]);

  // Helper functions
  const getMetricLabel = (metric: LeaderboardMetric): string => {
    const labels: Record<LeaderboardMetric, string> = {
      doors_knocked: 'Doors Knocked',
      appointments_set: 'Appointments Set',
      sales_closed: 'Sales Closed',
      revenue: 'Revenue',
      quality_score: 'Quality Score',
      appointment_speed: 'Appointment Speed',
      attachment_rate: 'Attachment Rate'
    };
    return labels[metric] || metric;
  };

  const formatMetricValue = (value: number, metric: LeaderboardMetric): string => {
    switch (metric) {
      case 'revenue':
        return formatCurrency(value);
      case 'quality_score':
      case 'appointment_speed':
      case 'attachment_rate':
        return formatPercentage(value);
      default:
        return formatLargeNumber(value);
    }
  };

  const getRankBadge = (rank: number): JSX.Element => {
    if (rank === 1) {
      return (
        <Badge className="bg-gradient-to-r from-yellow-500 to-amber-500 text-white font-bold">
          🥇 {rank}
        </Badge>
      );
    } else if (rank === 2) {
      return (
        <Badge className="bg-gradient-to-r from-gray-400 to-slate-500 text-white font-bold">
          🥈 {rank}
        </Badge>
      );
    } else if (rank === 3) {
      return (
        <Badge className="bg-gradient-to-r from-orange-500 to-amber-600 text-white font-bold">
          🥉 {rank}
        </Badge>
      );
    } else {
      return (
        <Badge variant="secondary" className="font-bold">
          {rank}
        </Badge>
      );
    }
  };

  const getTrendIcon = (trend?: 'up' | 'down' | 'same' | 'new'): JSX.Element | null => {
    switch (trend) {
      case 'up':
        return <TrendingUp className="h-4 w-4 text-green-600" />;
      case 'down':
        return <TrendingDown className="h-4 w-4 text-red-600" />;
      case 'same':
        return <Minus className="h-4 w-4 text-gray-400" />;
      case 'new':
        return <Star className="h-4 w-4 text-blue-600" />;
      default:
        return null;
    }
  };

  const getRowBackgroundClass = (rank: number): string => {
    if (rank === 1) {
      return 'bg-gradient-to-r from-yellow-50 to-amber-50 border-l-4 border-yellow-500';
    } else if (rank === 2) {
      return 'bg-gradient-to-r from-gray-50 to-slate-50 border-l-4 border-gray-400';
    } else if (rank === 3) {
      return 'bg-gradient-to-r from-orange-50 to-amber-50 border-l-4 border-orange-500';
    }
    return '';
  };

  const handleExportCSV = async () => {
    if (!data?.leaderboard) return;

    setIsExporting(true);
    try {
      const exportData = data.leaderboard.map(entry => ({
        rank: entry.rank,
        userName: entry.userName,
        userEmail: entry.userEmail,
        office: entry.office,
        role: entry.role,
        metricValue: formatMetricValue(entry.metricValue, metric),
        metricType: getMetricLabel(metric),
        trend: entry.trend || 'same'
      }));

      const headers = {
        rank: 'Rank',
        userName: 'Name',
        userEmail: 'Email',
        office: 'Office',
        role: 'Role',
        metricValue: getMetricLabel(metric),
        metricType: 'Metric Type',
        trend: 'Trend'
      };

      const filename = `leaderboard-${metric}-${timeRange}-${new Date().toISOString().split('T')[0]}.csv`;
      await exportAnalyticsToCSV(exportData, filename, headers);
      toast.success('Leaderboard exported successfully');
    } catch (error) {
      console.error('Export error:', error);
      toast.error('Failed to export leaderboard');
    } finally {
      setIsExporting(false);
    }
  };

  const handleEntryClick = (entry: LeaderboardEntry) => {
    if (onEntryClick) {
      onEntryClick(entry);
    } else {
      router.push(`/analytics/rep/${entry.userId}`);
    }
  };

  const handleRefresh = () => {
    refetch();
    toast.success('Refreshing leaderboard...');
  };

  // Loading state
  if (isLoading) {
    return <ConfigurableLeaderboardSkeleton />;
  }

  // Error state
  if (error) {
    return (
      <Card className={cn('w-full', className)}>
        <CardHeader>
          <CardTitle className="text-red-600">Error Loading Leaderboard</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-red-600 mb-4">
            Failed to load leaderboard data. Please try again.
          </div>
          <Button onClick={() => refetch()} variant="outline">
            Retry
          </Button>
        </CardContent>
      </Card>
    );
  }

  // Empty state
  if (!data?.leaderboard || data.leaderboard.length === 0) {
    return (
      <Card className={cn('w-full', className)}>
        <CardHeader>
          <CardTitle>{title}</CardTitle>
          {description && <p className="text-sm text-gray-600">{description}</p>}
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <Trophy className="h-12 w-12 mx-auto mb-4 text-gray-300" />
            <p className="text-lg font-medium">No leaderboard data available</p>
            <p className="text-sm">Try adjusting your filters or time range</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Collapsible open={isOpen} onOpenChange={setIsOpen} disabled={!collapsible}>
      <Card className={cn('w-full', className)}>
        {/* Header Section */}
        <CardHeader>
          <div className="flex items-center justify-between">
            <div className="flex items-center space-x-2">
              {collapsible && (
                <CollapsibleTrigger asChild>
                  <Button variant="ghost" size="sm" className="p-0">
                    {isOpen ? <ChevronUp /> : <ChevronDown />}
                  </Button>
                </CollapsibleTrigger>
              )}
              <Trophy className="h-6 w-6 text-yellow-600" />
              <CardTitle>{title}</CardTitle>
            </div>
            <div className="flex items-center space-x-2">
              {showRefresh && (
                <Button variant="ghost" size="sm" onClick={handleRefresh}>
                  <RefreshCw className="h-4 w-4" />
                </Button>
              )}
              {showExport && (
                <Button
                  variant="outline"
                  size="sm"
                  onClick={handleExportCSV}
                  disabled={isExporting || !data?.leaderboard?.length}
                >
                  {isExporting ? (
                    <>
                      <FileDown className="h-4 w-4 mr-2 animate-pulse" />
                      Exporting...
                    </>
                  ) : (
                    <>
                      <FileDown className="h-4 w-4 mr-2" />
                      Export CSV
                    </>
                  )}
                </Button>
              )}
            </div>
          </div>
          {description && <p className="text-sm text-gray-600">{description}</p>}
          <p className="text-xs text-gray-500">
            {data.leaderboard.length} entries • Last updated: {new Date(data.metadata.calculatedAt).toLocaleTimeString()}
          </p>
        </CardHeader>

        <CollapsibleContent>
          <CardContent>
            {/* Filters Bar */}
            {showFilters && (
              <div className="flex flex-col md:flex-row gap-4 mb-6 flex-wrap">
                {/* Role Filter */}
                <Select value={role} onValueChange={(value: LeaderboardRole) => setRole(value)}>
                  <SelectTrigger className="w-full md:w-32">
                    <SelectValue placeholder="Role" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="all">All Roles</SelectItem>
                    <SelectItem value="setter">Setters</SelectItem>
                    <SelectItem value="closer">Closers</SelectItem>
                  </SelectContent>
                </Select>

                {/* Metric Filter */}
                <Select value={metric} onValueChange={(value: LeaderboardMetric) => setMetric(value)}>
                  <SelectTrigger className="w-full md:w-48">
                    <SelectValue placeholder="Metric" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="quality_score">Quality Score</SelectItem>
                    <SelectItem value="doors_knocked">Doors Knocked</SelectItem>
                    <SelectItem value="appointments_set">Appointments Set</SelectItem>
                    <SelectItem value="sales_closed">Sales Closed</SelectItem>
                    <SelectItem value="revenue">Revenue</SelectItem>
                    <SelectItem value="appointment_speed">Appointment Speed</SelectItem>
                    <SelectItem value="attachment_rate">Attachment Rate</SelectItem>
                  </SelectContent>
                </Select>

                {/* Time Range Filter */}
                <div className="flex items-center gap-2">
                  {['today', 'week', 'month', 'quarter', 'ytd'].map((range) => (
                    <Button
                      key={range}
                      variant={timeRange === range ? 'default' : 'outline'}
                      size="sm"
                      onClick={() => setTimeRange(range as TimeRange)}
                      className="capitalize"
                    >
                      {range === 'today' ? 'Today' : range === 'ytd' ? 'This Year' : `This ${range}`}
                    </Button>
                  ))}
                  <DateRangePicker
                    value={customDateRange ? {
                      from: new Date(customDateRange.startDate),
                      to: new Date(customDateRange.endDate)
                    } : undefined}
                    onChange={(range) => {
                      if (range?.from && range?.to) {
                        setCustomDateRange({
                          startDate: format(range.from, 'yyyy-MM-dd'),
                          endDate: format(range.to, 'yyyy-MM-dd')
                        });
                        setTimeRange('custom');
                      }
                    }}
                  />
                </div>

                {/* Office Multi-Select Filter */}
                <OfficeMultiSelect
                  selectedOfficeIds={selectedOfficeIds}
                  availableOffices={availableOffices}
                  onChange={setSelectedOfficeIds}
                  placeholder="All Offices"
                />
              </div>
            )}

            {/* Leaderboard Table */}
            <div className="overflow-x-auto">
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead className="w-20">Rank</TableHead>
                    <TableHead className="min-w-48">Name</TableHead>
                    <TableHead className="min-w-32">Office</TableHead>
                    <TableHead className="w-24">Role</TableHead>
                    <TableHead className="text-right min-w-32">
                      {getMetricLabel(metric)}
                    </TableHead>
                    <TableHead className="w-16 text-center">Trend</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {data.leaderboard.map((entry) => (
                    <TableRow
                      key={entry.userId}
                      className={cn(
                        getRowBackgroundClass(entry.rank),
                        'hover:bg-slate-100 transition-colors cursor-pointer'
                      )}
                      onClick={() => handleEntryClick(entry)}
                    >
                      <TableCell className="font-medium">
                        {getRankBadge(entry.rank)}
                      </TableCell>
                      <TableCell className="font-medium">
                        <div className="flex items-center space-x-2">
                          <span className="text-blue-600 hover:text-blue-800 hover:underline">
                            {entry.userName}
                          </span>
                          <ExternalLink className="h-3 w-3 text-gray-400" />
                        </div>
                      </TableCell>
                      <TableCell>{entry.office}</TableCell>
                      <TableCell>
                        <Badge variant={entry.role === 'closer' ? 'default' : 'secondary'}>
                          {entry.role}
                        </Badge>
                      </TableCell>
                      <TableCell className="text-right font-semibold">
                        {formatMetricValue(entry.metricValue, metric)}
                      </TableCell>
                      <TableCell className="text-center">
                        {getTrendIcon(entry.trend)}
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </div>

            {/* Footer with metadata */}
            <div className="mt-4 text-xs text-gray-500 text-center">
              Showing {data.leaderboard.length} of {data.metadata.totalEntries} entries
              {data.metadata.cached && ' • Cached data'}
            </div>
          </CardContent>
        </CollapsibleContent>
      </Card>
    </Collapsible>
  );
}

// Loading skeleton component
export function ConfigurableLeaderboardSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <Skeleton className="h-6 w-6" />
            <Skeleton className="h-6 w-32" />
          </div>
          <div className="flex items-center space-x-2">
            <Skeleton className="h-8 w-8" />
            <Skeleton className="h-8 w-24" />
          </div>
        </div>
        <Skeleton className="h-4 w-48" />
      </CardHeader>
      <CardContent>
        {/* Filters skeleton */}
        <div className="flex flex-col md:flex-row gap-4 mb-6 flex-wrap">
          <Skeleton className="h-10 w-32" />
          <Skeleton className="h-10 w-48" />
          <div className="flex gap-2">
            <Skeleton className="h-10 w-16" />
            <Skeleton className="h-10 w-16" />
            <Skeleton className="h-10 w-16" />
            <Skeleton className="h-10 w-16" />
            <Skeleton className="h-10 w-16" />
          </div>
          <Skeleton className="h-10 w-40" />
        </div>

        {/* Table skeleton */}
        <div className="overflow-x-auto">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead className="w-20">Rank</TableHead>
                <TableHead className="min-w-48">Name</TableHead>
                <TableHead className="min-w-32">Office</TableHead>
                <TableHead className="w-24">Role</TableHead>
                <TableHead className="text-right min-w-32">Metric</TableHead>
                <TableHead className="w-16 text-center">Trend</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {Array.from({ length: 5 }).map((_, i) => (
                <TableRow key={i}>
                  <TableCell><Skeleton className="h-6 w-12" /></TableCell>
                  <TableCell><Skeleton className="h-4 w-32" /></TableCell>
                  <TableCell><Skeleton className="h-4 w-24" /></TableCell>
                  <TableCell><Skeleton className="h-6 w-16" /></TableCell>
                  <TableCell className="text-right"><Skeleton className="h-4 w-20" /></TableCell>
                  <TableCell className="text-center"><Skeleton className="h-4 w-4" /></TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </div>
      </CardContent>
    </Card>
  );
}

export default ConfigurableLeaderboard;
