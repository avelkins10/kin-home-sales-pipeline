'use client';

import { useState, useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import Link from 'next/link';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { 
  ArrowUp, 
  ArrowDown, 
  ArrowUpDown, 
  FileDown, 
  Search, 
  DollarSign, 
  ChevronDown, 
  ChevronUp 
} from 'lucide-react';
import { formatLargeNumber, formatPercentage, formatCurrency } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import type { LeaderboardResponse } from '@/lib/repcard/types';
import { toast } from 'sonner';

interface CloserPerformanceTableProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
  showExport?: boolean;
}

type SortColumn = 'closerName' | 'appointmentsSat' | 'salesClosed' | 'sitCloseRate' | 'revenue' | 'avgDealSize' | 'followUps' | 'office';
type SortDirection = 'ascending' | 'descending';

interface CloserMetrics {
  userId: string;
  closerName: string;
  closerEmail: string;
  office: string;
  appointmentsSat: number; // from QuickBase projects count
  salesClosed: number;
  sitCloseRate: number; // calculated
  revenue: number;
  avgDealSize: number; // calculated
  followUps: number; // from quality metrics
  rank?: number;
}

export function CloserPerformanceTable({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds,
  showExport = true
}: CloserPerformanceTableProps) {
  const [sortColumn, setSortColumn] = useState<SortColumn>('revenue');
  const [sortDirection, setSortDirection] = useState<SortDirection>('descending');
  const [searchQuery, setSearchQuery] = useState('');
  const [isExporting, setIsExporting] = useState(false);
  const [isOpen, setIsOpen] = useState(true);

  // Load collapsible state from localStorage
  useEffect(() => {
    const savedState = localStorage.getItem('closer-performance-table-open');
    if (savedState !== null) {
      setIsOpen(savedState === 'true');
    }
  }, []);

  // Save collapsible state to localStorage
  const handleToggle = () => {
    const newState = !isOpen;
    setIsOpen(newState);
    localStorage.setItem('closer-performance-table-open', String(newState));
  };

  // Build query parameters
  const buildQueryParams = (metric: string) => {
    // Normalize unsupported time ranges
    let normalizedTimeRange = timeRange;
    let normalizedStartDate = customDateRange?.startDate;
    let normalizedEndDate = customDateRange?.endDate;
    
    if (timeRange === 'last_12_months') {
      normalizedTimeRange = 'custom';
      const now = new Date();
      const last12 = new Date(now);
      last12.setMonth(now.getMonth() - 12);
      normalizedStartDate = last12.toISOString().split('T')[0];
      normalizedEndDate = now.toISOString().split('T')[0];
    }
    
    const params = new URLSearchParams({
      role: 'closer',
      metric,
      timeRange: normalizedTimeRange,
      limit: '100'
    });
    
    if (normalizedStartDate && normalizedEndDate) {
      params.set('startDate', normalizedStartDate);
      params.set('endDate', normalizedEndDate);
    }
    
    if (officeIds && officeIds.length > 0) {
      params.set('officeIds', officeIds.join(','));
    }
    
    return params.toString();
  };

  // Fetch sales closed data
  const { data: salesClosedData, isLoading: salesLoading, error: salesError } = useQuery({
    queryKey: ['closer-sales-closed', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams('sales_closed');
      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch sales closed data');
      return response.json() as Promise<LeaderboardResponse>;
    },
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  // Fetch revenue data
  const { data: revenueData, isLoading: revenueLoading, error: revenueError } = useQuery({
    queryKey: ['closer-revenue', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams('revenue');
      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch revenue data');
      return response.json() as Promise<LeaderboardResponse>;
    },
    staleTime: 15 * 60 * 1000,
  });

  // Fetch appointments sat data (via API)
  const { data: appointmentsSatData, isLoading: appointmentsLoading, error: appointmentsError } = useQuery({
    queryKey: ['closer-appointments-sat', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        timeRange,
        limit: '100'
      });
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }
      
      if (officeIds && officeIds.length > 0) {
        params.set('officeIds', officeIds.join(','));
      }
      
      const response = await fetch(`${getBaseUrl()}/api/analytics/closer-appointments-sat?${params}`);
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        throw new Error(errorData.error || 'Failed to fetch appointments sat data');
      }
      const data = await response.json();
      // Ensure we return an array
      return Array.isArray(data) ? data : [];
    },
    staleTime: 15 * 60 * 1000,
  });

  // Fetch follow-ups data (via API)
  const { data: followUpsData, isLoading: followUpsLoading, error: followUpsError } = useQuery({
    queryKey: ['closer-follow-ups', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        timeRange,
        limit: '100'
      });
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }
      
      if (officeIds && officeIds.length > 0) {
        params.set('officeIds', officeIds.join(','));
      }
      
      const response = await fetch(`${getBaseUrl()}/api/analytics/closer-followups?${params}`);
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        throw new Error(errorData.error || 'Failed to fetch follow-ups data');
      }
      const data = await response.json();
      // Ensure we return an array
      return Array.isArray(data) ? data : [];
    },
    staleTime: 30 * 60 * 1000, // 30 minutes (more expensive query)
  });

  // Combine data from all queries - build union of users across all datasets
  const allUserIds = new Set<string>();
  
  // Collect user IDs from all successful queries with error handling
  try {
    if (salesClosedData?.leaderboard && Array.isArray(salesClosedData.leaderboard)) {
      salesClosedData.leaderboard.forEach(entry => {
        if (entry?.userId) allUserIds.add(entry.userId);
      });
    }
    if (revenueData?.leaderboard && Array.isArray(revenueData.leaderboard)) {
      revenueData.leaderboard.forEach(entry => {
        if (entry?.userId) allUserIds.add(entry.userId);
      });
    }
    if (appointmentsSatData && Array.isArray(appointmentsSatData)) {
      appointmentsSatData.forEach(entry => {
        if (entry?.userId) allUserIds.add(entry.userId);
      });
    }
    if (followUpsData && Array.isArray(followUpsData)) {
      followUpsData.forEach(entry => {
        if (entry?.userId) allUserIds.add(entry.userId);
      });
    }
  } catch (error) {
    console.error('[CloserPerformanceTable] Error collecting user IDs:', error);
  }
  
  // Build combined metrics from union of all users with error handling
  let combinedMetrics: CloserMetrics[] = [];
  try {
    combinedMetrics = Array.from(allUserIds).map(userId => {
      const salesEntry = Array.isArray(salesClosedData?.leaderboard) 
        ? salesClosedData.leaderboard.find(e => e?.userId === userId) 
        : undefined;
      const revenueEntry = Array.isArray(revenueData?.leaderboard)
        ? revenueData.leaderboard.find(e => e?.userId === userId)
        : undefined;
      const appointmentsEntry = Array.isArray(appointmentsSatData)
        ? appointmentsSatData.find(e => e?.userId === userId)
        : undefined;
      const followUpsEntry = Array.isArray(followUpsData)
        ? followUpsData.find(e => e?.userId === userId)
        : undefined;
      
      // Use the first available entry for user info, fallback to defaults
      const baseEntry = salesEntry || revenueEntry || appointmentsEntry || followUpsEntry;
      
      const salesClosed = salesEntry?.metricValue || 0;
      const revenue = revenueEntry?.metricValue || 0;
      const appointmentsSat = appointmentsEntry?.appointmentsSat || 0;
      const followUps = followUpsEntry?.followUps || 0;
      
      return {
        userId,
        closerName: baseEntry?.userName || baseEntry?.closerName || 'Unknown',
        closerEmail: baseEntry?.userEmail || baseEntry?.closerEmail || '',
        office: baseEntry?.office || 'Unknown',
        appointmentsSat,
        salesClosed,
        sitCloseRate: appointmentsSat > 0 ? (salesClosed / appointmentsSat) * 100 : 0,
        revenue,
        avgDealSize: salesClosed > 0 ? revenue / salesClosed : 0,
        followUps
      };
    });
  } catch (error) {
    console.error('[CloserPerformanceTable] Error building combined metrics:', error);
    combinedMetrics = [];
  }

  // Apply search filter
  const filteredData = combinedMetrics.filter(closer => 
    closer.closerName.toLowerCase().includes(searchQuery.toLowerCase())
  );

  // Apply sorting
  const sortedData = [...filteredData].sort((a, b) => {
    let aValue: number | string;
    let bValue: number | string;

    switch (sortColumn) {
      case 'closerName':
        aValue = a.closerName;
        bValue = b.closerName;
        break;
      case 'office':
        aValue = a.office;
        bValue = b.office;
        break;
      case 'appointmentsSat':
        aValue = a.appointmentsSat;
        bValue = b.appointmentsSat;
        break;
      case 'salesClosed':
        aValue = a.salesClosed;
        bValue = b.salesClosed;
        break;
      case 'sitCloseRate':
        aValue = a.sitCloseRate;
        bValue = b.sitCloseRate;
        break;
      case 'revenue':
        aValue = a.revenue;
        bValue = b.revenue;
        break;
      case 'avgDealSize':
        aValue = a.avgDealSize;
        bValue = b.avgDealSize;
        break;
      case 'followUps':
        aValue = a.followUps;
        bValue = b.followUps;
        break;
      default:
        aValue = a.revenue;
        bValue = b.revenue;
    }

    if (typeof aValue === 'string' && typeof bValue === 'string') {
      return sortDirection === 'ascending' 
        ? aValue.localeCompare(bValue)
        : bValue.localeCompare(aValue);
    }

    return sortDirection === 'ascending' 
      ? (aValue as number) - (bValue as number)
      : (bValue as number) - (aValue as number);
  });

  // Assign ranks
  const rankedData = sortedData.map((closer, index) => ({
    ...closer,
    rank: index + 1
  }));

  // Handle sort column change
  const handleSort = (column: SortColumn) => {
    if (sortColumn === column) {
      setSortDirection(sortDirection === 'ascending' ? 'descending' : 'ascending');
    } else {
      setSortColumn(column);
      setSortDirection('descending');
    }
  };

  // Get sort icon
  const getSortIcon = (column: SortColumn) => {
    if (sortColumn !== column) return <ArrowUpDown className="h-4 w-4 opacity-50" />;
    return sortDirection === 'ascending' ? <ArrowUp className="h-4 w-4" /> : <ArrowDown className="h-4 w-4" />;
  };

  // Handle export
  const handleExport = async () => {
    setIsExporting(true);
    try {
      const exportData = rankedData.map(closer => ({
        rank: closer.rank || 0,
        closerName: closer.closerName,
        office: closer.office,
        appointmentsSat: closer.appointmentsSat,
        salesClosed: closer.salesClosed,
        sitCloseRate: closer.sitCloseRate.toFixed(1),
        revenue: closer.revenue.toFixed(0),
        avgDealSize: closer.avgDealSize.toFixed(0),
        followUps: closer.followUps
      }));
      
      const headers = {
        rank: 'Rank',
        closerName: 'Closer Name',
        office: 'Office',
        appointmentsSat: 'Appointments Sat',
        salesClosed: 'Sales Closed',
        sitCloseRate: 'Sit/Close (%)',
        revenue: 'Revenue',
        avgDealSize: 'Avg Deal Size',
        followUps: 'Follow-Ups'
      };
      
      const filename = `closer-performance-${new Date().toISOString().split('T')[0]}.csv`;
      exportAnalyticsToCSV(exportData, filename, headers);
      toast.success('Closer performance data exported successfully');
    } catch (error) {
      toast.error('Failed to export closer performance data');
    } finally {
      setIsExporting(false);
    }
  };

  // Get color class for sit/close rate
  const getSitCloseRateColor = (value: number) => {
    if (value >= 30) return 'text-green-600';
    if (value >= 20) return 'text-yellow-600';
    return 'text-red-600';
  };

  // Loading state
  const isLoading = salesLoading || revenueLoading || appointmentsLoading || followUpsLoading;
  
  // Soft error handling - only fail if critical queries fail
  const criticalErrors = salesError || revenueError; // Base metrics are critical
  const nonCriticalErrors = appointmentsError || followUpsError; // These can be zero
  
  // Show warnings for non-critical errors
  const hasNonCriticalErrors = appointmentsError || followUpsError;
  
  if (criticalErrors) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <DollarSign className="h-5 w-5" />
            Closer Performance
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8">
            <p className="text-muted-foreground mb-4">Failed to load critical performance data</p>
            <Button onClick={() => window.location.reload()}>Retry</Button>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Collapsible open={isOpen} onOpenChange={handleToggle}>
      <Card>
        <CollapsibleTrigger asChild>
          <CardHeader className="cursor-pointer hover:bg-muted/50 transition-colors">
            <CardTitle className="flex items-center justify-between">
              <div className="flex items-center gap-2">
                <DollarSign className="h-5 w-5" />
                Closer Performance
                <Badge variant="secondary" className="ml-2">
                  {rankedData.length} closers
                </Badge>
              </div>
              {isOpen ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />}
            </CardTitle>
          </CardHeader>
        </CollapsibleTrigger>
        
        <CollapsibleContent>
          <CardContent>
            {/* Non-critical error warnings */}
            {hasNonCriticalErrors && (
              <div className="mb-4 p-3 bg-yellow-50 border border-yellow-200 rounded-md">
                <p className="text-sm text-yellow-800">
                  Some metrics may be incomplete due to data loading issues. 
                  {appointmentsError && ' Appointments data unavailable. '}
                  {followUpsError && ' Follow-ups data unavailable. '}
                  Showing available data only.
                </p>
              </div>
            )}
            
            {/* Controls */}
            <div className="flex flex-col sm:flex-row gap-4 mb-6">
              <div className="flex-1">
                <div className="relative">
                  <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-muted-foreground" />
                  <Input
                    placeholder="Search closers..."
                    value={searchQuery}
                    onChange={(e) => setSearchQuery(e.target.value)}
                    className="pl-10"
                  />
                </div>
              </div>
              {showExport && (
                <Button
                  onClick={handleExport}
                  disabled={isExporting || rankedData.length === 0}
                  variant="outline"
                  className="flex items-center gap-2"
                >
                  <FileDown className="h-4 w-4" />
                  {isExporting ? 'Exporting...' : 'Export CSV'}
                </Button>
              )}
            </div>

            {/* Table */}
            {isLoading ? (
              <CloserPerformanceTableSkeleton />
            ) : rankedData.length === 0 ? (
              <div className="text-center py-8">
                <p className="text-muted-foreground">No closer performance data found</p>
              </div>
            ) : (
              <div className="overflow-x-auto">
                <Table>
                  <TableHeader>
                    <TableRow>
                      <TableHead className="w-16">
                        Rank
                      </TableHead>
                      <TableHead 
                        className="sticky left-0 bg-background cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('closerName')}
                      >
                        <div className="flex items-center gap-2">
                          Name
                          {getSortIcon('closerName')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('office')}
                      >
                        <div className="flex items-center gap-2">
                          Office
                          {getSortIcon('office')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('appointmentsSat')}
                      >
                        <div className="flex items-center gap-2">
                          Appointments Sat
                          {getSortIcon('appointmentsSat')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('salesClosed')}
                      >
                        <div className="flex items-center gap-2">
                          Sales Closed
                          {getSortIcon('salesClosed')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('sitCloseRate')}
                      >
                        <div className="flex items-center gap-2">
                          Sit/Close (%)
                          {getSortIcon('sitCloseRate')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('revenue')}
                      >
                        <div className="flex items-center gap-2">
                          Revenue
                          {getSortIcon('revenue')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('avgDealSize')}
                      >
                        <div className="flex items-center gap-2">
                          Avg Deal Size
                          {getSortIcon('avgDealSize')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('followUps')}
                      >
                        <div className="flex items-center gap-2">
                          Follow-Ups
                          {getSortIcon('followUps')}
                        </div>
                      </TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {rankedData.map((closer) => (
                      <TableRow key={closer.userId}>
                        <TableCell className="w-16 text-center">
                          <Badge variant="secondary" className="w-8 h-6 flex items-center justify-center text-xs">
                            {closer.rank}
                          </Badge>
                        </TableCell>
                        <TableCell className="sticky left-0 bg-background font-medium">
                          <Link 
                            href={`/analytics/rep/${closer.userId}`}
                            className="hover:underline"
                          >
                            {closer.closerName}
                          </Link>
                        </TableCell>
                        <TableCell>{closer.office}</TableCell>
                        <TableCell>{formatLargeNumber(closer.appointmentsSat)}</TableCell>
                        <TableCell>{formatLargeNumber(closer.salesClosed)}</TableCell>
                        <TableCell>
                          <span className={getSitCloseRateColor(closer.sitCloseRate)}>
                            {formatPercentage(closer.sitCloseRate)}
                          </span>
                        </TableCell>
                        <TableCell>{formatCurrency(closer.revenue)}</TableCell>
                        <TableCell>{formatCurrency(closer.avgDealSize)}</TableCell>
                        <TableCell>{formatLargeNumber(closer.followUps)}</TableCell>
                      </TableRow>
                    ))}
                  </TableBody>
                </Table>
              </div>
            )}
          </CardContent>
        </CollapsibleContent>
      </Card>
    </Collapsible>
  );
}

function CloserPerformanceTableSkeleton() {
  return (
    <div className="space-y-4">
      <div className="flex flex-col sm:flex-row gap-4">
        <Skeleton className="h-10 flex-1" />
        <Skeleton className="h-10 w-32" />
      </div>
      <Table>
        <TableHeader>
          <TableRow>
            {Array.from({ length: 8 }).map((_, i) => (
              <TableHead key={i}>
                <Skeleton className="h-4 w-20" />
              </TableHead>
            ))}
          </TableRow>
        </TableHeader>
        <TableBody>
          {Array.from({ length: 10 }).map((_, i) => (
            <TableRow key={i}>
              {Array.from({ length: 8 }).map((_, j) => (
                <TableCell key={j}>
                  <Skeleton className="h-4 w-16" />
                </TableCell>
              ))}
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </div>
  );
}
