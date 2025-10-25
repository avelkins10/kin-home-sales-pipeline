'use client';

import { useState, useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import Link from 'next/link';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { 
  ArrowUp, 
  ArrowDown, 
  ArrowUpDown, 
  FileDown, 
  Search, 
  Target, 
  ChevronDown, 
  ChevronUp 
} from 'lucide-react';
import { formatLargeNumber, formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import type { LeaderboardResponse } from '@/lib/repcard/types';
import { toast } from 'sonner';

interface SetterPerformanceTableProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
  showExport?: boolean;
}

type SortColumn = 'setterName' | 'doorsKnocked' | 'appointmentsSet' | 'setRate' | 'showRate' | 'closeRate' | 'qualityScore' | 'office';
type SortDirection = 'ascending' | 'descending';

interface SetterMetrics {
  userId: string;
  setterName: string;
  setterEmail: string;
  office: string;
  doorsKnocked: number;
  appointmentsSet: number;
  setRate: number; // calculated
  showRate: number; // from appointment_speed metric
  closeRate: number; // calculated from QuickBase
  qualityScore: number;
  rank?: number; // assigned after sorting
}

export function SetterPerformanceTable({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds,
  showExport = true
}: SetterPerformanceTableProps) {
  const [sortColumn, setSortColumn] = useState<SortColumn>('qualityScore');
  const [sortDirection, setSortDirection] = useState<SortDirection>('descending');
  const [searchQuery, setSearchQuery] = useState('');
  const [isExporting, setIsExporting] = useState(false);
  const [isOpen, setIsOpen] = useState(true);

  // Load collapsible state from localStorage
  useEffect(() => {
    const savedState = localStorage.getItem('setter-performance-table-open');
    if (savedState !== null) {
      setIsOpen(savedState === 'true');
    }
  }, []);

  // Save collapsible state to localStorage
  const handleToggle = () => {
    const newState = !isOpen;
    setIsOpen(newState);
    localStorage.setItem('setter-performance-table-open', String(newState));
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
      role: 'setter',
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

  // Fetch doors knocked data
  const { data: doorsKnockedData, isLoading: doorsLoading, error: doorsError } = useQuery({
    queryKey: ['setter-doors-knocked', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams('doors_knocked');
      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch doors knocked data');
      return response.json() as Promise<LeaderboardResponse>;
    },
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  // Fetch appointments set data
  const { data: appointmentsSetData, isLoading: appointmentsLoading, error: appointmentsError } = useQuery({
    queryKey: ['setter-appointments-set', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams('appointments_set');
      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch appointments set data');
      return response.json() as Promise<LeaderboardResponse>;
    },
    staleTime: 15 * 60 * 1000,
  });

  // Fetch quality score data
  const { data: qualityScoreData, isLoading: qualityLoading, error: qualityError } = useQuery({
    queryKey: ['setter-quality-score', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams('quality_score');
      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch quality score data');
      return response.json() as Promise<LeaderboardResponse>;
    },
    staleTime: 15 * 60 * 1000,
  });

  // Fetch appointment speed data (show rate proxy)
  const { data: appointmentSpeedData, isLoading: speedLoading, error: speedError } = useQuery({
    queryKey: ['setter-appointment-speed', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams('appointment_speed');
      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch appointment speed data');
      return response.json() as Promise<LeaderboardResponse>;
    },
    staleTime: 15 * 60 * 1000,
  });

  // Fetch sales closed data
  const { data: salesClosedData, isLoading: salesLoading, error: salesError } = useQuery({
    queryKey: ['setter-sales-closed', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams('sales_closed');
      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch sales closed data');
      return response.json() as Promise<LeaderboardResponse>;
    },
    staleTime: 15 * 60 * 1000,
  });

  // Combine data from all queries - build union of users across all datasets
  const allUserIds = new Set<string>();
  
  // Collect user IDs from all successful queries
  if (doorsKnockedData?.leaderboard) {
    doorsKnockedData.leaderboard.forEach(entry => allUserIds.add(entry.userId));
  }
  if (appointmentsSetData?.leaderboard) {
    appointmentsSetData.leaderboard.forEach(entry => allUserIds.add(entry.userId));
  }
  if (qualityScoreData?.leaderboard) {
    qualityScoreData.leaderboard.forEach(entry => allUserIds.add(entry.userId));
  }
  if (appointmentSpeedData?.leaderboard) {
    appointmentSpeedData.leaderboard.forEach(entry => allUserIds.add(entry.userId));
  }
  if (salesClosedData?.leaderboard) {
    salesClosedData.leaderboard.forEach(entry => allUserIds.add(entry.userId));
  }
  
  // Build combined metrics from union of all users
  const combinedMetrics: SetterMetrics[] = Array.from(allUserIds).map(userId => {
    const doorsEntry = doorsKnockedData?.leaderboard?.find(e => e.userId === userId);
    const appointmentsEntry = appointmentsSetData?.leaderboard?.find(e => e.userId === userId);
    const qualityEntry = qualityScoreData?.leaderboard?.find(e => e.userId === userId);
    const showRateEntry = appointmentSpeedData?.leaderboard?.find(e => e.userId === userId);
    const salesEntry = salesClosedData?.leaderboard?.find(e => e.userId === userId);
    
    // Use the first available entry for user info, fallback to defaults
    const baseEntry = doorsEntry || appointmentsEntry || qualityEntry || showRateEntry || salesEntry;
    
    const doorsKnocked = doorsEntry?.metricValue || 0;
    const appointmentsSet = appointmentsEntry?.metricValue || 0;
    const salesClosed = salesEntry?.metricValue || 0;
    
    return {
      userId,
      setterName: baseEntry?.userName || baseEntry?.setterName || 'Unknown',
      setterEmail: baseEntry?.userEmail || baseEntry?.setterEmail || '',
      office: baseEntry?.office || 'Unknown',
      doorsKnocked,
      appointmentsSet,
      setRate: doorsKnocked > 0 ? (appointmentsSet / doorsKnocked) * 100 : 0,
      showRate: showRateEntry?.metricValue || 0,
      closeRate: appointmentsSet > 0 ? (salesClosed / appointmentsSet) * 100 : 0,
      qualityScore: qualityEntry?.metricValue || 0
    };
  });

  // Apply search filter
  const filteredData = combinedMetrics.filter(setter => 
    setter.setterName.toLowerCase().includes(searchQuery.toLowerCase())
  );

  // Apply sorting
  const sortedData = [...filteredData].sort((a, b) => {
    let aValue: number | string;
    let bValue: number | string;

    switch (sortColumn) {
      case 'setterName':
        aValue = a.setterName;
        bValue = b.setterName;
        break;
      case 'office':
        aValue = a.office;
        bValue = b.office;
        break;
      case 'doorsKnocked':
        aValue = a.doorsKnocked;
        bValue = b.doorsKnocked;
        break;
      case 'appointmentsSet':
        aValue = a.appointmentsSet;
        bValue = b.appointmentsSet;
        break;
      case 'setRate':
        aValue = a.setRate;
        bValue = b.setRate;
        break;
      case 'showRate':
        aValue = a.showRate;
        bValue = b.showRate;
        break;
      case 'closeRate':
        aValue = a.closeRate;
        bValue = b.closeRate;
        break;
      case 'qualityScore':
        aValue = a.qualityScore;
        bValue = b.qualityScore;
        break;
      default:
        aValue = a.qualityScore;
        bValue = b.qualityScore;
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
  const rankedData = sortedData.map((setter, index) => ({
    ...setter,
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
      const exportData = rankedData.map(setter => ({
        rank: setter.rank || 0,
        setterName: setter.setterName,
        office: setter.office,
        doorsKnocked: setter.doorsKnocked,
        appointmentsSet: setter.appointmentsSet,
        setRate: setter.setRate.toFixed(1),
        showRate: setter.showRate.toFixed(1),
        closeRate: setter.closeRate.toFixed(1),
        qualityScore: setter.qualityScore.toFixed(1)
      }));
      
      const headers = {
        rank: 'Rank',
        setterName: 'Setter Name',
        office: 'Office',
        doorsKnocked: 'Doors Knocked',
        appointmentsSet: 'Appointments Set',
        setRate: 'Set Rate (%)',
        showRate: 'Show Rate (%)',
        closeRate: 'Close Rate (%)',
        qualityScore: 'Quality Score'
      };
      
      const filename = `setter-performance-${new Date().toISOString().split('T')[0]}.csv`;
      exportAnalyticsToCSV(exportData, filename, headers);
      toast.success('Setter performance data exported successfully');
    } catch (error) {
      toast.error('Failed to export setter performance data');
    } finally {
      setIsExporting(false);
    }
  };

  // Get color class for metrics
  const getMetricColor = (value: number, thresholds: { green: number; yellow: number }) => {
    if (value >= thresholds.green) return 'text-green-600';
    if (value >= thresholds.yellow) return 'text-yellow-600';
    return 'text-red-600';
  };

  // Loading state
  const isLoading = doorsLoading || appointmentsLoading || qualityLoading || speedLoading || salesLoading;
  
  // Soft error handling - only fail if critical queries fail
  const criticalErrors = doorsError || appointmentsError; // Base metrics are critical
  const nonCriticalErrors = qualityError || speedError || salesError; // These can be zero
  
  // Show warnings for non-critical errors
  const hasNonCriticalErrors = qualityError || speedError || salesError;
  
  if (criticalErrors) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Target className="h-5 w-5" />
            Setter Performance
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
                <Target className="h-5 w-5" />
                Setter Performance
                <Badge variant="secondary" className="ml-2">
                  {rankedData.length} setters
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
                  {qualityError && ' Quality score data unavailable. '}
                  {speedError && ' Show rate data unavailable. '}
                  {salesError && ' Close rate data unavailable. '}
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
                    placeholder="Search setters..."
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
              <SetterPerformanceTableSkeleton />
            ) : rankedData.length === 0 ? (
              <div className="text-center py-8">
                <p className="text-muted-foreground">No setter performance data found</p>
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
                        onClick={() => handleSort('setterName')}
                      >
                        <div className="flex items-center gap-2">
                          Name
                          {getSortIcon('setterName')}
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
                        onClick={() => handleSort('doorsKnocked')}
                      >
                        <div className="flex items-center gap-2">
                          Doors Knocked
                          {getSortIcon('doorsKnocked')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('appointmentsSet')}
                      >
                        <div className="flex items-center gap-2">
                          Appointments Set
                          {getSortIcon('appointmentsSet')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('setRate')}
                      >
                        <div className="flex items-center gap-2">
                          Set Rate (%)
                          {getSortIcon('setRate')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('showRate')}
                      >
                        <div className="flex items-center gap-2">
                          Show Rate (%)
                          {getSortIcon('showRate')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('closeRate')}
                      >
                        <div className="flex items-center gap-2">
                          Close Rate (%)
                          {getSortIcon('closeRate')}
                        </div>
                      </TableHead>
                      <TableHead 
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => handleSort('qualityScore')}
                      >
                        <div className="flex items-center gap-2">
                          Quality Score
                          {getSortIcon('qualityScore')}
                        </div>
                      </TableHead>
                    </TableRow>
                  </TableHeader>
                  <TableBody>
                    {rankedData.map((setter) => (
                      <TableRow key={setter.userId}>
                        <TableCell className="w-16 text-center">
                          <Badge variant="secondary" className="w-8 h-6 flex items-center justify-center text-xs">
                            {setter.rank}
                          </Badge>
                        </TableCell>
                        <TableCell className="sticky left-0 bg-background font-medium">
                          <Link 
                            href={`/analytics/rep/${setter.userId}`}
                            className="hover:underline"
                          >
                            {setter.setterName}
                          </Link>
                        </TableCell>
                        <TableCell>{setter.office}</TableCell>
                        <TableCell>{formatLargeNumber(setter.doorsKnocked)}</TableCell>
                        <TableCell>{formatLargeNumber(setter.appointmentsSet)}</TableCell>
                        <TableCell>
                          <span className={getMetricColor(setter.setRate, { green: 20, yellow: 10 })}>
                            {formatPercentage(setter.setRate)}
                          </span>
                        </TableCell>
                        <TableCell>
                          <span className={getMetricColor(setter.showRate, { green: 80, yellow: 60 })}>
                            {formatPercentage(setter.showRate)}
                          </span>
                        </TableCell>
                        <TableCell>
                          <span className={getMetricColor(setter.closeRate, { green: 30, yellow: 20 })}>
                            {formatPercentage(setter.closeRate)}
                          </span>
                        </TableCell>
                        <TableCell>
                          <span className={getMetricColor(setter.qualityScore, { green: 80, yellow: 60 })}>
                            {formatPercentage(setter.qualityScore)}
                          </span>
                        </TableCell>
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

function SetterPerformanceTableSkeleton() {
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
