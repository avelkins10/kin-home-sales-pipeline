'use client';

import { useQuery } from '@tanstack/react-query';
import { useState, useMemo } from 'react';
import Link from 'next/link';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { LineChart, Line, ResponsiveContainer } from 'recharts';
import { ArrowUp, ArrowDown, ArrowUpDown, Building2, FileDown, Loader2 } from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import type { OfficeMetrics, OfficeComparisonRow, SortColumn, SortDirection, OfficeSparklineData } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';
import { format } from 'date-fns';

interface OfficeComparisonTableProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
  maxOffices?: number;
}

// Mock sparkline data generator (fallback for when real data is not available)
function generateMockSparklineData(): OfficeSparklineData[] {
  const months = [];
  const now = new Date();
  for (let i = 5; i >= 0; i--) {
    const date = new Date(now.getFullYear(), now.getMonth() - i, 1);
    months.push({
      month: date.toISOString().slice(0, 7), // YYYY-MM format
      installs: Math.floor(Math.random() * 20) + 5 // Random installs between 5-25
    });
  }
  return months;
}

// Sparkline component for trend visualization
function OfficeSparkline({ data }: { data: OfficeSparklineData[] }) {
  return (
    <div className="w-24 h-12">
      <ResponsiveContainer width="100%" height="100%">
        <LineChart data={data}>
          <Line 
            type="monotone" 
            dataKey="installs" 
            stroke="#3b82f6" 
            strokeWidth={2}
            dot={false}
          />
        </LineChart>
      </ResponsiveContainer>
    </div>
  );
}

// Helper to determine if a metric is 'lower is better'
function isLowerBetterMetric(metric: keyof OfficeMetrics): boolean {
  const lowerBetterMetrics: (keyof OfficeMetrics)[] = ['avgCycleTime', 'cancelledProjects', 'onHoldProjects'];
  return lowerBetterMetrics.includes(metric);
}

// Calculate outlier thresholds for highlighting
function calculateOutliers(data: OfficeMetrics[], metric: keyof OfficeMetrics): { high: number; low: number } {
  const values = data.map(office => {
    const value = office[metric];
    return typeof value === 'number' ? value : 0;
  }).filter(v => v > 0);
  
  if (values.length === 0) return { high: 0, low: 0 };
  
  values.sort((a, b) => a - b);
  const highIndex = Math.floor(values.length * 0.8);
  const lowIndex = Math.floor(values.length * 0.2);
  
  return {
    high: values[highIndex] || values[values.length - 1],
    low: values[lowIndex] || values[0]
  };
}

// Sort data based on column and direction
function sortData(data: OfficeMetrics[], sortColumn: SortColumn, sortDirection: SortDirection): OfficeMetrics[] {
  return [...data].sort((a, b) => {
    let aValue: any = a[sortColumn];
    let bValue: any = b[sortColumn];
    
    // Handle null values
    if (aValue === null || aValue === undefined) aValue = 0;
    if (bValue === null || bValue === undefined) bValue = 0;
    
    // Handle string comparison for officeName
    if (typeof aValue === 'string' && typeof bValue === 'string') {
      return sortDirection === 'asc' 
        ? aValue.localeCompare(bValue)
        : bValue.localeCompare(aValue);
    }
    
    // Handle numeric comparison
    const comparison = aValue - bValue;
    return sortDirection === 'asc' ? comparison : -comparison;
  });
}

// Transform OfficeMetrics to OfficeComparisonRow with additional properties
function transformToComparisonRows(data: OfficeMetrics[], sortColumn: SortColumn, sortDirection: SortDirection): OfficeComparisonRow[] {
  const sortedData = sortData(data, sortColumn, sortDirection);
  const outliers = calculateOutliers(data, sortColumn);
  
  return sortedData.map((office, index) => {
    const value = office[sortColumn];
    const isLowerBetter = isLowerBetterMetric(sortColumn);
    
    // For 'lower is better' metrics, invert the outlier logic
    let isOutlierHigh: boolean;
    let isOutlierLow: boolean;
    
    if (typeof value === 'number') {
      if (isLowerBetter) {
        // For lower-is-better metrics: lower values are good (green), higher values are bad (red)
        isOutlierHigh = value <= outliers.low;  // Low values are highlighted as good
        isOutlierLow = value >= outliers.high;  // High values are highlighted as bad
      } else {
        // For higher-is-better metrics: higher values are good (green), lower values are bad (red)
        isOutlierHigh = value >= outliers.high; // High values are highlighted as good
        isOutlierLow = value <= outliers.low;   // Low values are highlighted as bad
      }
    } else {
      isOutlierHigh = false;
      isOutlierLow = false;
    }
    
    return {
      ...office,
      rank: index + 1,
      sparklineData: office.monthlyInstalls || generateMockSparklineData(),
      isOutlierHigh,
      isOutlierLow
    };
  });
}

// Loading skeleton component
function OfficeComparisonTableSkeleton() {
  return (
    <Card>
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-48" />
        </div>
        <Skeleton className="h-4 w-64" />
      </CardHeader>
      <CardContent>
        <div className="overflow-x-auto">
          <Table>
            <TableHeader>
              <TableRow>
                {Array.from({ length: 10 }).map((_, i) => (
                  <TableHead key={i}>
                    <Skeleton className="h-4 w-20" />
                  </TableHead>
                ))}
              </TableRow>
            </TableHeader>
            <TableBody>
              {Array.from({ length: 5 }).map((_, i) => (
                <TableRow key={i}>
                  {Array.from({ length: 10 }).map((_, j) => (
                    <TableCell key={j}>
                      <Skeleton className="h-4 w-16" />
                    </TableCell>
                  ))}
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </div>
      </CardContent>
    </Card>
  );
}

export function OfficeComparisonTable({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds,
  maxOffices = 5
}: OfficeComparisonTableProps) {
  const [sortColumn, setSortColumn] = useState<SortColumn>('totalProjects');
  const [sortDirection, setSortDirection] = useState<SortDirection>('desc');
  const [showAll, setShowAll] = useState(false);
  const [isExporting, setIsExporting] = useState(false);

  const { data, isLoading, error } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-comparison', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/office-metrics?timeRange=${timeRange}`;

      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }

      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }

      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch office comparison data');
      const result = await response.json();
      return result.metrics || [];
    },
  });

  // Calculate comparison rows - always call hooks unconditionally
  const comparisonRows = useMemo(() =>
    data ? transformToComparisonRows(data, sortColumn, sortDirection) : [],
    [data, sortColumn, sortDirection]
  );
  const sortedData = useMemo(() =>
    data ? sortData(data, sortColumn, sortDirection) : [],
    [data, sortColumn, sortDirection]
  );
  const displayRows = showAll ? comparisonRows : comparisonRows.slice(0, maxOffices);

  const handleSort = (column: SortColumn) => {
    if (sortColumn === column) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortColumn(column);
      setSortDirection('desc');
    }
  };

  const handleExport = async () => {
    if (!data) return;

    setIsExporting(true);
    try {
      // Prepare data for export
      const exportData = comparisonRows.map(office => ({
        rank: office.rank,
        officeName: office.officeName,
        totalProjects: office.totalProjects,
        avgSystemSize: office.avgSystemSize,
        avgGrossPpw: office.avgGrossPpw,
        avgNetPpw: office.avgNetPpw,
        avgCommissionablePpw: office.avgCommissionablePpw,
        avgCycleTime: office.avgCycleTime || 'N/A',
        activeProjects: office.activeProjects,
        cancelledProjects: office.cancelledProjects,
        onHoldProjects: office.onHoldProjects,
        installs: office.installs
      }));
      
      // Define headers
      const headers = {
        rank: 'Rank',
        officeName: 'Office Name',
        totalProjects: 'Total Projects',
        avgSystemSize: 'Avg System Size (kW)',
        avgGrossPpw: 'Avg Gross PPW',
        avgNetPpw: 'Avg Net PPW',
        avgCommissionablePpw: 'Avg Commissionable PPW',
        avgCycleTime: 'Avg Cycle Time (days)',
        activeProjects: 'Active',
        cancelledProjects: 'Cancelled',
        onHoldProjects: 'On Hold',
        installs: 'Installs'
      };
      
      // Generate filename with timestamp
      const filename = `office-comparison-${format(new Date(), 'yyyy-MM-dd')}.csv`;
      
      // Export to CSV
      exportAnalyticsToCSV(exportData, filename, headers);
      
      toast.success('Office comparison data exported successfully');
    } catch (error) {
      toast.error('Failed to export office comparison data');
      console.error('Export error:', error);
    } finally {
      setIsExporting(false);
    }
  };

  const getSortIcon = (column: SortColumn) => {
    if (sortColumn !== column) {
      return <ArrowUpDown className="h-4 w-4 text-gray-400" />;
    }
    return sortDirection === 'asc' 
      ? <ArrowUp className="h-4 w-4 text-blue-600" />
      : <ArrowDown className="h-4 w-4 text-blue-600" />;
  };

  if (isLoading) {
    return <OfficeComparisonTableSkeleton />;
  }

  if (error) {
    return (
      <Card>
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load office comparison data</p>
            <p className="text-red-500 text-sm mt-1">{error.message}</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!data || data.length === 0) {
    return (
      <Card>
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No offices found for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <Building2 className="h-6 w-6 text-blue-600" />
            <CardTitle>Office Comparison</CardTitle>
          </div>
          <Button
            variant="outline"
            size="sm"
            onClick={handleExport}
            disabled={isExporting || isLoading}
          >
            {isExporting ? (
              <>
                <Loader2 className="h-4 w-4 mr-2 animate-spin" />
                Exporting...
              </>
            ) : (
              <>
                <FileDown className="h-4 w-4 mr-2" />
                Export CSV
              </>
            )}
          </Button>
        </div>
        <p className="text-sm text-gray-600">
          {data.length} office{data.length !== 1 ? 's' : ''} • Sorted by {sortColumn} ({sortDirection})
        </p>
      </CardHeader>
      <CardContent>
        <div className="overflow-x-auto">
          <Table className="min-w-max" aria-label="Office comparison table">
            <TableHeader>
              <TableRow>
                <TableHead className="w-16">
                  <div className="flex items-center space-x-1">
                    <span>Rank</span>
                  </div>
                </TableHead>
                <TableHead 
                  className="min-w-40 sticky left-0 cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('officeName')}
                  aria-sort={sortColumn === 'officeName' ? sortDirection : 'none'}
                >
                  <div className="flex items-center space-x-1">
                    <span>Office Name</span>
                    {getSortIcon('officeName')}
                  </div>
                </TableHead>
                <TableHead 
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('totalProjects')}
                  aria-sort={sortColumn === 'totalProjects' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Total Projects</span>
                    {getSortIcon('totalProjects')}
                  </div>
                </TableHead>
                <TableHead 
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('avgSystemSize')}
                  aria-sort={sortColumn === 'avgSystemSize' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Avg Size</span>
                    {getSortIcon('avgSystemSize')}
                  </div>
                </TableHead>
                <TableHead 
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('avgNetPpw')}
                  aria-sort={sortColumn === 'avgNetPpw' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Avg PPW</span>
                    {getSortIcon('avgNetPpw')}
                  </div>
                </TableHead>
                <TableHead 
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('avgCycleTime')}
                  aria-sort={sortColumn === 'avgCycleTime' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Avg Cycle Time</span>
                    {getSortIcon('avgCycleTime')}
                  </div>
                </TableHead>
                <TableHead 
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('activeProjects')}
                  aria-sort={sortColumn === 'activeProjects' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Active</span>
                    {getSortIcon('activeProjects')}
                  </div>
                </TableHead>
                <TableHead 
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('cancelledProjects')}
                  aria-sort={sortColumn === 'cancelledProjects' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Cancelled</span>
                    {getSortIcon('cancelledProjects')}
                  </div>
                </TableHead>
                <TableHead 
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('onHoldProjects')}
                  aria-sort={sortColumn === 'onHoldProjects' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Holds</span>
                    {getSortIcon('onHoldProjects')}
                  </div>
                </TableHead>
                <TableHead className="w-24 text-center">
                  <span>Trend</span>
                </TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {displayRows.map((office) => (
                <TableRow 
                  key={office.officeId}
                  className={`
                    ${office.isOutlierHigh ? 'bg-green-50 border-l-4 border-green-500' : ''}
                    ${office.isOutlierLow ? 'bg-red-50 border-l-4 border-red-500' : ''}
                    hover:bg-slate-100
                  `}
                >
                  <TableCell className="font-medium">
                    <span className="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-700 rounded-full text-xs font-semibold">
                      {office.rank}
                    </span>
                  </TableCell>
                  <TableCell className="sticky left-0 font-medium">
                    <Link 
                      href={`/analytics/office/${office.officeId}`}
                      className="text-blue-600 hover:underline"
                    >
                      {office.officeName}
                    </Link>
                  </TableCell>
                  <TableCell className="text-right">
                    {office.totalProjects.toLocaleString()}
                  </TableCell>
                  <TableCell className="text-right">
                    {formatSystemSize(office.avgSystemSize)}
                  </TableCell>
                  <TableCell className="text-right">
                    {formatPPW(office.avgNetPpw)}
                  </TableCell>
                  <TableCell className="text-right">
                    {office.avgCycleTime ? `${Math.round(office.avgCycleTime)} days` : 'N/A'}
                  </TableCell>
                  <TableCell className="text-right">
                    {office.activeProjects.toLocaleString()}
                  </TableCell>
                  <TableCell className="text-right">
                    {office.cancelledProjects.toLocaleString()}
                  </TableCell>
                  <TableCell className="text-right">
                    {office.onHoldProjects.toLocaleString()}
                  </TableCell>
                  <TableCell className="text-center">
                    <OfficeSparkline data={office.sparklineData} />
                  </TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </div>
        
        {data.length > maxOffices && (
          <div className="mt-4 flex justify-center">
            <Button
              variant="outline"
              onClick={() => setShowAll(!showAll)}
              className="border-slate-300 hover:bg-slate-50"
            >
              {showAll ? 'Show Top 5' : `Show All ${data.length} Offices`}
            </Button>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
