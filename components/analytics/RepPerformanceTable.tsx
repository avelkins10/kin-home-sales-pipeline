'use client';

import { useQuery } from '@tanstack/react-query';
import { useState } from 'react';
import Link from 'next/link';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { ArrowUp, ArrowDown, ArrowUpDown, FileDown, Search, Users } from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import type { RepPerformance } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';

interface RepPerformanceTableProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
  showExport?: boolean;
}

type SortColumn = 'totalProjects' | 'avgSystemSize' | 'avgNetPpw' | 'avgCycleTime' | 'firstTimePassRate' | 'cancellationRate' | 'holdRate' | 'repName' | 'role' | 'officeName';
type SortDirection = 'ascending' | 'descending';

// Loading skeleton component
function RepPerformanceTableSkeleton() {
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
        <div className="space-y-4 mb-4">
          <div className="flex flex-col md:flex-row gap-4">
            <Skeleton className="h-10 w-32" />
            <Skeleton className="h-10 w-64" />
          </div>
        </div>
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
              {Array.from({ length: 10 }).map((_, i) => (
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

export function RepPerformanceTable({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds,
  showExport = true
}: RepPerformanceTableProps) {
  const [sortColumn, setSortColumn] = useState<SortColumn>('totalProjects');
  const [sortDirection, setSortDirection] = useState<SortDirection>('descending');
  const [filterRole, setFilterRole] = useState<'all' | 'closer' | 'setter'>('all');
  const [searchQuery, setSearchQuery] = useState('');
  const [isExporting, setIsExporting] = useState(false);

  const { data, isLoading, error } = useQuery<RepPerformance[]>({
    queryKey: ['rep-performance', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/rep-performance?timeRange=${timeRange}`;
      
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch rep performance data');
      const result = await response.json();
      return result.metrics || [];
    },
  });

  const handleSort = (column: SortColumn) => {
    if (sortColumn === column) {
      setSortDirection(sortDirection === 'ascending' ? 'descending' : 'ascending');
    } else {
      setSortColumn(column);
      setSortDirection('descending');
    }
  };

  const getSortIcon = (column: SortColumn) => {
    if (sortColumn !== column) {
      return <ArrowUpDown className="h-4 w-4 text-gray-400" />;
    }
    return sortDirection === 'ascending' 
      ? <ArrowUp className="h-4 w-4 text-blue-600" />
      : <ArrowDown className="h-4 w-4 text-blue-600" />;
  };

  const handleExport = async () => {
    if (!rankedData || rankedData.length === 0) return;
    
    setIsExporting(true);
    try {
      // Prepare data for export using the currently displayed, ranked list
      const exportData = rankedData.map(rep => ({
        rank: rep.rank || 0,
        repName: rep.repName,
        role: rep.role,
        officeName: rep.officeName,
        totalProjects: rep.totalProjects,
        avgSystemSize: rep.avgSystemSize,
        avgNetPpw: rep.avgNetPpw,
        avgCommissionablePpw: rep.avgCommissionablePpw,
        avgCycleTime: rep.avgCycleTime || 'N/A',
        firstTimePassRate: rep.firstTimePassRate,
        rejectionRate: rep.rejectionRate,
        cancellationRate: rep.cancellationRate,
        holdRate: rep.holdRate,
        activeProjects: rep.activeProjects,
        cancelledProjects: rep.cancelledProjects,
        onHoldProjects: rep.onHoldProjects,
        installs: rep.installs
      }));
      
      // Define headers
      const headers = {
        rank: 'Rank',
        repName: 'Name',
        role: 'Role',
        officeName: 'Office',
        totalProjects: 'Total Projects',
        avgSystemSize: 'Avg System Size (kW)',
        avgNetPpw: 'Avg Net PPW',
        avgCommissionablePpw: 'Avg Commissionable PPW',
        avgCycleTime: 'Avg Cycle Time (days)',
        firstTimePassRate: 'First-Time Pass Rate (%)',
        rejectionRate: 'Rejection Rate (%)',
        cancellationRate: 'Cancellation Rate (%)',
        holdRate: 'Hold Rate (%)',
        activeProjects: 'Active',
        cancelledProjects: 'Cancelled',
        onHoldProjects: 'On Hold',
        installs: 'Installs'
      };
      
      // Generate filename with timestamp
      const filename = `rep-performance-${new Date().toISOString().split('T')[0]}.csv`;
      
      // Export to CSV
      exportAnalyticsToCSV(exportData, filename, headers);
      
      toast.success('Rep performance data exported successfully');
    } catch (error) {
      toast.error('Failed to export rep performance data');
      console.error('Export error:', error);
    } finally {
      setIsExporting(false);
    }
  };

  if (isLoading) {
    return <RepPerformanceTableSkeleton />;
  }

  if (error) {
    return (
      <Card>
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load rep performance data</p>
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
            <p className="text-slate-600">No reps found for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Filter and sort data
  let filteredData = data.filter(rep => {
    // Role filter
    if (filterRole !== 'all' && rep.role.toLowerCase() !== filterRole) {
      return false;
    }
    
    // Search filter
    if (searchQuery && !rep.repName.toLowerCase().includes(searchQuery.toLowerCase())) {
      return false;
    }
    
    return true;
  });

  // Sort data
  const sortedData = [...filteredData].sort((a, b) => {
    let aValue: any = a[sortColumn];
    let bValue: any = b[sortColumn];
    
    // Handle null values
    if (aValue === null || aValue === undefined) aValue = 0;
    if (bValue === null || bValue === undefined) bValue = 0;
    
    // Handle string comparison
    if (typeof aValue === 'string' && typeof bValue === 'string') {
      return sortDirection === 'ascending' 
        ? aValue.localeCompare(bValue)
        : bValue.localeCompare(aValue);
    }
    
    // Handle numeric comparison
    const comparison = aValue - bValue;
    return sortDirection === 'ascending' ? comparison : -comparison;
  });

  // Calculate ranks based on current sort
  const rankedData = sortedData.map((rep, index) => ({
    ...rep,
    rank: index + 1
  }));

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <Users className="h-6 w-6 text-blue-600" />
            <CardTitle>Rep Performance</CardTitle>
          </div>
          {showExport && (
            <Button
              variant="outline"
              size="sm"
              onClick={handleExport}
              disabled={isExporting}
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
        <p className="text-sm text-gray-600">
          {rankedData.length} rep{rankedData.length !== 1 ? 's' : ''} • Sorted by {sortColumn} ({sortDirection})
        </p>
      </CardHeader>
      <CardContent>
        {/* Filters */}
        <div className="flex flex-col md:flex-row gap-4 mb-4">
          <Select value={filterRole} onValueChange={(value: 'all' | 'closer' | 'setter') => setFilterRole(value)}>
            <SelectTrigger className="w-full md:w-32">
              <SelectValue placeholder="All Roles" />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="all">All Roles</SelectItem>
              <SelectItem value="closer">Closers</SelectItem>
              <SelectItem value="setter">Setters</SelectItem>
            </SelectContent>
          </Select>
          
          <div className="relative flex-1">
            <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
            <Input
              placeholder="Search by rep name..."
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              className="pl-10"
            />
          </div>
        </div>

        <div className="overflow-x-auto">
          <Table className="min-w-max" aria-label="Rep performance table">
            <TableHeader>
              <TableRow>
                <TableHead className="w-16">Rank</TableHead>
                <TableHead 
                  className="min-w-40 sticky left-0 bg-white cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('repName')}
                  aria-sort={sortColumn === 'repName' ? sortDirection : 'none'}
                >
                  <div className="flex items-center space-x-1">
                    <span>Name</span>
                    {getSortIcon('repName')}
                  </div>
                </TableHead>
                <TableHead 
                  className="w-24 cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('role')}
                  aria-sort={sortColumn === 'role' ? sortDirection : 'none'}
                >
                  <div className="flex items-center space-x-1">
                    <span>Role</span>
                    {getSortIcon('role')}
                  </div>
                </TableHead>
                <TableHead 
                  className="min-w-32 cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('officeName')}
                  aria-sort={sortColumn === 'officeName' ? sortDirection : 'none'}
                >
                  <div className="flex items-center space-x-1">
                    <span>Office</span>
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
                  onClick={() => handleSort('firstTimePassRate')}
                  aria-sort={sortColumn === 'firstTimePassRate' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>First-Time Pass %</span>
                    {getSortIcon('firstTimePassRate')}
                  </div>
                </TableHead>
                <TableHead
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('cancellationRate')}
                  aria-sort={sortColumn === 'cancellationRate' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Cancellation Rate</span>
                    {getSortIcon('cancellationRate')}
                  </div>
                </TableHead>
                <TableHead
                  className="text-right cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('holdRate')}
                  aria-sort={sortColumn === 'holdRate' ? sortDirection : 'none'}
                >
                  <div className="flex items-center justify-end space-x-1">
                    <span>Hold Rate</span>
                    {getSortIcon('holdRate')}
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
              </TableRow>
            </TableHeader>
            <TableBody>
              {rankedData.map((rep, index) => {
                const isTopPerformer = index === 0;
                const isBottomPerformer = index === rankedData.length - 1;
                
                return (
                  <TableRow 
                    key={rep.repId}
                    className={`
                      ${isTopPerformer ? 'bg-green-50 border-l-4 border-green-500' : ''}
                      ${isBottomPerformer ? 'bg-red-50 border-l-4 border-red-500' : ''}
                      ${index % 2 === 0 ? 'bg-slate-50' : ''}
                      hover:bg-slate-100 transition-colors cursor-pointer
                    `}
                    onClick={() => {
                      // Navigate to rep detail page
                      window.location.href = `/analytics/rep/${rep.repId || rep.repEmail}`;
                    }}
                  >
                    <TableCell className="font-medium">
                      <span className="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-700 rounded-full text-xs font-semibold">
                        {rep.rank}
                      </span>
                    </TableCell>
                    <TableCell className="sticky left-0 bg-white font-medium">
                      <Link
                        href={`/analytics/rep/${rep.repId || rep.repEmail}`}
                        className="text-blue-600 hover:text-blue-800 hover:underline"
                      >
                        {rep.repName}
                      </Link>
                    </TableCell>
                    <TableCell>
                      <Badge variant={rep.role.toLowerCase() === 'closer' ? 'default' : 'secondary'}>
                        {rep.role}
                      </Badge>
                    </TableCell>
                    <TableCell>{rep.officeName}</TableCell>
                    <TableCell className="text-right">
                      {rep.totalProjects.toLocaleString()}
                    </TableCell>
                    <TableCell className="text-right">
                      {formatSystemSize(rep.avgSystemSize)}
                    </TableCell>
                    <TableCell className="text-right">
                      {formatPPW(rep.avgNetPpw)}
                    </TableCell>
                    <TableCell className="text-right">
                      <span className={`
                        ${(rep.firstTimePassRate ?? 0) >= 90 ? 'text-green-600 font-semibold' : ''}
                        ${(rep.firstTimePassRate ?? 0) >= 75 && (rep.firstTimePassRate ?? 0) < 90 ? 'text-yellow-600' : ''}
                        ${(rep.firstTimePassRate ?? 0) < 75 ? 'text-red-600 font-semibold' : ''}
                      `}>
                        {rep.firstTimePassRate !== undefined ? formatPercentage(rep.firstTimePassRate) : 'N/A'}
                      </span>
                    </TableCell>
                    <TableCell className="text-right">
                      <span className={`
                        ${(rep.cancellationRate ?? 0) > 15 ? 'text-red-600 font-semibold' : ''}
                        ${(rep.cancellationRate ?? 100) < 5 ? 'text-green-600 font-semibold' : ''}
                      `}>
                        {rep.cancellationRate !== undefined ? formatPercentage(rep.cancellationRate) : 'N/A'}
                      </span>
                    </TableCell>
                    <TableCell className="text-right">
                      <span className={`
                        ${(rep.holdRate ?? 0) > 20 ? 'text-red-600 font-semibold' : ''}
                        ${(rep.holdRate ?? 100) < 10 ? 'text-green-600 font-semibold' : ''}
                      `}>
                        {rep.holdRate !== undefined ? formatPercentage(rep.holdRate) : 'N/A'}
                      </span>
                    </TableCell>
                    <TableCell className="text-right">
                      {rep.avgCycleTime ? `${Math.round(rep.avgCycleTime)} days` : 'N/A'}
                    </TableCell>
                  </TableRow>
                );
              })}
            </TableBody>
          </Table>
        </div>
        
        {filteredData.length === 0 && (filterRole !== 'all' || searchQuery) && (
          <div className="mt-4 text-center">
            <p className="text-slate-600">No reps found matching your criteria</p>
            <p className="text-sm text-slate-500 mt-1">Try adjusting your filters</p>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
