'use client';

import { useState } from 'react';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { ArrowUp, ArrowDown, ArrowUpDown, Info } from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import type { RepPerformance } from '@/lib/types/analytics';

interface OfficeRepTableProps {
  reps: RepPerformance[];
  isLoading?: boolean;
  officeId: number;
}

// Sort data based on column and direction
function sortReps(data: RepPerformance[], sortColumn: string, sortDirection: 'asc' | 'desc'): RepPerformance[] {
  return [...data].sort((a, b) => {
    let aValue: any;
    let bValue: any;
    
    // Handle derived metrics
    if (sortColumn === 'cancellationRate') {
      aValue = calculateCancellationRate(a);
      bValue = calculateCancellationRate(b);
    } else if (sortColumn === 'holdRate') {
      aValue = calculateHoldRate(a);
      bValue = calculateHoldRate(b);
    } else {
      aValue = a[sortColumn as keyof RepPerformance];
      bValue = b[sortColumn as keyof RepPerformance];
    }
    
    // Handle null values
    if (aValue === null || aValue === undefined) aValue = 0;
    if (bValue === null || bValue === undefined) bValue = 0;
    
    // Handle string comparison for repName
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

// Calculate cancellation rate
function calculateCancellationRate(rep: RepPerformance): number {
  if (rep.totalProjects === 0) return 0;
  return (rep.cancelledProjects / rep.totalProjects) * 100;
}

// Calculate hold rate
function calculateHoldRate(rep: RepPerformance): number {
  if (rep.totalProjects === 0) return 0;
  return (rep.onHoldProjects / rep.totalProjects) * 100;
}

// Loading skeleton component
function RepTableSkeleton() {
  return (
    <div className="overflow-x-auto">
      <Table>
        <TableHeader>
          <TableRow>
            {Array.from({ length: 9 }).map((_, i) => (
              <TableHead key={i}>
                <Skeleton className="h-4 w-20" />
              </TableHead>
            ))}
          </TableRow>
        </TableHeader>
        <TableBody>
          {Array.from({ length: 5 }).map((_, i) => (
            <TableRow key={i}>
              {Array.from({ length: 9 }).map((_, j) => (
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

export function OfficeRepTable({ reps, isLoading = false, officeId }: OfficeRepTableProps) {
  const [sortColumn, setSortColumn] = useState<string>('totalProjects');
  const [sortDirection, setSortDirection] = useState<'asc' | 'desc'>('desc');

  const handleSort = (column: string) => {
    if (sortColumn === column) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortColumn(column);
      setSortDirection('desc');
    }
  };

  const getSortIcon = (column: string) => {
    if (sortColumn !== column) {
      return <ArrowUpDown className="h-4 w-4 text-gray-400" />;
    }
    return sortDirection === 'asc' 
      ? <ArrowUp className="h-4 w-4 text-blue-600" />
      : <ArrowDown className="h-4 w-4 text-blue-600" />;
  };

  if (isLoading) {
    return <RepTableSkeleton />;
  }

  if (!reps || reps.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center py-12">
        <div className="bg-slate-100 rounded-full p-4 mb-4">
          <Info className="h-8 w-8 text-slate-400" />
        </div>
        <h3 className="text-lg font-semibold text-slate-900 mb-2">No Team Members Found</h3>
        <p className="text-sm text-slate-600 text-center max-w-md">
          No team members found for this office. This might be because there are no active reps assigned to this office.
        </p>
      </div>
    );
  }

  const sortedReps = sortReps(reps, sortColumn, sortDirection);

  return (
    <div className="overflow-x-auto">
      <Table className="min-w-max" aria-label="Team performance table">
        <TableHeader>
          <TableRow>
            <TableHead className="w-16">
              <div className="flex items-center space-x-1">
                <span>Rank</span>
              </div>
            </TableHead>
            <TableHead 
              className="min-w-40 sticky left-0 cursor-pointer hover:bg-gray-50"
              onClick={() => handleSort('repName')}
              aria-sort={sortColumn === 'repName' ? sortDirection : 'none'}
            >
              <div className="flex items-center space-x-1">
                <span>Name</span>
                {getSortIcon('repName')}
              </div>
            </TableHead>
            <TableHead 
              className="cursor-pointer hover:bg-gray-50"
              onClick={() => handleSort('role')}
              aria-sort={sortColumn === 'role' ? sortDirection : 'none'}
            >
              <div className="flex items-center space-x-1">
                <span>Role</span>
                {getSortIcon('role')}
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
          </TableRow>
        </TableHeader>
        <TableBody>
          {sortedReps.map((rep, index) => {
            const rank = index + 1;
            const cancellationRate = calculateCancellationRate(rep);
            const holdRate = calculateHoldRate(rep);
            const isTopPerformer = rank === 1;
            const isBottomPerformer = rank === sortedReps.length;
            
            return (
              <TableRow 
                key={`${rep.repEmail}-${rep.officeId}`}
                className={`
                  ${isTopPerformer ? 'bg-green-50 border-l-4 border-green-500' : ''}
                  ${isBottomPerformer && sortedReps.length > 1 ? 'bg-red-50 border-l-4 border-red-500' : ''}
                  ${index % 2 === 0 ? 'bg-slate-50' : ''}
                  hover:bg-slate-100
                `}
              >
                <TableCell className="font-medium">
                  <span className="inline-flex items-center justify-center w-6 h-6 bg-blue-100 text-blue-700 rounded-full text-xs font-semibold">
                    {rank}
                  </span>
                </TableCell>
                <TableCell className="sticky left-0 font-medium">
                  <div>
                    <div className="font-medium text-gray-900">{rep.repName}</div>
                    {rep.repEmail && (
                      <div className="text-sm text-gray-500">{rep.repEmail}</div>
                    )}
                  </div>
                </TableCell>
                <TableCell>
                  <Badge 
                    variant="secondary"
                    className={
                      rep.role === 'closer' 
                        ? 'bg-blue-100 text-blue-700' 
                        : 'bg-purple-100 text-purple-700'
                    }
                  >
                    {rep.role === 'closer' ? 'Closer' : 'Setter'}
                  </Badge>
                </TableCell>
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
                  {rep.avgCycleTime ? `${Math.round(rep.avgCycleTime)} days` : 'N/A'}
                </TableCell>
                <TableCell className="text-right">
                  <span className={cancellationRate > 20 ? 'text-red-600 font-medium' : ''}>
                    {formatPercentage(cancellationRate)}
                  </span>
                </TableCell>
                <TableCell className="text-right">
                  <span className={holdRate > 15 ? 'text-orange-600 font-medium' : ''}>
                    {formatPercentage(holdRate)}
                  </span>
                </TableCell>
              </TableRow>
            );
          })}
        </TableBody>
      </Table>
    </div>
  );
}
