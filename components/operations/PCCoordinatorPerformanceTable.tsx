'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { ScrollArea } from '@/components/ui/scroll-area';
import { PCCoordinatorPerformance } from '@/lib/types/operations';
import { Users, ChevronUp, ChevronDown } from 'lucide-react';
import { useState } from 'react';
import { cn } from '@/lib/utils';
import { formatPercentage } from '@/lib/utils/formatters';

interface PCCoordinatorPerformanceTableProps {
  coordinators: PCCoordinatorPerformance[];
  onCoordinatorClick?: (email: string) => void;
}

type SortColumn = 'coordinatorName' | 'totalProjects' | 'dailyOutreach' | 'responseRate' | 'avgTimeToContact' | 'escalationRate' | 'slaCompliance' | 'projectsCompleted' | 'avgProjectVelocity';
type SortDirection = 'asc' | 'desc';

const getResponseRateColor = (rate: number) => {
  if (rate >= 80) return 'text-green-600';
  if (rate >= 60) return 'text-yellow-600';
  return 'text-red-600';
};

const getEscalationRateColor = (rate: number) => {
  if (rate < 10) return 'text-green-600';
  if (rate < 20) return 'text-yellow-600';
  return 'text-red-600';
};

const getSLAComplianceColor = (compliance: number) => {
  if (compliance >= 90) return 'text-green-600';
  if (compliance >= 75) return 'text-yellow-600';
  return 'text-red-600';
};

export function PCCoordinatorPerformanceTable({ 
  coordinators, 
  onCoordinatorClick 
}: PCCoordinatorPerformanceTableProps) {
  const [sortColumn, setSortColumn] = useState<SortColumn>('totalProjects');
  const [sortDirection, setSortDirection] = useState<SortDirection>('desc');

  const handleSort = (column: SortColumn) => {
    if (sortColumn === column) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortColumn(column);
      setSortDirection('desc');
    }
  };

  const sortedCoordinators = [...coordinators].sort((a, b) => {
    const aValue = a[sortColumn];
    const bValue = b[sortColumn];
    
    if (typeof aValue === 'string' && typeof bValue === 'string') {
      return sortDirection === 'asc' 
        ? aValue.localeCompare(bValue)
        : bValue.localeCompare(aValue);
    }
    
    const aNum = Number(aValue);
    const bNum = Number(bValue);
    
    return sortDirection === 'asc' ? aNum - bNum : bNum - aNum;
  });

  if (!coordinators || coordinators.length === 0) {
    return (
      <Card className="h-64 mobile:h-72 ipad:h-80">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Users className="h-5 w-5 text-gray-400" />
            Coordinator Performance
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <Users className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>No coordinator data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card className="shadow-sm hover:shadow-md transition-shadow">
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Users className="h-5 w-5 text-blue-600" />
          Coordinator Performance
        </CardTitle>
        <p className="text-sm text-gray-600">
          {coordinators.length} coordinators
        </p>
      </CardHeader>
      <CardContent>
        <ScrollArea className="h-96">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('coordinatorName')}
                >
                  <div className="flex items-center gap-1">
                    Coordinator Name
                    {sortColumn === 'coordinatorName' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('totalProjects')}
                >
                  <div className="flex items-center gap-1">
                    Total Projects
                    {sortColumn === 'totalProjects' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('dailyOutreach')}
                >
                  <div className="flex items-center gap-1">
                    Daily Outreach
                    {sortColumn === 'dailyOutreach' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('responseRate')}
                >
                  <div className="flex items-center gap-1">
                    Response Rate
                    {sortColumn === 'responseRate' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('avgTimeToContact')}
                >
                  <div className="flex items-center gap-1">
                    Avg Time to Contact
                    {sortColumn === 'avgTimeToContact' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('escalationRate')}
                >
                  <div className="flex items-center gap-1">
                    Escalation Rate
                    {sortColumn === 'escalationRate' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('slaCompliance')}
                >
                  <div className="flex items-center gap-1">
                    SLA Compliance
                    {sortColumn === 'slaCompliance' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('projectsCompleted')}
                >
                  <div className="flex items-center gap-1">
                    Projects Completed
                    {sortColumn === 'projectsCompleted' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('avgProjectVelocity')}
                >
                  <div className="flex items-center gap-1">
                    Avg Velocity
                    {sortColumn === 'avgProjectVelocity' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sortedCoordinators.map((coordinator, index) => (
                <TableRow 
                  key={coordinator.coordinatorEmail}
                  className={cn(
                    "hover:bg-gray-50",
                    onCoordinatorClick && "cursor-pointer",
                    index < Math.ceil(coordinators.length * 0.2) && "font-semibold"
                  )}
                  onClick={() => onCoordinatorClick?.(coordinator.coordinatorEmail)}
                >
                  <TableCell className="font-medium">
                    {coordinator.coordinatorName}
                  </TableCell>
                  <TableCell>{coordinator.totalProjects}</TableCell>
                  <TableCell>{coordinator.dailyOutreach}</TableCell>
                  <TableCell className={getResponseRateColor(coordinator.responseRate)}>
                    {formatPercentage(coordinator.responseRate)}
                  </TableCell>
                  <TableCell>{coordinator.avgTimeToContact.toFixed(1)} days</TableCell>
                  <TableCell className={getEscalationRateColor(coordinator.escalationRate)}>
                    {formatPercentage(coordinator.escalationRate)}
                  </TableCell>
                  <TableCell className={getSLAComplianceColor(coordinator.slaCompliance)}>
                    {formatPercentage(coordinator.slaCompliance)}
                  </TableCell>
                  <TableCell>{coordinator.projectsCompleted}</TableCell>
                  <TableCell>{coordinator.avgProjectVelocity.toFixed(1)} days</TableCell>
                </TableRow>
              ))}
            </TableBody>
          </Table>
        </ScrollArea>
      </CardContent>
    </Card>
  );
}
