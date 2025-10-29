'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Badge } from '@/components/ui/badge';
import { CrewPerformanceMetrics } from '@/lib/types/operations';
import { Users, ChevronUp, ChevronDown, Star, TrendingUp, TrendingDown } from 'lucide-react';
import { useState } from 'react';
import { cn } from '@/lib/utils';

interface CrewPerformanceTableProps {
  crewMetrics: CrewPerformanceMetrics[];
  teamAverages: Partial<CrewPerformanceMetrics>;
  onCrewClick?: (entityId: number) => void;
}

type SortColumn = 'crew_name' | 'tasks_completed_month' | 'avg_completion_time_minutes' | 'on_time_percentage' | 'customer_rating_avg' | 'tasks_currently_assigned';
type SortDirection = 'asc' | 'desc';

// Color coding functions
const getCompletionTimeColor = (time: number | null, average: number | null) => {
  if (!time || !average) return 'text-gray-600';
  return time <= average ? 'text-green-600' : 'text-red-600';
};

const getOnTimeColor = (percentage: number) => {
  if (percentage >= 80) return 'text-green-600';
  if (percentage >= 60) return 'text-yellow-600';
  return 'text-red-600';
};

const getRatingColor = (rating: number | null) => {
  if (!rating) return 'text-gray-600';
  if (rating >= 4.5) return 'text-green-600';
  if (rating >= 3.5) return 'text-yellow-600';
  return 'text-red-600';
};

const getPerformanceBadge = (crew: CrewPerformanceMetrics, teamAvg: Partial<CrewPerformanceMetrics>) => {
  const tasksScore = crew.tasks_completed_month >= (teamAvg.tasks_completed_month || 0) ? 1 : 0;
  const onTimeScore = crew.on_time_percentage >= (teamAvg.on_time_percentage || 0) ? 1 : 0;
  const ratingScore = (crew.customer_rating_avg || 0) >= (teamAvg.customer_rating_avg || 0) ? 1 : 0;
  
  const totalScore = tasksScore + onTimeScore + ratingScore;
  
  if (totalScore >= 2) {
    return <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200">Above Average</Badge>;
  } else {
    return <Badge variant="outline" className="bg-red-50 text-red-700 border-red-200">Below Average</Badge>;
  }
};

const formatCompletionTime = (minutes: number | null): string => {
  if (!minutes) return 'N/A';
  const hours = Math.floor(minutes / 60);
  const mins = Math.round(minutes % 60);
  if (hours > 0) {
    return `${hours}h ${mins}m`;
  }
  return `${mins}m`;
};

export function CrewPerformanceTable({ 
  crewMetrics, 
  teamAverages,
  onCrewClick 
}: CrewPerformanceTableProps) {
  const [sortColumn, setSortColumn] = useState<SortColumn>('tasks_completed_month');
  const [sortDirection, setSortDirection] = useState<SortDirection>('desc');

  const handleSort = (column: SortColumn) => {
    if (sortColumn === column) {
      setSortDirection(sortDirection === 'asc' ? 'desc' : 'asc');
    } else {
      setSortColumn(column);
      setSortDirection('desc');
    }
  };

  const sortedCrewMetrics = [...crewMetrics].sort((a, b) => {
    const aValue = a[sortColumn];
    const bValue = b[sortColumn];
    
    if (typeof aValue === 'string' && typeof bValue === 'string') {
      return sortDirection === 'asc' 
        ? aValue.localeCompare(bValue)
        : bValue.localeCompare(aValue);
    }
    
    const aNum = Number(aValue || 0);
    const bNum = Number(bValue || 0);
    
    return sortDirection === 'asc' ? aNum - bNum : bNum - aNum;
  });

  if (!crewMetrics || crewMetrics.length === 0) {
    return (
      <Card className="h-64 mobile:h-72 ipad:h-80">
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Users className="h-5 w-5 text-gray-400" />
            Crew Performance
          </CardTitle>
        </CardHeader>
        <CardContent className="flex items-center justify-center h-full">
          <div className="text-center text-gray-500">
            <Users className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p>No crew performance data available</p>
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
          Crew Performance Details
        </CardTitle>
        <p className="text-sm text-gray-600">
          {crewMetrics.length} crew members
        </p>
      </CardHeader>
      <CardContent>
        <ScrollArea className="h-96">
          <Table>
            <TableHeader>
              <TableRow>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('crew_name')}
                >
                  <div className="flex items-center gap-1">
                    Crew Member
                    {sortColumn === 'crew_name' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('tasks_completed_month')}
                >
                  <div className="flex items-center gap-1">
                    Tasks Completed
                    {sortColumn === 'tasks_completed_month' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('avg_completion_time_minutes')}
                >
                  <div className="flex items-center gap-1">
                    Avg Time
                    {sortColumn === 'avg_completion_time_minutes' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('on_time_percentage')}
                >
                  <div className="flex items-center gap-1">
                    On-Time %
                    {sortColumn === 'on_time_percentage' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('customer_rating_avg')}
                >
                  <div className="flex items-center gap-1">
                    Rating
                    {sortColumn === 'customer_rating_avg' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead 
                  className="cursor-pointer hover:bg-gray-50"
                  onClick={() => handleSort('tasks_currently_assigned')}
                >
                  <div className="flex items-center gap-1">
                    Active Tasks
                    {sortColumn === 'tasks_currently_assigned' && (
                      sortDirection === 'asc' ? <ChevronUp className="h-4 w-4" /> : <ChevronDown className="h-4 w-4" />
                    )}
                  </div>
                </TableHead>
                <TableHead>Performance</TableHead>
              </TableRow>
            </TableHeader>
            <TableBody>
              {sortedCrewMetrics.map((crew) => {
                const isTopPerformer = crew.tasks_completed_month >= (teamAverages.tasks_completed_month || 0) * 1.2;
                
                return (
                  <TableRow 
                    key={crew.entity_id}
                    className={cn(
                      "hover:bg-gray-50",
                      onCrewClick && "cursor-pointer",
                      isTopPerformer && "font-semibold"
                    )}
                    onClick={() => onCrewClick?.(crew.entity_id)}
                  >
                    <TableCell>
                      <div>
                        <div className="font-medium">{crew.crew_name}</div>
                        <div className="text-xs text-gray-500">{crew.crew_email}</div>
                      </div>
                    </TableCell>
                    <TableCell>
                      <div className="space-y-1">
                        <div className="text-sm">
                          Today: <span className="font-semibold">{crew.tasks_completed_today}</span>
                        </div>
                        <div className="text-sm">
                          Week: <span className="font-semibold">{crew.tasks_completed_week}</span>
                        </div>
                        <div className="text-sm">
                          Month: <span className="font-semibold">{crew.tasks_completed_month}</span>
                        </div>
                      </div>
                    </TableCell>
                    <TableCell className={getCompletionTimeColor(crew.avg_completion_time_minutes, teamAverages.avg_completion_time_minutes || null)}>
                      {formatCompletionTime(crew.avg_completion_time_minutes)}
                      {crew.avg_completion_time_minutes && teamAverages.avg_completion_time_minutes && (
                        crew.avg_completion_time_minutes <= teamAverages.avg_completion_time_minutes ? (
                          <TrendingUp className="inline h-3 w-3 ml-1" />
                        ) : (
                          <TrendingDown className="inline h-3 w-3 ml-1" />
                        )
                      )}
                    </TableCell>
                    <TableCell className={getOnTimeColor(crew.on_time_percentage)}>
                      {crew.on_time_percentage.toFixed(1)}%
                    </TableCell>
                    <TableCell>
                      {crew.customer_rating_avg ? (
                        <div className={cn("flex items-center gap-1", getRatingColor(crew.customer_rating_avg))}>
                          <Star className="h-4 w-4 fill-current" />
                          <span>{crew.customer_rating_avg.toFixed(1)}</span>
                          <span className="text-xs text-gray-500">({crew.customer_rating_count})</span>
                        </div>
                      ) : (
                        <span className="text-gray-400">No ratings</span>
                      )}
                    </TableCell>
                    <TableCell>
                      <Badge variant="outline">{crew.tasks_currently_assigned}</Badge>
                    </TableCell>
                    <TableCell>
                      {getPerformanceBadge(crew, teamAverages)}
                    </TableCell>
                  </TableRow>
                );
              })}
            </TableBody>
          </Table>
        </ScrollArea>
      </CardContent>
    </Card>
  );
}

