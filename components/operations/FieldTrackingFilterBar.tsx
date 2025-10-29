'use client';

import { useState, useEffect } from 'react';
import { Search, Filter, X, Calendar, Users, Tag, ArrowUpDown } from 'lucide-react';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from '@/components/ui/select';
import { Badge } from '@/components/ui/badge';
import { Card } from '@/components/ui/card';
import type { FieldTrackingTaskType, FieldTrackingTaskStatus, FieldTrackingEntity } from '@/lib/types/operations';

export interface FieldTrackingFilterState {
  search: string;
  status: FieldTrackingTaskStatus | 'all';
  taskType: FieldTrackingTaskType | 'all';
  crewMember: string | 'all'; // entity_id or 'all'
  dateFilter: 'today' | 'tomorrow' | 'yesterday' | 'this_week' | 'last_week' | 'last_7_days' | 'last_30_days' | 'overdue' | 'all';
  sortBy: 'scheduled_start' | 'status' | 'customer_name' | 'task_type';
  sortOrder: 'asc' | 'desc';
}

interface FieldTrackingFilterBarProps {
  filters: FieldTrackingFilterState;
  onFilterChange: (filters: FieldTrackingFilterState) => void;
  crewMembers: FieldTrackingEntity[];
  taskCounts?: {
    total: number;
    byStatus: Record<string, number>;
    byType: Record<string, number>;
  };
}

const STATUS_OPTIONS: { value: FieldTrackingTaskStatus | 'all'; label: string; color: string }[] = [
  { value: 'all', label: 'All Statuses', color: 'gray' },
  { value: 'NOT_STARTED', label: 'Not Started', color: 'gray' },
  { value: 'ENROUTE', label: 'En Route', color: 'blue' },
  { value: 'STARTED', label: 'Started', color: 'yellow' },
  { value: 'COMPLETE', label: 'Complete', color: 'green' },
  { value: 'LATE', label: 'Late', color: 'red' },
  { value: 'NOSHOW', label: 'No Show', color: 'orange' },
  { value: 'EXCEPTION', label: 'Exception', color: 'purple' },
  { value: 'CANCELLED', label: 'Cancelled', color: 'gray' },
];

const TASK_TYPE_OPTIONS: { value: FieldTrackingTaskType | 'all'; label: string }[] = [
  { value: 'all', label: 'All Types' },
  { value: 'survey', label: 'Survey' },
  { value: 'install', label: 'Install' },
  { value: 'inspection', label: 'Inspection' },
  { value: 'service', label: 'Service' },
  { value: 'other', label: 'Other' },
];

const DATE_FILTER_OPTIONS = [
  { value: 'today', label: 'Today' },
  { value: 'tomorrow', label: 'Tomorrow' },
  { value: 'yesterday', label: 'Yesterday' },
  { value: 'last_7_days', label: 'Last 7 Days' },
  { value: 'this_week', label: 'This Week' },
  { value: 'last_week', label: 'Last Week' },
  { value: 'last_30_days', label: 'Last 30 Days' },
  { value: 'overdue', label: 'Overdue' },
  { value: 'all', label: 'All Dates' },
] as const;

const SORT_OPTIONS = [
  { value: 'scheduled_start', label: 'Scheduled Time' },
  { value: 'status', label: 'Status' },
  { value: 'customer_name', label: 'Customer Name' },
  { value: 'task_type', label: 'Task Type' },
] as const;

export function FieldTrackingFilterBar({
  filters,
  onFilterChange,
  crewMembers,
  taskCounts,
}: FieldTrackingFilterBarProps) {
  const [showFilters, setShowFilters] = useState(false);
  const [localSearch, setLocalSearch] = useState(filters.search);

  // Debounce search input
  useEffect(() => {
    const timer = setTimeout(() => {
      if (localSearch !== filters.search) {
        onFilterChange({ ...filters, search: localSearch });
      }
    }, 300);
    return () => clearTimeout(timer);
  }, [localSearch]);

  const updateFilter = (key: keyof FieldTrackingFilterState, value: any) => {
    onFilterChange({ ...filters, [key]: value });
  };

  const resetFilters = () => {
    const defaultFilters: FieldTrackingFilterState = {
      search: '',
      status: 'all',
      taskType: 'all',
      crewMember: 'all',
      dateFilter: 'today',
      sortBy: 'scheduled_start',
      sortOrder: 'asc',
    };
    setLocalSearch('');
    onFilterChange(defaultFilters);
  };

  const getActiveFilterCount = () => {
    let count = 0;
    if (filters.search) count++;
    if (filters.status !== 'all') count++;
    if (filters.taskType !== 'all') count++;
    if (filters.crewMember !== 'all') count++;
    if (filters.dateFilter !== 'today') count++;
    return count;
  };

  const activeFilterCount = getActiveFilterCount();

  return (
    <Card className="p-4 space-y-4">
      {/* Main Filter Bar */}
      <div className="flex flex-col lg:flex-row gap-4">
        {/* Search */}
        <div className="flex-1 relative">
          <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
          <Input
            type="text"
            placeholder="Search by customer name, project ID, or address..."
            value={localSearch}
            onChange={(e) => setLocalSearch(e.target.value)}
            className="pl-10 pr-10"
          />
          {localSearch && (
            <button
              onClick={() => {
                setLocalSearch('');
                onFilterChange({ ...filters, search: '' });
              }}
              className="absolute right-3 top-1/2 transform -translate-y-1/2 text-gray-400 hover:text-gray-600"
            >
              <X className="h-4 w-4" />
            </button>
          )}
        </div>

        {/* Quick Date Filter */}
        <Select
          value={filters.dateFilter}
          onValueChange={(value) => updateFilter('dateFilter', value)}
        >
          <SelectTrigger className="w-full lg:w-[180px]">
            <Calendar className="mr-2 h-4 w-4" />
            <SelectValue />
          </SelectTrigger>
          <SelectContent>
            {DATE_FILTER_OPTIONS.map((option) => (
              <SelectItem key={option.value} value={option.value}>
                {option.label}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>

        {/* Show Filters Toggle */}
        <Button
          variant={showFilters ? 'default' : 'outline'}
          onClick={() => setShowFilters(!showFilters)}
          className="w-full lg:w-auto"
        >
          <Filter className="mr-2 h-4 w-4" />
          Filters
          {activeFilterCount > 0 && (
            <Badge variant="secondary" className="ml-2">
              {activeFilterCount}
            </Badge>
          )}
        </Button>

        {/* Reset Filters */}
        {activeFilterCount > 0 && (
          <Button variant="ghost" onClick={resetFilters} className="w-full lg:w-auto">
            <X className="mr-2 h-4 w-4" />
            Reset
          </Button>
        )}
      </div>

      {/* Expanded Filters */}
      {showFilters && (
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 pt-4 border-t">
          {/* Status Filter */}
          <div className="space-y-2">
            <label className="text-sm font-medium flex items-center gap-2">
              <Tag className="h-4 w-4" />
              Status
            </label>
            <Select
              value={filters.status}
              onValueChange={(value) => updateFilter('status', value)}
            >
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {STATUS_OPTIONS.map((option) => (
                  <SelectItem key={option.value} value={option.value}>
                    <div className="flex items-center gap-2">
                      <div className={`w-2 h-2 rounded-full bg-${option.color}-500`} />
                      {option.label}
                      {taskCounts?.byStatus[option.value] && (
                        <span className="text-xs text-gray-500 ml-auto">
                          ({taskCounts.byStatus[option.value]})
                        </span>
                      )}
                    </div>
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Task Type Filter */}
          <div className="space-y-2">
            <label className="text-sm font-medium flex items-center gap-2">
              <Tag className="h-4 w-4" />
              Task Type
            </label>
            <Select
              value={filters.taskType}
              onValueChange={(value) => updateFilter('taskType', value)}
            >
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                {TASK_TYPE_OPTIONS.map((option) => (
                  <SelectItem key={option.value} value={option.value}>
                    {option.label}
                    {taskCounts?.byType[option.value] && (
                      <span className="text-xs text-gray-500 ml-2">
                        ({taskCounts.byType[option.value]})
                      </span>
                    )}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Crew Member Filter */}
          <div className="space-y-2">
            <label className="text-sm font-medium flex items-center gap-2">
              <Users className="h-4 w-4" />
              Crew Member
            </label>
            <Select
              value={filters.crewMember}
              onValueChange={(value) => updateFilter('crewMember', value)}
            >
              <SelectTrigger>
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Crew Members</SelectItem>
                {crewMembers.map((crew) => (
                  <SelectItem key={crew.id} value={crew.id.toString()}>
                    {crew.name}
                  </SelectItem>
                ))}
              </SelectContent>
            </Select>
          </div>

          {/* Sort */}
          <div className="space-y-2">
            <label className="text-sm font-medium flex items-center gap-2">
              <ArrowUpDown className="h-4 w-4" />
              Sort By
            </label>
            <div className="flex gap-2">
              <Select
                value={filters.sortBy}
                onValueChange={(value) => updateFilter('sortBy', value)}
              >
                <SelectTrigger className="flex-1">
                  <SelectValue />
                </SelectTrigger>
                <SelectContent>
                  {SORT_OPTIONS.map((option) => (
                    <SelectItem key={option.value} value={option.value}>
                      {option.label}
                    </SelectItem>
                  ))}
                </SelectContent>
              </Select>
              <Button
                variant="outline"
                size="icon"
                onClick={() =>
                  updateFilter('sortOrder', filters.sortOrder === 'asc' ? 'desc' : 'asc')
                }
              >
                <ArrowUpDown className="h-4 w-4" />
              </Button>
            </div>
          </div>
        </div>
      )}

      {/* Active Filters Display */}
      {activeFilterCount > 0 && (
        <div className="flex flex-wrap gap-2 pt-2 border-t">
          {filters.search && (
            <Badge variant="secondary" className="gap-1">
              Search: {filters.search}
              <button onClick={() => { setLocalSearch(''); onFilterChange({ ...filters, search: '' }); }}>
                <X className="h-3 w-3" />
              </button>
            </Badge>
          )}
          {filters.status !== 'all' && (
            <Badge variant="secondary" className="gap-1">
              Status: {STATUS_OPTIONS.find((o) => o.value === filters.status)?.label}
              <button onClick={() => updateFilter('status', 'all')}>
                <X className="h-3 w-3" />
              </button>
            </Badge>
          )}
          {filters.taskType !== 'all' && (
            <Badge variant="secondary" className="gap-1">
              Type: {TASK_TYPE_OPTIONS.find((o) => o.value === filters.taskType)?.label}
              <button onClick={() => updateFilter('taskType', 'all')}>
                <X className="h-3 w-3" />
              </button>
            </Badge>
          )}
          {filters.crewMember !== 'all' && (
            <Badge variant="secondary" className="gap-1">
              Crew: {crewMembers.find((c) => c.id.toString() === filters.crewMember)?.name}
              <button onClick={() => updateFilter('crewMember', 'all')}>
                <X className="h-3 w-3" />
              </button>
            </Badge>
          )}
          {filters.dateFilter !== 'today' && (
            <Badge variant="secondary" className="gap-1">
              Date: {DATE_FILTER_OPTIONS.find((o) => o.value === filters.dateFilter)?.label}
              <button onClick={() => updateFilter('dateFilter', 'today')}>
                <X className="h-3 w-3" />
              </button>
            </Badge>
          )}
        </div>
      )}
    </Card>
  );
}
