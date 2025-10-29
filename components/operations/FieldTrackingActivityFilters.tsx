'use client';

import { useState, useEffect } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Search, X, ChevronDown } from 'lucide-react';
import { ActivityFeedFilters } from '@/lib/types/operations';
import { DateRangePicker } from '@/components/ui/date-range-picker';
import { startOfDay, endOfDay, subDays } from 'date-fns';

interface FieldTrackingActivityFiltersProps {
  filters: ActivityFeedFilters;
  onFiltersChange: (filters: ActivityFeedFilters) => void;
  availableCrewMembers: string[];
  availableTaskTypes: string[];
  showFilters: boolean;
  onToggleFilters: () => void;
}

export function FieldTrackingActivityFilters({
  filters,
  onFiltersChange,
  availableCrewMembers,
  availableTaskTypes,
  showFilters,
  onToggleFilters,
}: FieldTrackingActivityFiltersProps) {
  const [searchValue, setSearchValue] = useState(filters.search);

  // Debounce search input
  useEffect(() => {
    const timer = setTimeout(() => {
      if (searchValue !== filters.search) {
        onFiltersChange({ ...filters, search: searchValue });
      }
    }, 300);

    return () => clearTimeout(timer);
  }, [searchValue, filters, onFiltersChange]);

  const handleFilterChange = (key: keyof ActivityFeedFilters, value: any) => {
    onFiltersChange({ ...filters, [key]: value });
  };

  const clearFilters = () => {
    const defaultFilters: ActivityFeedFilters = {
      eventType: 'all',
      dateRange: null,
      crewMember: 'all',
      taskType: 'all',
      search: '',
    };
    setSearchValue('');
    onFiltersChange(defaultFilters);
  };

  const getActiveFilterCount = (): number => {
    let count = 0;
    if (filters.eventType !== 'all') count++;
    if (filters.dateRange !== null) count++;
    if (filters.crewMember !== 'all') count++;
    if (filters.taskType !== 'all') count++;
    if (filters.search) count++;
    return count;
  };

  const getEventTypeDisplayName = (eventType: string): string => {
    const displayNames: Record<string, string> = {
      TASK_CREATED: 'Task Created',
      TASK_STATUS: 'Task Status Update',
      CREW_ASSIGNED: 'Crew Assigned',
      ARRIVING: 'Arriving',
      LATE: 'Late',
      NOSHOW: 'No Show',
      EXCEPTION: 'Exception',
      CANCELLED: 'Cancelled',
      TASK_RATING: 'Task Rating',
    };
    return displayNames[eventType] || eventType;
  };

  const activeFilterCount = getActiveFilterCount();

  return (
    <div className="space-y-3">
      {/* Filter Toggle Button */}
      <div className="flex items-center justify-between">
        <Button
          variant="outline"
          size="sm"
          onClick={onToggleFilters}
          className="gap-2"
        >
          <ChevronDown className={`h-4 w-4 transition-transform ${showFilters ? 'rotate-180' : ''}`} />
          {showFilters ? 'Hide Filters' : 'Show Filters'}
          {activeFilterCount > 0 && (
            <Badge variant="secondary" className="ml-1">
              {activeFilterCount}
            </Badge>
          )}
        </Button>
      </div>

      {/* Filter Grid */}
      {showFilters && (
        <Card>
          <CardContent className="pt-6">
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {/* Event Type Filter */}
              <div>
                <Label htmlFor="event-type-filter" className="text-sm font-medium mb-2 block">
                  Event Type
                </Label>
                <Select
                  value={filters.eventType}
                  onValueChange={(value) => handleFilterChange('eventType', value)}
                >
                  <SelectTrigger id="event-type-filter" className="h-10">
                    <SelectValue placeholder="All Events" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="all">All Events</SelectItem>
                    <SelectItem value="TASK_CREATED">Task Created</SelectItem>
                    <SelectItem value="TASK_STATUS">Task Status Update</SelectItem>
                    <SelectItem value="CREW_ASSIGNED">Crew Assigned</SelectItem>
                    <SelectItem value="ARRIVING">Arriving</SelectItem>
                    <SelectItem value="LATE">Late</SelectItem>
                    <SelectItem value="NOSHOW">No Show</SelectItem>
                    <SelectItem value="EXCEPTION">Exception</SelectItem>
                    <SelectItem value="CANCELLED">Cancelled</SelectItem>
                    <SelectItem value="TASK_RATING">Task Rating</SelectItem>
                  </SelectContent>
                </Select>
              </div>

              {/* Crew Member Filter */}
              {availableCrewMembers.length > 0 && (
                <div>
                  <Label htmlFor="crew-member-filter" className="text-sm font-medium mb-2 block">
                    Crew Member
                  </Label>
                  <Select
                    value={filters.crewMember}
                    onValueChange={(value) => handleFilterChange('crewMember', value)}
                  >
                    <SelectTrigger id="crew-member-filter" className="h-10">
                      <SelectValue placeholder="All Crew Members" />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All Crew Members</SelectItem>
                      {availableCrewMembers.map((member) => (
                        <SelectItem key={member} value={member}>
                          {member}
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                </div>
              )}

              {/* Task Type Filter */}
              <div>
                <Label htmlFor="task-type-filter" className="text-sm font-medium mb-2 block">
                  Task Type
                </Label>
                <Select
                  value={filters.taskType}
                  onValueChange={(value) => handleFilterChange('taskType', value)}
                >
                  <SelectTrigger id="task-type-filter" className="h-10">
                    <SelectValue placeholder="All Task Types" />
                  </SelectTrigger>
                  <SelectContent>
                    <SelectItem value="all">All Task Types</SelectItem>
                    {availableTaskTypes.map((type) => (
                      <SelectItem key={type} value={type}>
                        {type}
                      </SelectItem>
                    ))}
                  </SelectContent>
                </Select>
              </div>

              {/* Date Range Filter */}
              <div className="md:col-span-2">
                <Label htmlFor="date-range-filter" className="text-sm font-medium mb-2 block">
                  Date Range
                </Label>
                <DateRangePicker
                  value={
                    filters.dateRange
                      ? { from: filters.dateRange.start, to: filters.dateRange.end }
                      : undefined
                  }
                  onChange={(range) => {
                    const mappedRange = range?.from
                      ? { start: range.from, end: range.to ?? range.from }
                      : null;
                    handleFilterChange('dateRange', mappedRange);
                  }}
                />
              </div>

              {/* Search Input */}
              <div className="md:col-span-2 lg:col-span-3">
                <Label htmlFor="search-filter" className="text-sm font-medium mb-2 block">
                  Search
                </Label>
                <div className="relative">
                  <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
                  <Input
                    id="search-filter"
                    placeholder="Search by customer name or task ID..."
                    value={searchValue}
                    onChange={(e) => setSearchValue(e.target.value)}
                    className="h-10 pl-10 pr-10"
                  />
                  {searchValue && (
                    <button
                      onClick={() => {
                        setSearchValue('');
                        handleFilterChange('search', '');
                      }}
                      className="absolute right-3 top-1/2 transform -translate-y-1/2 text-gray-400 hover:text-gray-600"
                      aria-label="Clear search"
                    >
                      <X className="h-4 w-4" />
                    </button>
                  )}
                </div>
              </div>
            </div>

            {/* Reset Button */}
            {activeFilterCount > 0 && (
              <div className="mt-4 pt-4 border-t flex items-center justify-between">
                <div className="flex items-center gap-2 flex-wrap">
                  <span className="text-sm text-gray-600">Active filters:</span>
                  <Badge variant="secondary" className="text-xs">
                    {activeFilterCount} filter{activeFilterCount !== 1 ? 's' : ''}
                  </Badge>

                  {filters.eventType !== 'all' && (
                    <Badge
                      variant="outline"
                      className="text-xs cursor-pointer hover:bg-gray-100"
                      onClick={() => handleFilterChange('eventType', 'all')}
                    >
                      Event: {getEventTypeDisplayName(filters.eventType)}
                      <X className="ml-1 h-3 w-3" />
                    </Badge>
                  )}

                  {filters.crewMember !== 'all' && (
                    <Badge
                      variant="outline"
                      className="text-xs cursor-pointer hover:bg-gray-100"
                      onClick={() => handleFilterChange('crewMember', 'all')}
                    >
                      Crew: {filters.crewMember}
                      <X className="ml-1 h-3 w-3" />
                    </Badge>
                  )}

                  {filters.taskType !== 'all' && (
                    <Badge
                      variant="outline"
                      className="text-xs cursor-pointer hover:bg-gray-100"
                      onClick={() => handleFilterChange('taskType', 'all')}
                    >
                      Task: {filters.taskType}
                      <X className="ml-1 h-3 w-3" />
                    </Badge>
                  )}

                  {filters.dateRange && (
                    <Badge
                      variant="outline"
                      className="text-xs cursor-pointer hover:bg-gray-100"
                      onClick={() => handleFilterChange('dateRange', null)}
                    >
                      Date Range
                      <X className="ml-1 h-3 w-3" />
                    </Badge>
                  )}

                  {filters.search && (
                    <Badge
                      variant="outline"
                      className="text-xs cursor-pointer hover:bg-gray-100"
                      onClick={() => {
                        setSearchValue('');
                        handleFilterChange('search', '');
                      }}
                    >
                      Search: "{filters.search}"
                      <X className="ml-1 h-3 w-3" />
                    </Badge>
                  )}
                </div>

                <Button variant="outline" size="sm" onClick={clearFilters}>
                  Reset All
                </Button>
              </div>
            )}
          </CardContent>
        </Card>
      )}
    </div>
  );
}

