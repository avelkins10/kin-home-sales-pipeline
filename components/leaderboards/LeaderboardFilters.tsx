'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Button } from '@/components/ui/button';
import { format } from 'date-fns';
import { DateRangePicker, type DateRange } from '@/components/ui/date-range-picker';
import { OfficeMultiSelect } from '@/components/settings/OfficeMultiSelect';
import {
  Popover,
  PopoverContent,
  PopoverTrigger,
} from '@/components/ui/popover';
import { Badge } from '@/components/ui/badge';
import { Check, ChevronDown, Users, X } from 'lucide-react';
import { cn } from '@/lib/utils/cn';

type TimeRange = 'today' | 'week' | 'month' | 'ytd' | 'custom';

interface CustomDateRange {
  startDate: string;
  endDate: string;
}

interface LeaderboardFiltersProps {
  timeRange: TimeRange;
  onTimeRangeChange: (range: TimeRange) => void;
  customDateRange?: CustomDateRange;
  onCustomDateRangeChange: (range: CustomDateRange | undefined) => void;
  selectedOfficeIds: number[];
  onOfficeIdsChange: (ids: number[]) => void;
  selectedTeamNames: string[];
  onTeamNamesChange: (names: string[]) => void;
  availableTeams: string[];
}

export function LeaderboardFilters({
  timeRange,
  onTimeRangeChange,
  customDateRange,
  onCustomDateRangeChange,
  selectedOfficeIds,
  onOfficeIdsChange,
  selectedTeamNames,
  onTeamNamesChange,
  availableTeams,
}: LeaderboardFiltersProps) {
  const [teamPopoverOpen, setTeamPopoverOpen] = useState(false);

  // Fetch available offices
  const { data: officesData } = useQuery({
    queryKey: ['leaderboard-offices'],
    queryFn: async () => {
      const response = await fetch('/api/analytics/office-metrics');
      if (!response.ok) {
        throw new Error('Failed to fetch offices');
      }
      return response.json();
    },
    staleTime: 300000, // 5 minutes
  });

  const availableOffices = officesData?.metrics?.map((metric: any) => ({
    id: metric.officeId,
    name: metric.officeName,
    projectCount: metric.totalProjects
  })) || [];

  const handleTeamToggle = (teamName: string) => {
    if (selectedTeamNames.includes(teamName)) {
      onTeamNamesChange(selectedTeamNames.filter(name => name !== teamName));
    } else {
      onTeamNamesChange([...selectedTeamNames, teamName]);
    }
  };

  const handleClearTeams = () => {
    onTeamNamesChange([]);
  };

  return (
    <div className="flex flex-wrap items-center gap-4">
      {/* Time Range Buttons */}
      <div className="flex items-center gap-2">
        <Button
          variant={timeRange === 'today' ? 'default' : 'outline'}
          size="sm"
          onClick={() => onTimeRangeChange('today')}
        >
          Today
        </Button>
        <Button
          variant={timeRange === 'week' ? 'default' : 'outline'}
          size="sm"
          onClick={() => onTimeRangeChange('week')}
        >
          Week to Date
        </Button>
        <Button
          variant={timeRange === 'month' ? 'default' : 'outline'}
          size="sm"
          onClick={() => onTimeRangeChange('month')}
        >
          Month to Date
        </Button>
        <Button
          variant={timeRange === 'ytd' ? 'default' : 'outline'}
          size="sm"
          onClick={() => onTimeRangeChange('ytd')}
        >
          Year to Date
        </Button>
      </div>

      {/* Custom Date Range */}
      <DateRangePicker
        value={
          customDateRange
            ? {
                from: new Date(customDateRange.startDate),
                to: new Date(customDateRange.endDate),
              }
            : undefined
        }
        onChange={(range: DateRange | undefined) => {
          if (range?.from && range?.to) {
            onCustomDateRangeChange({
              startDate: format(range.from, 'yyyy-MM-dd'),
              endDate: format(range.to, 'yyyy-MM-dd'),
            });
            onTimeRangeChange('custom');
          }
        }}
      />

      {/* Office Multi-Select Filter */}
      <OfficeMultiSelect
        selectedOfficeIds={selectedOfficeIds}
        availableOffices={availableOffices}
        onChange={onOfficeIdsChange}
        placeholder="Select offices"
      />

      {/* Team Multi-Select Filter */}
      {availableTeams.length > 0 && (
        <Popover open={teamPopoverOpen} onOpenChange={setTeamPopoverOpen}>
          <PopoverTrigger asChild>
            <Button
              variant="outline"
              size="sm"
              className="h-9 min-w-[180px] justify-between"
            >
              <div className="flex items-center gap-2">
                <Users className="w-4 h-4" />
                <span>
                  {selectedTeamNames.length === 0
                    ? 'Select teams'
                    : selectedTeamNames.length === 1
                    ? selectedTeamNames[0]
                    : `${selectedTeamNames.length} teams`}
                </span>
              </div>
              <ChevronDown className="w-4 h-4 opacity-50" />
            </Button>
          </PopoverTrigger>
          <PopoverContent className="w-[250px] p-0" align="start">
            <div className="p-2">
              <div className="flex items-center justify-between mb-2">
                <span className="text-sm font-medium">Teams</span>
                {selectedTeamNames.length > 0 && (
                  <Button
                    variant="ghost"
                    size="sm"
                    className="h-6 px-2 text-xs"
                    onClick={handleClearTeams}
                  >
                    Clear
                  </Button>
                )}
              </div>
              <div className="max-h-[300px] overflow-y-auto">
                {availableTeams.map((team) => {
                  const isSelected = selectedTeamNames.includes(team);
                  return (
                    <div
                      key={team}
                      className="flex items-center gap-2 p-2 rounded-md hover:bg-accent cursor-pointer"
                      onClick={() => handleTeamToggle(team)}
                    >
                      <div
                        className={cn(
                          'flex h-4 w-4 items-center justify-center rounded-sm border',
                          isSelected
                            ? 'bg-primary border-primary text-primary-foreground'
                            : 'border-input'
                        )}
                      >
                        {isSelected && <Check className="h-3 w-3" />}
                      </div>
                      <span className="text-sm flex-1">{team}</span>
                    </div>
                  );
                })}
              </div>
            </div>
          </PopoverContent>
        </Popover>
      )}
    </div>
  );
}
