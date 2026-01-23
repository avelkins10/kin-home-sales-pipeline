'use client';

import { useQuery } from '@tanstack/react-query';
import { Button } from '@/components/ui/button';
import { format } from 'date-fns';
import { DateRangePicker, type DateRange } from '@/components/ui/date-range-picker';
import { OfficeMultiSelect } from '@/components/settings/OfficeMultiSelect';

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
}

export function LeaderboardFilters({
  timeRange,
  onTimeRangeChange,
  customDateRange,
  onCustomDateRangeChange,
  selectedOfficeIds,
  onOfficeIdsChange,
}: LeaderboardFiltersProps) {
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
    </div>
  );
}
