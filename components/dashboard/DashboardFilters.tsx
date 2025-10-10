'use client'

// components/dashboard/DashboardFilters.tsx
import { Button } from '@/components/ui/button';
import type { TimeRange } from '@/lib/types/dashboard';

interface DashboardFiltersProps {
  selectedTimeRange: TimeRange;
  onTimeRangeChange: (range: TimeRange) => void;
}

export function DashboardFilters({ selectedTimeRange, onTimeRangeChange }: DashboardFiltersProps) {
  const timeRanges: { value: TimeRange; label: string }[] = [
    { value: 'lifetime', label: 'Lifetime' },
    { value: 'month', label: 'This Month' },
    { value: 'week', label: 'This Week' },
  ];

  return (
    <div className="flex items-center space-x-1" role="group" aria-label="Filter dashboard by time range">
      {timeRanges.map((range, index) => {
        const isActive = selectedTimeRange === range.value;
        const isFirst = index === 0;
        const isLast = index === timeRanges.length - 1;

        return (
          <Button
            key={range.value}
            variant={isActive ? 'default' : 'outline'}
            size="sm"
            onClick={() => onTimeRangeChange(range.value)}
            className={`
              px-4 py-2 text-sm font-medium transition-colors
              ${isFirst ? 'rounded-l-lg' : ''}
              ${isLast ? 'rounded-r-lg' : ''}
              ${!isFirst && !isLast ? 'rounded-none' : ''}
              ${isFirst ? 'border-r-0' : ''}
              ${!isLast ? 'border-r-0' : ''}
              ${isActive 
                ? 'bg-blue-600 text-white hover:bg-blue-700' 
                : 'bg-white text-gray-700 hover:bg-gray-50 border-gray-300'
              }
            `}
            aria-pressed={isActive}
          >
            {range.label}
          </Button>
        );
      })}
    </div>
  );
}
