'use client'

// components/dashboard/DashboardFilters.tsx
import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { DateRangePicker, type DateRange } from '@/components/ui/date-range-picker';
import { format } from 'date-fns';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

interface DashboardFiltersProps {
  selectedTimeRange: TimeRange;
  customDateRange?: CustomDateRange;
  onTimeRangeChange: (range: TimeRange, customRange?: CustomDateRange) => void;
}

export function DashboardFilters({
  selectedTimeRange,
  customDateRange,
  onTimeRangeChange
}: DashboardFiltersProps) {
  const [showDatePicker, setShowDatePicker] = useState(false);

  const timeRanges: { value: TimeRange; label: string }[] = [
    { value: 'lifetime', label: 'Lifetime' },
    { value: 'ytd', label: 'Year to Date' },
    { value: 'month', label: 'This Month' },
    { value: 'week', label: 'This Week' },
  ];

  const handleDateRangeSelect = (range: DateRange | undefined) => {
    if (range?.from && range?.to) {
      const customRange: CustomDateRange = {
        startDate: format(range.from, 'yyyy-MM-dd'),
        endDate: format(range.to, 'yyyy-MM-dd'),
      };
      onTimeRangeChange('custom', customRange);
      setShowDatePicker(false);
    }
  };

  const getCustomLabel = () => {
    if (customDateRange) {
      const from = new Date(customDateRange.startDate);
      const to = new Date(customDateRange.endDate);
      return `${format(from, 'MMM d')} - ${format(to, 'MMM d, yyyy')}`;
    }
    return 'Custom Range';
  };

  return (
    <div className="flex items-center space-x-1" role="group" aria-label="Filter dashboard by time range">
      {timeRanges.map((range, index) => {
        const isActive = selectedTimeRange === range.value;
        const isFirst = index === 0;

        return (
          <Button
            key={range.value}
            variant={isActive ? 'default' : 'outline'}
            size="sm"
            onClick={() => onTimeRangeChange(range.value)}
            className={`
              px-4 py-2 text-sm font-medium transition-colors
              ${isFirst ? 'rounded-l-lg' : ''}
              rounded-none
              ${isFirst ? 'border-r-0' : ''}
              border-r-0
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

      {/* Custom Range Button */}
      {showDatePicker ? (
        <div className="ml-2">
          <DateRangePicker
            value={customDateRange ? {
              from: new Date(customDateRange.startDate),
              to: new Date(customDateRange.endDate),
            } : undefined}
            onChange={handleDateRangeSelect}
          />
        </div>
      ) : (
        <Button
          variant={selectedTimeRange === 'custom' ? 'default' : 'outline'}
          size="sm"
          onClick={() => setShowDatePicker(true)}
          className={`
            px-4 py-2 text-sm font-medium transition-colors
            rounded-r-lg
            ${selectedTimeRange === 'custom'
              ? 'bg-blue-600 text-white hover:bg-blue-700'
              : 'bg-white text-gray-700 hover:bg-gray-50 border-gray-300'
            }
          `}
          aria-pressed={selectedTimeRange === 'custom'}
        >
          {getCustomLabel()}
        </Button>
      )}
    </div>
  );
}
