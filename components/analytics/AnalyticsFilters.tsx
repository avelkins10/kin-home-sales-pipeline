'use client';

import { useState } from 'react';
import { Button } from '@/components/ui/button';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue, SelectGroup, SelectLabel } from '@/components/ui/select';
import { DateRangePicker, type DateRange } from '@/components/ui/date-range-picker';
import { format } from 'date-fns';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { OfficeMultiSelect } from './OfficeMultiSelect';

interface Office {
  id: number;
  name: string;
  projectCount?: number;
}

interface Rep {
  email: string;
  name: string;
  role: 'closer' | 'setter';
}

interface AnalyticsFiltersProps {
  selectedTimeRange: TimeRange;
  customDateRange?: CustomDateRange;
  selectedOfficeIds: number[];
  selectedRepEmail?: string;
  availableOffices: Office[];
  availableReps: Rep[];
  onTimeRangeChange: (range: TimeRange, customRange?: CustomDateRange) => void;
  onOfficeIdsChange: (officeIds: number[]) => void;
  onRepEmailChange: (repEmail?: string) => void;
  isLoading?: boolean;
}

export function AnalyticsFilters({
  selectedTimeRange,
  customDateRange,
  selectedOfficeIds,
  selectedRepEmail,
  availableOffices,
  availableReps,
  onTimeRangeChange,
  onOfficeIdsChange,
  onRepEmailChange,
  isLoading = false
}: AnalyticsFiltersProps) {
  const [showDatePicker, setShowDatePicker] = useState(false);

  const timeRanges: { value: TimeRange; label: string }[] = [
    { value: 'ytd', label: 'Year to Date' },
    { value: 'last_30', label: 'Last 30 Days' },
    { value: 'last_90', label: 'Last 90 Days' },
    { value: 'last_12_months', label: 'Last 12 Months' },
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

  const closers = availableReps.filter(rep => rep.role === 'closer');
  const setters = availableReps.filter(rep => rep.role === 'setter');

  return (
    <div className="flex flex-col md:flex-row gap-4 flex-wrap">
      {/* Time Range Filter */}
      <div className="flex items-center space-x-1" role="group" aria-label="Filter analytics by time range">
        {timeRanges.map((range, index) => {
          const isActive = selectedTimeRange === range.value;
          const isFirst = index === 0;

          return (
            <Button
              key={range.value}
              variant={isActive ? 'default' : 'outline'}
              size="sm"
              onClick={() => onTimeRangeChange(range.value)}
              disabled={isLoading}
              className={`
                px-4 py-2 text-sm font-medium transition-colors h-10
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
            disabled={isLoading}
            className={`
              px-4 py-2 text-sm font-medium transition-colors h-10
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

      {/* Office Multi-Select Filter */}
      <OfficeMultiSelect
        selectedOfficeIds={selectedOfficeIds}
        availableOffices={availableOffices}
        onChange={onOfficeIdsChange}
        disabled={isLoading}
        placeholder="Select offices"
      />

      {/* Rep Filter */}
      <Select
        value={selectedRepEmail || 'all'}
        onValueChange={(value) => onRepEmailChange(value === 'all' ? undefined : value)}
        disabled={isLoading}
      >
        <SelectTrigger className="w-full md:w-[200px] h-10">
          <SelectValue placeholder="Select rep" />
        </SelectTrigger>
        <SelectContent>
          <SelectItem value="all">All Reps ({availableReps.length})</SelectItem>
          {closers.length > 0 && (
            <SelectGroup>
              <SelectLabel>Closers ({closers.length})</SelectLabel>
              {closers.map((rep) => (
                <SelectItem key={rep.email} value={rep.email}>
                  {rep.name}
                </SelectItem>
              ))}
            </SelectGroup>
          )}
          {setters.length > 0 && (
            <SelectGroup>
              <SelectLabel>Setters ({setters.length})</SelectLabel>
              {setters.map((rep) => (
                <SelectItem key={rep.email} value={rep.email}>
                  {rep.name}
                </SelectItem>
              ))}
            </SelectGroup>
          )}
        </SelectContent>
      </Select>
    </div>
  );
}
