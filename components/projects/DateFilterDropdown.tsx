'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { Calendar, Check, ChevronDown } from 'lucide-react';
import { cn } from '@/lib/utils/cn';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
  DropdownMenuSeparator,
} from '@/components/ui/dropdown-menu';
import { Button } from '@/components/ui/button';
import { useState } from 'react';

interface DateFilterDropdownProps {
  isFetching?: boolean;
}

type DateFilterOption = 'all' | 'this-week' | 'last-week' | 'this-month' | 'last-month' | 'custom';

interface DateFilterConfig {
  value: DateFilterOption;
  label: string;
}

const dateFilterOptions: DateFilterConfig[] = [
  { value: 'all', label: 'All Time' },
  { value: 'this-week', label: 'This Week' },
  { value: 'last-week', label: 'Last Week' },
  { value: 'this-month', label: 'This Month' },
  { value: 'last-month', label: 'Last Month' },
  { value: 'custom', label: 'Custom Range' },
];

export function DateFilterDropdown({ isFetching = false }: DateFilterDropdownProps) {
  const router = useRouter();
  const searchParams = useSearchParams();

  const currentFilter = (searchParams.get('dateFilter') || 'all') as DateFilterOption;
  const [showCustomPicker, setShowCustomPicker] = useState(false);
  const [customStartDate, setCustomStartDate] = useState(searchParams.get('startDate') || '');
  const [customEndDate, setCustomEndDate] = useState(searchParams.get('endDate') || '');

  const handleFilterChange = (filter: DateFilterOption) => {
    const params = new URLSearchParams(searchParams.toString());

    if (filter === 'all') {
      params.delete('dateFilter');
      params.delete('startDate');
      params.delete('endDate');
      router.push(`/projects?${params.toString()}`);
    } else if (filter === 'custom') {
      setShowCustomPicker(true);
    } else {
      params.set('dateFilter', filter);
      params.delete('startDate');
      params.delete('endDate');
      router.push(`/projects?${params.toString()}`);
    }
  };

  const handleCustomDateApply = () => {
    if (!customStartDate || !customEndDate) return;

    const params = new URLSearchParams(searchParams.toString());
    params.set('dateFilter', 'custom');
    params.set('startDate', customStartDate);
    params.set('endDate', customEndDate);
    router.push(`/projects?${params.toString()}`);
    setShowCustomPicker(false);
  };

  const getFilterLabel = () => {
    const option = dateFilterOptions.find(opt => opt.value === currentFilter);
    if (currentFilter === 'custom' && customStartDate && customEndDate) {
      return `${customStartDate} to ${customEndDate}`;
    }
    return option?.label || 'All Time';
  };

  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          className={cn(
            'w-full sm:w-auto justify-between gap-2',
            'bg-white hover:bg-slate-50',
            'border-slate-200 hover:border-slate-300',
            'transition-colors',
            isFetching && 'opacity-50 cursor-not-allowed'
          )}
          disabled={isFetching}
        >
          <div className="flex items-center gap-2">
            <Calendar className="h-4 w-4 text-slate-600" />
            <span className="font-medium text-slate-700">{getFilterLabel()}</span>
          </div>
          <ChevronDown className="h-4 w-4 text-slate-400" />
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="start" className="w-56">
        {showCustomPicker ? (
          <div className="p-3 space-y-3">
            <div className="space-y-2">
              <label className="text-xs font-medium text-slate-700">Start Date</label>
              <input
                type="date"
                value={customStartDate}
                onChange={(e) => setCustomStartDate(e.target.value)}
                className="w-full px-2 py-1 text-sm border border-slate-200 rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500"
              />
            </div>
            <div className="space-y-2">
              <label className="text-xs font-medium text-slate-700">End Date</label>
              <input
                type="date"
                value={customEndDate}
                onChange={(e) => setCustomEndDate(e.target.value)}
                className="w-full px-2 py-1 text-sm border border-slate-200 rounded-md focus:outline-none focus:ring-2 focus:ring-indigo-500"
              />
            </div>
            <div className="flex gap-2">
              <Button
                size="sm"
                variant="outline"
                onClick={() => setShowCustomPicker(false)}
                className="flex-1"
              >
                Cancel
              </Button>
              <Button
                size="sm"
                onClick={handleCustomDateApply}
                disabled={!customStartDate || !customEndDate}
                className="flex-1"
              >
                Apply
              </Button>
            </div>
          </div>
        ) : (
          <>
            {dateFilterOptions.map((option, index) => (
              <div key={option.value}>
                {index === 1 && <DropdownMenuSeparator />}
                {index === 5 && <DropdownMenuSeparator />}
                <DropdownMenuItem
                  onClick={() => handleFilterChange(option.value)}
                  className={cn(
                    'cursor-pointer',
                    currentFilter === option.value && 'bg-slate-100'
                  )}
                >
                  <div className="flex items-center justify-between w-full">
                    <span>{option.label}</span>
                    {currentFilter === option.value && <Check className="h-4 w-4 text-indigo-600" />}
                  </div>
                </DropdownMenuItem>
              </div>
            ))}
          </>
        )}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
