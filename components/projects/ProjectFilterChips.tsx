'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { cn } from '@/lib/utils/cn';
import { Loader2 } from 'lucide-react';

const filterChips = [
  { value: 'all', label: 'All Projects', count: null },
  { value: 'active', label: 'Active', count: null },
  { value: 'on-hold', label: 'On Hold', count: null },
  { value: 'install-ready', label: 'Install Ready', count: null },
  { value: 'install-scheduled', label: 'Install Scheduled', count: null },
  { value: 'install-completed', label: 'Install Completed', count: null },
  { value: 'pending-cancel', label: 'Pending Cancel', count: null },
  { value: 'cancelled', label: 'Cancelled', count: null },
  { value: 'needs-attention', label: 'Needs Attention', count: null },
];

export function ProjectFilterChips({ isFetching = false }: { isFetching?: boolean }) {
  const router = useRouter();
  const searchParams = useSearchParams();

  // Get current view parameter, default to 'all'
  const currentView = searchParams.get('view') || 'all';

  const handleFilterChange = (value: string) => {
    const params = new URLSearchParams(searchParams.toString());

    if (value === 'all') {
      params.delete('view');
    } else {
      params.set('view', value);
    }

    router.push(`/projects?${params.toString()}`);
  };

  return (
    <div className="relative">
      {/* Loading overlay */}
      {isFetching && (
        <div className="absolute inset-0 bg-white/60 backdrop-blur-sm z-20 flex items-center justify-center pointer-events-auto">
          <div className="flex items-center gap-2 text-indigo-600">
            <Loader2 className="h-4 w-4 animate-spin" />
            <span className="text-sm font-medium">Loading...</span>
          </div>
        </div>
      )}
      
      {/* Fade gradient on left */}
      <div className="absolute left-0 top-0 bottom-0 w-8 bg-gradient-to-r from-slate-50 to-transparent pointer-events-none z-10" />

      {/* Scrollable chips container */}
      <div className="flex gap-2 overflow-x-auto pb-2 scrollbar-hide">
        {filterChips.map((chip) => {
          const isActive = currentView === chip.value;

          return (
            <button
              key={chip.value}
              onClick={() => handleFilterChange(chip.value)}
              disabled={isFetching}
              className={cn(
                'px-4 py-2 rounded-full text-sm font-medium whitespace-nowrap',
                'transition-all duration-200 ease-in-out',
                'focus:outline-none focus:ring-2 focus:ring-indigo-500 focus:ring-offset-2',
                'disabled:opacity-50 disabled:cursor-not-allowed',
                isActive
                  ? 'bg-indigo-600 text-white shadow-md hover:bg-indigo-700 scale-105'
                  : 'bg-white text-slate-700 border border-slate-200 hover:border-indigo-300 hover:bg-indigo-50 hover:text-indigo-700 shadow-sm'
              )}
              aria-pressed={isActive}
              aria-label={`Filter by ${chip.label}`}
            >
              {chip.label}
            </button>
          );
        })}
      </div>

      {/* Fade gradient on right */}
      <div className="absolute right-0 top-0 bottom-0 w-8 bg-gradient-to-l from-slate-50 to-transparent pointer-events-none z-10" />
    </div>
  );
}
