'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { cn } from '@/lib/utils/cn';
import { Loader2 } from 'lucide-react';
import { QuickbaseProject } from '@/lib/types/project';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

const filterChips = [
  { value: 'all', label: 'All Projects' },
  { value: 'active', label: 'Active' },
  { value: 'pending-kca', label: 'Pending KCA' },
  { value: 'rejected', label: 'Rejected' },
  { value: 'on-hold', label: 'On Hold' },
  { value: 'pending-cancel', label: 'Pending Cancel' },
  { value: 'install-ready', label: 'Install Ready' },
  { value: 'install-scheduled', label: 'Install Scheduled' },
  { value: 'install-completed', label: 'Install Completed' },
  { value: 'cancelled', label: 'Cancelled' },
  { value: 'needs-attention', label: 'Needs Attention' },
];

interface ProjectFilterChipsProps {
  isFetching?: boolean;
}

export function ProjectFilterChips({ isFetching = false }: ProjectFilterChipsProps) {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { data: session } = useSession();

  // Get current view parameter, default to 'all'
  const currentView = searchParams.get('view') || 'all';

  // Fetch all projects for count calculation (lightweight query)
  const { data: projects = [] } = useQuery({
    queryKey: ['project-counts', session?.user?.quickbaseUserId, session?.user?.role],
    queryFn: async () => {
      if (!session?.user?.quickbaseUserId || !session?.user?.role) return [];
      // Note: API gets userId/role from session, not query params
      const response = await fetch(`/api/projects`);
      if (!response.ok) return [];
      return response.json();
    },
    enabled: !!session?.user?.quickbaseUserId && !!session?.user?.role,
    staleTime: 300000, // 5 minutes
  });

  // Helper function to check if project matches filter
  const projectMatchesFilter = (project: QuickbaseProject, filterValue: string): boolean => {
    const status = (project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '').toLowerCase();
    const financeIntakeApproved = project[PROJECT_FIELDS.FINANCE_INTAKE_APPROVED]?.value;
    const webhookIntakeComplete = project[PROJECT_FIELDS.WEBHOOK_INTAKE_COMPLETE]?.value;
    const isIntakeApproved = !!(financeIntakeApproved || webhookIntakeComplete);

    switch (filterValue) {
      case 'all':
        return true;
      case 'active':
        return status === 'active' && isIntakeApproved;
      case 'pending-kca':
        // Pending KCA = Active status but intake NOT yet approved
        return status === 'active' && !isIntakeApproved;
      case 'rejected':
        return status.includes('reject');
      case 'on-hold':
        return status.includes('hold');
      case 'pending-cancel':
        return status.includes('pending cancel');
      case 'cancelled':
        return status.includes('cancel') && !status.includes('pending');
      case 'install-ready':
        return status.includes('install ready');
      case 'install-scheduled':
        return status.includes('install scheduled');
      case 'install-completed':
        return status.includes('install complete');
      case 'needs-attention':
        // Projects that need attention (on hold, pending cancel, rejected, or pending KCA > 7 days)
        return status.includes('hold') || status.includes('pending cancel') || status.includes('reject');
      default:
        return false;
    }
  };

  // Calculate counts for each filter
  const filterCounts = filterChips.reduce((acc, chip) => {
    acc[chip.value] = projects.filter((p: QuickbaseProject) => projectMatchesFilter(p, chip.value)).length;
    return acc;
  }, {} as Record<string, number>);

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
          const count = filterCounts[chip.value] || 0;

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
                'flex items-center gap-2',
                isActive
                  ? 'bg-indigo-600 text-white shadow-md hover:bg-indigo-700 scale-105'
                  : 'bg-white text-slate-700 border border-slate-200 hover:border-indigo-300 hover:bg-indigo-50 hover:text-indigo-700 shadow-sm'
              )}
              aria-pressed={isActive}
              aria-label={`Filter by ${chip.label}`}
            >
              <span>{chip.label}</span>
              {projects.length > 0 && (
                <span
                  className={cn(
                    'px-2 py-0.5 rounded-full text-xs font-semibold',
                    isActive
                      ? 'bg-indigo-500 text-white'
                      : 'bg-slate-100 text-slate-600'
                  )}
                >
                  {count}
                </span>
              )}
            </button>
          );
        })}
      </div>

      {/* Fade gradient on right */}
      <div className="absolute right-0 top-0 bottom-0 w-8 bg-gradient-to-l from-slate-50 to-transparent pointer-events-none z-10" />
    </div>
  );
}
