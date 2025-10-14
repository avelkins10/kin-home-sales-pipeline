'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { cn } from '@/lib/utils/cn';
import { Loader2, X } from 'lucide-react';
import { Badge } from '@/components/ui/badge';
import { QuickbaseProject } from '@/lib/types/project';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';

const filterChips = [
  { value: 'all', label: 'All Projects', color: 'slate' },
  { value: 'active', label: 'Active (In Progress)', color: 'emerald' },
  { value: 'on-hold', label: 'On Hold', color: 'amber' },
  { value: 'install-completed', label: 'Installed', color: 'blue' },
  { value: 'pending-cancel', label: 'Pending Cancel', color: 'orange' },
  { value: 'cancelled', label: 'Cancelled', color: 'rose' },
  { value: 'needs-attention', label: 'Needs Attention', color: 'red' },
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
  
  // Get current ownership parameter, default to 'all'
  const currentOwnership = searchParams.get('ownership') || 'all';

  // Fetch all projects for count calculation (lightweight query)
  const { data: projects = [] } = useQuery({
    queryKey: ['project-counts', session?.user?.quickbaseUserId, session?.user?.role, currentOwnership],
    queryFn: async () => {
      if (!session?.user?.quickbaseUserId || !session?.user?.role) return [];
      // Note: API gets userId/role from session, not query params
      const params = new URLSearchParams();
      if (currentOwnership && currentOwnership !== 'all') {
        params.set('ownership', currentOwnership);
      }
      const response = await fetch(`/api/projects?${params.toString()}`);
      if (!response.ok) return [];
      return response.json();
    },
    enabled: !!session?.user?.quickbaseUserId && !!session?.user?.role,
    staleTime: 300000, // 5 minutes
  });

  // Helper function to check if project matches filter
  // Uses PROJECT_STATUS as the master status field
  const projectMatchesFilter = (project: QuickbaseProject, filterValue: string): boolean => {
    const status = project[PROJECT_FIELDS.PROJECT_STATUS]?.value || '';
    const financeIntakeApproved = project[PROJECT_FIELDS.FINANCE_INTAKE_APPROVED]?.value === 'Yes';
    const webhookIntakeComplete = project[PROJECT_FIELDS.WEBHOOK_INTAKE_COMPLETE]?.value === 'Yes';
    const isIntakeApproved = financeIntakeApproved || webhookIntakeComplete;
    const installCompletedDate = project[PROJECT_FIELDS.INSTALL_COMPLETED_DATE]?.value;
    const ptoApproved = project[PROJECT_FIELDS.PTO_APPROVED]?.value;

    switch (filterValue) {
      case 'all':
        return true;
      case 'active':
        // Active projects that are NOT on hold, NOT installed, NOT PTO
        // Includes: "Active" but excludes "Active - On Hold", "Active - Installed", "Active - PTO"
        return status.startsWith('Active') &&
               !status.includes('On Hold') &&
               !status.includes('Installed') &&
               !status.includes('PTO') &&
               isIntakeApproved;
      case 'on-hold':
        // PROJECT_STATUS contains "On Hold"
        return status.includes('On Hold');
      case 'install-completed':
        // PROJECT_STATUS = "Active - Installed" OR has install completed date
        return status.includes('Installed') || (!!installCompletedDate && !ptoApproved);
      case 'pending-cancel':
        // PROJECT_STATUS contains "Pending Cancel"
        return status.includes('Pending Cancel');
      case 'cancelled':
        // PROJECT_STATUS = "Cancelled" (not pending)
        return status === 'Cancelled' || (status.includes('Cancel') && !status.includes('Pending'));
      case 'needs-attention':
        // Projects >90 days old OR on hold >7 days
        const projectAge = parseInt(project[PROJECT_FIELDS.PROJECT_AGE]?.value || '0');
        const dateOnHold = project[PROJECT_FIELDS.DATE_ON_HOLD]?.value;
        const sevenDaysAgo = new Date(Date.now() - 7 * 24 * 60 * 60 * 1000);
        const isOldHold = status.includes('On Hold') && dateOnHold && new Date(dateOnHold) < sevenDaysAgo;
        return projectAge > 90 || isOldHold;
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

  const handleRemoveOwnershipFilter = () => {
    const params = new URLSearchParams(searchParams.toString());
    params.delete('ownership');
    router.push(`/projects?${params.toString()}`);
  };

  // Color mapping for active state
  const getActiveColors = (color: string) => {
    const colorMap: Record<string, { bg: string; hover: string; ring: string; badge: string }> = {
      slate: { bg: 'bg-slate-600', hover: 'hover:bg-slate-700', ring: 'focus:ring-slate-500', badge: 'bg-slate-500' },
      emerald: { bg: 'bg-emerald-600', hover: 'hover:bg-emerald-700', ring: 'focus:ring-emerald-500', badge: 'bg-emerald-500' },
      amber: { bg: 'bg-amber-500', hover: 'hover:bg-amber-600', ring: 'focus:ring-amber-400', badge: 'bg-amber-400' },
      blue: { bg: 'bg-blue-600', hover: 'hover:bg-blue-700', ring: 'focus:ring-blue-500', badge: 'bg-blue-500' },
      orange: { bg: 'bg-orange-500', hover: 'hover:bg-orange-600', ring: 'focus:ring-orange-400', badge: 'bg-orange-400' },
      rose: { bg: 'bg-rose-500', hover: 'hover:bg-rose-600', ring: 'focus:ring-rose-400', badge: 'bg-rose-400' },
      red: { bg: 'bg-red-600', hover: 'hover:bg-red-700', ring: 'focus:ring-red-500', badge: 'bg-red-500' },
    };
    return colorMap[color] || colorMap.slate;
  };

  // Color mapping for inactive state (subtle)
  const getInactiveColors = (color: string) => {
    const colorMap: Record<string, { border: string; hover: string; text: string }> = {
      slate: { border: 'border-slate-200', hover: 'hover:border-slate-300 hover:bg-slate-50', text: 'hover:text-slate-900' },
      emerald: { border: 'border-emerald-200', hover: 'hover:border-emerald-300 hover:bg-emerald-50', text: 'hover:text-emerald-700' },
      amber: { border: 'border-amber-200', hover: 'hover:border-amber-300 hover:bg-amber-50', text: 'hover:text-amber-700' },
      blue: { border: 'border-blue-200', hover: 'hover:border-blue-300 hover:bg-blue-50', text: 'hover:text-blue-700' },
      orange: { border: 'border-orange-200', hover: 'hover:border-orange-300 hover:bg-orange-50', text: 'hover:text-orange-700' },
      rose: { border: 'border-rose-200', hover: 'hover:border-rose-300 hover:bg-rose-50', text: 'hover:text-rose-700' },
      red: { border: 'border-red-200', hover: 'hover:border-red-300 hover:bg-red-50', text: 'hover:text-red-700' },
    };
    return colorMap[color] || colorMap.slate;
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

      {/* Active Filters Display */}
      {(currentOwnership !== 'all' || searchParams.get('memberEmail') || searchParams.get('office')) && (
        <div className="flex items-center gap-2 mb-3">
          <span className="text-sm text-gray-600 font-medium">Active Filters:</span>
          <div className="flex gap-2 flex-wrap">
            {currentOwnership !== 'all' && (
              <Badge
                variant="secondary"
                className="px-3 py-1.5 cursor-pointer hover:bg-gray-200 transition-colors flex items-center gap-2"
                onClick={handleRemoveOwnershipFilter}
                data-testid="active-filter-chip"
              >
                <span className="text-sm">
                  {currentOwnership === 'my-projects' ? 'My Projects' : 'Team Projects'}
                </span>
                <X className="h-3 w-3" />
              </Badge>
            )}
            {searchParams.get('office') && (
              <Badge
                variant="secondary"
                className="px-3 py-1.5 cursor-pointer hover:bg-gray-200 transition-colors flex items-center gap-2"
                onClick={() => {
                  const params = new URLSearchParams(searchParams.toString());
                  params.delete('office');
                  router.push(`/projects?${params.toString()}`);
                }}
              >
                <span className="text-sm">Office: {searchParams.get('office')}</span>
                <X className="h-3 w-3" />
              </Badge>
            )}
            {searchParams.get('memberEmail') && (
              <Badge
                variant="secondary"
                className="px-3 py-1.5 cursor-pointer hover:bg-gray-200 transition-colors flex items-center gap-2"
                onClick={() => {
                  const params = new URLSearchParams(searchParams.toString());
                  params.delete('memberEmail');
                  router.push(`/projects?${params.toString()}`);
                }}
              >
                <span className="text-sm">Team Member: {searchParams.get('memberEmail')}</span>
                <X className="h-3 w-3" />
              </Badge>
            )}
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
          const activeColors = getActiveColors(chip.color);
          const inactiveColors = getInactiveColors(chip.color);

          return (
            <button
              key={chip.value}
              onClick={() => handleFilterChange(chip.value)}
              disabled={isFetching}
              className={cn(
                'px-4 py-2 rounded-lg text-sm font-medium whitespace-nowrap',
                'transition-all duration-200 ease-in-out',
                'focus:outline-none focus:ring-2 focus:ring-offset-2',
                'disabled:opacity-50 disabled:cursor-not-allowed',
                'flex items-center gap-2',
                isActive
                  ? `${activeColors.bg} text-white shadow-md ${activeColors.hover} ${activeColors.ring}`
                  : `bg-white text-slate-700 border ${inactiveColors.border} ${inactiveColors.hover} ${inactiveColors.text} shadow-sm`
              )}
              aria-pressed={isActive}
              aria-label={`Filter by ${chip.label}`}
            >
              <span>{chip.label}</span>
              {projects.length > 0 && (
                <span
                  className={cn(
                    'px-2 py-0.5 rounded-md text-xs font-semibold',
                    isActive
                      ? `${activeColors.badge} text-white`
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
