'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { MapPin, Check, ChevronDown } from 'lucide-react';
import { cn } from '@/lib/utils/cn';
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
  DropdownMenuSeparator,
} from '@/components/ui/dropdown-menu';
import { Button } from '@/components/ui/button';
import { PROJECT_FIELDS } from '@/lib/constants/fieldIds';
import { QuickbaseProject } from '@/lib/types/project';

interface StateFilterDropdownProps {
  isFetching?: boolean;
}

export function StateFilterDropdown({ isFetching = false }: StateFilterDropdownProps) {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { data: session } = useSession();

  const currentState = searchParams.get('state') || 'all';

  // Fetch projects to get unique states
  const { data: projects = [] } = useQuery({
    queryKey: ['project-states', session?.user?.quickbaseUserId, session?.user?.role],
    queryFn: async () => {
      if (!session?.user?.quickbaseUserId || !session?.user?.role) return [];
      const response = await fetch('/api/projects');
      if (!response.ok) return [];
      return response.json();
    },
    enabled: !!session?.user?.quickbaseUserId && !!session?.user?.role,
    staleTime: 300000, // 5 minutes
  });

  // Extract unique states from projects
  const states: string[] = Array.from(
    new Set<string>(
      projects
        .map((p: QuickbaseProject) => p[PROJECT_FIELDS.CUSTOMER_STATE]?.value)
        .filter((state: any): state is string => !!state && state.trim() !== '')
    )
  ).sort();

  const handleStateChange = (state: string) => {
    const params = new URLSearchParams(searchParams.toString());

    if (state === 'all') {
      params.delete('state');
    } else {
      params.set('state', state);
    }

    router.push(`/projects?${params.toString()}`);
  };

  const getStateCount = (state: string) => {
    if (state === 'all') return projects.length;
    return projects.filter(
      (p: QuickbaseProject) => p[PROJECT_FIELDS.CUSTOMER_STATE]?.value === state
    ).length;
  };

  const selectedStateLabel = currentState === 'all' ? 'All States' : currentState;
  const selectedCount = getStateCount(currentState);

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
            <MapPin className="h-4 w-4 text-slate-600" />
            <span className="font-medium text-slate-700">{selectedStateLabel}</span>
          </div>
          <div className="flex items-center gap-2">
            {projects.length > 0 && (
              <span className="px-2 py-0.5 rounded-md text-xs font-semibold bg-slate-100 text-slate-600">
                {selectedCount}
              </span>
            )}
            <ChevronDown className="h-4 w-4 text-slate-400" />
          </div>
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="start" className="w-56 max-h-[300px] overflow-y-auto">
        <DropdownMenuItem
          onClick={() => handleStateChange('all')}
          className={cn(
            'cursor-pointer',
            currentState === 'all' && 'bg-slate-100'
          )}
        >
          <div className="flex items-center justify-between w-full">
            <span>All States</span>
            <div className="flex items-center gap-2">
              <span className="text-xs text-slate-500">{projects.length}</span>
              {currentState === 'all' && <Check className="h-4 w-4 text-indigo-600" />}
            </div>
          </div>
        </DropdownMenuItem>

        {states.length > 0 && <DropdownMenuSeparator />}

        {states.map((state: string) => (
          <DropdownMenuItem
            key={state}
            onClick={() => handleStateChange(state)}
            className={cn(
              'cursor-pointer',
              currentState === state && 'bg-slate-100'
            )}
          >
            <div className="flex items-center justify-between w-full">
              <span>{state}</span>
              <div className="flex items-center gap-2">
                <span className="text-xs text-slate-500">{getStateCount(state)}</span>
                {currentState === state && <Check className="h-4 w-4 text-indigo-600" />}
              </div>
            </div>
          </DropdownMenuItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}



