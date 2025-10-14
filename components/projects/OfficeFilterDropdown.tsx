'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { Building2, Check, ChevronDown } from 'lucide-react';
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

interface OfficeFilterDropdownProps {
  isFetching?: boolean;
}

export function OfficeFilterDropdown({ isFetching = false }: OfficeFilterDropdownProps) {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { data: session } = useSession();

  const currentOffice = searchParams.get('office') || 'all';

  // Fetch projects to get unique offices
  const { data: projects = [] } = useQuery({
    queryKey: ['project-offices', session?.user?.quickbaseUserId, session?.user?.role],
    queryFn: async () => {
      if (!session?.user?.quickbaseUserId || !session?.user?.role) return [];
      const response = await fetch('/api/projects');
      if (!response.ok) return [];
      return response.json();
    },
    enabled: !!session?.user?.quickbaseUserId && !!session?.user?.role,
    staleTime: 300000, // 5 minutes
  });

  // Extract unique offices from projects
  const offices: string[] = Array.from(
    new Set<string>(
      projects
        .map((p: QuickbaseProject) => p[PROJECT_FIELDS.SALES_OFFICE]?.value)
        .filter((office: any): office is string => !!office && office.trim() !== '')
    )
  ).sort();

  const handleOfficeChange = (office: string) => {
    const params = new URLSearchParams(searchParams.toString());

    if (office === 'all') {
      params.delete('office');
    } else {
      params.set('office', office);
    }

    router.push(`/projects?${params.toString()}`);
  };

  const getOfficeCount = (office: string) => {
    if (office === 'all') return projects.length;
    return projects.filter(
      (p: QuickbaseProject) => p[PROJECT_FIELDS.SALES_OFFICE]?.value === office
    ).length;
  };

  const selectedOfficeLabel = currentOffice === 'all' ? 'All Offices' : currentOffice;
  const selectedCount = getOfficeCount(currentOffice);

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
            <Building2 className="h-4 w-4 text-slate-600" />
            <span className="font-medium text-slate-700">{selectedOfficeLabel}</span>
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
      <DropdownMenuContent align="start" className="w-56">
        <DropdownMenuItem
          onClick={() => handleOfficeChange('all')}
          className={cn(
            'cursor-pointer',
            currentOffice === 'all' && 'bg-slate-100'
          )}
        >
          <div className="flex items-center justify-between w-full">
            <span>All Offices</span>
            <div className="flex items-center gap-2">
              <span className="text-xs text-slate-500">{projects.length}</span>
              {currentOffice === 'all' && <Check className="h-4 w-4 text-indigo-600" />}
            </div>
          </div>
        </DropdownMenuItem>

        {offices.length > 0 && <DropdownMenuSeparator />}

        {offices.map((office: string) => (
          <DropdownMenuItem
            key={office}
            onClick={() => handleOfficeChange(office)}
            className={cn(
              'cursor-pointer',
              currentOffice === office && 'bg-slate-100'
            )}
          >
            <div className="flex items-center justify-between w-full">
              <span>{office}</span>
              <div className="flex items-center gap-2">
                <span className="text-xs text-slate-500">{getOfficeCount(office)}</span>
                {currentOffice === office && <Check className="h-4 w-4 text-indigo-600" />}
              </div>
            </div>
          </DropdownMenuItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
