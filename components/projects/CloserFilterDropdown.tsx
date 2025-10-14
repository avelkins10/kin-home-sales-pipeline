'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { UserCircle, Check, ChevronDown } from 'lucide-react';
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

interface CloserFilterDropdownProps {
  isFetching?: boolean;
}

interface CloserOption {
  email: string;
  name: string;
  count: number;
}

export function CloserFilterDropdown({ isFetching = false }: CloserFilterDropdownProps) {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { data: session } = useSession();

  const currentCloser = searchParams.get('closer') || 'all';

  // Fetch projects to get unique closers
  const { data: projects = [] } = useQuery({
    queryKey: ['project-closers', session?.user?.quickbaseUserId, session?.user?.role],
    queryFn: async () => {
      if (!session?.user?.quickbaseUserId || !session?.user?.role) return [];
      const response = await fetch('/api/projects');
      if (!response.ok) return [];
      return response.json();
    },
    enabled: !!session?.user?.quickbaseUserId && !!session?.user?.role,
    staleTime: 300000, // 5 minutes
  });

  // Extract unique closers from projects (email + name)
  const closerMap = new Map<string, CloserOption>();

  projects.forEach((p: QuickbaseProject) => {
    const closerEmail = p[PROJECT_FIELDS.CLOSER_EMAIL]?.value;
    const closerName = p[PROJECT_FIELDS.CLOSER_NAME]?.value;

    if (closerEmail && closerEmail.trim() !== '') {
      if (!closerMap.has(closerEmail)) {
        closerMap.set(closerEmail, {
          email: closerEmail,
          name: closerName || closerEmail,
          count: 0,
        });
      }
      const closer = closerMap.get(closerEmail)!;
      closer.count++;
    }
  });

  // Convert to array and sort by name
  const closers: CloserOption[] = Array.from(closerMap.values()).sort((a, b) =>
    a.name.localeCompare(b.name)
  );

  const handleCloserChange = (closerEmail: string) => {
    const params = new URLSearchParams(searchParams.toString());

    if (closerEmail === 'all') {
      params.delete('closer');
    } else {
      params.set('closer', closerEmail);
    }

    router.push(`/projects?${params.toString()}`);
  };

  const getCloserCount = (closerEmail: string) => {
    if (closerEmail === 'all') return projects.length;
    return projects.filter(
      (p: QuickbaseProject) => p[PROJECT_FIELDS.CLOSER_EMAIL]?.value === closerEmail
    ).length;
  };

  // Find current closer's display name
  const currentCloserObj = closers.find(c => c.email === currentCloser);
  const selectedCloserLabel = currentCloser === 'all' ? 'All Closers' : (currentCloserObj?.name || currentCloser);
  const selectedCount = getCloserCount(currentCloser);

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
            <UserCircle className="h-4 w-4 text-slate-600" />
            <span className="font-medium text-slate-700">{selectedCloserLabel}</span>
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
      <DropdownMenuContent align="start" className="w-56 max-h-96 overflow-y-auto">
        <DropdownMenuItem
          onClick={() => handleCloserChange('all')}
          className={cn(
            'cursor-pointer',
            currentCloser === 'all' && 'bg-slate-100'
          )}
        >
          <div className="flex items-center justify-between w-full">
            <span>All Closers</span>
            <div className="flex items-center gap-2">
              <span className="text-xs text-slate-500">{projects.length}</span>
              {currentCloser === 'all' && <Check className="h-4 w-4 text-indigo-600" />}
            </div>
          </div>
        </DropdownMenuItem>

        {closers.length > 0 && <DropdownMenuSeparator />}

        {closers.map((closer: CloserOption) => (
          <DropdownMenuItem
            key={closer.email}
            onClick={() => handleCloserChange(closer.email)}
            className={cn(
              'cursor-pointer',
              currentCloser === closer.email && 'bg-slate-100'
            )}
          >
            <div className="flex items-center justify-between w-full">
              <div className="flex flex-col">
                <span className="text-sm">{closer.name}</span>
                {closer.name !== closer.email && (
                  <span className="text-xs text-slate-500">{closer.email}</span>
                )}
              </div>
              <div className="flex items-center gap-2">
                <span className="text-xs text-slate-500">{closer.count}</span>
                {currentCloser === closer.email && <Check className="h-4 w-4 text-indigo-600" />}
              </div>
            </div>
          </DropdownMenuItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
