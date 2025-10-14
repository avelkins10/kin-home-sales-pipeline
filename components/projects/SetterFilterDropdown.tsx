'use client';

import { useRouter, useSearchParams } from 'next/navigation';
import { useQuery } from '@tanstack/react-query';
import { useSession } from 'next-auth/react';
import { UserCheck, Check, ChevronDown } from 'lucide-react';
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

interface SetterFilterDropdownProps {
  isFetching?: boolean;
}

interface SetterOption {
  email: string;
  name: string;
  count: number;
}

export function SetterFilterDropdown({ isFetching = false }: SetterFilterDropdownProps) {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { data: session } = useSession();

  const currentSetter = searchParams.get('setter') || 'all';

  // Fetch projects to get unique setters
  const { data: projects = [] } = useQuery({
    queryKey: ['project-setters', session?.user?.quickbaseUserId, session?.user?.role],
    queryFn: async () => {
      if (!session?.user?.quickbaseUserId || !session?.user?.role) return [];
      const response = await fetch('/api/projects');
      if (!response.ok) return [];
      return response.json();
    },
    enabled: !!session?.user?.quickbaseUserId && !!session?.user?.role,
    staleTime: 300000, // 5 minutes
  });

  // Extract unique setters from projects (email + name)
  const setterMap = new Map<string, SetterOption>();

  projects.forEach((p: QuickbaseProject) => {
    const setterEmail = p[PROJECT_FIELDS.SETTER_EMAIL]?.value;
    const setterName = p[PROJECT_FIELDS.SETTER_NAME]?.value;

    if (setterEmail && setterEmail.trim() !== '') {
      if (!setterMap.has(setterEmail)) {
        setterMap.set(setterEmail, {
          email: setterEmail,
          name: setterName || setterEmail,
          count: 0,
        });
      }
      const setter = setterMap.get(setterEmail)!;
      setter.count++;
    }
  });

  // Convert to array and sort by name
  const setters: SetterOption[] = Array.from(setterMap.values()).sort((a, b) =>
    a.name.localeCompare(b.name)
  );

  const handleSetterChange = (setterEmail: string) => {
    const params = new URLSearchParams(searchParams.toString());

    if (setterEmail === 'all') {
      params.delete('setter');
    } else {
      params.set('setter', setterEmail);
    }

    router.push(`/projects?${params.toString()}`);
  };

  const getSetterCount = (setterEmail: string) => {
    if (setterEmail === 'all') return projects.length;
    return projects.filter(
      (p: QuickbaseProject) => p[PROJECT_FIELDS.SETTER_EMAIL]?.value === setterEmail
    ).length;
  };

  // Find current setter's display name
  const currentSetterObj = setters.find(s => s.email === currentSetter);
  const selectedSetterLabel = currentSetter === 'all' ? 'All Setters' : (currentSetterObj?.name || currentSetter);
  const selectedCount = getSetterCount(currentSetter);

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
            <UserCheck className="h-4 w-4 text-slate-600" />
            <span className="font-medium text-slate-700">{selectedSetterLabel}</span>
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
          onClick={() => handleSetterChange('all')}
          className={cn(
            'cursor-pointer',
            currentSetter === 'all' && 'bg-slate-100'
          )}
        >
          <div className="flex items-center justify-between w-full">
            <span>All Setters</span>
            <div className="flex items-center gap-2">
              <span className="text-xs text-slate-500">{projects.length}</span>
              {currentSetter === 'all' && <Check className="h-4 w-4 text-indigo-600" />}
            </div>
          </div>
        </DropdownMenuItem>

        {setters.length > 0 && <DropdownMenuSeparator />}

        {setters.map((setter: SetterOption) => (
          <DropdownMenuItem
            key={setter.email}
            onClick={() => handleSetterChange(setter.email)}
            className={cn(
              'cursor-pointer',
              currentSetter === setter.email && 'bg-slate-100'
            )}
          >
            <div className="flex items-center justify-between w-full">
              <div className="flex flex-col">
                <span className="text-sm">{setter.name}</span>
                {setter.name !== setter.email && (
                  <span className="text-xs text-slate-500">{setter.email}</span>
                )}
              </div>
              <div className="flex items-center gap-2">
                <span className="text-xs text-slate-500">{setter.count}</span>
                {currentSetter === setter.email && <Check className="h-4 w-4 text-indigo-600" />}
              </div>
            </div>
          </DropdownMenuItem>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
