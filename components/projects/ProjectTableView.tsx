'use client';

import React from 'react';
import { useQuery } from '@tanstack/react-query';
// Removed offline import - using API route instead
import { ProjectRow } from './ProjectRow';
import { projectsListKey } from '@/lib/queryKeys';
import { Skeleton } from '@/components/ui/skeleton';
import { Button } from '@/components/ui/button';
import { Loader2 } from 'lucide-react';

interface ProjectTableViewProps {
  userId: string;
  role: string;
  userEmail: string; // NEW: Current user's email for ownership determination
  view: string;
  search: string;
  sort: string;
  memberEmail?: string;
  ownership?: string; // NEW: Ownership filter (all | my-projects | team-projects)
  office?: string; // NEW: Office filter
  setter?: string; // NEW: Setter filter
  closer?: string; // NEW: Closer filter
  withTasks?: string; // NEW: Task filter
  dateFilter?: string; // NEW: Date filter
  startDate?: string; // NEW: Custom start date
  endDate?: string; // NEW: Custom end date
  onFetchingChange?: (isFetching: boolean, reason?: 'manual' | 'background') => void;
}

export function ProjectTableView({ userId, role, userEmail, view, search, sort, memberEmail, ownership = 'all', office, setter, closer, withTasks, dateFilter, startDate, endDate, onFetchingChange }: ProjectTableViewProps) {
  const [isManualRefetch, setIsManualRefetch] = React.useState(false);

  const { data: projects, isLoading, error, refetch, isFetching } = useQuery({
    queryKey: projectsListKey(userId, role, view, search, sort, memberEmail, ownership, office, setter, closer, withTasks, dateFilter, startDate, endDate),
    queryFn: async () => {
      const params = new URLSearchParams();
      if (view && view !== 'all') params.set('view', view);
      if (search) params.set('search', search);
      if (sort && sort !== 'default') params.set('sort', sort);
      if (memberEmail) params.set('memberEmail', memberEmail);
      if (ownership && ownership !== 'all') params.set('ownership', ownership);
      if (office) params.set('office', office);
      if (setter) params.set('setter', setter);
      if (closer) params.set('closer', closer);
      if (withTasks === 'true') params.set('withTasks', 'true');
      if (dateFilter) params.set('dateFilter', dateFilter);
      if (startDate) params.set('startDate', startDate);
      if (endDate) params.set('endDate', endDate);

      const response = await fetch(`/api/projects?${params.toString()}`);
      if (!response.ok) {
        throw new Error(`Failed to fetch projects: ${response.statusText}`);
      }
      return response.json();
    },
    staleTime: 300000, // 5 minutes - relies on global defaults, no auto-refresh
  });

  // Notify parent of fetching state changes with reason
  React.useEffect(() => {
    const reason = isManualRefetch ? 'manual' : 'background';
    onFetchingChange?.(isFetching, reason);
    if (!isFetching && isManualRefetch) {
      setIsManualRefetch(false);
    }
  }, [isFetching, onFetchingChange, isManualRefetch]);

  // Loading state
  if (isLoading) {
    return (
      <div className="space-y-4">
        {Array.from({ length: 6 }).map((_, index) => (
          <div key={index} className="bg-white rounded-lg border p-4 space-y-3">
            {/* Customer section skeleton */}
            <div className="space-y-2">
              <Skeleton className="h-4 w-32" />
              <Skeleton className="h-3 w-48" />
              <Skeleton className="h-3 w-36" />
            </div>
            
            {/* Traffic lights skeleton */}
            <div className="flex items-center gap-2">
              {Array.from({ length: 7 }).map((_, i) => (
                <Skeleton key={i} className="h-8 w-8 rounded-full" />
              ))}
            </div>
            
            {/* Metrics skeleton */}
            <div className="flex justify-between">
              <div className="space-y-1">
                <Skeleton className="h-3 w-16" />
                <Skeleton className="h-3 w-20" />
                <Skeleton className="h-3 w-24" />
              </div>
              <Skeleton className="h-6 w-12" />
            </div>
          </div>
        ))}
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div className="flex flex-col items-center justify-center py-16 px-4">
        <div className="bg-rose-50 rounded-full p-4 mb-4">
          <svg
            className="w-8 h-8 text-rose-600"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M12 9v2m0 4h.01m-6.938 4h13.856c1.54 0 2.502-1.667 1.732-3L13.732 4c-.77-1.333-2.694-1.333-3.464 0L3.34 16c-.77 1.333.192 3 1.732 3z"
            />
          </svg>
        </div>
        <h3 className="text-lg font-semibold text-slate-900 mb-2">Failed to load projects</h3>
        <p className="text-sm text-slate-600 mb-6 max-w-md text-center">
          We encountered an error while loading your projects. This might be a temporary issue.
        </p>
        <Button
          onClick={() => window.location.reload()}
          className="bg-indigo-600 hover:bg-indigo-700 text-white shadow-sm"
        >
          Try Again
        </Button>
      </div>
    );
  }

  // Empty state
  if (!projects || projects.length === 0) {
    return (
      <div className="flex flex-col items-center justify-center py-16 px-4">
        <div className="bg-slate-100 rounded-full p-4 mb-4">
          <svg
            className="w-8 h-8 text-slate-400"
            fill="none"
            stroke="currentColor"
            viewBox="0 0 24 24"
          >
            <path
              strokeLinecap="round"
              strokeLinejoin="round"
              strokeWidth={2}
              d="M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z"
            />
          </svg>
        </div>
        <h3 className="text-lg font-semibold text-slate-900 mb-2">No projects found</h3>
        <p className="text-sm text-slate-600 mb-6 max-w-md text-center">
          We couldn&apos;t find any projects matching your current filters. Try adjusting your search or filter criteria.
        </p>
        <Button
          onClick={() => (window.location.href = '/projects')}
          variant="outline"
          className="border-slate-300 hover:bg-slate-50"
        >
          Clear Filters
        </Button>
      </div>
    );
  }

  // Success state
  return (
    <div className="space-y-4">
      {/* Option B: Add a refresh button */}
      <div className="flex justify-end">
        <Button 
          onClick={() => {
            setIsManualRefetch(true);
            refetch();
          }} 
          variant="outline" 
          size="sm"
          disabled={isFetching}
          className={isFetching ? "opacity-75 cursor-wait" : ""}
        >
          {isFetching ? (
            <>
              <Loader2 className="h-4 w-4 mr-2 animate-spin" />
              Refreshing...
            </>
          ) : (
            'Refresh'
          )}
        </Button>
      </div>
      {projects.map((project: any) => {
        const recordId = project[3]?.value; // RECORD_ID field
        return (
          <ProjectRow 
            key={recordId} 
            project={project} 
            userEmail={userEmail} // NEW: Pass user email for ownership determination
            userRole={role} // NEW: Pass role for manager-specific logic
          />
        );
      })}
    </div>
  );
}
