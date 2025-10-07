'use client';

import { useQuery } from '@tanstack/react-query';
import { getProjectsForUserOffline } from '@/lib/offline/offlineQueries';
import { ProjectRow } from './ProjectRow';
import { Skeleton } from '@/components/ui/skeleton';
import { Button } from '@/components/ui/button';

interface ProjectTableViewProps {
  userId: string;
  role: string;
  view: string;
  search: string;
}

export function ProjectTableView({ userId, role, view, search }: ProjectTableViewProps) {
  const { data: projects, isLoading, error, refetch } = useQuery({
    queryKey: ['projects', userId, role, view, search],
    queryFn: () => getProjectsForUserOffline(userId, role, view, search),
    staleTime: 30000, // 30 seconds
    refetchInterval: 60000, // 1 minute for auto-refresh
  });

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
      {projects.map((project) => {
        const recordId = project[3]?.value; // RECORD_ID field
        return (
          <ProjectRow key={recordId} project={project} />
        );
      })}
    </div>
  );
}
