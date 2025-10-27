'use client'

import { Suspense, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery } from '@tanstack/react-query';
import { Skeleton } from '@/components/ui/skeleton';
import { Button } from '@/components/ui/button';
import { RefreshCw } from 'lucide-react';
import { cn } from '@/lib/utils';
import { syncUserTimezone } from '@/lib/utils/timezone';
import { isManagerRole } from '@/lib/utils/role-helpers';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { useRealtimeUpdates } from '@/lib/hooks/useRealtimeUpdates';
import {
  PCMetricsCards,
  PCMetricsSkeleton,
  PCPriorityQueue,
  PCPriorityQueueSkeleton,
  PCProjectPipeline,
  PCProjectPipelineSkeleton
} from '@/components/operations';
import type { PCDashboardData } from '@/lib/types/operations';

// PC Dashboard Skeleton
function PCDashboardSkeleton() {
  return (
    <div className="space-y-4 mobile:space-y-6">
      {/* Metrics Skeleton */}
      <PCMetricsSkeleton />

      {/* Two Column Layout Skeleton */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 mobile:gap-6">
        <PCPriorityQueueSkeleton />
        <PCProjectPipelineSkeleton />
      </div>
    </div>
  );
}

export default function OperationsPage() {
  const { data: session, status } = useSession();

  // Sync user timezone on mount (must be before early returns)
  useEffect(() => {
    if (session?.user?.timezone) {
      syncUserTimezone(session.user.timezone);
    } else {
      // Fallback: log missing timezone data for debugging
      console.warn('User timezone not available in session, using browser default');
    }
  }, [session]);

  // Fetch PC dashboard data
  const { data: dashboardData, isLoading, error } = useQuery<PCDashboardData>({
    queryKey: ['pc-dashboard', session?.user?.email],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/operations/dashboard`);
      if (!response.ok) throw new Error('Failed to fetch PC dashboard data');
      return response.json();
    },
    enabled: !!session?.user?.email,
  });

  // Enable real-time updates for dashboard data
  const { isRefreshing, manualRefresh } = useRealtimeUpdates({
    queryKeys: [['pc-dashboard', session?.user?.email]],
    interval: 30000, // 30 seconds
    enabled: !!session?.user?.email
  });

  if (status === 'loading' || isLoading) {
    return (
      <div className="space-y-4 mobile:space-y-6">
        <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
          <div>
            <div className="h-8 w-48 bg-gray-200 rounded animate-pulse mb-2"></div>
            <div className="h-4 w-64 bg-gray-200 rounded animate-pulse"></div>
          </div>
        </div>
        <PCDashboardSkeleton />
      </div>
    );
  }

  if (error) {
    return (
      <div className="space-y-4 mobile:space-y-6">
        <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
          <div>
            <h1 className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
              Welcome back, {session?.user?.name}
            </h1>
            <p className="text-sm mobile:text-base text-gray-600 mt-0.5">
              PC Operations Dashboard
            </p>
          </div>
        </div>
        <div className="p-6 border rounded-lg bg-red-50">
          <p className="text-red-600">Error loading dashboard data. Please try again.</p>
        </div>
      </div>
    );
  }

  const isManager = isManagerRole(session?.user?.role);

  const getRoleDisplayName = (role: string) => {
    switch (role) {
      case 'operations_coordinator':
        return 'Operations Coordinator Dashboard';
      case 'operations_manager':
        return 'Operations Manager Dashboard';
      case 'office_leader':
        return 'Office Leader Dashboard';
      case 'regional':
        return 'Regional Manager Dashboard';
      case 'super_admin':
        return 'Super Admin Dashboard';
      default:
        return 'Operations Dashboard';
    }
  };

  return (
    <div className="space-y-4 mobile:space-y-6">
      {/* Page Header */}
      <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
        <div>
          <h1 className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
            Welcome back, {session?.user?.name}
          </h1>
          <p className="text-sm mobile:text-base text-gray-600 mt-0.5">
            {getRoleDisplayName(session?.user?.role || '')} -
            {isManager ? 'Team Operations Dashboard' : 'Your Operations Dashboard'}
          </p>
        </div>
        <Button
          variant="outline"
          size="sm"
          onClick={manualRefresh}
          disabled={isRefreshing}
          className="flex items-center gap-2"
        >
          <RefreshCw className={cn("h-4 w-4", isRefreshing && "animate-spin")} />
          Refresh
        </Button>
      </div>

      {/* PC Metrics Cards */}
      <Suspense fallback={<PCMetricsSkeleton />}>
        <PCMetricsCards metrics={dashboardData?.metrics || {
          totalProjects: 0,
          pendingOutreach: 0,
          unresponsiveCustomers: 0,
          activeEscalations: 0,
          todaysInstalls: 0,
          slaCompliance: 0
        }} />
      </Suspense>

      {/* Two Column Layout */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 mobile:gap-6">
        {/* Left Column - Priority Queue */}
        <Suspense fallback={<PCPriorityQueueSkeleton />}>
          <PCPriorityQueue priorityQueue={dashboardData?.priorityQueue || []} />
        </Suspense>

        {/* Right Column - Project Pipeline */}
        <Suspense fallback={<PCProjectPipelineSkeleton />}>
          <PCProjectPipeline pipeline={dashboardData?.pipeline || []} />
        </Suspense>
      </div>
    </div>
  );
}
