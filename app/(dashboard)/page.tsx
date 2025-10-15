'use client'

import { Suspense, useState, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { redirect } from 'next/navigation';
import { Skeleton } from '@/components/ui/skeleton';
import { DashboardMetrics } from '@/components/dashboard/DashboardMetrics';
import { UrgentAlerts } from '@/components/dashboard/UrgentAlerts';
import { RecentProjects } from '@/components/dashboard/RecentProjects';
import { NotificationsFeed } from '@/components/dashboard/NotificationsFeed';
import { TeamActivityFeed, TeamActivityFeedSkeleton } from '@/components/dashboard/TeamActivityFeed';
import { DashboardFilters } from '@/components/dashboard/DashboardFilters';
import { DashboardScopeToggle } from '@/components/dashboard/DashboardScopeToggle';
import { EnhancedDashboardData } from '@/components/dashboard/EnhancedDashboardData';
import { syncUserTimezone } from '@/lib/utils/timezone';
import { isManagerRole } from '@/lib/utils/role-helpers';
import type { TimeRange, MetricsScope } from '@/lib/types/dashboard';

// Skeleton components for loading states
function UrgentAlertsSkeleton() {
  return <Skeleton className="h-6 w-full" />;
}

function DashboardMetricsSkeleton() {
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
      {Array.from({ length: 4 }).map((_, i) => (
        <div key={i} className="p-6 border rounded-lg">
          <div className="flex items-center justify-between">
            <div className="space-y-2">
              <Skeleton className="h-4 w-20" />
              <Skeleton className="h-8 w-16" />
            </div>
            <Skeleton className="h-12 w-12 rounded-full" />
          </div>
        </div>
      ))}
    </div>
  );
}

function PerformanceMetricsSkeleton() {
  return (
    <div className="p-6 border rounded-lg">
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {Array.from({ length: 5 }).map((_, i) => (
          <div key={i} className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
            <Skeleton className="h-9 w-9 rounded-lg" />
            <div className="flex-1">
              <Skeleton className="h-4 w-20 mb-2" />
              <Skeleton className="h-8 w-16" />
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

function CommissionSummarySkeleton() {
  return (
    <div className="p-6 border rounded-lg space-y-3">
      {Array.from({ length: 4 }).map((_, i) => (
        <div key={i} className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
          <Skeleton className="h-8 w-8 rounded-lg" />
          <div className="flex-1">
            <Skeleton className="h-4 w-16 mb-1" />
            <Skeleton className="h-3 w-32" />
          </div>
          <Skeleton className="h-6 w-20" />
        </div>
      ))}
    </div>
  );
}

function ProjectBucketsSkeleton() {
  return (
    <div className="p-6 border rounded-lg">
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
        {Array.from({ length: 6 }).map((_, i) => (
          <div key={i} className="p-4 bg-gray-50 rounded-lg">
            <div className="text-center">
              <Skeleton className="h-8 w-12 mx-auto mb-2" />
              <Skeleton className="h-12 w-12 mx-auto mb-2 rounded-lg" />
              <Skeleton className="h-4 w-20 mx-auto mb-1" />
              <Skeleton className="h-3 w-24 mx-auto" />
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

function NotificationsFeedSkeleton() {
  return (
    <div className="p-6 border rounded-lg space-y-3">
      {Array.from({ length: 3 }).map((_, i) => (
        <div key={i} className="flex items-start space-x-3 p-3">
          <Skeleton className="w-2 h-2 rounded-full mt-2" />
          <div className="flex-1">
            <Skeleton className="h-4 w-full mb-1" />
            <Skeleton className="h-3 w-16" />
          </div>
        </div>
      ))}
    </div>
  );
}


function RecentProjectsSkeleton() {
  return (
    <div className="space-y-4">
      {Array.from({ length: 3 }).map((_, i) => (
        <div key={i} className="p-4 border rounded-lg">
          <div className="flex items-center justify-between">
            <div className="space-y-2">
              <Skeleton className="h-4 w-32" />
              <Skeleton className="h-3 w-48" />
            </div>
            <Skeleton className="h-6 w-16" />
          </div>
        </div>
      ))}
    </div>
  );
}

export default function DashboardPage() {
  const { data: session, status } = useSession();
  const [timeRange, setTimeRange] = useState<TimeRange>('ytd'); // Default to Year-to-Date
  const [customDateRange, setCustomDateRange] = useState<{ startDate: string; endDate: string } | undefined>();
  const [scope, setScope] = useState<MetricsScope>('team'); // Default to team view

  // Sync user timezone on mount (must be before early returns)
  useEffect(() => {
    if (session?.user?.timezone) {
      syncUserTimezone(session.user.timezone);
    }
  }, [session]);

  if (status === 'loading') {
    return <div>Loading...</div>;
  }

  if (!session) {
    redirect('/login');
  }

  const isManager = isManagerRole(session.user.role);

  const handleTimeRangeChange = (range: TimeRange, customRange?: { startDate: string; endDate: string }) => {
    setTimeRange(range);
    if (range === 'custom' && customRange) {
      setCustomDateRange(customRange);
    } else {
      setCustomDateRange(undefined);
    }
  };

  const getRoleDisplayName = (role: string) => {
    switch (role) {
      case 'closer':
        return 'Closer Dashboard';
      case 'setter':
        return 'Setter Dashboard';
      case 'office_leader':
        return 'Office Leader Dashboard';
      case 'regional':
        return 'Regional Manager Dashboard';
      case 'super_admin':
        return 'Super Admin Dashboard';
      default:
        return 'Dashboard';
    }
  };

  return (
    <div className="space-y-4 mobile:space-y-6">
      {/* Page Header */}
      <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
        <div>
          <h1 className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
            Welcome back, {session.user.name}
          </h1>
          <p className="text-sm mobile:text-base text-gray-600 mt-0.5">
            {getRoleDisplayName(session.user.role)} -
            {isManager && scope === 'team' ? 'Team Performance Dashboard' : 'Your Performance Dashboard'}
          </p>
        </div>
        <div className="flex flex-col mobile:flex-row items-stretch mobile:items-center gap-2 mobile:gap-4 mobile:space-x-0">
          {/* Show scope toggle only for managers */}
          {isManager && (
            <DashboardScopeToggle
              selectedScope={scope}
              onScopeChange={setScope}
            />
          )}
          <DashboardFilters
            selectedTimeRange={timeRange}
            customDateRange={customDateRange}
            onTimeRangeChange={handleTimeRangeChange}
          />
        </div>
      </div>

      {/* Urgent Alerts */}
      <Suspense fallback={<UrgentAlertsSkeleton />}>
        <UrgentAlerts userId={session.user.quickbaseUserId} role={session.user.role} />
      </Suspense>

      {/* Enhanced Dashboard Data */}
      <Suspense fallback={<PerformanceMetricsSkeleton />}>
        <EnhancedDashboardData
          userId={session.user.quickbaseUserId}
          role={session.user.role}
          timeRange={timeRange}
          customDateRange={customDateRange}
          scope={isManager ? scope : 'personal'} // Force personal for non-managers
        />
      </Suspense>

      {/* Two Column Layout */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-4 mobile:gap-6">
        {/* Left Column - 2/3 width */}
        <div className="lg:col-span-2 space-y-4 mobile:space-y-6">
          {/* Basic Metrics */}
          <Suspense fallback={<DashboardMetricsSkeleton />}>
            <DashboardMetrics
              userId={session.user.quickbaseUserId}
              role={session.user.role}
              timeRange={timeRange}
              customDateRange={customDateRange}
            />
          </Suspense>
        </div>

        {/* Right Column - 1/3 width */}
        <div className="lg:col-span-1 space-y-4 mobile:space-y-6">
          {/* Notifications Feed */}
          <Suspense fallback={<NotificationsFeedSkeleton />}>
            <NotificationsFeed
              userId={session.user.quickbaseUserId}
              role={session.user.role}
            />
          </Suspense>

          {/* Team Activity Feed (managers only) */}
          {isManager && (
            <Suspense fallback={<TeamActivityFeedSkeleton />}>
              <TeamActivityFeed
                userId={session.user.quickbaseUserId}
              />
            </Suspense>
          )}
        </div>
      </div>

      {/* Recent Projects */}
      <Suspense fallback={<RecentProjectsSkeleton />}>
        <RecentProjects userId={session.user.quickbaseUserId} role={session.user.role} />
      </Suspense>
    </div>
  );
}
