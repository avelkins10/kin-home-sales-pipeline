import { Suspense } from 'react';
import { getServerSession } from 'next-auth';
import { authOptions } from '@/lib/auth/next-auth.config';
import { redirect } from 'next/navigation';
import { Skeleton } from '@/components/ui/skeleton';
import { DashboardMetrics } from '@/components/dashboard/DashboardMetrics';
import { UrgentAlerts } from '@/components/dashboard/UrgentAlerts';
import { RecentProjects } from '@/components/dashboard/RecentProjects';

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

export default async function DashboardPage() {
  const session = await getServerSession(authOptions);

  if (!session) {
    redirect('/login');
  }

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
    <div className="space-y-6">
      {/* Page Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">
            Welcome back, {session.user.name}
          </h1>
          <p className="text-gray-600">
            {getRoleDisplayName(session.user.role)}
          </p>
        </div>
      </div>

      {/* Urgent Alerts */}
      <Suspense fallback={<UrgentAlertsSkeleton />}>
        <UrgentAlerts userId={session.user.quickbaseUserId} role={session.user.role} />
      </Suspense>

      {/* Dashboard Metrics */}
      <Suspense fallback={<DashboardMetricsSkeleton />}>
        <DashboardMetrics userId={session.user.quickbaseUserId} role={session.user.role} />
      </Suspense>

      {/* Recent Projects */}
      <Suspense fallback={<RecentProjectsSkeleton />}>
        <RecentProjects userId={session.user.quickbaseUserId} role={session.user.role} />
      </Suspense>
    </div>
  );
}
