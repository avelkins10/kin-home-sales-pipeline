'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Calendar, TrendingUp, Pause, CheckCircle } from 'lucide-react';
import { getBaseUrl } from '@/lib/utils/baseUrl';
// fetching via API route

interface DashboardMetricsProps {
  userId: string;
  role: string;
}

interface DashboardMetricsData {
  installsThisWeek: number;
  activeProjects: number;
  onHold: number;
  holdBreakdown: string;
  installsThisMonth: number;
}

function MetricsSkeleton() {
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
      {Array.from({ length: 4 }).map((_, i) => (
        <Card key={i} className="hover:shadow-md transition-shadow">
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div className="space-y-2">
                <Skeleton className="h-4 w-24" />
                <Skeleton className="h-8 w-16" />
                <Skeleton className="h-3 w-20" />
              </div>
              <Skeleton className="h-14 w-14 rounded-full" />
            </div>
          </CardContent>
        </Card>
      ))}
    </div>
  );
}

export function DashboardMetrics({ userId, role }: DashboardMetricsProps) {
  const { data: metrics, isLoading } = useQuery<DashboardMetricsData>({
    queryKey: ['dashboard-metrics', userId, role],
    queryFn: async () => {
      const response = await fetch(`${getBaseUrl()}/api/dashboard/metrics`);
      if (!response.ok) throw new Error('Failed to fetch metrics');
      return response.json();
    },
  });

  if (isLoading) {
    return <MetricsSkeleton />;
  }

  const stats = [
    {
      label: 'Installs This Week',
      value: metrics?.installsThisWeek || 0,
      icon: Calendar,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
    },
    {
      label: 'Active Accounts',
      value: metrics?.activeProjects || 0,
      subtitle: 'Not on hold',
      icon: TrendingUp,
      color: 'text-green-600',
      bgColor: 'bg-green-50',
    },
    {
      label: 'On Hold',
      value: metrics?.onHold || 0,
      subtitle: metrics?.holdBreakdown || '',
      icon: Pause,
      color: 'text-red-600',
      bgColor: 'bg-red-50',
    },
    {
      label: 'Monthly Installs',
      value: metrics?.installsThisMonth || 0,
      icon: CheckCircle,
      color: 'text-purple-600',
      bgColor: 'bg-purple-50',
    },
  ];

  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
      {stats.map((stat) => {
        const Icon = stat.icon;
        return (
          <Card key={stat.label} className="hover:shadow-md transition-shadow">
            <CardContent className="p-6">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm font-medium text-gray-600">{stat.label}</p>
                  <p className="text-3xl font-bold text-gray-900">{stat.value}</p>
                  {stat.subtitle && (
                    <p className="text-xs text-gray-500 mt-1">{stat.subtitle}</p>
                  )}
                </div>
                <div className={`p-3 rounded-full ${stat.bgColor}`}>
                  <Icon className={`h-8 w-8 ${stat.color}`} />
                </div>
              </div>
            </CardContent>
          </Card>
        );
      })}
    </div>
  );
}
