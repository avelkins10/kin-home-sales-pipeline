'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Calendar, UserX, Paperclip, Clock, AlertCircle } from 'lucide-react';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { Button } from '@/components/ui/button';

interface AppointmentMetricsProps {
  userId: string;
  userRole: string;
  queryParams: string;
}

interface AppointmentMetricsData {
  totalAppointments: number;
  powerBillCoverage: {
    count: number;
    percentage: number;
  };
  averageScheduleOutTime: {
    hours: number;
    days: number;
    formatted: string;
  } | null;
  unassignedAppointments: number;
  confirmationRate: {
    count: number;
    total: number;
    percentage: number;
  };
  rescheduleStats: {
    count: number;
    percentage: number;
  };
}

function MetricsSkeleton() {
  return (
    <div className="grid grid-cols-2 md:grid-cols-2 lg:grid-cols-4 gap-3 mobile:gap-4 ipad:gap-6">
      {Array.from({ length: 4 }).map((_, i) => (
        <Card key={i} className="hover:shadow-md transition-shadow">
          <CardContent className="p-3 mobile:p-4 ipad:p-6">
            <div className="flex flex-col mobile:flex-row items-start mobile:items-center mobile:justify-between gap-2 mobile:gap-0">
              <div className="flex-1 min-w-0 space-y-2">
                <Skeleton className="h-3 mobile:h-4 w-20 mobile:w-24" />
                <Skeleton className="h-6 mobile:h-8 w-12 mobile:w-16" />
                <Skeleton className="h-2 mobile:h-3 w-16 mobile:w-20" />
              </div>
              <Skeleton className="h-10 w-10 mobile:h-12 mobile:w-12 ipad:h-14 ipad:w-14 rounded-full" />
            </div>
          </CardContent>
        </Card>
      ))}
    </div>
  );
}

function getColorForPercentage(pct: number) {
  if (pct >= 80) return { text: 'text-green-600', bg: 'bg-green-50' };
  if (pct >= 50) return { text: 'text-yellow-600', bg: 'bg-yellow-50' };
  return { text: 'text-red-600', bg: 'bg-red-50' };
}

export function AppointmentMetrics({ userId, userRole, queryParams }: AppointmentMetricsProps) {
  const { data: metrics, isLoading, error, refetch } = useQuery<AppointmentMetricsData>({
    queryKey: ['repcard-appointments-metrics', userId, queryParams],
    queryFn: async () => {
      const res = await fetch(`${getBaseUrl()}/api/repcard/appointments/metrics?${queryParams}`);
      if (!res.ok) {
        const errorData = await res.json().catch(() => ({ message: 'Failed to fetch metrics' }));
        throw new Error(errorData.message || 'Failed to fetch metrics');
      }
      return res.json();
    },
    staleTime: 60000, // 60 seconds
    refetchInterval: 30000, // Auto-refetch every 30 seconds
  });

  if (isLoading) {
    return <MetricsSkeleton />;
  }

  if (error) {
    return (
      <Alert variant="destructive">
        <AlertCircle className="h-4 w-4" />
        <AlertDescription className="flex items-center justify-between">
          <span>Failed to load metrics: {error instanceof Error ? error.message : 'Unknown error'}</span>
          <Button
            variant="outline"
            size="sm"
            onClick={() => refetch()}
            className="ml-4"
          >
            Retry
          </Button>
        </AlertDescription>
      </Alert>
    );
  }

  if (!metrics) {
    return null;
  }

  // Calculate dynamic colors for power bill coverage
  const powerBillColors = getColorForPercentage(metrics.powerBillCoverage.percentage);

  // Calculate dynamic colors for unassigned appointments
  const unassignedColors = metrics.unassignedAppointments > 0
    ? { text: 'text-red-600', bg: 'bg-red-50' }
    : { text: 'text-gray-600', bg: 'bg-gray-50' };

  const stats = [
    {
      label: 'Total Appointments',
      value: metrics.totalAppointments,
      subtitle: 'Scheduled in period',
      icon: Calendar,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
    },
    {
      label: 'Unassigned Appointments',
      value: metrics.unassignedAppointments,
      subtitle: metrics.unassignedAppointments > 0 ? 'Need closer assignment' : 'All assigned',
      icon: UserX,
      color: unassignedColors.text,
      bgColor: unassignedColors.bg,
    },
    {
      label: 'Power Bill Coverage',
      value: `${metrics.powerBillCoverage.count} (${metrics.powerBillCoverage.percentage}%)`,
      subtitle: 'Have power bills',
      icon: Paperclip,
      color: powerBillColors.text,
      bgColor: powerBillColors.bg,
    },
    {
      label: 'Avg Schedule Out Time',
      value: metrics.averageScheduleOutTime?.formatted || 'N/A',
      subtitle: 'Door knock to appointment',
      icon: Clock,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
    },
  ];

  return (
    <div className="grid grid-cols-2 md:grid-cols-2 lg:grid-cols-4 gap-3 mobile:gap-4 ipad:gap-6">
      {stats.map((stat) => {
        const Icon = stat.icon;
        return (
          <Card key={stat.label} className="hover:shadow-md transition-shadow">
            <CardContent className="p-3 mobile:p-4 ipad:p-6">
              <div className="flex flex-col mobile:flex-row items-start mobile:items-center mobile:justify-between gap-2 mobile:gap-0">
                <div className="flex-1 min-w-0">
                  <p className="text-xs mobile:text-sm font-medium text-gray-600 truncate">{stat.label}</p>
                  <p className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">{stat.value}</p>
                  {stat.subtitle && (
                    <p className="text-xs text-gray-500 mt-0.5 mobile:mt-1 truncate">{stat.subtitle}</p>
                  )}
                </div>
                <div className={`p-2 mobile:p-2.5 ipad:p-3 rounded-full ${stat.bgColor} flex-shrink-0`}>
                  <Icon className={`h-5 w-5 mobile:h-6 mobile:w-6 ipad:h-8 ipad:w-8 ${stat.color}`} />
                </div>
              </div>
            </CardContent>
          </Card>
        );
      })}
    </div>
  );
}
