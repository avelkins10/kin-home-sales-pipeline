'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { DoorOpen, Calendar, TrendingUp, Users } from 'lucide-react';
import { formatLargeNumber, formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

interface CanvassingOverviewCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds: number[];
}

export function CanvassingOverviewCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: CanvassingOverviewCardProps) {
  // Fetch doors knocked data
  const { data: doorsData, isLoading: doorsLoading, error: doorsError } = useQuery({
    queryKey: ['canvassing-overview', 'doors', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'doors_knocked',
        timeRange
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch doors data');
      return response.json();
    }
  });

  // Fetch appointments set data
  const { data: appointmentsData, isLoading: appointmentsLoading, error: appointmentsError } = useQuery({
    queryKey: ['canvassing-overview', 'appointments', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'appointments_set',
        timeRange
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch appointments data');
      return response.json();
    }
  });

  const isLoading = doorsLoading || appointmentsLoading;
  const hasError = doorsError || appointmentsError;

  // Calculate metrics
  const totalDoors = doorsData?.leaderboard?.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0) || 0;
  const totalAppointments = appointmentsData?.leaderboard?.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0) || 0;
  const conversionRate = totalDoors > 0 ? (totalAppointments / totalDoors) * 100 : 0;
  
  // Count unique active reps from doors data
  const activeReps = new Set(doorsData?.leaderboard?.map((entry: any) => entry.userId)).size || 0;

  if (hasError) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <DoorOpen className="h-5 w-5 text-purple-600" />
            Canvassing Overview
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8">
            <p className="text-red-600 mb-4">Failed to load canvassing data</p>
            <p className="text-sm text-gray-500">
              {doorsError?.message || appointmentsError?.message}
            </p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <DoorOpen className="h-5 w-5 text-purple-600" />
            Canvassing Overview
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            {Array.from({ length: 4 }).map((_, i) => (
              <div key={i} className="space-y-2">
                <Skeleton className="h-4 w-20" />
                <Skeleton className="h-8 w-16" />
                <Skeleton className="h-3 w-12" />
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  const metrics = [
    {
      label: 'Total Doors Knocked',
      value: formatLargeNumber(totalDoors),
      icon: DoorOpen,
      color: 'text-purple-600',
      bgColor: 'bg-purple-50',
      borderColor: 'border-purple-200'
    },
    {
      label: 'Total Appointments Set',
      value: formatLargeNumber(totalAppointments),
      icon: Calendar,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
      borderColor: 'border-blue-200'
    },
    {
      label: 'Conversion Rate',
      value: formatPercentage(conversionRate),
      icon: TrendingUp,
      color: 'text-green-600',
      bgColor: 'bg-green-50',
      borderColor: 'border-green-200'
    },
    {
      label: 'Active Reps',
      value: formatLargeNumber(activeReps),
      icon: Users,
      color: 'text-gray-600',
      bgColor: 'bg-gray-50',
      borderColor: 'border-gray-200'
    }
  ];

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <DoorOpen className="h-5 w-5 text-purple-600" />
          Canvassing Overview
        </CardTitle>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          {metrics.map((metric, index) => {
            const IconComponent = metric.icon;
            return (
              <div
                key={index}
                className={`p-4 rounded-lg border ${metric.bgColor} ${metric.borderColor}`}
              >
                <div className="flex items-center gap-2 mb-2">
                  <IconComponent className={`h-4 w-4 ${metric.color}`} />
                  <span className="text-sm font-medium text-gray-700">
                    {metric.label}
                  </span>
                </div>
                <div className={`text-2xl font-bold ${metric.color}`}>
                  {metric.value}
                </div>
              </div>
            );
          })}
        </div>
      </CardContent>
    </Card>
  );
}
