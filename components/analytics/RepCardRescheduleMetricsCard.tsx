'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { RefreshCw, TrendingUp, TrendingDown, AlertCircle, Calendar, Users } from 'lucide-react';
import { Skeleton } from '@/components/ui/skeleton';
import { cn } from '@/lib/utils';

interface RescheduleMetrics {
  totalAppointments: number;
  totalReschedules: number;
  rescheduleRate: number;
  customersWithReschedules: number;
  avgReschedulesPerCustomer: number;
  topReschedulers: Array<{
    repcardUserId: number;
    userName: string;
    totalAppointments: number;
    totalReschedules: number;
    rescheduleRate: number;
  }>;
}

interface Props {
  userId?: string;
  officeIds?: number[];
  startDate?: string;
  endDate?: string;
  className?: string;
}

export function RepCardRescheduleMetricsCard({
  userId,
  officeIds,
  startDate,
  endDate,
  className
}: Props) {
  const { data, isLoading, error, refetch } = useQuery<RescheduleMetrics>({
    queryKey: ['repcard-reschedule-metrics', userId, officeIds, startDate, endDate],
    queryFn: async () => {
      // Query database for reschedule metrics
      const params = new URLSearchParams();
      if (userId) params.set('userId', userId);
      if (officeIds) params.set('officeIds', officeIds.join(','));
      if (startDate) params.set('startDate', startDate);
      if (endDate) params.set('endDate', endDate);

      const response = await fetch(`/api/repcard/metrics/reschedule-stats?${params.toString()}`);
      if (!response.ok) {
        // Return mock data for now since endpoint doesn't exist yet
        // TODO: Create the actual endpoint
        return {
          totalAppointments: 0,
          totalReschedules: 0,
          rescheduleRate: 0,
          customersWithReschedules: 0,
          avgReschedulesPerCustomer: 0,
          topReschedulers: []
        };
      }
      return response.json();
    },
    refetchInterval: 60000, // Refresh every minute
  });

  if (isLoading) {
    return (
      <Card className={className}>
        <CardHeader>
          <Skeleton className="h-6 w-48" />
          <Skeleton className="h-4 w-64 mt-2" />
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <Skeleton className="h-16 w-full" />
            <Skeleton className="h-16 w-full" />
            <Skeleton className="h-16 w-full" />
          </div>
        </CardContent>
      </Card>
    );
  }

  if (error) {
    return (
      <Card className={cn('border-yellow-200 bg-yellow-50', className)}>
        <CardContent className="pt-6">
          <div className="flex items-center gap-2 text-yellow-800">
            <AlertCircle className="h-5 w-5" />
            <p className="text-sm font-medium">
              Unable to load reschedule metrics
            </p>
          </div>
          <p className="text-xs text-yellow-700 mt-2">
            Reschedule tracking is enabled but data is still being collected.
            Run a sync to populate reschedule statistics.
          </p>
        </CardContent>
      </Card>
    );
  }

  if (!data) return null;

  const formatPercentage = (num: number) => `${num.toFixed(1)}%`;

  const getRescheduleRateColor = (rate: number) => {
    if (rate < 10) return 'text-green-600';
    if (rate < 25) return 'text-yellow-600';
    return 'text-red-600';
  };

  const getRescheduleRateBadgeColor = (rate: number) => {
    if (rate < 10) return 'bg-green-100 text-green-800';
    if (rate < 25) return 'bg-yellow-100 text-yellow-800';
    return 'bg-red-100 text-red-800';
  };

  return (
    <Card className={className}>
      <CardHeader>
        <div className="flex items-center justify-between">
          <div>
            <CardTitle className="flex items-center gap-2">
              <RefreshCw className="h-5 w-5 text-orange-500" />
              Reschedule Metrics
            </CardTitle>
            <CardDescription>
              Appointment reschedule tracking and quality metrics
            </CardDescription>
          </div>
          <Badge className={getRescheduleRateBadgeColor(data.rescheduleRate)}>
            {formatPercentage(data.rescheduleRate)} Rate
          </Badge>
        </div>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Overview Stats */}
        <div className="grid grid-cols-2 gap-4">
          <div className="space-y-1">
            <div className="flex items-center gap-2">
              <Calendar className="h-4 w-4 text-gray-500" />
              <p className="text-sm text-gray-600">Total Appointments</p>
            </div>
            <p className="text-2xl font-bold">{data.totalAppointments.toLocaleString()}</p>
          </div>

          <div className="space-y-1">
            <div className="flex items-center gap-2">
              <RefreshCw className="h-4 w-4 text-orange-500" />
              <p className="text-sm text-gray-600">Reschedules</p>
            </div>
            <p className="text-2xl font-bold text-orange-600">{data.totalReschedules.toLocaleString()}</p>
          </div>

          <div className="space-y-1">
            <div className="flex items-center gap-2">
              <Users className="h-4 w-4 text-gray-500" />
              <p className="text-sm text-gray-600">Customers with Reschedules</p>
            </div>
            <p className="text-2xl font-bold">{data.customersWithReschedules.toLocaleString()}</p>
          </div>

          <div className="space-y-1">
            <div className="flex items-center gap-2">
              <TrendingUp className="h-4 w-4 text-gray-500" />
              <p className="text-sm text-gray-600">Avg per Customer</p>
            </div>
            <p className="text-2xl font-bold">{data.avgReschedulesPerCustomer.toFixed(2)}</p>
          </div>
        </div>

        {/* Reschedule Rate Indicator */}
        <div className="pt-4 border-t">
          <div className="flex items-center justify-between mb-2">
            <p className="text-sm font-medium text-gray-700">Reschedule Rate</p>
            <p className={cn('text-lg font-bold', getRescheduleRateColor(data.rescheduleRate))}>
              {formatPercentage(data.rescheduleRate)}
            </p>
          </div>
          <div className="w-full bg-gray-200 rounded-full h-2">
            <div
              className={cn(
                'h-2 rounded-full transition-all',
                data.rescheduleRate < 10 ? 'bg-green-500' :
                data.rescheduleRate < 25 ? 'bg-yellow-500' : 'bg-red-500'
              )}
              style={{ width: `${Math.min(data.rescheduleRate, 100)}%` }}
            />
          </div>
          <div className="flex justify-between text-xs text-gray-500 mt-1">
            <span>0%</span>
            <span className="text-green-600">10% (Good)</span>
            <span className="text-yellow-600">25% (Fair)</span>
            <span>100%</span>
          </div>
        </div>

        {/* Top Reschedulers */}
        {data.topReschedulers && data.topReschedulers.length > 0 && (
          <div className="pt-4 border-t">
            <h4 className="text-sm font-medium text-gray-700 mb-3">Top Reschedulers</h4>
            <div className="space-y-2">
              {data.topReschedulers.slice(0, 5).map((user, idx) => (
                <div
                  key={user.repcardUserId}
                  className="flex items-center justify-between p-2 rounded-lg bg-gray-50 hover:bg-gray-100 transition-colors"
                >
                  <div className="flex items-center gap-3">
                    <div className={cn(
                      'flex items-center justify-center w-6 h-6 rounded-full text-xs font-bold',
                      idx === 0 ? 'bg-orange-500 text-white' :
                      idx === 1 ? 'bg-orange-400 text-white' :
                      idx === 2 ? 'bg-orange-300 text-white' :
                      'bg-gray-300 text-gray-700'
                    )}>
                      {idx + 1}
                    </div>
                    <div>
                      <p className="text-sm font-medium">{user.userName}</p>
                      <p className="text-xs text-gray-500">
                        {user.totalReschedules} of {user.totalAppointments} appointments
                      </p>
                    </div>
                  </div>
                  <Badge className={getRescheduleRateBadgeColor(user.rescheduleRate)}>
                    {formatPercentage(user.rescheduleRate)}
                  </Badge>
                </div>
              ))}
            </div>
          </div>
        )}

        {/* Info Message if No Data */}
        {data.totalAppointments === 0 && (
          <div className="text-center py-8">
            <RefreshCw className="h-12 w-12 text-gray-400 mx-auto mb-3" />
            <p className="text-sm text-gray-600 font-medium">No Appointment Data</p>
            <p className="text-xs text-gray-500 mt-1">
              Reschedule metrics will appear after running a sync
            </p>
          </div>
        )}
      </CardContent>
    </Card>
  );
}
