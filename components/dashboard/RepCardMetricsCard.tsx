'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { DoorOpen, Calendar, Users, FileText, TrendingUp, AlertCircle } from 'lucide-react';
import { formatLargeNumber, formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { cn } from '@/lib/utils/cn';

interface RepCardMetricsCardProps {
  userId: string;
  role: string;
  timeRange: string;
  customDateRange?: { startDate: string; endDate: string };
  className?: string;
}

export function RepCardMetricsCard({
  userId,
  role,
  timeRange,
  customDateRange,
  className
}: RepCardMetricsCardProps) {
  // Calculate date range
  const calculateDateRange = () => {
    if (timeRange === 'custom' && customDateRange) {
      return {
        startDate: customDateRange.startDate,
        endDate: customDateRange.endDate
      };
    }
    
    const now = new Date();
    let startDate: string;
    
    switch (timeRange) {
      case 'today':
        startDate = now.toISOString().split('T')[0];
        break;
      case 'week':
        const weekStart = new Date(now);
        weekStart.setDate(now.getDate() - now.getDay());
        startDate = weekStart.toISOString().split('T')[0];
        break;
      case 'month':
        startDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
        break;
      case 'quarter':
        const quarterStart = new Date(now.getFullYear(), Math.floor(now.getMonth() / 3) * 3, 1);
        startDate = quarterStart.toISOString().split('T')[0];
        break;
      case 'ytd':
        startDate = new Date(now.getFullYear(), 0, 1).toISOString().split('T')[0];
        break;
      default:
        startDate = new Date(now.getFullYear(), now.getMonth(), 1).toISOString().split('T')[0];
    }
    
    return {
      startDate,
      endDate: now.toISOString().split('T')[0]
    };
  };

  const dateRange = calculateDateRange();

  // Fetch RepCard stats
  const { data: stats, isLoading, error } = useQuery({
    queryKey: ['repcard-metrics', userId, timeRange, dateRange],
    queryFn: async () => {
      const response = await fetch(
        `${getBaseUrl()}/api/repcard/users/${userId}/stats?startDate=${dateRange.startDate}&endDate=${dateRange.endDate}`
      );
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        throw new Error(errorData.error || `Failed to fetch RepCard stats: ${response.status} ${response.statusText}`);
      }
      const data = await response.json();
      
      // Check if user has RepCard data
      if (data.hasRepcardData === false) {
        return { hasRepcardData: false, message: data.message || 'User not linked to RepCard' };
      }
      
      return data;
    },
    refetchInterval: 30000, // Refresh every 30 seconds
    staleTime: 60000,
    enabled: !!userId,
    retry: 2,
    retryDelay: 1000
  });

  // Fetch attachment count
  const { data: attachmentsData } = useQuery({
    queryKey: ['repcard-attachments', userId, dateRange],
    queryFn: async () => {
      const response = await fetch(
        `${getBaseUrl()}/api/repcard/data?type=attachments&userId=${userId}&startDate=${dateRange.startDate}&endDate=${dateRange.endDate}&limit=1000`
      );
      if (!response.ok) {
        return { data: [], pagination: { total: 0 } };
      }
      return response.json();
    },
    refetchInterval: 60000, // Refresh every minute
    staleTime: 120000,
    enabled: !!userId && !!stats
  });

  if (isLoading) {
    return (
      <Card className={cn('w-full', className)}>
        <CardHeader>
          <CardTitle>RepCard Metrics</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {Array.from({ length: 4 }).map((_, i) => (
              <Skeleton key={i} className="h-20 w-full" />
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  // Handle error or no RepCard data
  if (error) {
    return (
      <Card className={cn('w-full', className)}>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Users className="h-5 w-5 text-blue-600" />
            RepCard Metrics
          </CardTitle>
          <CardDescription>Canvassing and quality metrics from RepCard</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="flex items-center space-x-2 text-yellow-600 py-4">
            <AlertCircle className="h-5 w-5" />
            <div>
              <p className="text-sm font-medium">RepCard Data Unavailable</p>
              <p className="text-xs text-gray-600 mt-1">
                {error instanceof Error ? error.message : 'Failed to load RepCard metrics'}
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Handle case where user is not linked to RepCard
  if (!stats || stats.hasRepcardData === false) {
    return (
      <Card className={cn('w-full', className)}>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Users className="h-5 w-5 text-blue-600" />
            RepCard Metrics
          </CardTitle>
          <CardDescription>Canvassing and quality metrics from RepCard</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="flex items-center space-x-2 text-gray-500 py-4">
            <AlertCircle className="h-5 w-5" />
            <div>
              <p className="text-sm font-medium">No RepCard Data</p>
              <p className="text-xs text-gray-600 mt-1">
                {stats?.message || 'User not linked to RepCard. Contact your administrator to link your account.'}
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  const { volumeStats, qualityStats } = stats;
  const attachmentCount = attachmentsData?.pagination?.total || 0;
  const doorsKnocked = volumeStats.doorsKnocked || 0;
  const appointmentsSet = volumeStats.appointmentsSet || 0;
  const conversionRate = doorsKnocked > 0 ? (appointmentsSet / doorsKnocked) * 100 : 0;

  const metrics = [
    {
      label: 'Doors Knocked',
      value: formatLargeNumber(doorsKnocked),
      icon: DoorOpen,
      color: 'text-purple-600',
      bgColor: 'bg-purple-50',
      borderColor: 'border-purple-200'
    },
    {
      label: 'Appointments Set',
      value: formatLargeNumber(appointmentsSet),
      icon: Calendar,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
      borderColor: 'border-blue-200'
    },
    {
      label: 'Conversion Rate',
      value: formatPercentage(conversionRate),
      icon: TrendingUp,
      color: conversionRate >= 10 ? 'text-green-600' : conversionRate >= 5 ? 'text-yellow-600' : 'text-red-600',
      bgColor: conversionRate >= 10 ? 'bg-green-50' : conversionRate >= 5 ? 'bg-yellow-50' : 'bg-red-50',
      borderColor: conversionRate >= 10 ? 'border-green-200' : conversionRate >= 5 ? 'border-yellow-200' : 'border-red-200'
    },
    {
      label: 'Attachments',
      value: formatLargeNumber(attachmentCount),
      icon: FileText,
      color: 'text-indigo-600',
      bgColor: 'bg-indigo-50',
      borderColor: 'border-indigo-200'
    }
  ];

  return (
    <Card className={cn('w-full', className)}>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Users className="h-5 w-5 text-blue-600" />
          RepCard Metrics
        </CardTitle>
        <CardDescription>
          Canvassing activity and quality metrics from RepCard
        </CardDescription>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          {metrics.map((metric, index) => {
            const IconComponent = metric.icon;
            return (
              <div
                key={index}
                className={cn(
                  'p-4 rounded-lg border transition-all hover:shadow-md',
                  metric.bgColor,
                  metric.borderColor
                )}
              >
                <div className="flex items-center gap-2 mb-2">
                  <IconComponent className={cn('h-4 w-4', metric.color)} />
                  <span className="text-sm font-medium text-gray-700">
                    {metric.label}
                  </span>
                </div>
                <div className={cn('text-2xl font-bold', metric.color)}>
                  {metric.value}
                </div>
              </div>
            );
          })}
        </div>

        {/* Quality Metrics Row */}
        {qualityStats && (
          <div className="mt-6 pt-6 border-t">
            <h4 className="text-sm font-semibold text-gray-700 mb-3">Quality Metrics</h4>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              <div className="text-center">
                <p className="text-xs text-gray-600 mb-1">Appointment Speed</p>
                <p className={cn(
                  'text-lg font-semibold',
                  qualityStats.appointmentSpeed.percentage >= 75 ? 'text-green-600' : 
                  qualityStats.appointmentSpeed.percentage >= 50 ? 'text-yellow-600' : 'text-red-600'
                )}>
                  {formatPercentage(qualityStats.appointmentSpeed.percentage)}
                </p>
                <p className="text-xs text-gray-500 mt-1">
                  Avg: {qualityStats.appointmentSpeed.averageHours.toFixed(1)}h
                </p>
              </div>
              <div className="text-center">
                <p className="text-xs text-gray-600 mb-1">Power Bill Rate</p>
                <p className={cn(
                  'text-lg font-semibold',
                  qualityStats.attachmentRate.percentage >= 75 ? 'text-green-600' : 
                  qualityStats.attachmentRate.percentage >= 50 ? 'text-yellow-600' : 'text-red-600'
                )}>
                  {formatPercentage(qualityStats.attachmentRate.percentage)}
                </p>
                <p className="text-xs text-gray-500 mt-1">
                  {qualityStats.attachmentRate.totalAttachments} attachments
                </p>
              </div>
              <div className="text-center">
                <p className="text-xs text-gray-600 mb-1">Reschedule Rate</p>
                <p className={cn(
                  'text-lg font-semibold',
                  qualityStats.rescheduleRate.average < 1 ? 'text-green-600' : 
                  qualityStats.rescheduleRate.average < 2 ? 'text-yellow-600' : 'text-red-600'
                )}>
                  {qualityStats.rescheduleRate.average.toFixed(2)}
                </p>
                <p className="text-xs text-gray-500 mt-1">
                  {qualityStats.rescheduleRate.totalReschedules} total
                </p>
              </div>
              <div className="text-center">
                <p className="text-xs text-gray-600 mb-1">Follow-Up Rate</p>
                <p className={cn(
                  'text-lg font-semibold',
                  qualityStats.followUpConsistency.percentage >= 75 ? 'text-green-600' : 
                  qualityStats.followUpConsistency.percentage >= 50 ? 'text-yellow-600' : 'text-red-600'
                )}>
                  {formatPercentage(qualityStats.followUpConsistency.percentage)}
                </p>
                <p className="text-xs text-gray-500 mt-1">
                  {qualityStats.followUpConsistency.totalFollowUps} follow-ups
                </p>
              </div>
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
}

