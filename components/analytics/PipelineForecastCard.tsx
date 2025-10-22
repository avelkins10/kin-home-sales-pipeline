'use client'

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Calendar, Clock, Info } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import type { PipelineForecast } from '@/lib/types/analytics';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface PipelineForecastCardProps {
  userId: string;
  role: string;
  officeIds?: number[];
}

function PipelineForecastSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-36" />
        </div>
        <Skeleton className="h-4 w-48" />
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          {Array.from({ length: 3 }).map((_, i) => (
            <div key={i} className="flex items-center space-x-3 p-3 bg-slate-50 rounded-lg">
              <Skeleton className="h-9 w-9 rounded-lg" />
              <div className="flex-1 space-y-2">
                <Skeleton className="h-4 w-20" />
                <Skeleton className="h-8 w-16" />
                <Skeleton className="h-3 w-24" />
              </div>
            </div>
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

export function PipelineForecastCard({
  userId,
  role,
  officeIds
}: PipelineForecastCardProps) {
  const { data, isLoading, error } = useQuery<PipelineForecast>({
    queryKey: ['pipeline-forecast', userId, role, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/pipeline-forecast?includeDetails=false`;
      
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch pipeline forecast');
      return response.json();
    },
  });

  if (isLoading) {
    return <PipelineForecastSkeleton />;
  }

  if (error) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load pipeline forecast</p>
            <p className="text-red-500 text-sm mt-1">{error.message}</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!data) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No forecast data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const totalForecast = (data.lastWeek ?? 0) + (data.thisWeek ?? 0) + (data.nextWeek ?? 0);

  if (totalForecast === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4 text-center">
            <Info className="h-8 w-8 text-slate-400 mx-auto mb-2" />
            <p className="text-slate-600">No installs tracked for this 3-week period</p>
            <p className="text-slate-500 text-sm mt-1">Check project statuses for updates</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const forecastPeriods = [
    {
      label: 'Last Week',
      value: data.lastWeek ?? 0,
      color: 'bg-slate-50',
      iconColor: 'text-slate-600',
      bgColor: 'bg-slate-100',
      description: 'Completed installs'
    },
    {
      label: 'This Week',
      value: data.thisWeek ?? 0,
      color: 'bg-blue-50',
      iconColor: 'text-blue-600',
      bgColor: 'bg-blue-100',
      description: 'Scheduled this week'
    },
    {
      label: 'Next Week',
      value: data.nextWeek ?? 0,
      color: 'bg-green-50',
      iconColor: 'text-green-600',
      bgColor: 'bg-green-100',
      description: 'Scheduled next week'
    }
  ];

  return (
    <Card className="w-full" aria-label="Pipeline forecast">
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <Calendar className="h-6 w-6 text-blue-600" />
          <span>Install Tracker</span>
        </CardTitle>
        <p className="text-sm text-gray-600">
          Last week • This week • Next week
        </p>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          {/* Forecast Periods */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {forecastPeriods.map((period) => (
              <div key={period.label} className={`flex flex-col space-y-2 p-4 ${period.color} rounded-lg border-l-4 ${
                period.label === 'Last Week' ? 'border-slate-500' :
                period.label === 'This Week' ? 'border-blue-500' :
                'border-green-500'
              }`}>
                <div className="flex items-center justify-between">
                  <p className="text-sm font-medium text-gray-700">{period.label}</p>
                  <div className={`p-1.5 ${period.bgColor} rounded`}>
                    <Clock className={`h-4 w-4 ${period.iconColor}`} />
                  </div>
                </div>
                <div>
                  <p className="text-3xl font-bold text-gray-900">{period.value.toLocaleString()}</p>
                  <p className="text-xs text-gray-600 mt-1">{period.description}</p>
                </div>
              </div>
            ))}
          </div>

          {/* Summary Stats */}
          <div className="bg-gradient-to-r from-blue-50 to-indigo-50 border border-blue-200 rounded-lg p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-blue-900">Total (3 Weeks)</p>
                <p className="text-2xl font-bold text-blue-900">{totalForecast.toLocaleString()}</p>
                <p className="text-xs text-blue-700 mt-1">installs tracked</p>
              </div>
              <div className="text-right space-y-1">
                {data.lastWeek > 0 && (
                  <p className="text-sm text-blue-700">
                    {formatPercentage((data.lastWeek / totalForecast) * 100)} last week
                  </p>
                )}
                {data.thisWeek > 0 && (
                  <p className="text-sm text-blue-700">
                    {formatPercentage((data.thisWeek / totalForecast) * 100)} this week
                  </p>
                )}
                {data.nextWeek > 0 && (
                  <p className="text-sm text-blue-700">
                    {formatPercentage((data.nextWeek / totalForecast) * 100)} next week
                  </p>
                )}
              </div>
            </div>
          </div>

          {/* Data Freshness Note */}
          <div className="text-xs text-gray-500 text-center">
            Based on scheduled and estimated install dates
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
