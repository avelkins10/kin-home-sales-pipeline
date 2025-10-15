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

  const totalForecast = (data.next30Days ?? 0) + (data.next60Days ?? 0) + (data.next90Days ?? 0);

  if (totalForecast === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4 text-center">
            <Info className="h-8 w-8 text-slate-400 mx-auto mb-2" />
            <p className="text-slate-600">No installs scheduled in the next 90 days</p>
            <p className="text-slate-500 text-sm mt-1">Check project statuses for updates</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const forecastPeriods = [
    {
      label: 'Next 30 Days',
      value: data.next30Days ?? 0,
      color: 'bg-green-50',
      iconColor: 'text-green-600',
      bgColor: 'bg-green-100'
    },
    {
      label: 'Next 60 Days',
      value: data.next60Days ?? 0,
      color: 'bg-blue-50',
      iconColor: 'text-blue-600',
      bgColor: 'bg-blue-100'
    },
    {
      label: 'Next 90 Days',
      value: data.next90Days ?? 0,
      color: 'bg-purple-50',
      iconColor: 'text-purple-600',
      bgColor: 'bg-purple-100'
    }
  ];

  return (
    <Card className="w-full" aria-label="Pipeline forecast">
      <CardHeader>
        <CardTitle className="flex items-center space-x-2">
          <Calendar className="h-6 w-6 text-blue-600" />
          <span>Pipeline Forecast</span>
        </CardTitle>
        <p className="text-sm text-gray-600">
          Expected installs
        </p>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          {/* Forecast Periods */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {forecastPeriods.map((period) => (
              <div key={period.label} className={`flex items-center space-x-3 p-3 ${period.color} rounded-lg`}>
                <div className={`p-2 ${period.bgColor} rounded-lg`}>
                  <Clock className={`h-5 w-5 ${period.iconColor}`} />
                </div>
                <div className="flex-1">
                  <p className="text-sm text-gray-600">{period.label}</p>
                  <p className="text-2xl font-semibold text-gray-900">{period.value.toLocaleString()}</p>
                  <p className="text-xs text-gray-500">projects scheduled for install</p>
                </div>
              </div>
            ))}
          </div>

          {/* Summary Stats */}
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-slate-900">Total Forecasted Installs</p>
                <p className="text-lg font-bold text-slate-900">{totalForecast.toLocaleString()}</p>
                <p className="text-sm text-slate-600">Next 90 days</p>
              </div>
              <div className="text-right">
                <p className="text-sm text-slate-600">
                  {data.next30Days > 0 && `${formatPercentage((data.next30Days / totalForecast) * 100)} in 30 days`}
                </p>
                <p className="text-sm text-slate-600">
                  {data.next60Days > 0 && `${formatPercentage((data.next60Days / totalForecast) * 100)} in 60 days`}
                </p>
                <p className="text-sm text-slate-600">
                  {data.next90Days > 0 && `${formatPercentage((data.next90Days / totalForecast) * 100)} in 90 days`}
                </p>
              </div>
            </div>
          </div>

          {/* Data Freshness Note */}
          <div className="text-xs text-gray-500 text-center">
            Forecast based on scheduled and estimated install dates
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
