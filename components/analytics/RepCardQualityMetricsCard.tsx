'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Zap, Clock, Paperclip, RefreshCw, CheckCircle } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { cn } from '@/lib/utils/cn';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

interface RepCardQualityMetricsCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds: number[];
}

export function RepCardQualityMetricsCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: RepCardQualityMetricsCardProps) {
  // Fetch quality metrics from leaderboard API
  const { data: appointmentSpeedData, isLoading: speedLoading } = useQuery({
    queryKey: ['repcard-quality', 'speed', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'appointment_speed',
        timeRange,
        role: 'all'
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch appointment speed data');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 60000
  });

  const { data: attachmentRateData, isLoading: attachmentLoading } = useQuery({
    queryKey: ['repcard-quality', 'attachment', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'attachment_rate',
        timeRange,
        role: 'all'
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch attachment rate data');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 60000
  });

  const { data: qualityScoreData, isLoading: qualityLoading } = useQuery({
    queryKey: ['repcard-quality', 'score', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'quality_score',
        timeRange,
        role: 'all'
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch quality score data');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 60000
  });

  const isLoading = speedLoading || attachmentLoading || qualityLoading;

  const speedList = appointmentSpeedData?.leaderboard || [];
  const attachmentList = attachmentRateData?.leaderboard || [];
  const qualityList = qualityScoreData?.leaderboard || [];

  // Calculate averages
  const avgAppointmentSpeed = speedList.length > 0
    ? speedList.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0) / speedList.length
    : 0;

  const avgAttachmentRate = attachmentList.length > 0
    ? attachmentList.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0) / attachmentList.length
    : 0;

  const avgQualityScore = qualityList.length > 0
    ? qualityList.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0) / qualityList.length
    : 0;

  // Count reps meeting targets
  const repsMeetingSpeedTarget = speedList.filter((e: any) => e.metricValue >= 75).length;
  const repsMeetingAttachmentTarget = attachmentList.filter((e: any) => e.metricValue >= 70).length;
  const repsMeetingQualityTarget = qualityList.filter((e: any) => e.metricValue >= 75).length;

  const totalReps = Math.max(speedList.length, attachmentList.length, qualityList.length);

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Zap className="h-5 w-5 text-purple-600" />
            Quality Metrics
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {[...Array(3)].map((_, i) => (
              <div key={i} className="space-y-3">
                <Skeleton className="h-16 w-full" />
                <Skeleton className="h-8 w-24" />
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  const qualityMetrics = [
    {
      label: 'Appointment Speed',
      value: avgAppointmentSpeed,
      target: 75,
      icon: Clock,
      color: 'blue',
      description: '% scheduled within 24 hours',
      repsMeeting: repsMeetingSpeedTarget,
      totalReps
    },
    {
      label: 'Power Bill Rate',
      value: avgAttachmentRate,
      target: 70,
      icon: Paperclip,
      color: 'green',
      description: '% customers with attachments',
      repsMeeting: repsMeetingAttachmentTarget,
      totalReps
    },
    {
      label: 'Quality Score',
      value: avgQualityScore,
      target: 75,
      icon: CheckCircle,
      color: 'purple',
      description: 'Composite quality metric',
      repsMeeting: repsMeetingQualityTarget,
      totalReps
    }
  ];

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Zap className="h-5 w-5 text-purple-600" />
          Quality Metrics
        </CardTitle>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          {qualityMetrics.map((metric) => {
            const Icon = metric.icon;
            const isAboveTarget = metric.value >= metric.target;
            const isNearTarget = metric.value >= metric.target * 0.8;
            
            const colorClasses = {
              blue: {
                bg: isAboveTarget ? 'from-blue-50 to-blue-100' : isNearTarget ? 'from-blue-50 to-blue-50' : 'from-red-50 to-red-50',
                border: isAboveTarget ? 'border-blue-300' : isNearTarget ? 'border-blue-200' : 'border-red-200',
                icon: isAboveTarget ? 'text-blue-600' : isNearTarget ? 'text-blue-500' : 'text-red-500',
                value: isAboveTarget ? 'text-blue-900' : isNearTarget ? 'text-blue-700' : 'text-red-700',
                text: 'text-blue-700'
              },
              green: {
                bg: isAboveTarget ? 'from-green-50 to-green-100' : isNearTarget ? 'from-green-50 to-green-50' : 'from-red-50 to-red-50',
                border: isAboveTarget ? 'border-green-300' : isNearTarget ? 'border-green-200' : 'border-red-200',
                icon: isAboveTarget ? 'text-green-600' : isNearTarget ? 'text-green-500' : 'text-red-500',
                value: isAboveTarget ? 'text-green-900' : isNearTarget ? 'text-green-700' : 'text-red-700',
                text: 'text-green-700'
              },
              purple: {
                bg: isAboveTarget ? 'from-purple-50 to-purple-100' : isNearTarget ? 'from-purple-50 to-purple-50' : 'from-red-50 to-red-50',
                border: isAboveTarget ? 'border-purple-300' : isNearTarget ? 'border-purple-200' : 'border-red-200',
                icon: isAboveTarget ? 'text-purple-600' : isNearTarget ? 'text-purple-500' : 'text-red-500',
                value: isAboveTarget ? 'text-purple-900' : isNearTarget ? 'text-purple-700' : 'text-red-700',
                text: 'text-purple-700'
              }
            };

            const colors = colorClasses[metric.color as keyof typeof colorClasses];

            return (
              <div
                key={metric.label}
                className={cn(
                  'p-4 rounded-lg border-2 bg-gradient-to-br',
                  colors.bg,
                  colors.border
                )}
              >
                <div className="flex items-center justify-between mb-3">
                  <div className="flex items-center gap-2">
                    <Icon className={cn('h-5 w-5', colors.icon)} />
                    <span className={cn('text-sm font-semibold', colors.text)}>
                      {metric.label}
                    </span>
                  </div>
                  <div className="flex items-center gap-1">
                    <span className={cn('text-xs font-medium', colors.text)}>
                      Target: {metric.target}%
                    </span>
                  </div>
                </div>
                
                <div className="space-y-2">
                  <p className={cn('text-3xl font-bold', colors.value)}>
                    {formatPercentage(metric.value)}
                  </p>
                  <p className={cn('text-xs', colors.text)}>
                    {metric.description}
                  </p>
                  
                  <div className="mt-3 pt-3 border-t border-current/20">
                    <div className="flex items-center justify-between">
                      <span className={cn('text-xs', colors.text)}>
                        Reps Meeting Target:
                      </span>
                      <span className={cn('text-sm font-semibold', colors.value)}>
                        {metric.repsMeeting} / {metric.totalReps}
                      </span>
                    </div>
                    <div className="mt-2 h-2 bg-current/10 rounded-full overflow-hidden">
                      <div
                        className={cn('h-full transition-all duration-300', colors.bg)}
                        style={{ width: `${(metric.repsMeeting / Math.max(metric.totalReps, 1)) * 100}%` }}
                      />
                    </div>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      </CardContent>
    </Card>
  );
}

