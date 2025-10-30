'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { DoorOpen, Calendar, TrendingUp, Award, Target, Zap } from 'lucide-react';
import { formatLargeNumber, formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { cn } from '@/lib/utils/cn';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

interface RepCardOverviewCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds: number[];
}

export function RepCardOverviewCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: RepCardOverviewCardProps) {
  // Fetch doors knocked data
  const { data: doorsData, isLoading: doorsLoading } = useQuery({
    queryKey: ['repcard-overview', 'doors', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'doors_knocked',
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
      if (!response.ok) throw new Error('Failed to fetch doors data');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 60000
  });

  // Fetch appointments set data
  const { data: appointmentsData, isLoading: appointmentsLoading } = useQuery({
    queryKey: ['repcard-overview', 'appointments', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'appointments_set',
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
      if (!response.ok) throw new Error('Failed to fetch appointments data');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 60000
  });

  // Fetch quality score data
  const { data: qualityData, isLoading: qualityLoading } = useQuery({
    queryKey: ['repcard-overview', 'quality', timeRange, customDateRange, officeIds],
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
      if (!response.ok) throw new Error('Failed to fetch quality data');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 60000
  });

  const isLoading = doorsLoading || appointmentsLoading || qualityLoading;

  // Calculate metrics
  const doorsList = doorsData?.leaderboard || [];
  const appointmentsList = appointmentsData?.leaderboard || [];
  const qualityList = qualityData?.leaderboard || [];
  
  const totalDoors = doorsList.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0);
  const totalAppointments = appointmentsList.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0);
  const conversionRate = totalDoors > 0 ? (totalAppointments / totalDoors) * 100 : 0;
  
  // Calculate average quality score
  const avgQualityScore = qualityList.length > 0
    ? qualityList.reduce((sum: number, entry: any) => sum + (entry.metricValue || 0), 0) / qualityList.length
    : 0;

  // Count active reps
  const activeReps = new Set(doorsList.filter((e: any) => e.metricValue > 0).map((entry: any) => entry.userId)).size;
  
  // Calculate average per rep
  const avgDoorsPerRep = activeReps > 0 ? totalDoors / activeReps : 0;
  const avgAppointmentsPerRep = activeReps > 0 ? totalAppointments / activeReps : 0;

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Zap className="h-5 w-5 text-purple-600" />
            RepCard Overview
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {[...Array(4)].map((_, i) => (
              <div key={i} className="space-y-2">
                <Skeleton className="h-8 w-20" />
                <Skeleton className="h-4 w-32" />
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Zap className="h-5 w-5 text-purple-600" />
          RepCard Overview
        </CardTitle>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          {/* Total Doors Knocked */}
          <div className="p-4 bg-gradient-to-br from-blue-50 to-blue-100 rounded-lg border border-blue-200">
            <div className="flex items-center justify-between mb-2">
              <DoorOpen className="h-5 w-5 text-blue-600" />
              <span className="text-xs text-blue-600 font-medium">Total</span>
            </div>
            <p className="text-2xl font-bold text-blue-900">{formatLargeNumber(totalDoors)}</p>
            <p className="text-xs text-blue-700 mt-1">Doors Knocked</p>
            {activeReps > 0 && (
              <p className="text-xs text-blue-600 mt-1">Avg: {avgDoorsPerRep.toFixed(1)} per rep</p>
            )}
          </div>

          {/* Total Appointments */}
          <div className="p-4 bg-gradient-to-br from-green-50 to-green-100 rounded-lg border border-green-200">
            <div className="flex items-center justify-between mb-2">
              <Calendar className="h-5 w-5 text-green-600" />
              <span className="text-xs text-green-600 font-medium">Total</span>
            </div>
            <p className="text-2xl font-bold text-green-900">{formatLargeNumber(totalAppointments)}</p>
            <p className="text-xs text-green-700 mt-1">Appointments Set</p>
            {activeReps > 0 && (
              <p className="text-xs text-green-600 mt-1">Avg: {avgAppointmentsPerRep.toFixed(1)} per rep</p>
            )}
          </div>

          {/* Conversion Rate */}
          <div className="p-4 bg-gradient-to-br from-purple-50 to-purple-100 rounded-lg border border-purple-200">
            <div className="flex items-center justify-between mb-2">
              <TrendingUp className="h-5 w-5 text-purple-600" />
              <span className="text-xs text-purple-600 font-medium">Rate</span>
            </div>
            <p className={cn(
              "text-2xl font-bold",
              conversionRate >= 20 ? "text-purple-900" : conversionRate >= 10 ? "text-purple-700" : "text-purple-600"
            )}>
              {formatPercentage(conversionRate)}
            </p>
            <p className="text-xs text-purple-700 mt-1">Conversion Rate</p>
            <div className="mt-2 flex items-center gap-1">
              <Target className="h-3 w-3 text-purple-500" />
              <span className="text-xs text-purple-600">Target: 20%</span>
            </div>
          </div>

          {/* Quality Score */}
          <div className="p-4 bg-gradient-to-br from-amber-50 to-amber-100 rounded-lg border border-amber-200">
            <div className="flex items-center justify-between mb-2">
              <Award className="h-5 w-5 text-amber-600" />
              <span className="text-xs text-amber-600 font-medium">Average</span>
            </div>
            <p className={cn(
              "text-2xl font-bold",
              avgQualityScore >= 75 ? "text-amber-900" : avgQualityScore >= 50 ? "text-amber-700" : "text-amber-600"
            )}>
              {avgQualityScore.toFixed(1)}
            </p>
            <p className="text-xs text-amber-700 mt-1">Quality Score</p>
            <p className="text-xs text-amber-600 mt-1">{activeReps} active reps</p>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

