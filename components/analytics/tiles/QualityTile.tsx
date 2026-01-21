'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Progress } from '@/components/ui/progress';
import { Target, RefreshCw, FileText, Zap, TrendingUp, TrendingDown, ChevronRight, AlertCircle } from 'lucide-react';
import { cn } from '@/lib/utils';

interface QualityData {
  totalAppointments: number;
  appointmentSpeed: number; // % within 48h
  powerBillRate: number; // % with power bill
  withBoth?: {
    count: number;
    percentage: number;
  };
  withNeither?: {
    count: number;
    percentage: number;
  };
  rescheduleRate: number;
}

interface TopRescheduler {
  userId: number;
  name: string;
  totalAppointments: number;
  reschedules: number;
  rescheduleRate: number;
}

interface Props {
  data: QualityData;
  topReschedulers: TopRescheduler[];
  onExpand?: () => void;
  isLoading?: boolean;
}

export function QualityTile({ data, topReschedulers, onExpand, isLoading }: Props) {
  const getQualityColor = (value: number, type: 'speed' | 'reschedule' | 'powerBill') => {
    if (type === 'speed') {
      if (value >= 70) return 'text-green-600';
      if (value >= 50) return 'text-yellow-600';
      return 'text-red-600';
    }
    if (type === 'reschedule') {
      if (value < 10) return 'text-green-600';
      if (value < 25) return 'text-yellow-600';
      return 'text-red-600';
    }
    if (type === 'powerBill') {
      if (value >= 80) return 'text-green-600';
      if (value >= 60) return 'text-yellow-600';
      return 'text-red-600';
    }
    return 'text-gray-600';
  };

  const getProgressColor = (value: number, type: 'speed' | 'reschedule' | 'powerBill') => {
    if (type === 'speed') {
      if (value >= 70) return 'bg-green-500';
      if (value >= 50) return 'bg-yellow-500';
      return 'bg-red-500';
    }
    if (type === 'reschedule') {
      if (value < 10) return 'bg-green-500';
      if (value < 25) return 'bg-yellow-500';
      return 'bg-red-500';
    }
    if (type === 'powerBill') {
      if (value >= 80) return 'bg-green-500';
      if (value >= 60) return 'bg-yellow-500';
      return 'bg-red-500';
    }
    return 'bg-gray-500';
  };

  const getTrendIcon = (value: number, isGoodHigh: boolean) => {
    const isHigh = isGoodHigh ? value >= 70 : value < 15;
    return isHigh ? (
      <TrendingUp className="h-4 w-4 text-green-600" />
    ) : (
      <TrendingDown className="h-4 w-4 text-red-600" />
    );
  };

  if (isLoading) {
    return (
      <Card className="hover:shadow-lg transition-all cursor-pointer border-l-4 border-l-purple-500">
        <CardHeader>
          <div className="flex items-center justify-between">
            <CardTitle className="flex items-center gap-2">
              <Target className="h-5 w-5 text-purple-600" />
              Appointment Quality
            </CardTitle>
          </div>
        </CardHeader>
        <CardContent>
          <div className="space-y-4 animate-pulse">
            <div className="h-16 bg-gray-200 rounded"></div>
            <div className="h-16 bg-gray-200 rounded"></div>
            <div className="h-16 bg-gray-200 rounded"></div>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card
      className="hover:shadow-xl transition-all cursor-pointer border-l-4 border-l-purple-500 group"
      onClick={onExpand}
    >
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Target className="h-5 w-5 text-purple-600" />
            Appointment Quality
          </CardTitle>
          <ChevronRight className="h-5 w-5 text-gray-400 group-hover:text-purple-600 transition-colors" />
        </div>
        <p className="text-sm text-muted-foreground">
          Based on {data.totalAppointments.toLocaleString()} appointments
        </p>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Key Metrics Grid */}
        <div className="grid grid-cols-3 gap-4">
          {/* 48-Hour Speed */}
          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <Zap className="h-4 w-4 text-purple-600" />
              <span className="text-xs font-medium text-gray-600">48-Hour Speed</span>
            </div>
            <div className="flex items-center gap-2">
              <p className={cn('text-2xl font-bold', getQualityColor(data.appointmentSpeed, 'speed'))}>
                {data.appointmentSpeed.toFixed(1)}%
              </p>
              {getTrendIcon(data.appointmentSpeed, true)}
            </div>
            <Progress
              value={data.appointmentSpeed}
              className={cn('h-2', getProgressColor(data.appointmentSpeed, 'speed'))}
            />
            <p className="text-xs text-muted-foreground">
              {data.appointmentSpeed >= 70 ? 'Excellent' : data.appointmentSpeed >= 50 ? 'Good' : 'Needs Work'}
            </p>
          </div>

          {/* Reschedule Rate */}
          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <RefreshCw className="h-4 w-4 text-orange-600" />
              <span className="text-xs font-medium text-gray-600">Reschedule Rate</span>
            </div>
            <div className="flex items-center gap-2">
              <p className={cn('text-2xl font-bold', getQualityColor(data.rescheduleRate, 'reschedule'))}>
                {data.rescheduleRate.toFixed(1)}%
              </p>
              {getTrendIcon(data.rescheduleRate, false)}
            </div>
            <Progress
              value={Math.min(data.rescheduleRate, 100)}
              className={cn('h-2', getProgressColor(data.rescheduleRate, 'reschedule'))}
            />
            <p className="text-xs text-muted-foreground">
              {data.rescheduleRate < 10 ? 'Excellent' : data.rescheduleRate < 25 ? 'Fair' : 'High'}
            </p>
          </div>

          {/* Power Bill Rate */}
          <div className="space-y-2">
            <div className="flex items-center gap-2">
              <FileText className="h-4 w-4 text-blue-600" />
              <span className="text-xs font-medium text-gray-600">Power Bill Rate</span>
            </div>
            <div className="flex items-center gap-2">
              <p className={cn('text-2xl font-bold', getQualityColor(data.powerBillRate, 'powerBill'))}>
                {data.powerBillRate.toFixed(1)}%
              </p>
              {getTrendIcon(data.powerBillRate, true)}
            </div>
            <Progress
              value={data.powerBillRate}
              className={cn('h-2', getProgressColor(data.powerBillRate, 'powerBill'))}
            />
            <p className="text-xs text-muted-foreground">
              {data.powerBillRate >= 80 ? 'Excellent' : data.powerBillRate >= 60 ? 'Good' : 'Needs Work'}
            </p>
          </div>
        </div>

        {/* Combined Quality Metrics - Shows Both and Neither */}
        {(data.withBoth || data.withNeither) && (
          <div className="pt-4 border-t">
            <h4 className="text-sm font-semibold text-gray-700 mb-3">Quality Breakdown</h4>
            <div className="grid grid-cols-2 gap-4">
              {/* High Quality: Both */}
              {data.withBoth && (
                <div className="p-3 rounded-lg bg-gradient-to-br from-green-50 to-green-100/50 border border-green-200">
                  <div className="flex items-center gap-2 mb-2">
                    <Target className="h-4 w-4 text-green-600" />
                    <span className="text-xs font-semibold text-green-700">High Quality</span>
                  </div>
                  <p className="text-2xl font-bold text-green-900">
                    {data.withBoth.count}
                  </p>
                  <p className="text-xs text-green-600 mt-1">
                    {data.withBoth.percentage.toFixed(1)}% have both PB & 48h
                  </p>
                  <p className="text-xs text-green-500 mt-1">
                    Higher likelihood to sit
                  </p>
                </div>
              )}

              {/* Low Quality: Neither */}
              {data.withNeither && (
                <div className="p-3 rounded-lg bg-gradient-to-br from-red-50 to-red-100/50 border border-red-200">
                  <div className="flex items-center gap-2 mb-2">
                    <AlertCircle className="h-4 w-4 text-red-600" />
                    <span className="text-xs font-semibold text-red-700">Low Quality</span>
                  </div>
                  <p className="text-2xl font-bold text-red-900">
                    {data.withNeither.count}
                  </p>
                  <p className="text-xs text-red-600 mt-1">
                    {data.withNeither.percentage.toFixed(1)}% have neither
                  </p>
                  <p className="text-xs text-red-500 mt-1">
                    Higher cancellation risk
                  </p>
                </div>
              )}
            </div>
          </div>
        )}

        {/* Top Reschedulers Preview */}
        {topReschedulers.length > 0 && (
          <div className="pt-4 border-t">
            <h4 className="text-sm font-semibold text-gray-700 mb-3 flex items-center gap-2">
              <RefreshCw className="h-4 w-4" />
              Top Reschedulers
            </h4>
            <div className="space-y-2">
              {topReschedulers.slice(0, 3).map((user, idx) => (
                <div
                  key={user.userId}
                  className="flex items-center justify-between p-2 rounded-lg bg-gray-50"
                >
                  <div className="flex items-center gap-2">
                    <Badge variant="outline" className="text-xs">
                      #{idx + 1}
                    </Badge>
                    <span className="text-sm font-medium">{user.name}</span>
                  </div>
                  <Badge
                    className={cn(
                      'text-xs',
                      user.rescheduleRate < 10
                        ? 'bg-green-100 text-green-800'
                        : user.rescheduleRate < 25
                        ? 'bg-yellow-100 text-yellow-800'
                        : 'bg-red-100 text-red-800'
                    )}
                  >
                    {user.rescheduleRate.toFixed(1)}%
                  </Badge>
                </div>
              ))}
            </div>
            {topReschedulers.length > 3 && (
              <p className="text-xs text-muted-foreground mt-2 text-center">
                +{topReschedulers.length - 3} more • Click to view all
              </p>
            )}
          </div>
        )}
      </CardContent>
    </Card>
  );
}
