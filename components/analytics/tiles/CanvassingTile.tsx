'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Activity, TrendingUp, Calendar, DoorOpen, ChevronRight } from 'lucide-react';
import { cn } from '@/lib/utils';

interface CanvassingData {
  last30Days: {
    totalDoors: number;
    totalAppointments: number;
    avgDoorsPerDay: number;
    avgConversionRate: number;
  };
  dailyTrends: Array<{
    date: string;
    doorsKnocked: number;
    appointmentsSet: number;
    conversionRate: number;
  }>;
}

interface Props {
  data: CanvassingData;
  onExpand?: () => void;
  isLoading?: boolean;
}

export function CanvassingTile({ data, onExpand, isLoading }: Props) {
  if (isLoading) {
    return (
      <Card className="hover:shadow-lg transition-all cursor-pointer border-l-4 border-l-orange-500">
        <CardHeader>
          <div className="flex items-center justify-between">
            <CardTitle className="flex items-center gap-2">
              <Activity className="h-5 w-5 text-orange-600" />
              Canvassing Activity
            </CardTitle>
          </div>
        </CardHeader>
        <CardContent>
          <div className="space-y-4 animate-pulse">
            <div className="h-20 bg-gray-200 rounded"></div>
            <div className="h-32 bg-gray-200 rounded"></div>
          </div>
        </CardContent>
      </Card>
    );
  }

  const recentTrends = data.dailyTrends.slice(0, 7).reverse(); // Last 7 days, chronological
  const maxDoors = Math.max(...recentTrends.map(d => d.doorsKnocked));

  return (
    <Card
      className="hover:shadow-xl transition-all cursor-pointer border-l-4 border-l-orange-500 group"
      onClick={onExpand}
    >
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Activity className="h-5 w-5 text-orange-600" />
            Canvassing Activity
          </CardTitle>
          <ChevronRight className="h-5 w-5 text-gray-400 group-hover:text-orange-600 transition-colors" />
        </div>
        <p className="text-sm text-muted-foreground">
          Last 30 days performance
        </p>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Overview Stats */}
        <div className="grid grid-cols-2 gap-4">
          <div className="space-y-1">
            <div className="flex items-center gap-2">
              <DoorOpen className="h-4 w-4 text-orange-600" />
              <span className="text-xs font-medium text-gray-600">Total Doors</span>
            </div>
            <p className="text-3xl font-bold text-orange-900">
              {data.last30Days.totalDoors.toLocaleString()}
            </p>
            <p className="text-xs text-muted-foreground">
              ~{Math.round(data.last30Days.avgDoorsPerDay)} per day
            </p>
          </div>

          <div className="space-y-1">
            <div className="flex items-center gap-2">
              <Calendar className="h-4 w-4 text-green-600" />
              <span className="text-xs font-medium text-gray-600">Appointments</span>
            </div>
            <p className="text-3xl font-bold text-green-900">
              {data.last30Days.totalAppointments.toLocaleString()}
            </p>
            <p className="text-xs text-muted-foreground">
              {data.last30Days.avgConversionRate.toFixed(1)}% conversion
            </p>
          </div>
        </div>

        {/* Conversion Rate Badge */}
        <div className="flex items-center justify-between p-3 bg-gradient-to-r from-orange-50 to-orange-100/50 rounded-lg border border-orange-200">
          <div className="flex items-center gap-2">
            <TrendingUp className="h-5 w-5 text-orange-600" />
            <div>
              <p className="text-xs text-gray-600">Avg Conversion Rate</p>
              <p className="text-lg font-bold text-orange-900">
                {data.last30Days.avgConversionRate.toFixed(1)}%
              </p>
            </div>
          </div>
          <Badge
            className={cn(
              data.last30Days.avgConversionRate >= 20
                ? 'bg-green-100 text-green-800'
                : data.last30Days.avgConversionRate >= 15
                ? 'bg-yellow-100 text-yellow-800'
                : 'bg-red-100 text-red-800'
            )}
          >
            {data.last30Days.avgConversionRate >= 20 ? 'Excellent' : data.last30Days.avgConversionRate >= 15 ? 'Good' : 'Needs Work'}
          </Badge>
        </div>

        {/* 7-Day Mini Chart */}
        <div className="pt-4 border-t">
          <h4 className="text-sm font-semibold text-gray-700 mb-3 flex items-center gap-2">
            <Activity className="h-4 w-4" />
            Last 7 Days
          </h4>
          <div className="space-y-2">
            {recentTrends.map((day) => {
              const barWidth = maxDoors > 0 ? (day.doorsKnocked / maxDoors) * 100 : 0;
              const isToday = new Date(day.date).toDateString() === new Date().toDateString();

              return (
                <div key={day.date} className="space-y-1">
                  <div className="flex items-center justify-between text-xs">
                    <span className={cn('font-medium', isToday && 'text-orange-600')}>
                      {new Date(day.date).toLocaleDateString('en-US', { weekday: 'short', month: 'short', day: 'numeric' })}
                      {isToday && <span className="ml-1 text-orange-600">(Today)</span>}
                    </span>
                    <div className="flex items-center gap-2">
                      <span className="text-gray-600">{day.doorsKnocked} doors</span>
                      <Badge variant="outline" className="text-xs">
                        {day.conversionRate.toFixed(1)}%
                      </Badge>
                    </div>
                  </div>
                  <div className="relative h-2 bg-gray-100 rounded-full overflow-hidden">
                    <div
                      className="absolute left-0 top-0 h-full bg-gradient-to-r from-orange-400 to-orange-600 rounded-full transition-all"
                      style={{ width: `${barWidth}%` }}
                    />
                  </div>
                </div>
              );
            })}
          </div>
          <p className="text-xs text-muted-foreground mt-3 text-center">
            Click to view full 30-day trends
          </p>
        </div>
      </CardContent>
    </Card>
  );
}
