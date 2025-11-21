'use client';

import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Progress } from '@/components/ui/progress';
import { Building2, TrendingUp, Users, ChevronRight, Award } from 'lucide-react';
import { cn } from '@/lib/utils';

interface OfficeData {
  officeId: number;
  officeName: string;
  doorsKnocked: number;
  appointmentsSet: number;
  salesClosed: number;
  activeReps: number;
  conversionRate: number;
  closeRate: number;
}

interface Props {
  offices: OfficeData[];
  onExpand?: () => void;
  isLoading?: boolean;
}

export function OfficeTile({ offices, onExpand, isLoading }: Props) {
  const getRateColor = (rate: number, type: 'conversion' | 'close') => {
    const threshold = type === 'conversion' ? 20 : 30;
    if (rate >= threshold) return 'text-green-600';
    if (rate >= threshold * 0.7) return 'text-yellow-600';
    return 'text-red-600';
  };

  const getProgressColor = (rate: number, type: 'conversion' | 'close') => {
    const threshold = type === 'conversion' ? 20 : 30;
    if (rate >= threshold) return 'bg-green-500';
    if (rate >= threshold * 0.7) return 'bg-yellow-500';
    return 'bg-red-500';
  };

  if (isLoading) {
    return (
      <Card className="hover:shadow-lg transition-all cursor-pointer border-l-4 border-l-blue-500">
        <CardHeader>
          <div className="flex items-center justify-between">
            <CardTitle className="flex items-center gap-2">
              <Building2 className="h-5 w-5 text-blue-600" />
              Office Performance
            </CardTitle>
          </div>
        </CardHeader>
        <CardContent>
          <div className="space-y-4 animate-pulse">
            <div className="h-20 bg-gray-200 rounded"></div>
            <div className="h-20 bg-gray-200 rounded"></div>
            <div className="h-20 bg-gray-200 rounded"></div>
          </div>
        </CardContent>
      </Card>
    );
  }

  const topOffice = offices[0];
  const totalDoors = offices.reduce((sum, o) => sum + o.doorsKnocked, 0);
  const totalAppointments = offices.reduce((sum, o) => sum + o.appointmentsSet, 0);
  const totalSales = offices.reduce((sum, o) => sum + o.salesClosed, 0);
  const totalReps = offices.reduce((sum, o) => sum + o.activeReps, 0);
  const avgConversion = totalDoors > 0 ? (totalAppointments / totalDoors) * 100 : 0;
  const avgCloseRate = totalAppointments > 0 ? (totalSales / totalAppointments) * 100 : 0;

  return (
    <Card
      className="hover:shadow-xl transition-all cursor-pointer border-l-4 border-l-blue-500 group"
      onClick={onExpand}
    >
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Building2 className="h-5 w-5 text-blue-600" />
            Office Performance
          </CardTitle>
          <ChevronRight className="h-5 w-5 text-gray-400 group-hover:text-blue-600 transition-colors" />
        </div>
        <p className="text-sm text-muted-foreground">
          {offices.length} offices • {totalReps} active reps
        </p>
      </CardHeader>
      <CardContent className="space-y-6">
        {/* Overview Stats */}
        <div className="grid grid-cols-3 gap-4">
          <div className="text-center p-3 bg-blue-50 rounded-lg">
            <p className="text-xs text-muted-foreground mb-1">Total Doors</p>
            <p className="text-2xl font-bold text-blue-900">{totalDoors.toLocaleString()}</p>
          </div>
          <div className="text-center p-3 bg-green-50 rounded-lg">
            <p className="text-xs text-muted-foreground mb-1">Appointments</p>
            <p className="text-2xl font-bold text-green-900">{totalAppointments.toLocaleString()}</p>
          </div>
          <div className="text-center p-3 bg-purple-50 rounded-lg">
            <p className="text-xs text-muted-foreground mb-1">Sales</p>
            <p className="text-2xl font-bold text-purple-900">{totalSales.toLocaleString()}</p>
          </div>
        </div>

        {/* Average Rates */}
        <div className="space-y-3">
          <div>
            <div className="flex items-center justify-between mb-1">
              <span className="text-sm font-medium text-gray-700">Avg Conversion Rate</span>
              <span className={cn('text-sm font-bold', getRateColor(avgConversion, 'conversion'))}>
                {avgConversion.toFixed(1)}%
              </span>
            </div>
            <Progress
              value={avgConversion}
              className={cn('h-2', getProgressColor(avgConversion, 'conversion'))}
            />
          </div>
          <div>
            <div className="flex items-center justify-between mb-1">
              <span className="text-sm font-medium text-gray-700">Avg Close Rate</span>
              <span className={cn('text-sm font-bold', getRateColor(avgCloseRate, 'close'))}>
                {avgCloseRate.toFixed(1)}%
              </span>
            </div>
            <Progress
              value={avgCloseRate}
              className={cn('h-2', getProgressColor(avgCloseRate, 'close'))}
            />
          </div>
        </div>

        {/* Top Performing Office */}
        {topOffice && (
          <div className="pt-4 border-t">
            <div className="flex items-center gap-2 mb-3">
              <Award className="h-4 w-4 text-yellow-500" />
              <h4 className="text-sm font-semibold text-gray-700">Top Performing Office</h4>
            </div>
            <Card className="bg-gradient-to-br from-yellow-50 to-amber-50 border-yellow-200">
              <CardContent className="pt-4">
                <div className="flex items-start justify-between mb-3">
                  <div>
                    <h5 className="font-bold text-lg text-gray-900">{topOffice.officeName}</h5>
                    <div className="flex items-center gap-2 mt-1">
                      <Badge variant="secondary" className="text-xs">
                        <Users className="h-3 w-3 mr-1" />
                        {topOffice.activeReps} reps
                      </Badge>
                    </div>
                  </div>
                  <Award className="h-8 w-8 text-yellow-500" />
                </div>
                <div className="grid grid-cols-2 gap-3">
                  <div>
                    <p className="text-xs text-gray-600">Doors Knocked</p>
                    <p className="text-lg font-bold">{topOffice.doorsKnocked.toLocaleString()}</p>
                  </div>
                  <div>
                    <p className="text-xs text-gray-600">Conversion</p>
                    <p className={cn('text-lg font-bold', getRateColor(topOffice.conversionRate, 'conversion'))}>
                      {topOffice.conversionRate.toFixed(1)}%
                    </p>
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>
        )}

        {/* Office List Preview */}
        {offices.length > 1 && (
          <div className="pt-4 border-t">
            <h4 className="text-sm font-semibold text-gray-700 mb-3">All Offices</h4>
            <div className="space-y-2">
              {offices.slice(0, 3).map((office, idx) => (
                <div
                  key={office.officeId}
                  className="flex items-center justify-between p-2 rounded-lg bg-gray-50"
                >
                  <div className="flex items-center gap-2 flex-1 min-w-0">
                    <Badge variant="outline" className="text-xs shrink-0">
                      #{idx + 1}
                    </Badge>
                    <span className="text-sm font-medium truncate">{office.officeName}</span>
                  </div>
                  <div className="flex items-center gap-3 shrink-0">
                    <div className="text-right">
                      <p className="text-xs text-muted-foreground">Conv</p>
                      <p className={cn('text-sm font-bold', getRateColor(office.conversionRate, 'conversion'))}>
                        {office.conversionRate.toFixed(1)}%
                      </p>
                    </div>
                    <div className="text-right">
                      <p className="text-xs text-muted-foreground">Close</p>
                      <p className={cn('text-sm font-bold', getRateColor(office.closeRate, 'close'))}>
                        {office.closeRate.toFixed(1)}%
                      </p>
                    </div>
                  </div>
                </div>
              ))}
            </div>
            {offices.length > 3 && (
              <p className="text-xs text-muted-foreground mt-2 text-center">
                +{offices.length - 3} more offices • Click to view all
              </p>
            )}
          </div>
        )}
      </CardContent>
    </Card>
  );
}
