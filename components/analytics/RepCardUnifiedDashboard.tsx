'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { AlertCircle, RefreshCw } from 'lucide-react';
import { cn } from '@/lib/utils';
import { QualityTile } from './tiles/QualityTile';
import { OfficeTile } from './tiles/OfficeTile';
import { LeaderboardTile } from './tiles/LeaderboardTile';
import { CanvassingTile } from './tiles/CanvassingTile';
import { useState } from 'react';
import { RepCardComprehensiveDashboard } from './RepCardComprehensiveDashboard';

interface Props {
  startDate?: string;
  endDate?: string;
  className?: string;
}

export function RepCardUnifiedDashboard({ startDate, endDate, className }: Props) {
  const [expandedView, setExpandedView] = useState<'quality' | 'office' | 'leaderboard' | 'canvassing' | null>(null);

  const { data, isLoading, error, refetch, isRefetching } = useQuery({
    queryKey: ['repcard-unified-dashboard', startDate, endDate],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (startDate) params.set('startDate', startDate);
      if (endDate) params.set('endDate', endDate);

      const response = await fetch(`/api/repcard/unified-dashboard?${params.toString()}`);
      if (!response.ok) throw new Error('Failed to fetch dashboard data');
      return response.json();
    },
    refetchInterval: 60000, // Refresh every minute
  });

  // If a view is expanded, show the comprehensive dashboard instead
  if (expandedView) {
    return (
      <div className="space-y-4">
        <Button
          variant="outline"
          onClick={() => setExpandedView(null)}
          className="mb-4"
        >
          ← Back to Dashboard
        </Button>
        <RepCardComprehensiveDashboard />
      </div>
    );
  }

  if (error) {
    return (
      <Card className="border-red-200 bg-red-50">
        <CardContent className="pt-6">
          <div className="flex items-center gap-2 text-red-800">
            <AlertCircle className="h-5 w-5" />
            <div>
              <p className="text-sm font-medium">
                {error instanceof Error ? error.message : 'Failed to load dashboard'}
              </p>
              <p className="text-xs text-red-700 mt-1">
                Please try again or contact support if the issue persists.
              </p>
            </div>
          </div>
          <Button
            variant="outline"
            size="sm"
            onClick={() => refetch()}
            className="mt-4"
          >
            <RefreshCw className="h-4 w-4 mr-2" />
            Retry
          </Button>
        </CardContent>
      </Card>
    );
  }

  return (
    <div className={cn('space-y-6', className)}>
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold tracking-tight">RepCard Dashboard</h2>
          <p className="text-sm text-muted-foreground mt-1">
            {startDate && endDate
              ? `${new Date(startDate).toLocaleDateString()} - ${new Date(endDate).toLocaleDateString()}`
              : 'Last 30 days'}
          </p>
        </div>
        <Button
          variant="outline"
          size="sm"
          onClick={() => refetch()}
          disabled={isRefetching}
          className="min-h-[44px]"
        >
          <RefreshCw className={cn('h-4 w-4 mr-2', isRefetching && 'animate-spin')} />
          Refresh
        </Button>
      </div>

      {/* 4-Tile Grid */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Tile 1: Quality Dashboard (Priority #1) */}
        <div className="lg:col-span-1">
          <QualityTile
            data={data?.quality || { totalAppointments: 0, appointmentSpeed: 0, rescheduleRate: 0, powerBillRate: 0 }}
            topReschedulers={data?.topReschedulers || []}
            onExpand={() => setExpandedView('quality')}
            isLoading={isLoading}
          />
        </div>

        {/* Tile 2: Office Performance (Priority #2) */}
        <div className="lg:col-span-1">
          <OfficeTile
            offices={data?.officePerformance || []}
            onExpand={() => setExpandedView('office')}
            isLoading={isLoading}
          />
        </div>

        {/* Tile 3: Rep Leaderboards (Priority #3) - Full width */}
        <div className="lg:col-span-2">
          <LeaderboardTile
            data={data?.leaderboards || { topDoors: [], topConverters: [], topClosers: [] }}
            onExpand={() => setExpandedView('leaderboard')}
            isLoading={isLoading}
          />
        </div>

        {/* Tile 4: Canvassing Activity */}
        <div className="lg:col-span-2">
          <CanvassingTile
            data={data?.canvassing || { last30Days: { totalDoors: 0, totalAppointments: 0, avgDoorsPerDay: 0, avgConversionRate: 0 }, dailyTrends: [] }}
            onExpand={() => setExpandedView('canvassing')}
            isLoading={isLoading}
          />
        </div>
      </div>

      {/* Quick Links */}
      {!isLoading && data && (
        <Card className="bg-gradient-to-r from-gray-50 to-gray-100/50 border-dashed">
          <CardContent className="pt-6">
            <div className="flex items-center justify-between">
              <div>
                <h3 className="text-sm font-semibold text-gray-700">Need more details?</h3>
                <p className="text-xs text-muted-foreground mt-1">
                  Click any tile above to drill down into detailed views
                </p>
              </div>
              <Button
                variant="outline"
                size="sm"
                onClick={() => setExpandedView('quality')}
              >
                View All Data →
              </Button>
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  );
}
