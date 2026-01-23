'use client';

import { useState, useMemo } from 'react';
import { useSession } from 'next-auth/react';
import { redirect } from 'next/navigation';
import { format } from 'date-fns';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { useQuery } from '@tanstack/react-query';
import { RefreshCw, Trophy, Users, Building2, Calendar, Clock } from 'lucide-react';
import { toast } from 'sonner';
import { getTodayLocal } from '@/lib/utils/timezone';
import { SettersLeaderboard } from '@/components/leaderboards/SettersLeaderboard';
import { ClosersLeaderboard } from '@/components/leaderboards/ClosersLeaderboard';
import { OfficesLeaderboard } from '@/components/leaderboards/OfficesLeaderboard';
import { LeaderboardFilters } from '@/components/leaderboards/LeaderboardFilters';

type TimeRange = 'today' | 'week' | 'month' | 'ytd' | 'custom';

interface CustomDateRange {
  startDate: string;
  endDate: string;
}

export default function LeaderboardsPage() {
  const { data: session, status } = useSession();
  const [timeRange, setTimeRange] = useState<TimeRange>('today');
  const [customDateRange, setCustomDateRange] = useState<CustomDateRange | undefined>();
  const [selectedOfficeIds, setSelectedOfficeIds] = useState<number[]>([]);
  const [selectedTeamNames, setSelectedTeamNames] = useState<string[]>([]);

  // Redirect if not authenticated
  if (status === 'unauthenticated') {
    redirect('/login');
  }

  // Check authorization
  const authorizedRoles = ['team_lead', 'area_director', 'divisional', 'office_leader', 'regional', 'super_admin'];
  if (status === 'authenticated' && session?.user?.role && !authorizedRoles.includes(session.user.role)) {
    redirect('/');
  }

  // Calculate date range
  const dateRange = useMemo(() => {
    if (timeRange === 'custom' && customDateRange) {
      return customDateRange;
    }

    const now = new Date();
    const endDate = format(now, 'yyyy-MM-dd');
    let startDate: string;

    switch (timeRange) {
      case 'today':
        startDate = getTodayLocal();
        break;
      case 'week':
        const weekStart = new Date(now);
        weekStart.setDate(now.getDate() - now.getDay() + 1); // Monday
        startDate = format(weekStart, 'yyyy-MM-dd');
        break;
      case 'month':
        startDate = format(new Date(now.getFullYear(), now.getMonth(), 1), 'yyyy-MM-dd');
        break;
      case 'ytd':
        startDate = format(new Date(now.getFullYear(), 0, 1), 'yyyy-MM-dd');
        break;
      default:
        startDate = getTodayLocal();
    }

    return { startDate, endDate };
  }, [timeRange, customDateRange]);

  // Fetch leaderboards data
  const { data, isLoading, error, refetch, isRefetching } = useQuery({
    queryKey: ['repcard-leaderboards', dateRange.startDate, dateRange.endDate, selectedOfficeIds, selectedTeamNames],
    queryFn: async () => {
      const params = new URLSearchParams();
      params.set('startDate', dateRange.startDate);
      params.set('endDate', dateRange.endDate);
      if (selectedOfficeIds.length > 0) {
        params.set('officeIds', selectedOfficeIds.join(','));
      }
      if (selectedTeamNames.length > 0) {
        params.set('teamIds', selectedTeamNames.join(','));
      }

      const response = await fetch(`/api/repcard/leaderboards?${params.toString()}`);
      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Failed to fetch leaderboards');
      }
      return response.json();
    },
    refetchInterval: 30000, // Refresh every 30 seconds
    staleTime: 5000,
  });

  // Extract unique teams from leaderboard data
  const availableTeams = useMemo(() => {
    const teamsSet = new Set<string>();
    if (data?.setters) {
      data.setters.forEach((setter: any) => {
        if (setter.team && setter.team !== 'No Team') {
          teamsSet.add(setter.team);
        }
      });
    }
    if (data?.closers) {
      data.closers.forEach((closer: any) => {
        if (closer.team && closer.team !== 'No Team') {
          teamsSet.add(closer.team);
        }
      });
    }
    return Array.from(teamsSet).sort();
  }, [data]);

  // Get sync status
  const syncStatus = useMemo(() => {
    if (!data?.metadata) return null;
    const lastSync = data.metadata.lastSyncTime;
    if (!lastSync) return null;
    
    const syncDate = new Date(lastSync);
    const now = new Date();
    const diffMinutes = Math.floor((now.getTime() - syncDate.getTime()) / 60000);
    
    if (diffMinutes < 1) return 'Just now';
    if (diffMinutes < 60) return `${diffMinutes} min ago`;
    const diffHours = Math.floor(diffMinutes / 60);
    if (diffHours < 24) return `${diffHours} hour${diffHours > 1 ? 's' : ''} ago`;
    const diffDays = Math.floor(diffHours / 24);
    return `${diffDays} day${diffDays > 1 ? 's' : ''} ago`;
  }, [data?.metadata]);

  // Date range display
  const dateRangeDisplay = useMemo(() => {
    if (timeRange === 'today') return 'Today';
    if (timeRange === 'week') return 'Week to Date';
    if (timeRange === 'month') return 'Month to Date';
    if (timeRange === 'ytd') return 'Year to Date';
    if (timeRange === 'custom' && customDateRange) {
      return `${format(new Date(customDateRange.startDate), 'MMM d')} - ${format(new Date(customDateRange.endDate), 'MMM d, yyyy')}`;
    }
    return 'Today';
  }, [timeRange, customDateRange]);

  if (status === 'loading') {
    return (
      <div className="min-h-screen bg-gray-50 p-6">
        <div className="max-w-7xl mx-auto">
          <div className="animate-pulse space-y-4">
            <div className="h-8 bg-gray-200 rounded w-1/4"></div>
            <div className="h-64 bg-gray-200 rounded"></div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gray-50 p-6">
      <div className="max-w-7xl mx-auto space-y-6">
        {/* Header */}
        <div className="flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold flex items-center gap-2">
              <Trophy className="w-8 h-8" />
              Leaderboards
            </h1>
            <p className="text-muted-foreground mt-1">
              RepCard performance rankings and quality metrics
            </p>
          </div>
          <div className="flex items-center gap-4">
            {syncStatus && (
              <Badge variant="outline" className="flex items-center gap-2">
                <Clock className="w-3 h-3" />
                Synced {syncStatus}
              </Badge>
            )}
            <Button
              variant="outline"
              onClick={() => refetch()}
              disabled={isRefetching}
            >
              <RefreshCw className={`w-4 h-4 mr-2 ${isRefetching ? 'animate-spin' : ''}`} />
              Refresh
            </Button>
          </div>
        </div>

        {/* Filters */}
        <LeaderboardFilters
          timeRange={timeRange}
          onTimeRangeChange={setTimeRange}
          customDateRange={customDateRange}
          onCustomDateRangeChange={setCustomDateRange}
          selectedOfficeIds={selectedOfficeIds}
          onOfficeIdsChange={setSelectedOfficeIds}
          selectedTeamNames={selectedTeamNames}
          onTeamNamesChange={setSelectedTeamNames}
          availableTeams={availableTeams}
        />

        {/* Date Range Display */}
        <div className="flex items-center gap-2 text-sm text-muted-foreground">
          <Calendar className="w-4 h-4" />
          <span>{dateRangeDisplay}</span>
        </div>

        {/* Error State */}
        {error && (
          <Card className="border-red-200 bg-red-50">
            <CardContent className="pt-6">
              <p className="text-red-600">
                {error instanceof Error ? error.message : 'Failed to load leaderboards'}
              </p>
            </CardContent>
          </Card>
        )}

        {/* Tabs */}
        <Tabs defaultValue="setters" className="space-y-4">
          <TabsList>
            <TabsTrigger value="setters">
              <Users className="w-4 h-4 mr-2" />
              Setters
            </TabsTrigger>
            <TabsTrigger value="closers">
              <Trophy className="w-4 h-4 mr-2" />
              Closers
            </TabsTrigger>
            <TabsTrigger value="offices">
              <Building2 className="w-4 h-4 mr-2" />
              Offices
            </TabsTrigger>
          </TabsList>

          <TabsContent value="setters">
            <SettersLeaderboard
              data={data?.setters || []}
              isLoading={isLoading}
              dateRange={dateRangeDisplay}
            />
          </TabsContent>

          <TabsContent value="closers">
            <ClosersLeaderboard
              data={data?.closers || []}
              isLoading={isLoading}
              dateRange={dateRangeDisplay}
            />
          </TabsContent>

          <TabsContent value="offices">
            <OfficesLeaderboard
              data={data?.offices || []}
              isLoading={isLoading}
              dateRange={dateRangeDisplay}
            />
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
}
