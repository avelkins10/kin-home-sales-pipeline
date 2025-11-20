'use client';

import { useQuery } from '@tanstack/react-query';
import { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { 
  Calendar, 
  DoorOpen, 
  Users, 
  TrendingUp, 
  TrendingDown,
  FileText, 
  MessageSquare, 
  Paperclip,
  Award,
  Building2,
  Clock,
  CheckCircle2,
  XCircle,
  AlertCircle,
  Target,
  Zap,
  BarChart3,
  RefreshCw,
  Filter,
  ChevronRight,
  Star,
  Activity
} from 'lucide-react';
import { cn } from '@/lib/utils';
import { Skeleton } from '@/components/ui/skeleton';
import { Progress } from '@/components/ui/progress';
import { formatRepCardDate, formatRepCardDateRange } from '@/lib/utils/repcard-date-helpers';

interface ComprehensiveStats {
  success: boolean;
  dateRange: {
    startDate: string;
    endDate: string;
  };
  overview: {
    doorsKnocked: number;
    appointmentsSet: number;
    salesClosed: number;
    customersWithAttachments: number;
    totalNotes: number;
    statusChanges: number;
    conversionRate: number;
    closeRate: number;
  };
  userPerformance: Array<{
    repcardUserId: number;
    name: string;
    email: string;
    office: string | null;
    officeId: number | null;
    team: string | null;
    doorsKnocked: number;
    appointmentsSet: number;
    appointmentsClosed: number;
    salesClosed: number;
    customersWithAttachments: number;
    notesWritten: number;
    appointmentsWithin48h: number;
    conversionRate: number;
    closeRate: number;
  }>;
  officeBreakdown: Array<{
    officeId: number;
    officeName: string;
    doorsKnocked: number;
    appointmentsSet: number;
    salesClosed: number;
    activeReps: number;
  }>;
  customers?: Array<any>;
  appointments?: Array<any>;
  statusLogs?: Array<any>;
  attachments?: Array<any>;
  notes?: Array<any>;
}

export function RepCardComprehensiveDashboard() {
  const [includeDetails, setIncludeDetails] = useState(true);
  const [dateRange, setDateRange] = useState({ startDate: '', endDate: '' });

  const { data, isLoading, error, refetch, isRefetching } = useQuery<ComprehensiveStats>({
    queryKey: ['repcard-comprehensive-stats', includeDetails, dateRange],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (includeDetails) params.set('includeDetails', 'true');
      if (dateRange.startDate) params.set('startDate', dateRange.startDate);
      if (dateRange.endDate) params.set('endDate', dateRange.endDate);
      
      const response = await fetch(`/api/repcard/comprehensive-stats?${params.toString()}`);
      if (!response.ok) throw new Error('Failed to fetch RepCard stats');
      return response.json();
    },
    refetchInterval: 30000, // Refresh every 30 seconds
  });

  if (isLoading) {
    return (
      <div className="space-y-6">
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          {[1, 2, 3, 4].map((i) => (
            <Skeleton key={i} className="h-32 w-full" />
          ))}
        </div>
        <Skeleton className="h-64 w-full" />
        <Skeleton className="h-96 w-full" />
      </div>
    );
  }

  if (error || !data?.success) {
    return (
      <Card className="border-red-200 bg-red-50">
        <CardContent className="pt-6">
          <div className="flex items-center gap-2 text-red-800">
            <AlertCircle className="h-5 w-5" />
            <p className="text-sm font-medium">
              {error instanceof Error ? error.message : 'Failed to load RepCard data'}
            </p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const { overview, userPerformance, officeBreakdown, customers = [], appointments = [], statusLogs = [], attachments = [], notes = [] } = data;

  const formatNumber = (num: number) => {
    if (num >= 1000000) return `${(num / 1000000).toFixed(1)}M`;
    if (num >= 1000) return `${(num / 1000).toFixed(1)}K`;
    return num.toLocaleString();
  };

  const formatPercentage = (num: number) => {
    return `${num.toFixed(1)}%`;
  };

  const getPerformanceColor = (rate: number, type: 'conversion' | 'close') => {
    const threshold = type === 'conversion' ? 20 : 30; // 20% conversion, 30% close rate
    if (rate >= threshold) return 'text-green-600';
    if (rate >= threshold * 0.7) return 'text-yellow-600';
    return 'text-red-600';
  };

  const getDispositionColor = (disposition: string) => {
    if (!disposition) return 'bg-gray-100 text-gray-800 border-gray-300';
    const d = disposition.toLowerCase();
    if (d.includes('closed')) return 'bg-green-100 text-green-800 border-green-300';
    if (d.includes('cancel')) return 'bg-red-100 text-red-800 border-red-300';
    if (d.includes('reschedule')) return 'bg-yellow-100 text-yellow-800 border-yellow-300';
    if (d.includes('no.show') || d.includes('no_show')) return 'bg-orange-100 text-orange-800 border-orange-300';
    return 'bg-blue-100 text-blue-800 border-blue-300';
  };

  const getStatusColor = (status: string) => {
    if (!status) return 'bg-gray-100 text-gray-800';
    const s = status.toLowerCase();
    if (s.includes('closed') || s.includes('sold')) return 'bg-green-100 text-green-800';
    if (s.includes('hold') || s.includes('cancel')) return 'bg-red-100 text-red-800';
    if (s.includes('pending') || s.includes('scheduled')) return 'bg-yellow-100 text-yellow-800';
    return 'bg-blue-100 text-blue-800';
  };

  // Calculate top performers
  const topDoorsKnocked = [...userPerformance].sort((a, b) => b.doorsKnocked - a.doorsKnocked).slice(0, 3);
  const topSales = [...userPerformance].sort((a, b) => b.salesClosed - a.salesClosed).slice(0, 3);
  const topConversion = [...userPerformance]
    .filter(u => u.doorsKnocked >= 10)
    .sort((a, b) => b.conversionRate - a.conversionRate)
    .slice(0, 3);

  // Calculate averages for context
  const avgConversionRate = userPerformance.length > 0
    ? userPerformance.reduce((sum, u) => sum + u.conversionRate, 0) / userPerformance.length
    : 0;
  const avgCloseRate = userPerformance.length > 0
    ? userPerformance.reduce((sum, u) => sum + u.closeRate, 0) / userPerformance.length
    : 0;

  return (
    <div className="space-y-6">
      {/* Header with Controls */}
      <div className="flex flex-col sm:flex-row justify-between items-start sm:items-center gap-4">
        <div>
          <h2 className="text-2xl font-bold tracking-tight">RepCard Analytics</h2>
          <p className="text-sm text-muted-foreground mt-1">
            {data.dateRange.startDate && data.dateRange.endDate
              ? formatRepCardDateRange(data.dateRange.startDate, data.dateRange.endDate)
              : 'Last 90 days'}
          </p>
        </div>
        <div className="flex items-center gap-2">
          <Button
            variant="outline"
            size="sm"
            onClick={() => refetch()}
            disabled={isRefetching}
            className="min-h-[44px]"
          >
            <RefreshCw className={cn("h-4 w-4 mr-2", isRefetching && "animate-spin")} />
            Refresh
          </Button>
        </div>
      </div>

      {/* Key Metrics Overview - Enhanced Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        {/* Doors Knocked */}
        <Card className="border-l-4 border-l-blue-500 hover:shadow-lg transition-shadow">
          <CardContent className="pt-6">
            <div className="flex items-center justify-between mb-4">
              <div className="p-2 bg-blue-100 rounded-lg">
                <DoorOpen className="h-6 w-6 text-blue-600" />
              </div>
              {overview.conversionRate > 0 && (
                <Badge variant="outline" className="text-xs">
                  {formatPercentage(overview.conversionRate)} conv
                </Badge>
              )}
            </div>
            <p className="text-3xl font-bold text-blue-900 mb-1">{formatNumber(overview.doorsKnocked)}</p>
            <p className="text-sm text-muted-foreground">Doors Knocked</p>
            {overview.doorsKnocked > 0 && (
              <Progress 
                value={Math.min((overview.doorsKnocked / 1000) * 100, 100)} 
                className="mt-3 h-2"
              />
            )}
          </CardContent>
        </Card>

        {/* Appointments Set */}
        <Card className="border-l-4 border-l-green-500 hover:shadow-lg transition-shadow">
          <CardContent className="pt-6">
            <div className="flex items-center justify-between mb-4">
              <div className="p-2 bg-green-100 rounded-lg">
                <Calendar className="h-6 w-6 text-green-600" />
              </div>
              {overview.closeRate > 0 && (
                <Badge variant="outline" className="text-xs">
                  {formatPercentage(overview.closeRate)} close
                </Badge>
              )}
            </div>
            <p className="text-3xl font-bold text-green-900 mb-1">{formatNumber(overview.appointmentsSet)}</p>
            <p className="text-sm text-muted-foreground">Appointments Set</p>
            {overview.appointmentsSet > 0 && overview.doorsKnocked > 0 && (
              <div className="mt-3">
                <Progress 
                  value={overview.conversionRate} 
                  className="h-2"
                />
                <p className="text-xs text-muted-foreground mt-1">
                  {formatPercentage(overview.conversionRate)} conversion rate
                </p>
              </div>
            )}
          </CardContent>
        </Card>

        {/* Sales Closed */}
        <Card className="border-l-4 border-l-purple-500 hover:shadow-lg transition-shadow">
          <CardContent className="pt-6">
            <div className="flex items-center justify-between mb-4">
              <div className="p-2 bg-purple-100 rounded-lg">
                <Award className="h-6 w-6 text-purple-600" />
              </div>
              {overview.closeRate > 0 && (
                <Badge className="bg-purple-600 text-xs">
                  {formatPercentage(overview.closeRate)}
                </Badge>
              )}
            </div>
            <p className="text-3xl font-bold text-purple-900 mb-1">{formatNumber(overview.salesClosed)}</p>
            <p className="text-sm text-muted-foreground">Sales Closed</p>
            {overview.salesClosed > 0 && overview.appointmentsSet > 0 && (
              <div className="mt-3">
                <Progress 
                  value={overview.closeRate} 
                  className="h-2"
                />
                <p className="text-xs text-muted-foreground mt-1">
                  {formatPercentage(overview.closeRate)} close rate
                </p>
              </div>
            )}
          </CardContent>
        </Card>

        {/* Active Reps */}
        <Card className="border-l-4 border-l-pink-500 hover:shadow-lg transition-shadow">
          <CardContent className="pt-6">
            <div className="flex items-center justify-between mb-4">
              <div className="p-2 bg-pink-100 rounded-lg">
                <Users className="h-6 w-6 text-pink-600" />
              </div>
              <Badge variant="outline" className="text-xs">
                {officeBreakdown.length} offices
              </Badge>
            </div>
            <p className="text-3xl font-bold text-pink-900 mb-1">{userPerformance.length}</p>
            <p className="text-sm text-muted-foreground">Active Reps</p>
            {userPerformance.length > 0 && (
              <div className="mt-3 flex items-center gap-2 text-xs text-muted-foreground">
                <Activity className="h-3 w-3" />
                <span>Avg: {formatPercentage(avgConversionRate)} conv, {formatPercentage(avgCloseRate)} close</span>
              </div>
            )}
          </CardContent>
        </Card>
      </div>

      {/* Secondary Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <Card className="hover:shadow-md transition-shadow">
          <CardContent className="pt-6">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-indigo-100 rounded-lg">
                <MessageSquare className="h-5 w-5 text-indigo-600" />
              </div>
              <div className="flex-1">
                <p className="text-2xl font-bold">{formatNumber(overview.totalNotes)}</p>
                <p className="text-xs text-muted-foreground">Total Notes</p>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card className="hover:shadow-md transition-shadow">
          <CardContent className="pt-6">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-teal-100 rounded-lg">
                <TrendingUp className="h-5 w-5 text-teal-600" />
              </div>
              <div className="flex-1">
                <p className="text-2xl font-bold">{formatNumber(overview.statusChanges)}</p>
                <p className="text-xs text-muted-foreground">Status Changes</p>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card className="hover:shadow-md transition-shadow">
          <CardContent className="pt-6">
            <div className="flex items-center gap-3">
              <div className="p-2 bg-amber-100 rounded-lg">
                <Paperclip className="h-5 w-5 text-amber-600" />
              </div>
              <div className="flex-1">
                <p className="text-2xl font-bold">{formatNumber(overview.customersWithAttachments)}</p>
                <p className="text-xs text-muted-foreground">Customers w/ Files</p>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Top Performers Quick View */}
      {(topDoorsKnocked.length > 0 || topSales.length > 0) && (
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
          <Card>
            <CardHeader>
              <CardTitle className="text-base flex items-center gap-2">
                <Star className="h-4 w-4 text-yellow-500" />
                Top Doors Knocked
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {topDoorsKnocked.map((user, idx) => (
                  <div key={user.repcardUserId} className="flex items-center justify-between p-2 rounded-lg hover:bg-muted/50 transition-colors">
                    <div className="flex items-center gap-2 flex-1 min-w-0">
                      <div className={cn(
                        "flex items-center justify-center w-6 h-6 rounded-full text-xs font-bold text-white",
                        idx === 0 && "bg-yellow-500",
                        idx === 1 && "bg-gray-400",
                        idx === 2 && "bg-orange-600"
                      )}>
                        {idx + 1}
                      </div>
                      <div className="flex-1 min-w-0">
                        <p className="font-medium text-sm truncate">{user.name}</p>
                        {user.office && <p className="text-xs text-muted-foreground truncate">{user.office}</p>}
                      </div>
                    </div>
                    <div className="text-right">
                      <p className="font-bold text-sm">{formatNumber(user.doorsKnocked)}</p>
                      <p className="text-xs text-muted-foreground">{formatPercentage(user.conversionRate)}</p>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle className="text-base flex items-center gap-2">
                <Award className="h-4 w-4 text-purple-500" />
                Top Sales Closers
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {topSales.map((user, idx) => (
                  <div key={user.repcardUserId} className="flex items-center justify-between p-2 rounded-lg hover:bg-muted/50 transition-colors">
                    <div className="flex items-center gap-2 flex-1 min-w-0">
                      <div className={cn(
                        "flex items-center justify-center w-6 h-6 rounded-full text-xs font-bold text-white",
                        idx === 0 && "bg-yellow-500",
                        idx === 1 && "bg-gray-400",
                        idx === 2 && "bg-orange-600"
                      )}>
                        {idx + 1}
                      </div>
                      <div className="flex-1 min-w-0">
                        <p className="font-medium text-sm truncate">{user.name}</p>
                        {user.office && <p className="text-xs text-muted-foreground truncate">{user.office}</p>}
                      </div>
                    </div>
                    <div className="text-right">
                      <p className="font-bold text-sm text-green-700">{formatNumber(user.salesClosed)}</p>
                      <p className="text-xs text-muted-foreground">{formatPercentage(user.closeRate)}</p>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle className="text-base flex items-center gap-2">
                <Target className="h-4 w-4 text-green-500" />
                Best Converters
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {topConversion.length > 0 ? topConversion.map((user, idx) => (
                  <div key={user.repcardUserId} className="flex items-center justify-between p-2 rounded-lg hover:bg-muted/50 transition-colors">
                    <div className="flex items-center gap-2 flex-1 min-w-0">
                      <div className={cn(
                        "flex items-center justify-center w-6 h-6 rounded-full text-xs font-bold text-white",
                        idx === 0 && "bg-yellow-500",
                        idx === 1 && "bg-gray-400",
                        idx === 2 && "bg-orange-600"
                      )}>
                        {idx + 1}
                      </div>
                      <div className="flex-1 min-w-0">
                        <p className="font-medium text-sm truncate">{user.name}</p>
                        {user.office && <p className="text-xs text-muted-foreground truncate">{user.office}</p>}
                      </div>
                    </div>
                    <div className="text-right">
                      <p className={cn("font-bold text-sm", getPerformanceColor(user.conversionRate, 'conversion'))}>
                        {formatPercentage(user.conversionRate)}
                      </p>
                      <p className="text-xs text-muted-foreground">{formatNumber(user.doorsKnocked)} doors</p>
                    </div>
                  </div>
                )) : (
                  <p className="text-sm text-muted-foreground text-center py-4">Need at least 10 doors knocked</p>
                )}
              </div>
            </CardContent>
          </Card>
        </div>
      )}

      {/* Main Content Tabs */}
      <Tabs defaultValue="performance" className="space-y-4">
        <TabsList className="grid w-full grid-cols-2 md:grid-cols-4 lg:grid-cols-7">
          <TabsTrigger value="performance" className="min-h-[44px]">
            <BarChart3 className="h-4 w-4 mr-2" />
            Performance
          </TabsTrigger>
          <TabsTrigger value="offices" className="min-h-[44px]">
            <Building2 className="h-4 w-4 mr-2" />
            Offices
          </TabsTrigger>
          <TabsTrigger value="customers" className="min-h-[44px]">
            Customers ({customers.length})
          </TabsTrigger>
          <TabsTrigger value="appointments" className="min-h-[44px]">
            Appointments ({appointments.length})
          </TabsTrigger>
          <TabsTrigger value="notes" className="min-h-[44px]">
            Notes ({notes.length})
          </TabsTrigger>
          <TabsTrigger value="attachments" className="min-h-[44px]">
            Files ({attachments.length})
          </TabsTrigger>
          <TabsTrigger value="status-logs" className="min-h-[44px]">
            History ({statusLogs.length})
          </TabsTrigger>
        </TabsList>

        {/* User Performance - Card-Based Leaderboard */}
        <TabsContent value="performance" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Rep Performance Leaderboard</CardTitle>
              <CardDescription>
                Comprehensive performance metrics for all active reps
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {userPerformance.map((user, idx) => {
                  const isTopPerformer = idx < 3;
                  return (
                    <Card 
                      key={user.repcardUserId} 
                      className={cn(
                        "hover:shadow-md transition-all cursor-pointer border-l-4",
                        isTopPerformer && idx === 0 && "border-l-yellow-500 bg-yellow-50/50",
                        isTopPerformer && idx === 1 && "border-l-gray-400 bg-gray-50/50",
                        isTopPerformer && idx === 2 && "border-l-orange-500 bg-orange-50/50",
                        !isTopPerformer && "border-l-transparent"
                      )}
                    >
                      <CardContent className="pt-6">
                        <div className="flex flex-col md:flex-row md:items-center gap-4">
                          {/* Rank & Name */}
                          <div className="flex items-center gap-3 flex-1 min-w-0">
                            {isTopPerformer && (
                              <div className={cn(
                                "flex items-center justify-center w-8 h-8 rounded-full text-sm font-bold text-white shrink-0",
                                idx === 0 && "bg-yellow-500",
                                idx === 1 && "bg-gray-400",
                                idx === 2 && "bg-orange-600"
                              )}>
                                {idx + 1}
                              </div>
                            )}
                            <div className="flex-1 min-w-0">
                              <p className="font-semibold text-base truncate">{user.name}</p>
                              <div className="flex items-center gap-2 mt-1 flex-wrap">
                                {user.office && (
                                  <Badge variant="outline" className="text-xs">
                                    <Building2 className="h-3 w-3 mr-1" />
                                    {user.office}
                                  </Badge>
                                )}
                                {user.team && (
                                  <Badge variant="outline" className="text-xs">
                                    {user.team}
                                  </Badge>
                                )}
                              </div>
                            </div>
                          </div>

                          {/* Metrics Grid */}
                          <div className="grid grid-cols-2 md:grid-cols-5 gap-3 flex-1">
                            <div className="text-center md:text-left">
                              <p className="text-xs text-muted-foreground mb-1">Doors</p>
                              <p className="font-bold text-sm">{formatNumber(user.doorsKnocked)}</p>
                            </div>
                            <div className="text-center md:text-left">
                              <p className="text-xs text-muted-foreground mb-1">Appts Set</p>
                              <p className="font-bold text-sm">{formatNumber(user.appointmentsSet)}</p>
                            </div>
                            <div className="text-center md:text-left">
                              <p className="text-xs text-muted-foreground mb-1">Sales</p>
                              <p className="font-bold text-sm text-green-700">{formatNumber(user.salesClosed)}</p>
                            </div>
                            <div className="text-center md:text-left">
                              <p className="text-xs text-muted-foreground mb-1">Conv %</p>
                              <p className={cn("font-bold text-sm", getPerformanceColor(user.conversionRate, 'conversion'))}>
                                {formatPercentage(user.conversionRate)}
                              </p>
                            </div>
                            <div className="text-center md:text-left">
                              <p className="text-xs text-muted-foreground mb-1">Close %</p>
                              <p className={cn("font-bold text-sm", getPerformanceColor(user.closeRate, 'close'))}>
                                {formatPercentage(user.closeRate)}
                              </p>
                            </div>
                          </div>

                          {/* Additional Metrics */}
                          <div className="flex items-center gap-4 text-xs text-muted-foreground">
                            <div className="flex items-center gap-1">
                              <Paperclip className="h-3 w-3" />
                              {user.customersWithAttachments}
                            </div>
                            <div className="flex items-center gap-1">
                              <MessageSquare className="h-3 w-3" />
                              {user.notesWritten}
                            </div>
                            {user.appointmentsWithin48h > 0 && (
                              <div className="flex items-center gap-1 text-green-600">
                                <Zap className="h-3 w-3" />
                                {user.appointmentsWithin48h} 48h
                              </div>
                            )}
                          </div>
                        </div>
                      </CardContent>
                    </Card>
                  );
                })}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Offices - Enhanced Cards */}
        <TabsContent value="offices" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            {officeBreakdown.map((office) => {
              const conversionRate = office.doorsKnocked > 0 
                ? (office.appointmentsSet / office.doorsKnocked) * 100 
                : 0;
              const closeRate = office.appointmentsSet > 0
                ? (office.salesClosed / office.appointmentsSet) * 100
                : 0;

              return (
                <Card key={office.officeId} className="hover:shadow-lg transition-shadow border-l-4 border-l-blue-500">
                  <CardHeader>
                    <CardTitle className="text-lg flex items-center gap-2">
                      <Building2 className="h-5 w-5 text-blue-600" />
                      {office.officeName}
                    </CardTitle>
                    <CardDescription>
                      {office.activeReps} active {office.activeReps === 1 ? 'rep' : 'reps'}
                    </CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="space-y-4">
                      {/* Key Metrics */}
                      <div className="grid grid-cols-3 gap-3">
                        <div className="text-center p-3 bg-blue-50 rounded-lg">
                          <p className="text-2xl font-bold text-blue-900">{formatNumber(office.doorsKnocked)}</p>
                          <p className="text-xs text-muted-foreground mt-1">Doors</p>
                        </div>
                        <div className="text-center p-3 bg-green-50 rounded-lg">
                          <p className="text-2xl font-bold text-green-900">{formatNumber(office.appointmentsSet)}</p>
                          <p className="text-xs text-muted-foreground mt-1">Appts</p>
                        </div>
                        <div className="text-center p-3 bg-purple-50 rounded-lg">
                          <p className="text-2xl font-bold text-purple-900">{formatNumber(office.salesClosed)}</p>
                          <p className="text-xs text-muted-foreground mt-1">Sales</p>
                        </div>
                      </div>

                      {/* Conversion Rates */}
                      <div className="space-y-2">
                        <div className="flex items-center justify-between text-sm">
                          <span className="text-muted-foreground">Conversion Rate</span>
                          <span className={cn("font-semibold", getPerformanceColor(conversionRate, 'conversion'))}>
                            {formatPercentage(conversionRate)}
                          </span>
                        </div>
                        <Progress value={conversionRate} className="h-2" />
                      </div>

                      <div className="space-y-2">
                        <div className="flex items-center justify-between text-sm">
                          <span className="text-muted-foreground">Close Rate</span>
                          <span className={cn("font-semibold", getPerformanceColor(closeRate, 'close'))}>
                            {formatPercentage(closeRate)}
                          </span>
                        </div>
                        <Progress value={closeRate} className="h-2" />
                      </div>
                    </div>
                  </CardContent>
                </Card>
              );
            })}
          </div>
        </TabsContent>

        {/* Customers - Enhanced Cards */}
        <TabsContent value="customers">
          <Card>
            <CardHeader>
              <CardTitle>Customers / Leads</CardTitle>
              <CardDescription>
                Recent customer activity and engagement
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {customers.length === 0 ? (
                  <p className="text-center text-muted-foreground py-8">No customers found</p>
                ) : (
                  customers.map((customer) => (
                    <Card key={customer.repcardCustomerId} className="hover:shadow-md transition-shadow">
                      <CardContent className="pt-6">
                        <div className="flex flex-col md:flex-row gap-4">
                          <div className="flex-1 min-w-0">
                            <div className="flex items-start justify-between mb-3">
                              <div>
                                <h3 className="font-semibold text-lg mb-1">{customer.name}</h3>
                                {customer.status && (
                                  <Badge className={getStatusColor(customer.status)}>
                                    {customer.status}
                                  </Badge>
                                )}
                              </div>
                              <div className="flex gap-2 shrink-0">
                                {customer.counts.appointments > 0 && (
                                  <Badge variant="secondary" className="text-xs">
                                    {customer.counts.appointments} appts
                                  </Badge>
                                )}
                                {customer.counts.attachments > 0 && (
                                  <Badge variant="secondary" className="text-xs">
                                    {customer.counts.attachments} files
                                  </Badge>
                                )}
                                {customer.counts.notes > 0 && (
                                  <Badge variant="secondary" className="text-xs">
                                    {customer.counts.notes} notes
                                  </Badge>
                                )}
                              </div>
                            </div>
                            
                            <div className="space-y-2 text-sm text-muted-foreground">
                              {customer.email && (
                                <p className="flex items-center gap-2">
                                  <span>{customer.email}</span>
                                </p>
                              )}
                              {customer.phone && (
                                <p className="flex items-center gap-2">
                                  <span>{customer.phone}</span>
                                </p>
                              )}
                              {customer.address && (
                                <p className="flex items-center gap-2">
                                  <span>{customer.address}, {customer.city} {customer.state} {customer.zip}</span>
                                </p>
                              )}
                              <p className="text-xs">
                                Created: {formatRepCardDate(customer.createdAt, { includeTime: false })}
                              </p>
                            </div>

                            <div className="flex flex-wrap gap-2 mt-3">
                              {customer.setter?.name && (
                                <Badge variant="outline" className="text-xs">
                                  Setter: {customer.setter.name}
                                </Badge>
                              )}
                              {customer.office?.officeName && (
                                <Badge variant="outline" className="text-xs">
                                  <Building2 className="h-3 w-3 mr-1 inline" />
                                  {customer.office.officeName}
                                </Badge>
                              )}
                            </div>

                            {Object.values(customer.customFields || {}).some(v => v) && (
                              <div className="mt-4 p-3 bg-muted rounded-lg">
                                <p className="text-xs font-semibold mb-2 text-muted-foreground">Custom Fields</p>
                                <div className="grid grid-cols-2 gap-2 text-xs">
                                  {customer.customFields?.systemSizeKW && (
                                    <p><span className="font-medium">System:</span> {customer.customFields.systemSizeKW} kW</p>
                                  )}
                                  {customer.customFields?.systemCost && (
                                    <p><span className="font-medium">Cost:</span> ${Number(customer.customFields.systemCost).toLocaleString()}</p>
                                  )}
                                  {customer.customFields?.financier && (
                                    <p><span className="font-medium">Financier:</span> {customer.customFields.financier}</p>
                                  )}
                                  {customer.customFields?.offset && (
                                    <p><span className="font-medium">Offset:</span> {customer.customFields.offset}%</p>
                                  )}
                                </div>
                              </div>
                            )}
                          </div>
                        </div>
                      </CardContent>
                    </Card>
                  ))
                )}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Appointments - Enhanced Cards */}
        <TabsContent value="appointments">
          <Card>
            <CardHeader>
              <CardTitle>Appointments</CardTitle>
              <CardDescription>
                Scheduled appointments and their outcomes
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {appointments.length === 0 ? (
                  <p className="text-center text-muted-foreground py-8">No appointments found</p>
                ) : (
                  appointments.map((appt) => (
                    <Card key={appt.repcardAppointmentId} className="hover:shadow-md transition-shadow">
                      <CardContent className="pt-6">
                        <div className="flex flex-col md:flex-row gap-4">
                          <div className="flex-1 min-w-0">
                            <div className="flex items-start justify-between mb-3">
                              <div>
                                <h3 className="font-semibold text-lg mb-2">{appt.customer?.name || 'Unknown Customer'}</h3>
                                {appt.disposition && (
                                  <Badge className={getDispositionColor(appt.disposition)}>
                                    {appt.disposition}
                                  </Badge>
                                )}
                              </div>
                              {appt.attachmentCount > 0 && (
                                <Badge variant="secondary" className="text-xs shrink-0">
                                  {appt.attachmentCount} files
                                </Badge>
                              )}
                            </div>

                            <div className="space-y-2 text-sm">
                              {appt.scheduledAt && (
                                <div className="flex items-center gap-2 text-muted-foreground">
                                  <Clock className="h-4 w-4" />
                                  <span>Scheduled: {formatRepCardDate(appt.scheduledAt)}</span>
                                </div>
                              )}
                              {appt.completedAt && (
                                <div className="flex items-center gap-2 text-muted-foreground">
                                  <CheckCircle2 className="h-4 w-4 text-green-600" />
                                  <span>Completed: {formatRepCardDate(appt.completedAt)}</span>
                                </div>
                              )}
                              {appt.duration && (
                                <p className="text-muted-foreground">Duration: {appt.duration} minutes</p>
                              )}
                            </div>

                            <div className="flex flex-wrap gap-2 mt-3">
                              {appt.setter?.name && (
                                <Badge variant="outline" className="text-xs">
                                  Setter: {appt.setter.name}
                                </Badge>
                              )}
                              {appt.closer?.name && (
                                <Badge variant="outline" className="text-xs">
                                  Closer: {appt.closer.name}
                                </Badge>
                              )}
                              {appt.office?.officeName && (
                                <Badge variant="outline" className="text-xs">
                                  <Building2 className="h-3 w-3 mr-1 inline" />
                                  {appt.office.officeName}
                                </Badge>
                              )}
                              {appt.isWithin48Hours && (
                                <Badge className="bg-green-100 text-green-800 border-green-300 text-xs">
                                  <Zap className="h-3 w-3 mr-1 inline" />
                                  Within 48h
                                </Badge>
                              )}
                              {appt.hasPowerBill && (
                                <Badge className="bg-blue-100 text-blue-800 border-blue-300 text-xs">
                                  Power Bill
                                </Badge>
                              )}
                            </div>

                            {appt.notes && (
                              <div className="mt-4 p-3 bg-muted rounded-lg">
                                <p className="text-xs font-semibold mb-1 text-muted-foreground">Notes</p>
                                <p className="text-sm whitespace-pre-wrap">{appt.notes}</p>
                              </div>
                            )}
                          </div>
                        </div>
                      </CardContent>
                    </Card>
                  ))
                )}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Notes */}
        <TabsContent value="notes">
          <Card>
            <CardHeader>
              <CardTitle>Customer Notes</CardTitle>
              <CardDescription>
                Interaction notes and communication history
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {notes.length === 0 ? (
                  <p className="text-center text-muted-foreground py-8">No notes found</p>
                ) : (
                  notes.map((note) => (
                    <Card key={note.repcardNoteId} className="hover:shadow-md transition-shadow">
                      <CardContent className="pt-6">
                        <div className="flex items-start justify-between mb-3">
                          <div>
                            <p className="font-semibold text-base mb-1">{note.customerName}</p>
                            <div className="flex items-center gap-2 text-xs text-muted-foreground">
                              <span>By {note.author?.name || 'Unknown'}</span>
                              <span>•</span>
                              <span>{formatRepCardDate(note.createdAt)}</span>
                            </div>
                          </div>
                        </div>
                        <p className="text-sm mt-3 whitespace-pre-wrap bg-muted p-3 rounded-lg">{note.note}</p>
                      </CardContent>
                    </Card>
                  ))
                )}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Attachments */}
        <TabsContent value="attachments">
          <Card>
            <CardHeader>
              <CardTitle>Attachments & Files</CardTitle>
              <CardDescription>
                Power bills, documents, and other uploaded files
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                {attachments.length === 0 ? (
                  <p className="col-span-full text-center text-muted-foreground py-8">No attachments found</p>
                ) : (
                  attachments.map((att) => (
                    <Card key={att.attachmentId} className="hover:shadow-md transition-shadow cursor-pointer">
                      <CardContent className="pt-6">
                        <div className="flex items-start gap-3">
                          <div className="p-2 bg-blue-100 rounded-lg shrink-0">
                            <Paperclip className="h-5 w-5 text-blue-600" />
                          </div>
                          <div className="flex-1 min-w-0">
                            <p className="font-medium text-sm truncate mb-1">{att.fileName || 'Untitled'}</p>
                            <p className="text-xs text-muted-foreground truncate mb-2">
                              {att.customerName}
                            </p>
                            <div className="flex items-center justify-between">
                              <p className="text-xs text-muted-foreground">
                                {att.uploadedBy?.name} • {formatRepCardDate(att.createdAt, { includeTime: false })}
                              </p>
                              {att.fileUrl && (
                                <a 
                                  href={att.fileUrl} 
                                  target="_blank" 
                                  rel="noopener noreferrer"
                                  className="text-xs text-blue-600 hover:underline flex items-center gap-1"
                                  onClick={(e) => e.stopPropagation()}
                                >
                                  View
                                  <ChevronRight className="h-3 w-3" />
                                </a>
                              )}
                            </div>
                            {att.attachmentType && (
                              <Badge variant="outline" className="text-xs mt-2">
                                {att.attachmentType}
                              </Badge>
                            )}
                          </div>
                        </div>
                      </CardContent>
                    </Card>
                  ))
                )}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Status Logs */}
        <TabsContent value="status-logs">
          <Card>
            <CardHeader>
              <CardTitle>Status Change History</CardTitle>
              <CardDescription>
                Timeline of customer status updates
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {statusLogs.length === 0 ? (
                  <p className="text-center text-muted-foreground py-8">No status changes found</p>
                ) : (
                  statusLogs.map((log) => (
                    <div key={log.repcardLogId} className="flex items-start gap-4 p-4 border rounded-lg hover:bg-muted/50 transition-colors">
                      <div className="flex-1 min-w-0">
                        <p className="font-medium mb-2">{log.customerName}</p>
                        <div className="flex items-center gap-2 flex-wrap">
                          <Badge variant="outline" className={getStatusColor(log.oldStatus || '')}>
                            {log.oldStatus || 'N/A'}
                          </Badge>
                          <ChevronRight className="h-4 w-4 text-muted-foreground" />
                          <Badge className={getStatusColor(log.newStatus)}>
                            {log.newStatus}
                          </Badge>
                        </div>
                        {log.notes && (
                          <p className="text-xs text-muted-foreground mt-2">{log.notes}</p>
                        )}
                      </div>
                      <div className="text-right text-xs text-muted-foreground shrink-0">
                        <p className="font-medium">{log.changedBy?.name || 'Unknown'}</p>
                        <p>{formatRepCardDate(log.changedAt)}</p>
                      </div>
                    </div>
                  ))
                )}
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}
