'use client';

import { useQuery } from '@tanstack/react-query';
import { useState, useMemo } from 'react';
import { useSession } from 'next-auth/react';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Badge } from '@/components/ui/badge';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { 
  Trophy, 
  Users, 
  Building2, 
  Target, 
  TrendingUp, 
  Calendar, 
  DoorOpen, 
  FileText,
  Zap,
  RefreshCw,
  Clock,
  Award,
  Activity,
  AlertCircle,
  CheckCircle2,
  BarChart3,
  ChevronRight,
  Paperclip,
  ExternalLink
} from 'lucide-react';
import { cn } from '@/lib/utils';
import { formatPercentage, formatLargeNumber } from '@/lib/utils/formatters';
import { Progress } from '@/components/ui/progress';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';

interface RepCardOptimizedDashboardProps {
  startDate?: string;
  endDate?: string;
  className?: string;
}

export function RepCardOptimizedDashboard({ 
  startDate, 
  endDate, 
  className 
}: RepCardOptimizedDashboardProps) {
  const { data: session } = useSession();
  const [activeView, setActiveView] = useState<'overview' | 'setters' | 'closers' | 'offices'>('overview');

  // Fetch unified dashboard data
  const { data, isLoading, error, refetch, isRefetching } = useQuery({
    queryKey: ['repcard-optimized-dashboard', startDate, endDate],
    queryFn: async () => {
      const params = new URLSearchParams();
      if (startDate) params.set('startDate', startDate);
      if (endDate) params.set('endDate', endDate);

      const response = await fetch(`/api/repcard/unified-dashboard?${params.toString()}`);
      if (!response.ok) throw new Error('Failed to fetch dashboard data');
      return response.json();
    },
    refetchInterval: 30000, // Refresh every 30 seconds
    staleTime: 60000, // Consider stale after 1 minute
  });

  // Calculate date range display
  const dateRangeDisplay = useMemo(() => {
    if (startDate && endDate) {
      return `${new Date(startDate).toLocaleDateString()} - ${new Date(endDate).toLocaleDateString()}`;
    }
    return 'Last 30 days';
  }, [startDate, endDate]);

  // Sync status
  const syncStatus = useMemo(() => {
    if (!data?.metadata?.lastSyncTime) return null;
    const lastSync = new Date(data.metadata.lastSyncTime);
    const now = new Date();
    const diffMs = now.getTime() - lastSync.getTime();
    const diffMinutes = Math.floor(diffMs / 60000);
    
    if (diffMinutes < 1) {
      return { text: 'Just now', color: 'text-green-600', bg: 'bg-green-50' };
    } else if (diffMinutes < 60) {
      return { 
        text: `${diffMinutes} min ago`, 
        color: diffMinutes <= 5 ? 'text-green-600' : diffMinutes <= 10 ? 'text-yellow-600' : 'text-orange-600',
        bg: diffMinutes <= 5 ? 'bg-green-50' : diffMinutes <= 10 ? 'bg-yellow-50' : 'bg-orange-50'
      };
    } else {
      const hours = Math.floor(diffMinutes / 60);
      return { text: `${hours} hr ago`, color: 'text-orange-600', bg: 'bg-orange-50' };
    }
  }, [data?.metadata?.lastSyncTime]);

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
              <Button
                variant="outline"
                size="sm"
                onClick={() => refetch()}
                className="mt-4"
              >
                <RefreshCw className="h-4 w-4 mr-2" />
                Retry
              </Button>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  const quality = data?.quality || {};
  const leaderboards = data?.leaderboards || {};
  const offices = data?.officePerformance || [];
  const canvassing = data?.canvassing || {};

  return (
    <div className={cn('space-y-6', className)}>
      {/* Header with Date Range and Sync Status */}
      <div className="flex items-center justify-between flex-wrap gap-4">
        <div>
          <h2 className="text-2xl font-bold tracking-tight">RepCard Analytics</h2>
          <div className="flex items-center gap-3 mt-1 flex-wrap">
            <Badge variant="outline" className="text-sm">
              <Calendar className="h-3 w-3 mr-1" />
              {dateRangeDisplay}
            </Badge>
            {syncStatus && (
              <Badge variant="outline" className={cn('text-xs', syncStatus.color, syncStatus.bg)}>
                <Clock className="h-3 w-3 mr-1" />
                Synced {syncStatus.text}
              </Badge>
            )}
            {data?.metadata?.cached && (
              <Badge variant="outline" className="text-xs">
                Cached
              </Badge>
            )}
            {quality.totalAppointments > 0 && (
              <Badge variant="outline" className="text-xs">
                {formatLargeNumber(quality.totalAppointments)} appointments
              </Badge>
            )}
          </div>
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

      {/* View Tabs */}
      <Tabs value={activeView} onValueChange={(v) => setActiveView(v as any)}>
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="overview">
            <BarChart3 className="w-4 h-4 mr-2" />
            Overview
          </TabsTrigger>
          <TabsTrigger value="setters">
            <Users className="w-4 h-4 mr-2" />
            Setters
          </TabsTrigger>
          <TabsTrigger value="closers">
            <Award className="w-4 h-4 mr-2" />
            Closers
          </TabsTrigger>
          <TabsTrigger value="offices">
            <Building2 className="w-4 h-4 mr-2" />
            Offices
          </TabsTrigger>
        </TabsList>

        {/* Overview Tab */}
        <TabsContent value="overview" className="space-y-6">
          {/* Quality Metrics - Top Priority */}
          <Card className="border-l-4 border-l-purple-500">
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Target className="h-5 w-5 text-purple-600" />
                Quality Metrics
              </CardTitle>
              <CardDescription>
                Appointment quality based on 48-hour speed and power bill collection
              </CardDescription>
            </CardHeader>
            <CardContent>
              {isLoading ? (
                <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                  {[1, 2, 3, 4].map(i => <Skeleton key={i} className="h-24" />)}
                </div>
              ) : (
                <>
                  {quality.hasNullValues && (
                    <Alert className="mb-4 border-yellow-200 bg-yellow-50">
                      <AlertCircle className="h-4 w-4 text-yellow-600" />
                      <AlertDescription className="text-yellow-800">
                        <strong>Data needs backfill:</strong> {quality.null48hCount || 0} appointments have NULL for 48h speed, {quality.nullPBCount || 0} have NULL for power bill.
                        <br />
                        <Button
                          variant="outline"
                          size="sm"
                          className="mt-2"
                          onClick={() => {
                            fetch('/api/admin/repcard/backfill-metrics', { method: 'POST' })
                              .then(res => res.json())
                              .then(data => {
                                alert('Backfill started! Refresh the page in a moment.');
                                setTimeout(() => window.location.reload(), 2000);
                              })
                              .catch(err => alert('Backfill failed: ' + err.message));
                          }}
                        >
                          Run Backfill Now
                        </Button>
                      </AlertDescription>
                    </Alert>
                  )}
                <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                  <QualityMetricCard
                    label="48-Hour Speed"
                    value={quality.appointmentSpeed || 0}
                    total={quality.totalAppointments || 0}
                    count={quality.within48h || 0}
                    icon={<Zap className="h-5 w-5" />}
                    color="purple"
                    threshold={70}
                  />
                  <QualityMetricCard
                    label="Power Bill Rate"
                    value={quality.powerBillRate || 0}
                    total={quality.totalAppointments || 0}
                    count={quality.withPowerBill || 0}
                    icon={<FileText className="h-5 w-5" />}
                    color="blue"
                    threshold={80}
                  />
                  <QualityMetricCard
                    label="High Quality"
                    value={quality.withBoth?.percentage || 0}
                    total={quality.totalAppointments || 0}
                    count={quality.withBoth?.count || 0}
                    icon={<CheckCircle2 className="h-5 w-5" />}
                    color="green"
                    description="Both PB & 48h"
                  />
                  <QualityMetricCard
                    label="Reschedule Rate"
                    value={quality.rescheduleRate || 0}
                    total={quality.totalAppointments || 0}
                    count={quality.reschedules || 0}
                    icon={<RefreshCw className="h-5 w-5" />}
                    color="orange"
                    threshold={10}
                    invertThreshold
                  />
                  </div>
                </>
              )}
            </CardContent>
          </Card>

          {/* Leaderboards Grid */}
          <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
            <LeaderboardCard
              title="Top Doors Knocked"
              data={leaderboards.topDoors || []}
              metric="doorsKnocked"
              icon={<DoorOpen className="h-5 w-5" />}
              isLoading={isLoading}
              limit={10}
            />
            <LeaderboardCard
              title="Top Appointment Setters"
              data={leaderboards.topAppointmentSetters || []}
              metric="appointmentsSet"
              icon={<Calendar className="h-5 w-5" />}
              isLoading={isLoading}
              showQuality
              limit={10}
            />
            <LeaderboardCard
              title="Top Closers"
              data={leaderboards.topClosers || []}
              metric="salesClosed"
              icon={<Trophy className="h-5 w-5" />}
              isLoading={isLoading}
              limit={10}
            />
          </div>

          {/* Office Performance */}
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center gap-2">
                <Building2 className="h-5 w-5" />
                Office Performance
              </CardTitle>
              <CardDescription>{dateRangeDisplay}</CardDescription>
            </CardHeader>
            <CardContent>
              {isLoading ? (
                <Skeleton className="h-64" />
              ) : offices.length > 0 ? (
                <div className="space-y-4">
                  <OfficePerformanceTable offices={offices} />
                </div>
              ) : (
                <p className="text-sm text-muted-foreground text-center py-8">No office data available</p>
              )}
            </CardContent>
          </Card>

          {/* Canvassing Activity */}
          {canvassing.last30Days && (
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center gap-2">
                  <Activity className="h-5 w-5" />
                  Canvassing Activity
                </CardTitle>
                <CardDescription>{dateRangeDisplay}</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                  <StatCard
                    label="Total Doors"
                    value={canvassing.last30Days.totalDoors || 0}
                    icon={<DoorOpen className="h-5 w-5" />}
                    color="blue"
                  />
                  <StatCard
                    label="Appointments Set"
                    value={canvassing.last30Days.totalAppointments || 0}
                    icon={<Calendar className="h-5 w-5" />}
                    color="green"
                  />
                  <StatCard
                    label="Avg Per Day"
                    value={Math.round(canvassing.last30Days.avgDoorsPerDay || 0)}
                    icon={<TrendingUp className="h-5 w-5" />}
                    color="orange"
                  />
                  <StatCard
                    label="Conversion Rate"
                    value={canvassing.last30Days.avgConversionRate || 0}
                    format="percentage"
                    icon={<Target className="h-5 w-5" />}
                    color="purple"
                  />
                </div>
                {/* Daily Trends Chart */}
                {canvassing.dailyTrends && canvassing.dailyTrends.length > 0 && (
                  <div className="mt-6 pt-6 border-t">
                    <h4 className="text-sm font-semibold mb-4">Daily Trends</h4>
                    <div className="space-y-2">
                      {canvassing.dailyTrends.slice(0, 7).reverse().map((day: any) => {
                        const maxDoors = Math.max(...canvassing.dailyTrends.map((d: any) => d.doorsKnocked || 0));
                        const barWidth = maxDoors > 0 ? ((day.doorsKnocked || 0) / maxDoors) * 100 : 0;
                        return (
                          <div key={day.date} className="space-y-1">
                            <div className="flex items-center justify-between text-xs">
                              <span>{new Date(day.date).toLocaleDateString('en-US', { month: 'short', day: 'numeric' })}</span>
                              <div className="flex items-center gap-2">
                                <span>{day.doorsKnocked || 0} doors</span>
                                <Badge variant="outline" className="text-xs">
                                  {formatPercentage(day.conversionRate || 0)}
                                </Badge>
                              </div>
                            </div>
                            <Progress value={barWidth} className="h-2" />
                          </div>
                        );
                      })}
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>
          )}
        </TabsContent>

        {/* Setters Tab */}
        <TabsContent value="setters" className="space-y-6">
          <SettersView 
            data={leaderboards.topAppointmentSetters || []}
            isLoading={isLoading}
            dateRange={dateRangeDisplay}
          />
        </TabsContent>

        {/* Closers Tab */}
        <TabsContent value="closers" className="space-y-6">
          <ClosersView 
            data={leaderboards.topClosers || []}
            isLoading={isLoading}
            dateRange={dateRangeDisplay}
          />
        </TabsContent>

        {/* Offices Tab */}
        <TabsContent value="offices" className="space-y-6">
          <OfficesView 
            offices={offices}
            isLoading={isLoading}
            dateRange={dateRangeDisplay}
          />
        </TabsContent>
      </Tabs>
    </div>
  );
}

// Helper Components
function QualityMetricCard({ 
  label, 
  value, 
  total, 
  count, 
  icon, 
  color, 
  threshold = 0,
  invertThreshold = false,
  description
}: {
  label: string;
  value: number;
  total: number;
  count: number;
  icon: React.ReactNode;
  color: 'purple' | 'blue' | 'green' | 'orange' | 'red';
  threshold?: number;
  invertThreshold?: boolean;
  description?: string;
}) {
  const getColorClasses = (color: string) => {
    const colors = {
      purple: { text: 'text-purple-600', bg: 'bg-purple-50', border: 'border-purple-200', progress: 'bg-purple-500' },
      blue: { text: 'text-blue-600', bg: 'bg-blue-50', border: 'border-blue-200', progress: 'bg-blue-500' },
      green: { text: 'text-green-600', bg: 'bg-green-50', border: 'border-green-200', progress: 'bg-green-500' },
      orange: { text: 'text-orange-600', bg: 'bg-orange-50', border: 'border-orange-200', progress: 'bg-orange-500' },
      red: { text: 'text-red-600', bg: 'bg-red-50', border: 'border-red-200', progress: 'bg-red-500' },
    };
    return colors[color as keyof typeof colors] || colors.blue;
  };

  const isGood = invertThreshold ? value < threshold : value >= threshold;
  const colors = getColorClasses(color);

  return (
    <Card className={cn('border-l-4', colors.border)}>
      <CardContent className="pt-6">
        <div className="flex items-center gap-2 mb-2">
          <div className={colors.text}>{icon}</div>
          <span className="text-sm font-medium text-gray-700">{label}</span>
        </div>
        <div className="space-y-2">
          <div className="flex items-baseline gap-2">
            <p className={cn('text-3xl font-bold', colors.text)}>
              {formatPercentage(value)}
            </p>
            {isGood ? (
              <CheckCircle2 className="h-4 w-4 text-green-600" />
            ) : (
              <AlertCircle className="h-4 w-4 text-yellow-600" />
            )}
          </div>
          <p className="text-xs text-muted-foreground">
            {count.toLocaleString()} of {total.toLocaleString()} appointments
            {description && ` • ${description}`}
          </p>
          <Progress 
            value={Math.min(value, 100)} 
            className={cn('h-2', colors.progress)} 
          />
        </div>
      </CardContent>
    </Card>
  );
}

function LeaderboardCard({
  title,
  data,
  metric,
  icon,
  isLoading,
  showQuality = false,
  limit = 5
}: {
  title: string;
  data: any[];
  metric: string;
  icon: React.ReactNode;
  isLoading: boolean;
  showQuality?: boolean;
  limit?: number;
}) {
  if (isLoading) {
    return <Card><CardContent className="pt-6"><Skeleton className="h-64" /></CardContent></Card>;
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2 text-lg">
          {icon}
          {title}
        </CardTitle>
      </CardHeader>
      <CardContent>
        {data.length > 0 ? (
          <div className="space-y-2">
            {data.slice(0, limit).map((rep, idx) => (
              <div
                key={rep.userId || idx}
                className="flex items-center justify-between p-3 rounded-lg bg-gray-50 hover:bg-gray-100 transition-colors"
              >
                <div className="flex items-center gap-3 flex-1 min-w-0">
                  <Badge 
                    variant={idx < 3 ? 'default' : 'outline'} 
                    className={cn(
                      'shrink-0',
                      idx === 0 && 'bg-yellow-500',
                      idx === 1 && 'bg-gray-400',
                      idx === 2 && 'bg-orange-600'
                    )}
                  >
                    #{idx + 1}
                  </Badge>
                  <div className="flex-1 min-w-0">
                    <p className="font-medium truncate">{rep.name || 'Unknown'}</p>
                    {rep.role && (
                      <p className="text-xs text-muted-foreground capitalize">{rep.role}</p>
                    )}
                  </div>
                </div>
                <div className="flex items-center gap-3 shrink-0">
                  <div className="text-right">
                    <p className="text-lg font-bold">{formatLargeNumber(rep[metric] || 0)}</p>
                    {showQuality && rep.within48hCount !== undefined && (
                      <div className="flex gap-1 mt-1">
                        <Badge variant="outline" className="text-[10px] px-1">
                          {rep.within48hCount || 0} 48h
                        </Badge>
                        <Badge variant="outline" className="text-[10px] px-1">
                          {rep.withPowerBillCount || 0} PB
                        </Badge>
                      </div>
                    )}
                  </div>
                </div>
              </div>
            ))}
            {data.length > limit && (
              <p className="text-xs text-muted-foreground text-center pt-2">
                +{data.length - limit} more (view full table in tabs above)
              </p>
            )}
          </div>
        ) : (
          <p className="text-sm text-muted-foreground text-center py-8">No data available</p>
        )}
      </CardContent>
    </Card>
  );
}

function OfficePerformanceTable({ offices }: { offices: any[] }) {
  return (
    <div className="overflow-x-auto">
      <Table>
        <TableHeader>
          <TableRow>
            <TableHead>Office</TableHead>
            <TableHead className="text-right">Doors</TableHead>
            <TableHead className="text-right">Appointments</TableHead>
            <TableHead className="text-right">Sales</TableHead>
            <TableHead className="text-right">Conversion</TableHead>
            <TableHead className="text-right">Close Rate</TableHead>
            <TableHead className="text-right">Reps</TableHead>
          </TableRow>
        </TableHeader>
        <TableBody>
          {offices.map((office) => (
            <TableRow key={office.officeId}>
              <TableCell className="font-medium">{office.officeName}</TableCell>
              <TableCell className="text-right">{formatLargeNumber(office.doorsKnocked)}</TableCell>
              <TableCell className="text-right">{formatLargeNumber(office.appointmentsSet)}</TableCell>
              <TableCell className="text-right">{formatLargeNumber(office.salesClosed)}</TableCell>
              <TableCell className="text-right">
                <Badge variant={office.conversionRate >= 20 ? 'default' : 'secondary'}>
                  {formatPercentage(office.conversionRate)}
                </Badge>
              </TableCell>
              <TableCell className="text-right">
                <Badge variant={office.closeRate >= 30 ? 'default' : 'secondary'}>
                  {formatPercentage(office.closeRate)}
                </Badge>
              </TableCell>
              <TableCell className="text-right">{office.activeReps}</TableCell>
            </TableRow>
          ))}
        </TableBody>
      </Table>
    </div>
  );
}

function StatCard({
  label,
  value,
  icon,
  color,
  format = 'number'
}: {
  label: string;
  value: number;
  icon: React.ReactNode;
  color: string;
  format?: 'number' | 'percentage';
}) {
  const colors = {
    blue: 'text-blue-600 bg-blue-50 border-blue-200',
    green: 'text-green-600 bg-green-50 border-green-200',
    orange: 'text-orange-600 bg-orange-50 border-orange-200',
    purple: 'text-purple-600 bg-purple-50 border-purple-200',
  }[color] || 'text-gray-600 bg-gray-50 border-gray-200';

  return (
    <Card className={cn('border-l-4', colors)}>
      <CardContent className="pt-6">
        <div className="flex items-center gap-2 mb-2">
          {icon}
          <span className="text-sm font-medium text-gray-700">{label}</span>
        </div>
        <p className="text-3xl font-bold">
          {format === 'percentage' ? formatPercentage(value) : formatLargeNumber(value)}
        </p>
      </CardContent>
    </Card>
  );
}

function SettersView({ data, isLoading, dateRange }: { data: any[]; isLoading: boolean; dateRange: string }) {
  const [selectedSetter, setSelectedSetter] = useState<any>(null);
  const [attachments, setAttachments] = useState<any[]>([]);
  const [loadingAttachments, setLoadingAttachments] = useState(false);

  if (isLoading) {
    return <Skeleton className="h-96" />;
  }

  // Calculate summary stats
  const totalDoors = data.reduce((sum, s) => sum + (s.doorsKnocked || 0), 0);
  const totalAppointments = data.reduce((sum, s) => sum + (s.appointmentsSet || 0), 0);
  const total48h = data.reduce((sum, s) => sum + (s.within48hCount || 0), 0);
  const totalPB = data.reduce((sum, s) => sum + (s.withPowerBillCount || 0), 0);
  const totalHighQuality = data.reduce((sum, s) => sum + (s.bothCount || 0), 0);
  const avgConversion = totalDoors > 0 ? (totalAppointments / totalDoors) * 100 : 0;

  const loadAttachments = async (setter: any) => {
    setSelectedSetter(setter);
    setLoadingAttachments(true);
    try {
      // Fetch attachments for this setter's appointments
      const response = await fetch(`/api/repcard/data?type=attachments&repcardUserId=${setter.userId}&limit=100`);
      if (response.ok) {
        const result = await response.json();
        setAttachments(result.data || []);
      }
    } catch (error) {
      console.error('Failed to load attachments:', error);
      setAttachments([]);
    } finally {
      setLoadingAttachments(false);
    }
  };

  return (
    <div className="space-y-6">
      {/* Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-5 gap-4">
        <StatCard label="Total Doors" value={totalDoors} icon={<DoorOpen className="h-5 w-5" />} color="blue" />
        <StatCard label="Appointments Set" value={totalAppointments} icon={<Calendar className="h-5 w-5" />} color="green" />
        <StatCard label="48h Speed" value={total48h} icon={<Zap className="h-5 w-5" />} color="purple" />
        <StatCard label="Power Bills" value={totalPB} icon={<FileText className="h-5 w-5" />} color="blue" />
        <StatCard label="High Quality" value={totalHighQuality} icon={<CheckCircle2 className="h-5 w-5" />} color="green" />
      </div>

      {/* Performance Table */}
      <Card>
        <CardHeader>
          <CardTitle>Setter Performance Leaderboard</CardTitle>
          <CardDescription>Top setters by appointments set and quality metrics - {dateRange}</CardDescription>
        </CardHeader>
        <CardContent>
          {data.length > 0 ? (
            <div className="overflow-x-auto">
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead>Rank</TableHead>
                    <TableHead>Setter</TableHead>
                    <TableHead className="text-right">Doors</TableHead>
                    <TableHead className="text-right">Appointments</TableHead>
                    <TableHead className="text-right">48h Speed</TableHead>
                    <TableHead className="text-right">Power Bill</TableHead>
                    <TableHead className="text-right">High Quality</TableHead>
                    <TableHead className="text-right">Low Quality</TableHead>
                    <TableHead className="text-right">Conversion</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {data.map((setter, idx) => {
                    const appointment48hRate = (setter.appointmentsSet || 0) > 0 
                      ? ((setter.within48hCount || 0) / (setter.appointmentsSet || 1)) * 100 
                      : 0;
                    const appointmentPBRate = (setter.appointmentsSet || 0) > 0 
                      ? ((setter.withPowerBillCount || 0) / (setter.appointmentsSet || 1)) * 100 
                      : 0;
                    
                    return (
                      <TableRow key={setter.userId} className={idx < 3 ? 'bg-yellow-50/30' : ''}>
                        <TableCell>
                          <Badge 
                            variant={idx < 3 ? 'default' : 'outline'}
                            className={cn(
                              idx === 0 && 'bg-yellow-500',
                              idx === 1 && 'bg-gray-400',
                              idx === 2 && 'bg-orange-600'
                            )}
                          >
                            #{idx + 1}
                          </Badge>
                        </TableCell>
                        <TableCell className="font-medium">{setter.name}</TableCell>
                        <TableCell className="text-right">{formatLargeNumber(setter.doorsKnocked || 0)}</TableCell>
                        <TableCell className="text-right font-bold text-lg">{formatLargeNumber(setter.appointmentsSet || 0)}</TableCell>
                        <TableCell className="text-right">
                          <div className="flex flex-col items-end">
                            <Badge variant={appointment48hRate >= 70 ? 'default' : 'secondary'} className="mb-1">
                              {setter.within48hCount || 0}
                            </Badge>
                            <span className="text-xs text-muted-foreground">
                              {formatPercentage(appointment48hRate)}
                            </span>
                          </div>
                        </TableCell>
                        <TableCell className="text-right">
                          <div className="flex flex-col items-end">
                            <Badge 
                              variant={appointmentPBRate >= 80 ? 'default' : 'secondary'} 
                              className="mb-1 cursor-help"
                              title={`${setter.withPowerBillCount || 0} appointments with attachments (any attachment = power bill)`}
                            >
                              {setter.withPowerBillCount || 0}
                            </Badge>
                            <span className="text-xs text-muted-foreground">
                              {formatPercentage(appointmentPBRate)}
                            </span>
                          </div>
                        </TableCell>
                        <TableCell className="text-right">
                          <Badge variant="outline" className="bg-green-50 text-green-700 border-green-200">
                            {setter.bothCount || 0}
                          </Badge>
                        </TableCell>
                        <TableCell className="text-right">
                          <Badge variant="outline" className="bg-red-50 text-red-700 border-red-200">
                            {setter.neitherCount || 0}
                          </Badge>
                        </TableCell>
                        <TableCell className="text-right">
                          <Badge variant={setter.conversionRate >= 20 ? 'default' : 'secondary'}>
                            {formatPercentage(setter.conversionRate || 0)}
                          </Badge>
                        </TableCell>
                      </TableRow>
                    );
                  })}
                </TableBody>
              </Table>
            </div>
          ) : (
            <p className="text-sm text-muted-foreground text-center py-8">No setter data available</p>
          )}
        </CardContent>
      </Card>
    </div>
  );
}

function ClosersView({ data, isLoading, dateRange }: { data: any[]; isLoading: boolean; dateRange: string }) {
  if (isLoading) {
    return <Skeleton className="h-96" />;
  }

  // Calculate summary stats
  const totalAppointmentsRun = data.reduce((sum, c) => sum + (c.appointmentsRun || 0), 0);
  const totalSalesClosed = data.reduce((sum, c) => sum + (c.salesClosed || 0), 0);
  const avgCloseRate = totalAppointmentsRun > 0 ? (totalSalesClosed / totalAppointmentsRun) * 100 : 0;

  return (
    <div className="space-y-6">
      {/* Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <StatCard 
          label="Total Appointments Run" 
          value={totalAppointmentsRun} 
          icon={<Calendar className="h-5 w-5" />} 
          color="blue" 
        />
        <StatCard 
          label="Total Sales Closed" 
          value={totalSalesClosed} 
          icon={<Trophy className="h-5 w-5" />} 
          color="green" 
        />
        <StatCard 
          label="Average Close Rate" 
          value={avgCloseRate} 
          format="percentage"
          icon={<Target className="h-5 w-5" />} 
          color="purple" 
        />
      </div>

      {/* Performance Table */}
      <Card>
        <CardHeader>
          <CardTitle>Closer Performance Leaderboard</CardTitle>
          <CardDescription>Top closers by sales closed and close rate - {dateRange}</CardDescription>
        </CardHeader>
        <CardContent>
          {data.length > 0 ? (
            <div className="overflow-x-auto">
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead>Rank</TableHead>
                    <TableHead>Closer</TableHead>
                    <TableHead className="text-right">Appointments Run</TableHead>
                    <TableHead className="text-right">Sales Closed</TableHead>
                    <TableHead className="text-right">Close Rate</TableHead>
                    <TableHead className="text-right">Performance</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {data.map((closer, idx) => (
                    <TableRow key={closer.userId} className={idx < 3 ? 'bg-yellow-50/30' : ''}>
                      <TableCell>
                        <Badge 
                          variant={idx < 3 ? 'default' : 'outline'}
                          className={cn(
                            idx === 0 && 'bg-yellow-500',
                            idx === 1 && 'bg-gray-400',
                            idx === 2 && 'bg-orange-600'
                          )}
                        >
                          #{idx + 1}
                        </Badge>
                      </TableCell>
                      <TableCell className="font-medium">{closer.name}</TableCell>
                      <TableCell className="text-right">{formatLargeNumber(closer.appointmentsRun || 0)}</TableCell>
                      <TableCell className="text-right font-bold text-lg text-green-600">
                        {formatLargeNumber(closer.salesClosed || 0)}
                      </TableCell>
                      <TableCell className="text-right">
                        <Badge variant={closer.closeRate >= 30 ? 'default' : closer.closeRate >= 20 ? 'secondary' : 'outline'}>
                          {formatPercentage(closer.closeRate || 0)}
                        </Badge>
                      </TableCell>
                      <TableCell className="text-right">
                        <div className="flex items-center justify-end gap-2">
                          <Progress 
                            value={Math.min(closer.closeRate || 0, 100)} 
                            className="w-20 h-2"
                          />
                          <span className={cn(
                            'text-xs',
                            closer.closeRate >= 30 ? 'text-green-600' : 
                            closer.closeRate >= 20 ? 'text-yellow-600' : 'text-red-600'
                          )}>
                            {closer.closeRate >= 30 ? 'Excellent' : closer.closeRate >= 20 ? 'Good' : 'Needs Work'}
                          </span>
                        </div>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </div>
          ) : (
            <p className="text-sm text-muted-foreground text-center py-8">No closer data available</p>
          )}
        </CardContent>
      </Card>
    </div>
  );
}

function OfficesView({ offices, isLoading, dateRange }: { offices: any[]; isLoading: boolean; dateRange: string }) {
  if (isLoading) {
    return <Skeleton className="h-96" />;
  }

  const totalDoors = offices.reduce((sum, o) => sum + (o.doorsKnocked || 0), 0);
  const totalAppointments = offices.reduce((sum, o) => sum + (o.appointmentsSet || 0), 0);
  const totalSales = offices.reduce((sum, o) => sum + (o.salesClosed || 0), 0);
  const totalReps = offices.reduce((sum, o) => sum + (o.activeReps || 0), 0);

  return (
    <div className="space-y-6">
      {/* Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <StatCard label="Total Doors" value={totalDoors} icon={<DoorOpen className="h-5 w-5" />} color="blue" />
        <StatCard label="Total Appointments" value={totalAppointments} icon={<Calendar className="h-5 w-5" />} color="green" />
        <StatCard label="Total Sales" value={totalSales} icon={<Trophy className="h-5 w-5" />} color="purple" />
        <StatCard label="Active Reps" value={totalReps} icon={<Users className="h-5 w-5" />} color="orange" />
      </div>

      {/* Office Table */}
      <Card>
        <CardHeader>
          <CardTitle>Office Performance Breakdown</CardTitle>
          <CardDescription>{dateRange}</CardDescription>
        </CardHeader>
        <CardContent>
          {offices.length > 0 ? (
            <OfficePerformanceTable offices={offices} />
          ) : (
            <p className="text-sm text-muted-foreground text-center py-8">No office data available</p>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
