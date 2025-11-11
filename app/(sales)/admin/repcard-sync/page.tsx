'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import {
  RefreshCw,
  PlayCircle,
  CheckCircle,
  XCircle,
  AlertTriangle,
  Clock,
  ChevronDown,
  ChevronRight,
  Loader2,
  Database,
  Users,
  Calendar,
  FileText
} from 'lucide-react';
import { formatDistanceToNow } from 'date-fns';
import { toast } from 'sonner';

interface SyncLogRecord {
  id: string;
  entity_type: 'customers' | 'appointments' | 'status_logs';
  sync_type: 'full' | 'incremental';
  started_at: string;
  completed_at: string | null;
  status: 'running' | 'completed' | 'failed';
  records_fetched: number;
  records_inserted: number;
  records_updated: number;
  records_failed: number;
  error_message: string | null;
}

interface SyncStatusResponse {
  latestSyncs: SyncLogRecord[];
  syncHistory: SyncLogRecord[];
  recordCounts: {
    customers: number;
    appointments: number;
    statusLogs: number;
    users: number;
  };
}

export default function RepCardSyncPage() {
  const [expandedRows, setExpandedRows] = useState<Set<string>>(new Set());
  const [startDate, setStartDate] = useState('2025-10-01');
  const [endDate, setEndDate] = useState('2025-10-28');
  const queryClient = useQueryClient();

  // Fetch sync status
  const { data, isLoading, error, refetch } = useQuery<SyncStatusResponse>({
    queryKey: ['repcard-sync-status'],
    queryFn: async () => {
      const res = await fetch('/api/admin/repcard/sync?limit=20');
      if (!res.ok) throw new Error('Failed to fetch sync status');
      return res.json();
    },
    refetchInterval: 10000, // Auto-refresh every 10 seconds
  });

  // Sequential full sync mutation (avoids timeouts)
  const fullSyncMutation = useMutation({
    mutationFn: async ({ startDate, endDate }: { startDate: string; endDate: string }) => {
      const results = [];

      // Step 1: Sync offices first (needed for users to get company_id)
      toast.info('Syncing offices...', { description: 'Step 1 of 4' });
      const officesRes = await fetch(
        `/api/admin/repcard/comprehensive-sync?skipUsers=true&skipCustomers=true&skipAppointments=true&skipStatusLogs=true&skipCustomerAttachments=true&skipAppointmentAttachments=true&skipCustomerNotes=true&skipCustomerStatuses=true&skipCalendars=true&skipCustomFields=true&skipLeaderboards=true&skipTeams=true`,
        { method: 'POST' }
      );
      if (!officesRes.ok) {
        const errorData = await officesRes.json().catch(() => ({ message: 'Unknown error' }));
        console.warn(`[Sync] Offices sync failed (non-critical): ${errorData.message || errorData.error || 'HTTP ' + officesRes.status}`);
        // Don't throw - offices sync is helpful but not critical
      } else {
        const officesData = await officesRes.json();
        console.log('[Sync] Offices result:', officesData);
        queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });
      }

      // Step 2: Sync users (needed for leaderboards to work)
      toast.info('Syncing users...', { description: 'Step 2 of 4' });
      const usersRes = await fetch(
        `/api/admin/repcard/sync?type=users`,
        { method: 'POST' }
      );
      if (!usersRes.ok) {
        const errorData = await usersRes.json().catch(() => ({ message: 'Unknown error' }));
        throw new Error(`Failed to sync users: ${errorData.message || errorData.error || 'HTTP ' + usersRes.status}`);
      }
      const usersData = await usersRes.json();
      console.log('[Sync] Users result:', usersData);
      results.push(usersData);
      queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });

      // Step 3: Sync customers
      toast.info('Syncing customers...', { description: 'Step 3 of 4' });
      const customersRes = await fetch(
        `/api/admin/repcard/sync?type=customers&startDate=${startDate}&endDate=${endDate}`,
        { method: 'POST' }
      );
      if (!customersRes.ok) {
        const errorData = await customersRes.json().catch(() => ({ message: 'Unknown error' }));
        throw new Error(`Failed to sync customers: ${errorData.message || errorData.error || 'HTTP ' + customersRes.status}`);
      }
      const customersData = await customersRes.json();
      console.log('[Sync] Customers result:', customersData);
      results.push(customersData);
      queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });

      // Step 4: Sync appointments
      toast.info('Syncing appointments...', { description: 'Step 4 of 4' });
      const appointmentsRes = await fetch(
        `/api/admin/repcard/sync?type=appointments&startDate=${startDate}&endDate=${endDate}`,
        { method: 'POST' }
      );
      if (!appointmentsRes.ok) {
        const errorData = await appointmentsRes.json().catch(() => ({ message: 'Unknown error' }));
        throw new Error(`Failed to sync appointments: ${errorData.message || errorData.error || 'HTTP ' + appointmentsRes.status}`);
      }
      const appointmentsData = await appointmentsRes.json();
      console.log('[Sync] Appointments result:', appointmentsData);
      results.push(appointmentsData);
      queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });

      // Status logs sync skipped - Canvassing tab doesn't need status logs (only uses customers table)
      // Status logs cause timeouts even for 1-day chunks due to massive data volume
      // Can be re-enabled later if needed for other analytics, with proper filtering by statusIds

      return { results, message: 'All syncs completed' };
    },
    onSuccess: (data) => {
      toast.success('Full sync completed successfully!', {
        description: `Synced data from ${startDate} to ${endDate}`
      });
      queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });
    },
    onError: (error) => {
      toast.error('Sync failed', {
        description: error.message
      });
      queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });
    },
  });

  // Incremental sync mutation
  const incrementalSyncMutation = useMutation({
    mutationFn: async () => {
      const res = await fetch('/api/admin/repcard/sync?type=incremental', { method: 'POST' });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.message || 'Failed to trigger incremental sync');
      }
      return res.json();
    },
    onSuccess: (data) => {
      toast.success('Incremental sync started successfully', {
        description: 'Fetching updates since last sync...'
      });
      queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });
    },
    onError: (error) => {
      toast.error('Failed to start incremental sync', {
        description: error.message
      });
    },
  });

  // Link users mutation
  const linkUsersMutation = useMutation({
    mutationFn: async () => {
      const res = await fetch('/api/admin/link-repcard-users', { method: 'POST' });
      if (!res.ok) {
        const error = await res.json().catch(() => ({ message: 'Unknown error' }));
        throw new Error(error.message || error.error || 'Failed to link users');
      }
      return res.json();
    },
    onSuccess: (data) => {
      toast.success('Users linked successfully', {
        description: 'Users have been linked to RepCard by email'
      });
      queryClient.invalidateQueries({ queryKey: ['repcard-sync-status'] });
      queryClient.invalidateQueries({ queryKey: ['repcard-diagnostic'] });
    },
    onError: (error) => {
      toast.error('Failed to link users', {
        description: error.message
      });
    },
  });

  const toggleRowExpansion = (runId: string) => {
    setExpandedRows(prev => {
      const newSet = new Set(prev);
      if (newSet.has(runId)) {
        newSet.delete(runId);
      } else {
        newSet.add(runId);
      }
      return newSet;
    });
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'completed':
        return <CheckCircle className="h-5 w-5 text-green-500" />;
      case 'failed':
        return <XCircle className="h-5 w-5 text-red-500" />;
      case 'running':
        return <Loader2 className="h-5 w-5 text-blue-500 animate-spin" />;
      default:
        return <Clock className="h-5 w-5 text-gray-500" />;
    }
  };

  const getStatusBadge = (status: string) => {
    const variants: Record<string, 'default' | 'secondary' | 'destructive' | 'outline'> = {
      completed: 'default',
      failed: 'destructive',
      running: 'secondary',
    };
    return (
      <Badge variant={variants[status] || 'outline'}>
        {status}
      </Badge>
    );
  };

  const getEntityIcon = (entityType: string) => {
    switch (entityType) {
      case 'customers':
        return <Users className="h-4 w-4" />;
      case 'appointments':
        return <Calendar className="h-4 w-4" />;
      case 'status_logs':
        return <FileText className="h-4 w-4" />;
      default:
        return <Database className="h-4 w-4" />;
    }
  };

  if (error) {
    return (
      <div className="container mx-auto p-6">
        <Card>
          <CardHeader>
            <CardTitle className="text-red-600 flex items-center gap-2">
              <XCircle className="h-5 w-5" />
              Error Loading Sync Status
            </CardTitle>
          </CardHeader>
          <CardContent>
            <p className="text-sm text-muted-foreground">{(error as Error).message}</p>
          </CardContent>
        </Card>
      </div>
    );
  }

  return (
    <div className="container mx-auto p-6 space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold">RepCard Sync Monitoring</h1>
          <p className="text-muted-foreground mt-1">
            Monitor and control RepCard data synchronization
          </p>
        </div>
        <Button
          variant="outline"
          size="sm"
          onClick={() => refetch()}
          disabled={isLoading}
        >
          <RefreshCw className={`h-4 w-4 mr-2 ${isLoading ? 'animate-spin' : ''}`} />
          Refresh
        </Button>
      </div>

      {/* Record Counts */}
      {data && (
        <div className="grid gap-4 md:grid-cols-4">
          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Users</CardTitle>
              <Users className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {data.recordCounts.users.toLocaleString()}
              </div>
              <p className="text-xs text-muted-foreground">
                Total RepCard users synced
              </p>
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Customers</CardTitle>
              <Users className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {data.recordCounts.customers.toLocaleString()}
              </div>
              <p className="text-xs text-muted-foreground">
                Total customer records
              </p>
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Appointments</CardTitle>
              <Calendar className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {data.recordCounts.appointments.toLocaleString()}
              </div>
              <p className="text-xs text-muted-foreground">
                Total appointment records
              </p>
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Status Logs</CardTitle>
              <FileText className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {data.recordCounts.statusLogs.toLocaleString()}
              </div>
              <p className="text-xs text-muted-foreground">
                Total status change logs
              </p>
            </CardContent>
          </Card>
        </div>
      )}

      {/* Sync Controls */}
      <Card>
        <CardHeader>
          <CardTitle>Sync Controls</CardTitle>
          <CardDescription>
            Trigger manual syncs to update RepCard data
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          {/* Quick Sync - Last 7 Days */}
          <div className="space-y-3 p-4 border rounded-lg bg-blue-50 dark:bg-blue-950">
            <h3 className="text-sm font-medium flex items-center gap-2">
              <PlayCircle className="h-4 w-4 text-blue-600" />
              Quick Sync (Last 7 Days)
            </h3>
            <p className="text-sm text-muted-foreground">
              Recommended for initial setup. Syncs users, customers + appointments from the last 7 days (~3-4 minutes). Status logs are skipped to avoid timeouts.
            </p>
            <Button
              onClick={() => {
                const today = new Date();
                const sevenDaysAgo = new Date(today);
                sevenDaysAgo.setDate(today.getDate() - 7);

                const endDateStr = today.toISOString().split('T')[0];
                const startDateStr = sevenDaysAgo.toISOString().split('T')[0];

                setStartDate(startDateStr);
                setEndDate(endDateStr);
                fullSyncMutation.mutate({ startDate: startDateStr, endDate: endDateStr });
              }}
              disabled={fullSyncMutation.isPending || incrementalSyncMutation.isPending}
              className="w-full bg-blue-600 hover:bg-blue-700"
            >
              {fullSyncMutation.isPending ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Syncing...
                </>
              ) : (
                <>
                  <PlayCircle className="mr-2 h-4 w-4" />
                  Start Quick Sync
                </>
              )}
            </Button>
          </div>

          {/* Full Sync */}
          <div className="space-y-3 pt-4 border-t">
            <h3 className="text-sm font-medium">Custom Date Range Sync</h3>
            <p className="text-sm text-muted-foreground">
              Sync customers + appointments for specific date range. Status logs are skipped to avoid timeouts.
            </p>
            <div className="flex gap-3 items-end">
              <div className="flex-1">
                <Label htmlFor="startDate" className="text-xs">Start Date</Label>
                <Input
                  id="startDate"
                  type="date"
                  value={startDate}
                  onChange={(e) => setStartDate(e.target.value)}
                  className="mt-1"
                />
              </div>
              <div className="flex-1">
                <Label htmlFor="endDate" className="text-xs">End Date</Label>
                <Input
                  id="endDate"
                  type="date"
                  value={endDate}
                  onChange={(e) => setEndDate(e.target.value)}
                  className="mt-1"
                />
              </div>
              <Button
                onClick={() => fullSyncMutation.mutate({ startDate, endDate })}
                disabled={fullSyncMutation.isPending || incrementalSyncMutation.isPending}
                className="flex-shrink-0"
              >
                {fullSyncMutation.isPending ? (
                  <>
                    <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                    Starting...
                  </>
                ) : (
                  <>
                    <PlayCircle className="mr-2 h-4 w-4" />
                    Run Full Sync
                  </>
                )}
              </Button>
            </div>
          </div>

          {/* Incremental Sync */}
          <div className="space-y-3 pt-4 border-t">
            <h3 className="text-sm font-medium">Incremental Sync</h3>
            <p className="text-sm text-muted-foreground">
              Fetch only records updated since the last sync. Runs automatically every 10 minutes via cron.
            </p>
            <Button
              variant="outline"
              onClick={() => incrementalSyncMutation.mutate()}
              disabled={fullSyncMutation.isPending || incrementalSyncMutation.isPending || linkUsersMutation.isPending}
            >
              {incrementalSyncMutation.isPending ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Starting...
                </>
              ) : (
                <>
                  <RefreshCw className="mr-2 h-4 w-4" />
                  Run Incremental Sync
                </>
              )}
            </Button>
          </div>

          {/* Link Users */}
          <div className="space-y-3 pt-4 border-t">
            <h3 className="text-sm font-medium flex items-center gap-2">
              <Users className="h-4 w-4 text-green-600" />
              Link Users to RepCard
            </h3>
            <p className="text-sm text-muted-foreground">
              Link users in your app to RepCard users by matching email addresses. Required for analytics to work.
            </p>
            <Button
              variant="outline"
              onClick={() => linkUsersMutation.mutate()}
              disabled={fullSyncMutation.isPending || incrementalSyncMutation.isPending || linkUsersMutation.isPending}
              className="border-green-300 text-green-700 hover:bg-green-50"
            >
              {linkUsersMutation.isPending ? (
                <>
                  <Loader2 className="mr-2 h-4 w-4 animate-spin" />
                  Linking...
                </>
              ) : (
                <>
                  <Users className="mr-2 h-4 w-4" />
                  Link Users to RepCard
                </>
              )}
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Latest Syncs */}
      {data?.latestSyncs && data.latestSyncs.length > 0 && (
        <Card>
          <CardHeader>
            <CardTitle>Latest Syncs (by Entity)</CardTitle>
            <CardDescription>
              Most recent sync for each entity type
            </CardDescription>
          </CardHeader>
          <CardContent>
            <div className="grid gap-3">
              {data.latestSyncs.map((sync) => (
                <div
                  key={sync.id}
                  className="flex items-center justify-between p-3 border rounded-lg"
                >
                  <div className="flex items-center gap-3">
                    {getEntityIcon(sync.entity_type)}
                    <div>
                      <div className="font-medium capitalize">{sync.entity_type.replace('_', ' ')}</div>
                      <div className="text-xs text-muted-foreground">
                        {sync.sync_type} sync • {formatDistanceToNow(new Date(sync.started_at))} ago
                      </div>
                    </div>
                  </div>
                  <div className="flex items-center gap-3">
                    <div className="text-right text-sm">
                      <div>{sync.records_fetched.toLocaleString()} fetched</div>
                      <div className="text-xs text-muted-foreground">
                        {sync.records_inserted} inserted, {sync.records_updated} updated
                        {sync.records_failed > 0 && (
                          <span className="text-red-600"> • {sync.records_failed} failed</span>
                        )}
                      </div>
                      {sync.error_message && (
                        <div className="text-xs text-red-600 mt-1 max-w-xs truncate" title={sync.error_message}>
                          Error: {sync.error_message}
                        </div>
                      )}
                    </div>
                    {getStatusIcon(sync.status)}
                  </div>
                </div>
              ))}
            </div>
          </CardContent>
        </Card>
      )}

      {/* Sync History */}
      <Card>
        <CardHeader>
          <CardTitle>Sync History</CardTitle>
          <CardDescription>
            Recent sync operations and their results
          </CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="space-y-3">
              {[...Array(5)].map((_, i) => (
                <Skeleton key={i} className="h-16 w-full" />
              ))}
            </div>
          ) : data?.syncHistory && data.syncHistory.length > 0 ? (
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead className="w-[50px]"></TableHead>
                  <TableHead>Entity</TableHead>
                  <TableHead>Type</TableHead>
                  <TableHead>Started</TableHead>
                  <TableHead>Records</TableHead>
                  <TableHead>Status</TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {data.syncHistory.map((sync) => {
                  const isExpanded = expandedRows.has(sync.id);
                  return (
                    <>
                      <TableRow
                        key={sync.id}
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => toggleRowExpansion(sync.id)}
                      >
                        <TableCell>
                          {isExpanded ? (
                            <ChevronDown className="h-4 w-4" />
                          ) : (
                            <ChevronRight className="h-4 w-4" />
                          )}
                        </TableCell>
                        <TableCell>
                          <div className="flex items-center gap-2">
                            {getEntityIcon(sync.entity_type)}
                            <span className="capitalize">{sync.entity_type.replace('_', ' ')}</span>
                          </div>
                        </TableCell>
                        <TableCell>
                          <Badge variant="outline" className="capitalize">
                            {sync.sync_type}
                          </Badge>
                        </TableCell>
                        <TableCell>
                          {formatDistanceToNow(new Date(sync.started_at))} ago
                        </TableCell>
                        <TableCell>
                          <div className="text-sm">
                            {sync.records_fetched.toLocaleString()}
                          </div>
                        </TableCell>
                        <TableCell>
                          {getStatusBadge(sync.status)}
                        </TableCell>
                      </TableRow>
                      {isExpanded && (
                        <TableRow>
                          <TableCell colSpan={6} className="bg-muted/30">
                            <div className="p-4 space-y-3">
                              <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                                <div>
                                  <div className="text-xs text-muted-foreground">Fetched</div>
                                  <div className="text-sm font-medium">{sync.records_fetched.toLocaleString()}</div>
                                </div>
                                <div>
                                  <div className="text-xs text-muted-foreground">Inserted</div>
                                  <div className="text-sm font-medium text-green-600">{sync.records_inserted.toLocaleString()}</div>
                                </div>
                                <div>
                                  <div className="text-xs text-muted-foreground">Updated</div>
                                  <div className="text-sm font-medium text-blue-600">{sync.records_updated.toLocaleString()}</div>
                                </div>
                                <div>
                                  <div className="text-xs text-muted-foreground">Failed</div>
                                  <div className="text-sm font-medium text-red-600">{sync.records_failed.toLocaleString()}</div>
                                </div>
                              </div>
                              {sync.error_message && (
                                <div className="mt-3 p-3 bg-red-50 border border-red-200 rounded">
                                  <div className="text-xs font-medium text-red-800">Error:</div>
                                  <div className="text-xs text-red-700 mt-1">{sync.error_message}</div>
                                </div>
                              )}
                            </div>
                          </TableCell>
                        </TableRow>
                      )}
                    </>
                  );
                })}
              </TableBody>
            </Table>
          ) : (
            <div className="text-center py-8 text-muted-foreground">
              <Database className="h-12 w-12 mx-auto mb-3 opacity-50" />
              <p>No sync history available</p>
              <p className="text-sm mt-1">Run your first sync to see results here</p>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
