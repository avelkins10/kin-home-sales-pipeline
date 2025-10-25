'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { 
  RefreshCw, 
  PlayCircle, 
  CheckCircle, 
  XCircle, 
  AlertTriangle, 
  Clock,
  ChevronDown,
  ChevronRight,
  Loader2
} from 'lucide-react';
import { formatDistanceToNow } from 'date-fns';
import { toast } from 'sonner';

interface SyncRunRecord {
  id: number;
  started_at: string;
  completed_at: string | null;
  status: 'running' | 'success' | 'partial' | 'failed';
  total_users: number;
  enriched: number;
  not_found: number;
  already_up_to_date: number;
  errors: number;
  error_details: any;
  not_found_samples: any;
  triggered_by: 'cron' | 'manual';
  triggered_by_user_id: string | null;
  execution_time_ms: number | null;
}

interface SyncRunStats {
  total_runs: number;
  success_rate: number;
  avg_execution_time_ms: number;
  last_success_at: string | null;
  consecutive_failures: number;
}

interface SyncHistoryResponse {
  history: SyncRunRecord[];
  stats: SyncRunStats;
  timestamp: string;
}

export default function SyncMonitoringPage() {
  const [expandedRows, setExpandedRows] = useState<Set<number>>(new Set());
  const queryClient = useQueryClient();

  // Fetch sync history and statistics
  const { data, isLoading, error, refetch } = useQuery<SyncHistoryResponse>({
    queryKey: ['sync-history'],
    queryFn: async () => {
      const res = await fetch('/api/admin/sync-users/history');
      if (!res.ok) throw new Error('Failed to fetch sync history');
      return res.json();
    },
    refetchInterval: 30000, // Auto-refresh every 30 seconds
  });

  // Manual sync trigger mutation
  const manualSyncMutation = useMutation({
    mutationFn: async (options: { force?: boolean; limit?: number; verbose?: boolean } = {}) => {
      const res = await fetch('/api/admin/sync-users', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(options),
      });
      if (!res.ok) throw new Error('Failed to trigger manual sync');
      return res.json();
    },
    onSuccess: (data) => {
      toast.success('Manual sync started successfully', {
        description: `Processing ${data.stats.totalUsers} users...`
      });
      queryClient.invalidateQueries({ queryKey: ['sync-history'] });
    },
    onError: (error) => {
      toast.error('Failed to start manual sync', {
        description: error.message
      });
    },
  });

  const toggleRowExpansion = (runId: number) => {
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

  const getStatusBadge = (status: string) => {
    const variants = {
      running: { color: 'bg-blue-100 text-blue-800', icon: Loader2, label: 'Running' },
      success: { color: 'bg-green-100 text-green-800', icon: CheckCircle, label: 'Success' },
      partial: { color: 'bg-yellow-100 text-yellow-800', icon: AlertTriangle, label: 'Partial' },
      failed: { color: 'bg-red-100 text-red-800', icon: XCircle, label: 'Failed' },
    };
    
    const variant = variants[status as keyof typeof variants] || variants.failed;
    const Icon = variant.icon;
    
    return (
      <Badge className={`${variant.color} flex items-center gap-1`}>
        <Icon className="h-3 w-3" />
        {variant.label}
      </Badge>
    );
  };

  const formatDuration = (ms: number | null) => {
    if (!ms) return 'N/A';
    const minutes = Math.floor(ms / 60000);
    const seconds = Math.floor((ms % 60000) / 1000);
    return `${minutes}m ${seconds}s`;
  };

  const formatTimestamp = (timestamp: string) => {
    return formatDistanceToNow(new Date(timestamp), { addSuffix: true });
  };

  if (error) {
    return (
      <div className="p-6">
        <Card>
          <CardHeader>
            <CardTitle className="text-red-600">Error Loading Sync History</CardTitle>
            <CardDescription>
              Failed to load sync monitoring data. Please try again.
            </CardDescription>
          </CardHeader>
          <CardContent>
            <Button onClick={() => refetch()} variant="outline">
              <RefreshCw className="h-4 w-4 mr-2" />
              Retry
            </Button>
          </CardContent>
        </Card>
      </div>
    );
  }

  return (
    <div className="p-6 space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold text-slate-900">User Sync Monitoring</h1>
          <p className="text-slate-600 mt-1">
            Monitor daily user synchronization from QuickBase Contacts table
          </p>
        </div>
        <Button 
          onClick={() => manualSyncMutation.mutate({ force: false })}
          disabled={manualSyncMutation.isPending}
          className="flex items-center gap-2"
        >
          {manualSyncMutation.isPending ? (
            <Loader2 className="h-4 w-4 animate-spin" />
          ) : (
            <PlayCircle className="h-4 w-4" />
          )}
          Run Manual Sync
        </Button>
      </div>

      {/* Statistics Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium text-slate-600">Total Runs</CardTitle>
          </CardHeader>
          <CardContent>
            {isLoading ? (
              <Skeleton className="h-8 w-16" />
            ) : (
              <div className="text-2xl font-bold">{data?.stats.total_runs || 0}</div>
            )}
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium text-slate-600">Success Rate</CardTitle>
          </CardHeader>
          <CardContent>
            {isLoading ? (
              <Skeleton className="h-8 w-16" />
            ) : (
              <div className="text-2xl font-bold text-green-600">
                {data?.stats.success_rate || 0}%
              </div>
            )}
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium text-slate-600">Avg Execution Time</CardTitle>
          </CardHeader>
          <CardContent>
            {isLoading ? (
              <Skeleton className="h-8 w-16" />
            ) : (
              <div className="text-2xl font-bold">
                {data?.stats.avg_execution_time_ms 
                  ? formatDuration(data.stats.avg_execution_time_ms)
                  : 'N/A'
                }
              </div>
            )}
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="pb-2">
            <CardTitle className="text-sm font-medium text-slate-600">Last Success</CardTitle>
          </CardHeader>
          <CardContent>
            {isLoading ? (
              <Skeleton className="h-8 w-16" />
            ) : (
              <div className="text-sm text-slate-600">
                {data?.stats.last_success_at 
                  ? formatTimestamp(data.stats.last_success_at)
                  : 'Never'
                }
              </div>
            )}
          </CardContent>
        </Card>
      </div>

      {/* History Table */}
      <Card>
        <CardHeader>
          <CardTitle>Sync Run History</CardTitle>
          <CardDescription>
            Recent sync runs with detailed statistics and error information
          </CardDescription>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="space-y-3">
              {[...Array(5)].map((_, i) => (
                <Skeleton key={i} className="h-12 w-full" />
              ))}
            </div>
          ) : !data?.history.length ? (
            <div className="text-center py-8 text-slate-500">
              <Clock className="h-12 w-12 mx-auto mb-4 text-slate-400" />
              <p className="text-lg font-medium">No sync runs found</p>
              <p className="text-sm">Run a manual sync to get started</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead>Status</TableHead>
                    <TableHead>Started At</TableHead>
                    <TableHead>Duration</TableHead>
                    <TableHead>Users Processed</TableHead>
                    <TableHead>Enriched</TableHead>
                    <TableHead>Errors</TableHead>
                    <TableHead>Triggered By</TableHead>
                    <TableHead>Actions</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {data.history.map((run) => (
                    <>
                      <TableRow key={run.id}>
                        <TableCell>{getStatusBadge(run.status)}</TableCell>
                        <TableCell>
                          <div className="text-sm">
                            {formatTimestamp(run.started_at)}
                          </div>
                          <div className="text-xs text-slate-500">
                            {new Date(run.started_at).toLocaleString()}
                          </div>
                        </TableCell>
                        <TableCell>
                          {run.status === 'running' ? (
                            <span className="text-blue-600">Running...</span>
                          ) : (
                            formatDuration(run.execution_time_ms)
                          )}
                        </TableCell>
                        <TableCell>{run.total_users}</TableCell>
                        <TableCell className="text-green-600">{run.enriched}</TableCell>
                        <TableCell className={run.errors > 0 ? 'text-red-600' : ''}>
                          {run.errors}
                        </TableCell>
                        <TableCell>
                          {run.triggered_by === 'cron' ? 'Cron' : 'Manual'}
                        </TableCell>
                        <TableCell>
                          <Button
                            variant="ghost"
                            size="sm"
                            onClick={() => toggleRowExpansion(run.id)}
                            className="flex items-center gap-1"
                          >
                            {expandedRows.has(run.id) ? (
                              <ChevronDown className="h-4 w-4" />
                            ) : (
                              <ChevronRight className="h-4 w-4" />
                            )}
                            Details
                          </Button>
                        </TableCell>
                      </TableRow>
                      
                      {/* Expanded Row Details */}
                      {expandedRows.has(run.id) && (
                        <TableRow>
                          <TableCell colSpan={8} className="bg-slate-50">
                            <div className="p-4 space-y-4">
                              {/* Statistics */}
                              <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                                <div>
                                  <div className="text-sm font-medium text-slate-600">Not Found</div>
                                  <div className="text-lg font-semibold">{run.not_found}</div>
                                </div>
                                <div>
                                  <div className="text-sm font-medium text-slate-600">Already Up-to-Date</div>
                                  <div className="text-lg font-semibold">{run.already_up_to_date}</div>
                                </div>
                                <div>
                                  <div className="text-sm font-medium text-slate-600">Error Rate</div>
                                  <div className="text-lg font-semibold">
                                    {run.total_users > 0 
                                      ? Math.round((run.errors / run.total_users) * 100)
                                      : 0
                                    }%
                                  </div>
                                </div>
                                <div>
                                  <div className="text-sm font-medium text-slate-600">Not Found Rate</div>
                                  <div className="text-lg font-semibold">
                                    {run.total_users > 0 
                                      ? Math.round((run.not_found / run.total_users) * 100)
                                      : 0
                                    }%
                                  </div>
                                </div>
                              </div>

                              {/* Error Details */}
                              {run.error_details && run.error_details.length > 0 && (
                                <div>
                                  <div className="text-sm font-medium text-slate-600 mb-2">Error Details</div>
                                  <div className="bg-red-50 border border-red-200 rounded-lg p-3">
                                    <div className="space-y-1">
                                      {run.error_details.slice(0, 5).map((error: any, index: number) => (
                                        <div key={index} className="text-sm">
                                          <span className="font-medium">{error.email}:</span> {error.error}
                                        </div>
                                      ))}
                                      {run.error_details.length > 5 && (
                                        <div className="text-sm text-slate-500">
                                          ... and {run.error_details.length - 5} more errors
                                        </div>
                                      )}
                                    </div>
                                  </div>
                                </div>
                              )}

                              {/* Not Found Samples */}
                              {run.not_found_samples && run.not_found_samples.length > 0 && (
                                <div>
                                  <div className="text-sm font-medium text-slate-600 mb-2">Not Found Sample Emails</div>
                                  <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-3">
                                    <div className="space-y-1">
                                      {run.not_found_samples.slice(0, 5).map((email: string, index: number) => (
                                        <div key={index} className="text-sm">{email}</div>
                                      ))}
                                      {run.not_found_samples.length > 5 && (
                                        <div className="text-sm text-slate-500">
                                          ... and {run.not_found_samples.length - 5} more emails
                                        </div>
                                      )}
                                    </div>
                                  </div>
                                </div>
                              )}
                            </div>
                          </TableCell>
                        </TableRow>
                      )}
                    </>
                  ))}
                </TableBody>
              </Table>
            </div>
          )}
        </CardContent>
      </Card>

      {/* Last Updated */}
      {data && (
        <div className="text-sm text-slate-500 text-center">
          Last updated: {formatTimestamp(data.timestamp)} • Auto-refreshes every 30 seconds
        </div>
      )}
    </div>
  );
}
