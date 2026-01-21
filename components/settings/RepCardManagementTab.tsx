'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { 
  RefreshCw, 
  CheckCircle, 
  XCircle, 
  AlertCircle, 
  Database,
  Settings,
  BarChart3,
  PlayCircle,
  Loader2
} from 'lucide-react';
import { toast } from 'sonner';

/**
 * Unified RepCard Management Tab
 * 
 * Consolidates all RepCard-related functionality:
 * - Sync Status & Management
 * - Metrics Backfill
 * - Data Overview
 * - Configuration (Leaderboards, Analytics, Metrics)
 */
export default function RepCardManagementTab() {
  const queryClient = useQueryClient();
  const [backfilling, setBackfilling] = useState(false);

  // Fetch sync status
  const { data: syncStatus, isLoading: loadingSync } = useQuery({
    queryKey: ['repcard-sync-status'],
    queryFn: async () => {
      const res = await fetch('/api/admin/repcard/sync');
      if (!res.ok) throw new Error('Failed to fetch sync status');
      return res.json();
    },
    refetchInterval: 30000, // Refresh every 30 seconds
  });

  // Fetch data overview
  const { data: dataOverview, isLoading: loadingOverview } = useQuery({
    queryKey: ['repcard-data-overview'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/data-debug?view=overview');
      if (!res.ok) throw new Error('Failed to fetch data overview');
      return res.json();
    },
  });

  // Backfill metrics mutation
  const backfillMutation = useMutation({
    mutationFn: async () => {
      const res = await fetch('/api/admin/repcard/backfill-metrics', {
        method: 'POST',
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.message || 'Failed to backfill metrics');
      }
      return res.json();
    },
    onSuccess: (data) => {
      toast.success('Metrics backfilled successfully!', {
        description: `Updated ${data.results.within48HoursUpdated} appointments for 48h, ${data.results.powerBillUpdated} for power bill`,
      });
      queryClient.invalidateQueries(['repcard-data-overview']);
      queryClient.invalidateQueries(['repcard-unified-dashboard']);
      setBackfilling(false);
    },
    onError: (error: Error) => {
      toast.error('Backfill failed', { description: error.message });
      setBackfilling(false);
    },
  });

  const handleBackfill = () => {
    if (confirm('This will recalculate is_within_48_hours and has_power_bill for all appointments. Continue?')) {
      setBackfilling(true);
      backfillMutation.mutate();
    }
  };

  const latestSyncs = syncStatus?.latestSyncs || [];
  const lastCustomerSync = latestSyncs.find((s: any) => s.entity_type === 'customers');
  const lastAppointmentSync = latestSyncs.find((s: any) => s.entity_type === 'appointments');
  const stats = dataOverview?.stats;

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold">RepCard Management</h2>
          <p className="text-muted-foreground mt-1">
            Monitor sync status, manage metrics, and configure RepCard settings
          </p>
        </div>
      </div>

      <Tabs defaultValue="overview" className="w-full">
        <TabsList>
          <TabsTrigger value="overview">
            <BarChart3 className="w-4 h-4 mr-2" />
            Overview
          </TabsTrigger>
          <TabsTrigger value="sync">
            <RefreshCw className="w-4 h-4 mr-2" />
            Sync Status
          </TabsTrigger>
          <TabsTrigger value="metrics">
            <Database className="w-4 h-4 mr-2" />
            Metrics
          </TabsTrigger>
          <TabsTrigger value="config">
            <Settings className="w-4 h-4 mr-2" />
            Configuration
          </TabsTrigger>
        </TabsList>

        {/* Overview Tab */}
        <TabsContent value="overview" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Card>
              <CardHeader>
                <CardTitle className="text-sm font-medium">Total Customers</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold">{stats?.total_customers || 0}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-sm font-medium">Total Appointments</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold">{stats?.total_appointments || 0}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-sm font-medium">Linked Users</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold">{stats?.linked_users || 0}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-sm font-medium">Closed Appointments</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold text-blue-600">{stats?.total_closed_appointments || 0}</div>
                <div className="text-sm text-muted-foreground mt-1">
                  {stats?.with_closer_assigned || 0} with closer assigned
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-sm font-medium">Without Closer</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="text-3xl font-bold text-yellow-600">{stats?.without_closer || 0}</div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="text-sm font-medium">Last Sync</CardTitle>
              </CardHeader>
              <CardContent>
                {lastAppointmentSync ? (
                  <div className="space-y-1">
                    <div className="flex items-center gap-2">
                      {lastAppointmentSync.status === 'completed' ? (
                        <CheckCircle className="h-4 w-4 text-green-500" />
                      ) : (
                        <XCircle className="h-4 w-4 text-red-500" />
                      )}
                      <span className="text-sm font-medium">{lastAppointmentSync.status}</span>
                    </div>
                    <div className="text-xs text-muted-foreground">
                      {lastAppointmentSync.completed_at
                        ? new Date(lastAppointmentSync.completed_at).toLocaleString()
                        : 'Never'}
                    </div>
                  </div>
                ) : (
                  <div className="text-sm text-muted-foreground">No sync data</div>
                )}
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        {/* Sync Status Tab */}
        <TabsContent value="sync" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Sync Status</CardTitle>
              <CardDescription>
                Monitor RepCard data synchronization status
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div className="p-4 bg-gray-50 rounded-lg">
                  <h3 className="font-semibold mb-2 flex items-center gap-2">
                    <Database className="w-4 h-4" />
                    Customers Sync
                  </h3>
                  {lastCustomerSync ? (
                    <>
                      <div className="flex items-center gap-2 mb-1">
                        {lastCustomerSync.status === 'completed' ? (
                          <CheckCircle className="h-4 w-4 text-green-500" />
                        ) : (
                          <XCircle className="h-4 w-4 text-red-500" />
                        )}
                        <span className="text-sm font-medium">{lastCustomerSync.status}</span>
                      </div>
                      <div className="text-xs text-muted-foreground space-y-1">
                        <div>Started: {new Date(lastCustomerSync.started_at).toLocaleString()}</div>
                        {lastCustomerSync.completed_at && (
                          <div>Completed: {new Date(lastCustomerSync.completed_at).toLocaleString()}</div>
                        )}
                        <div>
                          Fetched: {lastCustomerSync.records_fetched || 0} | 
                          Inserted: {lastCustomerSync.records_inserted || 0} | 
                          Updated: {lastCustomerSync.records_updated || 0}
                        </div>
                        {lastCustomerSync.error_message && (
                          <div className="text-red-600 mt-1">{lastCustomerSync.error_message}</div>
                        )}
                      </div>
                    </>
                  ) : (
                    <div className="text-sm text-muted-foreground">No sync history</div>
                  )}
                </div>

                <div className="p-4 bg-gray-50 rounded-lg">
                  <h3 className="font-semibold mb-2 flex items-center gap-2">
                    <Database className="w-4 h-4" />
                    Appointments Sync
                  </h3>
                  {lastAppointmentSync ? (
                    <>
                      <div className="flex items-center gap-2 mb-1">
                        {lastAppointmentSync.status === 'completed' ? (
                          <CheckCircle className="h-4 w-4 text-green-500" />
                        ) : (
                          <XCircle className="h-4 w-4 text-red-500" />
                        )}
                        <span className="text-sm font-medium">{lastAppointmentSync.status}</span>
                      </div>
                      <div className="text-xs text-muted-foreground space-y-1">
                        <div>Started: {new Date(lastAppointmentSync.started_at).toLocaleString()}</div>
                        {lastAppointmentSync.completed_at && (
                          <div>Completed: {new Date(lastAppointmentSync.completed_at).toLocaleString()}</div>
                        )}
                        <div>
                          Fetched: {lastAppointmentSync.records_fetched || 0} | 
                          Inserted: {lastAppointmentSync.records_inserted || 0} | 
                          Updated: {lastAppointmentSync.records_updated || 0}
                        </div>
                        {lastAppointmentSync.error_message && (
                          <div className="text-red-600 mt-1">{lastAppointmentSync.error_message}</div>
                        )}
                      </div>
                    </>
                  ) : (
                    <div className="text-sm text-muted-foreground">No sync history</div>
                  )}
                </div>
              </div>

              <Alert>
                <AlertCircle className="h-4 w-4" />
                <AlertDescription>
                  Sync runs automatically every 5 minutes via Vercel Cron. 
                  Last sync time is shown above.
                </AlertDescription>
              </Alert>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Metrics Tab */}
        <TabsContent value="metrics" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Metrics Backfill</CardTitle>
              <CardDescription>
                Recalculate is_within_48_hours and has_power_bill for all appointments
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <Alert>
                <AlertCircle className="h-4 w-4" />
                <AlertDescription>
                  This will update all appointments to calculate:
                  <ul className="list-disc list-inside mt-2 space-y-1">
                    <li><strong>is_within_48_hours:</strong> Whether appointment was scheduled within 48 hours of customer creation</li>
                    <li><strong>has_power_bill:</strong> Whether customer/appointment has a power bill attachment</li>
                  </ul>
                </AlertDescription>
              </Alert>

              <Button 
                onClick={handleBackfill} 
                disabled={backfilling}
                className="w-full md:w-auto"
              >
                {backfilling ? (
                  <>
                    <Loader2 className="w-4 h-4 mr-2 animate-spin" />
                    Backfilling...
                  </>
                ) : (
                  <>
                    <PlayCircle className="w-4 h-4 mr-2" />
                    Run Metrics Backfill
                  </>
                )}
              </Button>

              {backfillMutation.data && (
                <Card className="bg-green-50 border-green-200">
                  <CardContent className="pt-6">
                    <div className="space-y-2">
                      <div className="font-semibold text-green-800">Backfill Complete!</div>
                      <div className="text-sm text-green-700 space-y-1">
                        <div>Updated {backfillMutation.data.results.within48HoursUpdated} appointments for 48-hour calculation</div>
                        <div>Updated {backfillMutation.data.results.powerBillUpdated} appointments for power bill status</div>
                        {backfillMutation.data.results.verification && (
                          <div className="mt-2 pt-2 border-t border-green-200">
                            <div className="font-medium">Verification:</div>
                            <div>Total: {backfillMutation.data.results.verification.totalAppointments}</div>
                            <div>Within 48h: {backfillMutation.data.results.verification.within48h} ({backfillMutation.data.results.verification.within48hPercentage}%)</div>
                            <div>With Power Bill: {backfillMutation.data.results.verification.withPowerBill} ({backfillMutation.data.results.verification.powerBillPercentage}%)</div>
                          </div>
                        )}
                      </div>
                    </div>
                  </CardContent>
                </Card>
              )}
            </CardContent>
          </Card>
        </TabsContent>

        {/* Configuration Tab - Placeholder for now */}
        <TabsContent value="config" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Configuration</CardTitle>
              <CardDescription>
                Configure leaderboards, analytics widgets, and metric definitions
              </CardDescription>
            </CardHeader>
            <CardContent>
              <Alert>
                <AlertCircle className="h-4 w-4" />
                <AlertDescription>
                  Configuration moved to separate tab. Use the RepCard Config tab in settings.
                </AlertDescription>
              </Alert>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}
