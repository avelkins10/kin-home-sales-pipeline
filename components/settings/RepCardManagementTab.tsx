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
  Loader2,
  ChevronDown,
  ChevronRight,
  Users,
  Calendar,
  FileText,
  StickyNote,
  TrendingUp,
  Building,
  Activity
} from 'lucide-react';
import { toast } from 'sonner';
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible';

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

  // Fetch comprehensive metrics - ALL RepCard data
  const { data: comprehensiveMetrics, isLoading: loadingMetrics } = useQuery({
    queryKey: ['repcard-comprehensive-metrics'],
    queryFn: async () => {
      const res = await fetch('/api/admin/repcard/comprehensive-metrics');
      if (!res.ok) throw new Error('Failed to fetch comprehensive metrics');
      return res.json();
    },
    refetchInterval: 60000, // Refresh every minute
  });

  // Fetch data overview (legacy)
  const { data: dataOverview, isLoading: loadingOverview } = useQuery({
    queryKey: ['repcard-data-overview'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/data-debug?view=overview');
      if (!res.ok) throw new Error('Failed to fetch data overview');
      return res.json();
    },
  });

  // Run migration 032 mutation
  const migrationMutation = useMutation({
    mutationFn: async () => {
      const res = await fetch('/api/admin/repcard/run-migration-032', {
        method: 'POST',
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.message || 'Failed to run migration');
      }
      return res.json();
    },
    onSuccess: (data) => {
      toast.success('Migration 032 completed successfully!', {
        description: `Created audit table and enhanced triggers. Found ${data.verification.triggerFunctionsFound} trigger functions.`,
      });
      queryClient.invalidateQueries(['repcard-data-overview']);
      queryClient.invalidateQueries(['repcard-comprehensive-metrics']);
    },
    onError: (error: Error) => {
      toast.error('Migration failed', { description: error.message });
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
        description: `Triggered recalculation for ${data.results.within48HoursUpdated} appointments using event-driven triggers`,
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

  const handleMigration = () => {
    if (confirm('This will create the audit trail table and enhanced triggers for event-driven metrics. Continue?')) {
      migrationMutation.mutate();
    }
  };

  const handleBackfill = () => {
    if (confirm('This will trigger recalculation of is_within_48_hours and has_power_bill for all appointments using triggers. Continue?')) {
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

        {/* Overview Tab - Comprehensive Metrics */}
        <TabsContent value="overview" className="space-y-4">
          {loadingMetrics ? (
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center justify-center py-8">
                  <Loader2 className="w-6 h-6 animate-spin text-muted-foreground" />
                  <span className="ml-2 text-muted-foreground">Loading comprehensive metrics...</span>
                </div>
              </CardContent>
            </Card>
          ) : comprehensiveMetrics?.metrics ? (
            <div className="space-y-4">
              <Alert>
                <Database className="h-4 w-4" />
                <AlertDescription>
                  <strong>Source of Truth:</strong> This page shows every metric available from RepCard data. 
                  All metrics are calculated from the synced database tables.
                  {comprehensiveMetrics.generatedAt && (
                    <span className="block mt-1 text-xs">
                      Last updated: {new Date(comprehensiveMetrics.generatedAt).toLocaleString()}
                    </span>
                  )}
                </AlertDescription>
              </Alert>

              {/* Customers Section */}
              <MetricSection
                title="Customers (Door Knocks)"
                icon={<Users className="w-5 h-5" />}
                metrics={comprehensiveMetrics.metrics.customers}
                breakdowns={{
                  byStatus: comprehensiveMetrics.metrics.customers.byStatus,
                  byOffice: comprehensiveMetrics.metrics.customers.byOffice,
                }}
              />

              {/* Appointments Section */}
              <MetricSection
                title="Appointments"
                icon={<Calendar className="w-5 h-5" />}
                metrics={comprehensiveMetrics.metrics.appointments}
                breakdowns={{
                  byDisposition: comprehensiveMetrics.metrics.appointments.byDisposition,
                  byStatusCategory: comprehensiveMetrics.metrics.appointments.byStatusCategory,
                }}
              />

              {/* Quality Metrics Section */}
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <TrendingUp className="w-5 h-5" />
                    Quality Metrics (Last 30 Days)
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                    <MetricCard
                      label="48-Hour Speed"
                      value={`${comprehensiveMetrics.metrics.quality.within_48h_percentage || 0}%`}
                      subValue={`${comprehensiveMetrics.metrics.quality.within_48h_count || 0} of ${comprehensiveMetrics.metrics.quality.total_appointments || 0}`}
                    />
                    <MetricCard
                      label="Power Bill Rate"
                      value={`${comprehensiveMetrics.metrics.quality.power_bill_percentage || 0}%`}
                      subValue={`${comprehensiveMetrics.metrics.quality.with_power_bill_count || 0} appointments`}
                    />
                    <MetricCard
                      label="Both Metrics"
                      value={`${comprehensiveMetrics.metrics.quality.both_count || 0}`}
                      subValue="High quality"
                    />
                    <MetricCard
                      label="Reschedule Rate"
                      value={`${comprehensiveMetrics.metrics.quality.reschedule_percentage || 0}%`}
                      subValue={`${comprehensiveMetrics.metrics.quality.reschedule_count || 0} reschedules`}
                    />
                  </div>
                </CardContent>
              </Card>

              {/* Attachments Section */}
              <MetricSection
                title="Attachments"
                icon={<FileText className="w-5 h-5" />}
                metrics={comprehensiveMetrics.metrics.attachments}
                breakdowns={{
                  byType: comprehensiveMetrics.metrics.attachments.byType,
                }}
              />

              {/* Notes Section */}
              <MetricSection
                title="Notes"
                icon={<StickyNote className="w-5 h-5" />}
                metrics={comprehensiveMetrics.metrics.notes}
              />

              {/* Status Logs Section */}
              <MetricSection
                title="Status Changes"
                icon={<Activity className="w-5 h-5" />}
                metrics={comprehensiveMetrics.metrics.statusLogs}
                breakdowns={{
                  byStatus: comprehensiveMetrics.metrics.statusLogs.byStatus,
                }}
              />

              {/* Users Section */}
              <MetricSection
                title="Users"
                icon={<Users className="w-5 h-5" />}
                metrics={comprehensiveMetrics.metrics.users}
                breakdowns={{
                  byRole: comprehensiveMetrics.metrics.users.byRole,
                }}
              />

              {/* Offices Section */}
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <Building className="w-5 h-5" />
                    Offices
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="grid grid-cols-2 gap-4">
                    <MetricCard
                      label="Total Offices"
                      value={comprehensiveMetrics.metrics.offices.total_offices || 0}
                    />
                    <MetricCard
                      label="Unique Companies"
                      value={comprehensiveMetrics.metrics.offices.unique_companies || 0}
                    />
                  </div>
                </CardContent>
              </Card>

              {/* Conversion Metrics */}
              <Card>
                <CardHeader>
                  <CardTitle className="flex items-center gap-2">
                    <TrendingUp className="w-5 h-5" />
                    Conversion Rates
                  </CardTitle>
                </CardHeader>
                <CardContent>
                  <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
                    <MetricCard
                      label="Customer → Appointment"
                      value={`${comprehensiveMetrics.metrics.conversion.customer_to_appointment_rate || 0}%`}
                      subValue={`${comprehensiveMetrics.metrics.conversion.total_appointments || 0} appointments from ${comprehensiveMetrics.metrics.conversion.total_customers || 0} customers`}
                    />
                    <MetricCard
                      label="Appointment → Close"
                      value={`${comprehensiveMetrics.metrics.conversion.appointment_to_close_rate || 0}%`}
                      subValue={`${comprehensiveMetrics.metrics.conversion.closed_appointments || 0} closed`}
                    />
                    <MetricCard
                      label="Total Closed"
                      value={comprehensiveMetrics.metrics.conversion.closed_appointments || 0}
                      subValue="Sales closed"
                    />
                  </div>
                </CardContent>
              </Card>

              {/* Sync Status */}
              {comprehensiveMetrics.metrics.sync && comprehensiveMetrics.metrics.sync.length > 0 && (
                <Card>
                  <CardHeader>
                    <CardTitle className="flex items-center gap-2">
                      <RefreshCw className="w-5 h-5" />
                      Sync History
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="space-y-2">
                      {comprehensiveMetrics.metrics.sync.map((sync: any) => (
                        <div key={sync.entity_type} className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                          <div>
                            <div className="font-medium capitalize">{sync.entity_type}</div>
                            <div className="text-sm text-muted-foreground">
                              Last sync: {sync.last_sync ? new Date(sync.last_sync).toLocaleString() : 'Never'}
                            </div>
                          </div>
                          <div className="text-right text-sm">
                            <div>Fetched: {sync.total_fetched?.toLocaleString() || 0}</div>
                            <div>Inserted: {sync.total_inserted?.toLocaleString() || 0}</div>
                            <div>Updated: {sync.total_updated?.toLocaleString() || 0}</div>
                            <div className="mt-1">
                              <Badge variant={sync.failed_syncs > 0 ? 'destructive' : 'default'}>
                                {sync.successful_syncs} success, {sync.failed_syncs} failed
                              </Badge>
                            </div>
                          </div>
                        </div>
                      ))}
                    </div>
                  </CardContent>
                </Card>
              )}
            </div>
          ) : (
            <Card>
              <CardContent className="pt-6">
                <div className="text-center text-muted-foreground">No metrics data available</div>
              </CardContent>
            </Card>
          )}
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
          {/* Step 1: Migration */}
          <Card>
            <CardHeader>
              <CardTitle>Step 1: Run Migration 032 (One-time Setup)</CardTitle>
              <CardDescription>
                Creates audit trail table and enhanced triggers for event-driven metrics
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <Alert>
                <Info className="h-4 w-4" />
                <AlertDescription>
                  This migration creates the <code className="bg-muted px-1 py-0.5 rounded text-xs">repcard_metric_audit</code> table and enhanced triggers that automatically calculate metrics when data changes. Only needs to be run once.
                </AlertDescription>
              </Alert>

              <Button 
                onClick={handleMigration} 
                disabled={migrationMutation.isPending}
                className="w-full md:w-auto"
              >
                {migrationMutation.isPending ? (
                  <>
                    <Loader2 className="w-4 h-4 mr-2 animate-spin" />
                    Running Migration...
                  </>
                ) : (
                  <>
                    <Database className="w-4 h-4 mr-2" />
                    Run Migration 032
                  </>
                )}
              </Button>

              {migrationMutation.data && (
                <div className="mt-4 p-4 bg-green-50 border border-green-200 rounded-lg">
                  <div className="font-semibold text-green-800 mb-2">Migration Complete!</div>
                  <div className="text-sm text-green-700 space-y-1">
                    <div>✅ Created repcard_metric_audit table</div>
                    <div>✅ Created {migrationMutation.data.verification.triggerFunctionsFound} trigger functions</div>
                    {migrationMutation.data.verification.functions && (
                      <div className="text-xs mt-2 text-green-600">
                        Functions: {migrationMutation.data.verification.functions.join(', ')}
                      </div>
                    )}
                  </div>
                </div>
              )}

              {migrationMutation.error && (
                <Alert variant="destructive" className="mt-4">
                  <AlertCircle className="h-4 w-4" />
                  <AlertDescription>
                    Error: {migrationMutation.error.message}
                  </AlertDescription>
                </Alert>
              )}
            </CardContent>
          </Card>

          {/* Step 2: Backfill */}
          <Card>
            <CardHeader>
              <CardTitle>Step 2: Run Metrics Backfill</CardTitle>
              <CardDescription>
                Trigger recalculation of is_within_48_hours and has_power_bill for all appointments using event-driven triggers
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <Alert>
                <AlertCircle className="h-4 w-4" />
                <AlertDescription>
                  This will update all appointments to trigger recalculation using the new trigger logic:
                  <ul className="list-disc list-inside mt-2 space-y-1">
                    <li><strong>is_within_48_hours:</strong> Whether appointment was scheduled within 48 hours of customer creation</li>
                    <li><strong>has_power_bill:</strong> Whether customer/appointment has any attachment (simplified: any attachment = power bill)</li>
                  </ul>
                </AlertDescription>
              </Alert>

              <Button 
                onClick={handleBackfill} 
                disabled={backfilling || !migrationMutation.data}
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
                    <div className="space-y-3">
                      <div className="font-semibold text-green-800">Backfill Complete!</div>
                      <div className="text-sm text-green-700 space-y-1">
                        <div className="font-medium">Updates Applied:</div>
                        <div>• Updated {backfillMutation.data.results.within48HoursUpdated} appointments for 48-hour calculation</div>
                        <div>• Updated {backfillMutation.data.results.powerBillUpdated} appointments for power bill status</div>
                        {backfillMutation.data.results.totalAppointmentsInDatabase !== undefined && (
                          <div className="mt-2 text-xs text-green-600">
                            Total appointments in database: {backfillMutation.data.results.totalAppointmentsInDatabase}
                          </div>
                        )}
                        {backfillMutation.data.results.verification && (
                          <div className="mt-3 pt-3 border-t border-green-200">
                            <div className="font-medium">Recent Stats (Last 30 Days):</div>
                            <div>Total: {backfillMutation.data.results.verification.totalAppointments}</div>
                            <div>Within 48h: {backfillMutation.data.results.verification.within48h} ({backfillMutation.data.results.verification.within48hPercentage}%)</div>
                            <div>With Power Bill: {backfillMutation.data.results.verification.withPowerBill} ({backfillMutation.data.results.verification.powerBillPercentage}%)</div>
                            {backfillMutation.data.results.verification.nullWithin48h > 0 && (
                              <div className="text-yellow-700 mt-1">
                                ⚠️ {backfillMutation.data.results.verification.nullWithin48h} appointments still have NULL for 48h
                              </div>
                            )}
                            {backfillMutation.data.results.verification.nullPowerBill > 0 && (
                              <div className="text-yellow-700 mt-1">
                                ⚠️ {backfillMutation.data.results.verification.nullPowerBill} appointments still have NULL for power bill
                              </div>
                            )}
                          </div>
                        )}
                        {backfillMutation.data.results.verificationAllTime && (
                          <div className="mt-2 pt-2 border-t border-green-200">
                            <div className="font-medium text-xs">All-Time Stats:</div>
                            <div className="text-xs">Total: {backfillMutation.data.results.verificationAllTime.totalAppointments} | 48h: {backfillMutation.data.results.verificationAllTime.within48hPercentage}% | PB: {backfillMutation.data.results.verificationAllTime.powerBillPercentage}%</div>
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

// Helper component for metric sections
function MetricSection({ 
  title, 
  icon, 
  metrics, 
  breakdowns 
}: { 
  title: string; 
  icon: React.ReactNode;
  metrics: any;
  breakdowns?: Record<string, any[]>;
}) {
  const [isOpen, setIsOpen] = useState(true);

  if (!metrics || !metrics.summary) return null;

  const summary = metrics.summary;

  return (
    <Card>
      <Collapsible open={isOpen} onOpenChange={setIsOpen}>
        <CollapsibleTrigger asChild>
          <CardHeader className="cursor-pointer hover:bg-gray-50 transition-colors">
            <CardTitle className="flex items-center justify-between">
              <div className="flex items-center gap-2">
                {icon}
                {title}
              </div>
              {isOpen ? <ChevronDown className="w-4 h-4" /> : <ChevronRight className="w-4 h-4" />}
            </CardTitle>
          </CardHeader>
        </CollapsibleTrigger>
        <CollapsibleContent>
          <CardContent className="space-y-4">
            {/* Summary metrics grid */}
            <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
              {Object.entries(summary).map(([key, value]: [string, any]) => {
                if (key.includes('earliest') || key.includes('latest')) return null;
                return (
                  <MetricCard
                    key={key}
                    label={key.replace(/_/g, ' ').replace(/\b\w/g, l => l.toUpperCase())}
                    value={typeof value === 'number' ? value.toLocaleString() : value || 'N/A'}
                  />
                );
              })}
            </div>

            {/* Breakdowns */}
            {breakdowns && Object.entries(breakdowns).map(([breakdownKey, breakdownData]: [string, any[]]) => {
              if (!breakdownData || breakdownData.length === 0) return null;
              return (
                <div key={breakdownKey} className="mt-4 pt-4 border-t">
                  <h4 className="font-semibold mb-2 capitalize">
                    {breakdownKey.replace(/([A-Z])/g, ' $1').trim()}
                  </h4>
                  <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-2">
                    {breakdownData.slice(0, 10).map((item: any, idx: number) => (
                      <div key={idx} className="flex items-center justify-between p-2 bg-gray-50 rounded text-sm">
                        <span className="truncate">{item[breakdownKey.includes('Status') ? 'new_status' : breakdownKey.includes('Disposition') ? 'disposition' : breakdownKey.includes('Office') ? 'office_name' : breakdownKey.includes('Role') ? 'role' : 'attachment_type'] || 'N/A'}</span>
                        <Badge variant="secondary" className="ml-2">{item.count}</Badge>
                      </div>
                    ))}
                    {breakdownData.length > 10 && (
                      <div className="text-sm text-muted-foreground p-2">
                        +{breakdownData.length - 10} more
                      </div>
                    )}
                  </div>
                </div>
              );
            })}
          </CardContent>
        </CollapsibleContent>
      </Collapsible>
    </Card>
  );
}

// Helper component for metric cards
function MetricCard({ 
  label, 
  value, 
  subValue 
}: { 
  label: string; 
  value: string | number;
  subValue?: string;
}) {
  return (
    <div className="p-4 bg-gray-50 rounded-lg">
      <div className="text-xs text-muted-foreground mb-1">{label}</div>
      <div className="text-2xl font-bold">{value}</div>
      {subValue && <div className="text-xs text-muted-foreground mt-1">{subValue}</div>}
    </div>
  );
}
