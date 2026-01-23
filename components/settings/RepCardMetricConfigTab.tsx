'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Checkbox } from '@/components/ui/checkbox';
import { Badge } from '@/components/ui/badge';
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from '@/components/ui/table';
import { toast } from 'sonner';
import { Save, RefreshCw, Search, CheckCircle2, XCircle, Loader2 } from 'lucide-react';
import type { MetricConfig } from '@/lib/repcard/metric-config';

const METRIC_DISPLAY_NAMES: Record<string, string> = {
  door_knocked: 'Door Knocked',
  appointment_set: 'Appointment Set',
  sit: 'Sit',
  sat_closed: 'Sat Closed',
  sat_no_close: 'Sat No Close',
  quality_metric: 'Quality Metric',
  appointments_run: 'Appointments Run'
};

export function RepCardMetricConfigTab() {
  const queryClient = useQueryClient();
  const [selectedMetric, setSelectedMetric] = useState<string>('appointment_set');
  const [searchQuery, setSearchQuery] = useState('');

  // Fetch all configurations
  const { data: configData, isLoading: loadingConfigs } = useQuery({
    queryKey: ['repcard-metric-configs'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/metrics/config');
      if (!res.ok) throw new Error('Failed to fetch metric configs');
      return res.json();
    }
  });

  // Fetch discovered dispositions
  const { data: discoveryData, isLoading: loadingDiscovery } = useQuery({
    queryKey: ['repcard-dispositions'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/metrics/config/discover');
      if (!res.ok) throw new Error('Failed to discover dispositions');
      return res.json();
    }
  });

  const configs: MetricConfig[] = configData?.configs || [];
  const dispositions = discoveryData?.dispositions || [];
  const statusCategories = discoveryData?.statusCategories || [];

  // Filter configs by selected metric
  const metricConfigs = configs.filter(c => c.metric_name === selectedMetric);

  // Save configuration mutation
  const saveMutation = useMutation({
    mutationFn: async (config: Omit<MetricConfig, 'id' | 'created_at' | 'updated_at'>) => {
      const res = await fetch('/api/repcard/metrics/config', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(config)
      });
      if (!res.ok) throw new Error('Failed to save configuration');
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['repcard-metric-configs'] });
      toast.success('Configuration saved successfully');
    },
    onError: (error: Error) => {
      toast.error(`Failed to save: ${error.message}`);
    }
  });

  // Delete configuration mutation
  const deleteMutation = useMutation({
    mutationFn: async (id: string) => {
      const res = await fetch(`/api/repcard/metrics/config?id=${id}`, {
        method: 'DELETE'
      });
      if (!res.ok) throw new Error('Failed to delete configuration');
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['repcard-metric-configs'] });
      toast.success('Configuration deleted successfully');
    },
    onError: (error: Error) => {
      toast.error(`Failed to delete: ${error.message}`);
    }
  });

  const handleToggleConfig = (disposition: string | null, statusCategory: string | null, currentIncluded: boolean) => {
    const existingConfig = metricConfigs.find(
      c => c.disposition_pattern === disposition && c.status_category === statusCategory
    );

    if (existingConfig) {
      // Update existing
      saveMutation.mutate({
        ...existingConfig,
        is_included: !currentIncluded
      });
    } else {
      // Create new
      saveMutation.mutate({
        metric_name: selectedMetric,
        disposition_pattern: disposition,
        status_category: statusCategory,
        is_included: !currentIncluded,
        priority: 0
      });
    }
  };

  const filteredDispositions = dispositions.filter(d => 
    !searchQuery || d.value.toLowerCase().includes(searchQuery.toLowerCase())
  );

  const filteredStatusCategories = statusCategories.filter(s =>
    !searchQuery || s.value.toLowerCase().includes(searchQuery.toLowerCase())
  );

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold">RepCard Metric Configuration</h2>
        <p className="text-muted-foreground">
          Configure which dispositions and statuses count toward each metric. Changes apply immediately to all leaderboards.
        </p>
      </div>

      <Tabs defaultValue="discover" className="w-full">
        <TabsList>
          <TabsTrigger value="discover">Discover Values</TabsTrigger>
          <TabsTrigger value="configure">Configure Metrics</TabsTrigger>
        </TabsList>

        {/* Discovery Tab */}
        <TabsContent value="discover" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Discovered Dispositions & Statuses</CardTitle>
              <CardDescription>
                All unique values found in your database. Use these to configure metrics.
              </CardDescription>
            </CardHeader>
            <CardContent>
              {loadingDiscovery ? (
                <div className="flex items-center justify-center py-8">
                  <Loader2 className="w-6 h-6 animate-spin text-muted-foreground" />
                  <span className="ml-2 text-muted-foreground">Discovering values...</span>
                </div>
              ) : (
                <div className="space-y-6">
                  <div>
                    <div className="flex items-center justify-between mb-4">
                      <Label>Search</Label>
                      <div className="relative w-64">
                        <Search className="absolute left-2 top-2.5 h-4 w-4 text-muted-foreground" />
                        <Input
                          placeholder="Search dispositions..."
                          value={searchQuery}
                          onChange={(e) => setSearchQuery(e.target.value)}
                          className="pl-8"
                        />
                      </div>
                    </div>
                  </div>

                  <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
                    {/* Dispositions */}
                    <div>
                      <h3 className="font-semibold mb-3">Dispositions ({dispositions.length})</h3>
                      <div className="border rounded-lg max-h-96 overflow-y-auto">
                        <Table>
                          <TableHeader>
                            <TableRow>
                              <TableHead>Value</TableHead>
                              <TableHead className="text-right">Count</TableHead>
                            </TableRow>
                          </TableHeader>
                          <TableBody>
                            {filteredDispositions.length === 0 ? (
                              <TableRow>
                                <TableCell colSpan={2} className="text-center text-muted-foreground">
                                  {searchQuery ? 'No matches' : 'No dispositions found'}
                                </TableCell>
                              </TableRow>
                            ) : (
                              filteredDispositions.map((d: any) => (
                                <TableRow key={d.value}>
                                  <TableCell className="font-mono text-sm">{d.value || '(null)'}</TableCell>
                                  <TableCell className="text-right">
                                    <Badge variant="secondary">{d.count}</Badge>
                                  </TableCell>
                                </TableRow>
                              ))
                            )}
                          </TableBody>
                        </Table>
                      </div>
                    </div>

                    {/* Status Categories */}
                    <div>
                      <h3 className="font-semibold mb-3">Status Categories ({statusCategories.length})</h3>
                      <div className="border rounded-lg max-h-96 overflow-y-auto">
                        <Table>
                          <TableHeader>
                            <TableRow>
                              <TableHead>Value</TableHead>
                              <TableHead className="text-right">Count</TableHead>
                            </TableRow>
                          </TableHeader>
                          <TableBody>
                            {filteredStatusCategories.length === 0 ? (
                              <TableRow>
                                <TableCell colSpan={2} className="text-center text-muted-foreground">
                                  {searchQuery ? 'No matches' : 'No status categories found'}
                                </TableCell>
                              </TableRow>
                            ) : (
                              filteredStatusCategories.map((s: any) => (
                                <TableRow key={s.value}>
                                  <TableCell className="font-mono text-sm">{s.value || '(null)'}</TableCell>
                                  <TableCell className="text-right">
                                    <Badge variant="secondary">{s.count}</Badge>
                                  </TableCell>
                                </TableRow>
                              ))
                            )}
                          </TableBody>
                        </Table>
                      </div>
                    </div>
                  </div>
                </div>
              )}
            </CardContent>
          </Card>
        </TabsContent>

        {/* Configuration Tab */}
        <TabsContent value="configure" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Configure Metrics</CardTitle>
              <CardDescription>
                Select a metric and configure which dispositions/statuses count toward it.
              </CardDescription>
            </CardHeader>
            <CardContent>
              {loadingConfigs ? (
                <div className="flex items-center justify-center py-8">
                  <Loader2 className="w-6 h-6 animate-spin text-muted-foreground" />
                  <span className="ml-2 text-muted-foreground">Loading configurations...</span>
                </div>
              ) : (
                <div className="space-y-6">
                  {/* Metric Selector */}
                  <div>
                    <Label>Select Metric</Label>
                    <div className="grid grid-cols-2 md:grid-cols-4 gap-2 mt-2">
                      {Object.entries(METRIC_DISPLAY_NAMES).map(([key, name]) => (
                        <Button
                          key={key}
                          variant={selectedMetric === key ? 'default' : 'outline'}
                          onClick={() => setSelectedMetric(key)}
                          className="justify-start"
                        >
                          {name}
                        </Button>
                      ))}
                    </div>
                  </div>

                  {/* Current Configuration */}
                  <div>
                    <Label>Current Configuration for {METRIC_DISPLAY_NAMES[selectedMetric]}</Label>
                    <div className="border rounded-lg mt-2">
                      <Table>
                        <TableHeader>
                          <TableRow>
                            <TableHead>Disposition Pattern</TableHead>
                            <TableHead>Status Category</TableHead>
                            <TableHead>Included</TableHead>
                            <TableHead>Priority</TableHead>
                            <TableHead>Actions</TableHead>
                          </TableRow>
                        </TableHeader>
                        <TableBody>
                          {metricConfigs.length === 0 ? (
                            <TableRow>
                              <TableCell colSpan={5} className="text-center text-muted-foreground">
                                No configurations found. Add one below.
                              </TableCell>
                            </TableRow>
                          ) : (
                            metricConfigs.map((config) => (
                              <TableRow key={config.id}>
                                <TableCell className="font-mono text-sm">
                                  {config.disposition_pattern || '(any)'}
                                </TableCell>
                                <TableCell className="font-mono text-sm">
                                  {config.status_category || '(any)'}
                                </TableCell>
                                <TableCell>
                                  {config.is_included ? (
                                    <Badge variant="default" className="bg-green-500">
                                      <CheckCircle2 className="w-3 h-3 mr-1" />
                                      Included
                                    </Badge>
                                  ) : (
                                    <Badge variant="destructive">
                                      <XCircle className="w-3 h-3 mr-1" />
                                      Excluded
                                    </Badge>
                                  )}
                                </TableCell>
                                <TableCell>{config.priority}</TableCell>
                                <TableCell>
                                  <Button
                                    variant="ghost"
                                    size="sm"
                                    onClick={() => deleteMutation.mutate(config.id)}
                                  >
                                    Delete
                                  </Button>
                                </TableCell>
                              </TableRow>
                            ))
                          )}
                        </TableBody>
                      </Table>
                    </div>
                  </div>

                  {/* Quick Configuration by Disposition */}
                  <div>
                    <Label>Quick Configure by Disposition</Label>
                    <div className="border rounded-lg mt-2 max-h-64 overflow-y-auto">
                      <Table>
                        <TableHeader>
                          <TableRow>
                            <TableHead>Disposition</TableHead>
                            <TableHead className="text-right">Count</TableHead>
                            <TableHead className="text-right">Include?</TableHead>
                          </TableRow>
                        </TableHeader>
                        <TableBody>
                          {filteredDispositions.slice(0, 20).map((d: any) => {
                            const config = metricConfigs.find(
                              c => c.disposition_pattern === d.value && !c.status_category
                            );
                            const isIncluded = config?.is_included ?? false;
                            
                            return (
                              <TableRow key={d.value}>
                                <TableCell className="font-mono text-sm">{d.value || '(null)'}</TableCell>
                                <TableCell className="text-right">
                                  <Badge variant="secondary">{d.count}</Badge>
                                </TableCell>
                                <TableCell className="text-right">
                                  <Checkbox
                                    checked={isIncluded}
                                    onCheckedChange={() => handleToggleConfig(d.value, null, isIncluded)}
                                  />
                                </TableCell>
                              </TableRow>
                            );
                          })}
                        </TableBody>
                      </Table>
                    </div>
                  </div>

                  {/* Quick Configuration by Status Category */}
                  <div>
                    <Label>Quick Configure by Status Category</Label>
                    <div className="border rounded-lg mt-2">
                      <Table>
                        <TableHeader>
                          <TableRow>
                            <TableHead>Status Category</TableHead>
                            <TableHead className="text-right">Count</TableHead>
                            <TableHead className="text-right">Include?</TableHead>
                          </TableRow>
                        </TableHeader>
                        <TableBody>
                          {filteredStatusCategories.map((s: any) => {
                            const config = metricConfigs.find(
                              c => !c.disposition_pattern && c.status_category === s.value
                            );
                            const isIncluded = config?.is_included ?? false;
                            
                            return (
                              <TableRow key={s.value}>
                                <TableCell className="font-mono text-sm">{s.value || '(null)'}</TableCell>
                                <TableCell className="text-right">
                                  <Badge variant="secondary">{s.count}</Badge>
                                </TableCell>
                                <TableCell className="text-right">
                                  <Checkbox
                                    checked={isIncluded}
                                    onCheckedChange={() => handleToggleConfig(null, s.value, isIncluded)}
                                  />
                                </TableCell>
                              </TableRow>
                            );
                          })}
                        </TableBody>
                      </Table>
                    </div>
                  </div>
                </div>
              )}
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}
