'use client';

import { useState } from 'react';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { Card, CardContent, CardHeader, CardTitle, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Textarea } from '@/components/ui/textarea';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import { Checkbox } from '@/components/ui/checkbox';
import { toast } from 'sonner';
import { Plus, Pencil, Trash2, Save, X } from 'lucide-react';

interface LeaderboardConfig {
  id: string;
  name: string;
  description?: string;
  leaderboard_type: string;
  enabled_metrics: string[];
  rank_by_metric: string;
  display_order: number;
  date_range_default: string;
  roles?: string[];
  office_ids?: number[];
  is_default: boolean;
  enabled: boolean;
}

interface AnalyticsConfig {
  id: string;
  name: string;
  description?: string;
  widget_type: string;
  metric_type: string;
  date_range_default: string;
  refresh_interval: number;
  enabled: boolean;
  display_order: number;
  roles?: string[];
  office_ids?: number[];
  config?: Record<string, any>;
}

interface MetricDefinition {
  id: string;
  metric_key: string;
  display_name: string;
  description?: string;
  category: string;
  data_source: string;
  unit?: string;
  format: string;
  aggregation_type: string;
  enabled: boolean;
  leaderboard_supported: boolean;
  analytics_supported: boolean;
}

export function RepCardConfigTab() {
  const queryClient = useQueryClient();
  const [editingLeaderboard, setEditingLeaderboard] = useState<string | null>(null);
  const [editingAnalytics, setEditingAnalytics] = useState<string | null>(null);
  const [newLeaderboard, setNewLeaderboard] = useState(false);
  const [newAnalytics, setNewAnalytics] = useState(false);

  // Fetch leaderboard configurations
  const { data: leaderboardData, isLoading: loadingLeaderboards } = useQuery({
    queryKey: ['repcard-leaderboard-configs'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/settings/leaderboards');
      if (!res.ok) throw new Error('Failed to fetch leaderboard configs');
      return res.json();
    }
  });

  // Fetch analytics configurations
  const { data: analyticsData, isLoading: loadingAnalytics } = useQuery({
    queryKey: ['repcard-analytics-configs'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/settings/analytics');
      if (!res.ok) throw new Error('Failed to fetch analytics configs');
      return res.json();
    }
  });

  // Fetch metric definitions
  const { data: metricsData, isLoading: loadingMetrics } = useQuery({
    queryKey: ['repcard-metrics'],
    queryFn: async () => {
      const res = await fetch('/api/repcard/settings/metrics');
      if (!res.ok) throw new Error('Failed to fetch metrics');
      return res.json();
    }
  });

  const leaderboards = leaderboardData?.configs || [];
  const analytics = analyticsData?.configs || [];
  const metrics = metricsData?.metrics || [];

  // Leaderboard mutations
  const createLeaderboardMutation = useMutation({
    mutationFn: async (config: Partial<LeaderboardConfig>) => {
      const res = await fetch('/api/repcard/settings/leaderboards', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(config)
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.error || 'Failed to create leaderboard');
      }
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['repcard-leaderboard-configs']);
      setNewLeaderboard(false);
      toast.success('Leaderboard configuration created');
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  const updateLeaderboardMutation = useMutation({
    mutationFn: async ({ id, ...updates }: { id: string } & Partial<LeaderboardConfig>) => {
      const res = await fetch('/api/repcard/settings/leaderboards', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ id, ...updates })
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.error || 'Failed to update leaderboard');
      }
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['repcard-leaderboard-configs']);
      setEditingLeaderboard(null);
      toast.success('Leaderboard configuration updated');
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  const deleteLeaderboardMutation = useMutation({
    mutationFn: async (id: string) => {
      const res = await fetch(`/api/repcard/settings/leaderboards?id=${id}`, {
        method: 'DELETE'
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.error || 'Failed to delete leaderboard');
      }
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['repcard-leaderboard-configs']);
      toast.success('Leaderboard configuration deleted');
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  // Analytics mutations
  const createAnalyticsMutation = useMutation({
    mutationFn: async (config: Partial<AnalyticsConfig>) => {
      const res = await fetch('/api/repcard/settings/analytics', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(config)
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.error || 'Failed to create analytics widget');
      }
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['repcard-analytics-configs']);
      setNewAnalytics(false);
      toast.success('Analytics widget created');
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  const updateAnalyticsMutation = useMutation({
    mutationFn: async ({ id, ...updates }: { id: string } & Partial<AnalyticsConfig>) => {
      const res = await fetch('/api/repcard/settings/analytics', {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ id, ...updates })
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.error || 'Failed to update analytics widget');
      }
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['repcard-analytics-configs']);
      setEditingAnalytics(null);
      toast.success('Analytics widget updated');
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  const deleteAnalyticsMutation = useMutation({
    mutationFn: async (id: string) => {
      const res = await fetch(`/api/repcard/settings/analytics?id=${id}`, {
        method: 'DELETE'
      });
      if (!res.ok) {
        const error = await res.json();
        throw new Error(error.error || 'Failed to delete analytics widget');
      }
      return res.json();
    },
    onSuccess: () => {
      queryClient.invalidateQueries(['repcard-analytics-configs']);
      toast.success('Analytics widget deleted');
    },
    onError: (error: Error) => {
      toast.error(error.message);
    }
  });

  const availableMetrics = metrics.filter((m: MetricDefinition) => m.enabled);
  const leaderboardMetrics = availableMetrics.filter((m: MetricDefinition) => m.leaderboard_supported);
  const analyticsMetrics = availableMetrics.filter((m: MetricDefinition) => m.analytics_supported);

  return (
    <div className="space-y-6">
      <div>
        <h2 className="text-2xl font-bold">RepCard Configuration</h2>
        <p className="text-muted-foreground">
          Configure leaderboards and analytics widgets for RepCard metrics
        </p>
      </div>

      <Tabs defaultValue="leaderboards" className="w-full">
        <TabsList>
          <TabsTrigger value="leaderboards">Leaderboards</TabsTrigger>
          <TabsTrigger value="analytics">Analytics Widgets</TabsTrigger>
          <TabsTrigger value="metrics">Available Metrics</TabsTrigger>
        </TabsList>

        <TabsContent value="leaderboards" className="space-y-4">
          <div className="flex justify-between items-center">
            <h3 className="text-lg font-semibold">Leaderboard Configurations</h3>
            <Button onClick={() => setNewLeaderboard(true)}>
              <Plus className="mr-2 h-4 w-4" />
              New Leaderboard
            </Button>
          </div>

          {loadingLeaderboards ? (
            <div className="text-center py-8 text-muted-foreground">Loading...</div>
          ) : leaderboards.length === 0 ? (
            <Card>
              <CardContent className="py-8 text-center text-muted-foreground">
                No leaderboard configurations found. Create one to get started.
              </CardContent>
            </Card>
          ) : (
            <div className="space-y-4">
              {leaderboards.map((config: LeaderboardConfig) => (
                <LeaderboardConfigCard
                  key={config.id}
                  config={config}
                  metrics={leaderboardMetrics}
                  isEditing={editingLeaderboard === config.id}
                  onEdit={() => setEditingLeaderboard(config.id)}
                  onCancel={() => setEditingLeaderboard(null)}
                  onSave={(updates) => updateLeaderboardMutation.mutate({ id: config.id, ...updates })}
                  onDelete={() => deleteLeaderboardMutation.mutate(config.id)}
                />
              ))}
            </div>
          )}

          {newLeaderboard && (
            <Card>
              <CardHeader>
                <CardTitle>New Leaderboard Configuration</CardTitle>
              </CardHeader>
              <CardContent>
                <LeaderboardConfigForm
                  metrics={leaderboardMetrics}
                  onSave={(config) => {
                    createLeaderboardMutation.mutate(config);
                  }}
                  onCancel={() => setNewLeaderboard(false)}
                />
              </CardContent>
            </Card>
          )}
        </TabsContent>

        <TabsContent value="analytics" className="space-y-4">
          <div className="flex justify-between items-center">
            <h3 className="text-lg font-semibold">Analytics Widgets</h3>
            <Button onClick={() => setNewAnalytics(true)}>
              <Plus className="mr-2 h-4 w-4" />
              New Widget
            </Button>
          </div>

          {loadingAnalytics ? (
            <div className="text-center py-8 text-muted-foreground">Loading...</div>
          ) : analytics.length === 0 ? (
            <Card>
              <CardContent className="py-8 text-center text-muted-foreground">
                No analytics widgets configured. Create one to get started.
              </CardContent>
            </Card>
          ) : (
            <div className="space-y-4">
              {analytics.map((config: AnalyticsConfig) => (
                <AnalyticsConfigCard
                  key={config.id}
                  config={config}
                  metrics={analyticsMetrics}
                  isEditing={editingAnalytics === config.id}
                  onEdit={() => setEditingAnalytics(config.id)}
                  onCancel={() => setEditingAnalytics(null)}
                  onSave={(updates) => updateAnalyticsMutation.mutate({ id: config.id, ...updates })}
                  onDelete={() => deleteAnalyticsMutation.mutate(config.id)}
                />
              ))}
            </div>
          )}

          {newAnalytics && (
            <Card>
              <CardHeader>
                <CardTitle>New Analytics Widget</CardTitle>
              </CardHeader>
              <CardContent>
                <AnalyticsConfigForm
                  metrics={analyticsMetrics}
                  onSave={(config) => {
                    createAnalyticsMutation.mutate(config);
                  }}
                  onCancel={() => setNewAnalytics(false)}
                />
              </CardContent>
            </Card>
          )}
        </TabsContent>

        <TabsContent value="metrics" className="space-y-4">
          <h3 className="text-lg font-semibold">Available Metrics</h3>
          {loadingMetrics ? (
            <div className="text-center py-8 text-muted-foreground">Loading...</div>
          ) : (
            <MetricsList metrics={metrics} />
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
}

// Helper components
function LeaderboardConfigCard({
  config,
  metrics,
  isEditing,
  onEdit,
  onCancel,
  onSave,
  onDelete
}: {
  config: LeaderboardConfig;
  metrics: MetricDefinition[];
  isEditing: boolean;
  onEdit: () => void;
  onCancel: () => void;
  onSave: (updates: Partial<LeaderboardConfig>) => void;
  onDelete: () => void;
}) {
  const [formData, setFormData] = useState(config);

  if (isEditing) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Edit Leaderboard</CardTitle>
        </CardHeader>
        <CardContent>
          <LeaderboardConfigForm
            config={formData}
            metrics={metrics}
            onSave={(updates) => {
              onSave(updates);
              setFormData({ ...formData, ...updates });
            }}
            onCancel={onCancel}
          />
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex justify-between items-start">
          <div>
            <CardTitle className="flex items-center gap-2">
              {config.name}
              {config.is_default && (
                <span className="text-xs bg-primary text-primary-foreground px-2 py-1 rounded">Default</span>
              )}
              {!config.enabled && (
                <span className="text-xs bg-muted px-2 py-1 rounded">Disabled</span>
              )}
            </CardTitle>
            <CardDescription>{config.description || 'No description'}</CardDescription>
          </div>
          <div className="flex gap-2">
            <Button variant="outline" size="sm" onClick={onEdit}>
              <Pencil className="h-4 w-4" />
            </Button>
            {!config.is_default && (
              <Button variant="outline" size="sm" onClick={onDelete}>
                <Trash2 className="h-4 w-4" />
              </Button>
            )}
          </div>
        </div>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div>
            <span className="font-medium">Type:</span> {config.leaderboard_type}
          </div>
          <div>
            <span className="font-medium">Rank By:</span> {config.rank_by_metric}
          </div>
          <div>
            <span className="font-medium">Default Range:</span> {config.date_range_default}
          </div>
          <div>
            <span className="font-medium">Metrics:</span> {config.enabled_metrics.length} selected
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

function LeaderboardConfigForm({
  config,
  metrics,
  onSave,
  onCancel
}: {
  config?: Partial<LeaderboardConfig>;
  metrics: MetricDefinition[];
  onSave: (config: Partial<LeaderboardConfig>) => void;
  onCancel: () => void;
}) {
  const [formData, setFormData] = useState<Partial<LeaderboardConfig>>({
    name: '',
    description: '',
    leaderboard_type: 'd2d',
    enabled_metrics: [],
    rank_by_metric: '',
    display_order: 0,
    date_range_default: 'month',
    roles: [],
    office_ids: [],
    is_default: false,
    enabled: true,
    ...config
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (!formData.name || !formData.leaderboard_type || !formData.rank_by_metric || formData.enabled_metrics?.length === 0) {
      toast.error('Please fill in all required fields');
      return;
    }
    onSave(formData);
  };

  const toggleMetric = (metricKey: string) => {
    const current = formData.enabled_metrics || [];
    const updated = current.includes(metricKey)
      ? current.filter(m => m !== metricKey)
      : [...current, metricKey];
    setFormData({ ...formData, enabled_metrics: updated });
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      <div className="grid grid-cols-2 gap-4">
        <div>
          <Label htmlFor="name">Name *</Label>
          <Input
            id="name"
            value={formData.name}
            onChange={(e) => setFormData({ ...formData, name: e.target.value })}
            required
          />
        </div>
        <div>
          <Label htmlFor="leaderboard_type">Type *</Label>
          <Select
            value={formData.leaderboard_type}
            onValueChange={(value) => setFormData({ ...formData, leaderboard_type: value })}
          >
            <SelectTrigger>
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="d2d">D2D</SelectItem>
              <SelectItem value="overview">Overview</SelectItem>
              <SelectItem value="engagements">Engagements</SelectItem>
              <SelectItem value="recruiting">Recruiting</SelectItem>
              <SelectItem value="custom">Custom</SelectItem>
            </SelectContent>
          </Select>
        </div>
      </div>

      <div>
        <Label htmlFor="description">Description</Label>
        <Textarea
          id="description"
          value={formData.description || ''}
          onChange={(e) => setFormData({ ...formData, description: e.target.value })}
        />
      </div>

      <div>
        <Label htmlFor="rank_by_metric">Rank By *</Label>
        <Select
          value={formData.rank_by_metric}
          onValueChange={(value) => setFormData({ ...formData, rank_by_metric: value })}
        >
          <SelectTrigger>
            <SelectValue placeholder="Select metric" />
          </SelectTrigger>
          <SelectContent>
            {metrics.map((metric) => (
              <SelectItem key={metric.metric_key} value={metric.metric_key}>
                {metric.display_name}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>
      </div>

      <div>
        <Label>Enabled Metrics *</Label>
        <div className="grid grid-cols-2 gap-2 mt-2 max-h-48 overflow-y-auto border rounded p-2">
          {metrics.map((metric) => (
            <div key={metric.metric_key} className="flex items-center space-x-2">
              <Checkbox
                checked={formData.enabled_metrics?.includes(metric.metric_key) || false}
                onCheckedChange={() => toggleMetric(metric.metric_key)}
              />
              <Label className="text-sm">{metric.display_name}</Label>
            </div>
          ))}
        </div>
      </div>

      <div className="grid grid-cols-2 gap-4">
        <div>
          <Label htmlFor="date_range_default">Default Date Range</Label>
          <Select
            value={formData.date_range_default}
            onValueChange={(value) => setFormData({ ...formData, date_range_default: value })}
          >
            <SelectTrigger>
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="today">Today</SelectItem>
              <SelectItem value="week">This Week</SelectItem>
              <SelectItem value="month">This Month</SelectItem>
              <SelectItem value="quarter">This Quarter</SelectItem>
              <SelectItem value="ytd">Year to Date</SelectItem>
              <SelectItem value="custom">Custom</SelectItem>
            </SelectContent>
          </Select>
        </div>
        <div>
          <Label htmlFor="display_order">Display Order</Label>
          <Input
            id="display_order"
            type="number"
            value={formData.display_order}
            onChange={(e) => setFormData({ ...formData, display_order: parseInt(e.target.value) || 0 })}
          />
        </div>
      </div>

      <div className="flex items-center space-x-4">
        <div className="flex items-center space-x-2">
          <Checkbox
            checked={formData.is_default || false}
            onCheckedChange={(checked) => setFormData({ ...formData, is_default: checked as boolean })}
          />
          <Label>Set as default</Label>
        </div>
        <div className="flex items-center space-x-2">
          <Checkbox
            checked={formData.enabled !== false}
            onCheckedChange={(checked) => setFormData({ ...formData, enabled: checked as boolean })}
          />
          <Label>Enabled</Label>
        </div>
      </div>

      <div className="flex gap-2">
        <Button type="submit">
          <Save className="mr-2 h-4 w-4" />
          Save
        </Button>
        <Button type="button" variant="outline" onClick={onCancel}>
          <X className="mr-2 h-4 w-4" />
          Cancel
        </Button>
      </div>
    </form>
  );
}

function AnalyticsConfigCard({
  config,
  metrics,
  isEditing,
  onEdit,
  onCancel,
  onSave,
  onDelete
}: {
  config: AnalyticsConfig;
  metrics: MetricDefinition[];
  isEditing: boolean;
  onEdit: () => void;
  onCancel: () => void;
  onSave: (updates: Partial<AnalyticsConfig>) => void;
  onDelete: () => void;
}) {
  if (isEditing) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Edit Analytics Widget</CardTitle>
        </CardHeader>
        <CardContent>
          <AnalyticsConfigForm
            config={config}
            metrics={metrics}
            onSave={onSave}
            onCancel={onCancel}
          />
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex justify-between items-start">
          <div>
            <CardTitle className="flex items-center gap-2">
              {config.name}
              {!config.enabled && (
                <span className="text-xs bg-muted px-2 py-1 rounded">Disabled</span>
              )}
            </CardTitle>
            <CardDescription>{config.description || 'No description'}</CardDescription>
          </div>
          <div className="flex gap-2">
            <Button variant="outline" size="sm" onClick={onEdit}>
              <Pencil className="h-4 w-4" />
            </Button>
            <Button variant="outline" size="sm" onClick={onDelete}>
              <Trash2 className="h-4 w-4" />
            </Button>
          </div>
        </div>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-2 gap-4 text-sm">
          <div>
            <span className="font-medium">Widget Type:</span> {config.widget_type}
          </div>
          <div>
            <span className="font-medium">Metric:</span> {config.metric_type}
          </div>
          <div>
            <span className="font-medium">Refresh Interval:</span> {config.refresh_interval}s
          </div>
          <div>
            <span className="font-medium">Default Range:</span> {config.date_range_default}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

function AnalyticsConfigForm({
  config,
  metrics,
  onSave,
  onCancel
}: {
  config?: Partial<AnalyticsConfig>;
  metrics: MetricDefinition[];
  onSave: (config: Partial<AnalyticsConfig>) => void;
  onCancel: () => void;
}) {
  const [formData, setFormData] = useState<Partial<AnalyticsConfig>>({
    name: '',
    description: '',
    widget_type: 'card',
    metric_type: '',
    date_range_default: 'month',
    refresh_interval: 30,
    enabled: true,
    display_order: 0,
    roles: [],
    office_ids: [],
    ...config
  });

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (!formData.name || !formData.widget_type || !formData.metric_type) {
      toast.error('Please fill in all required fields');
      return;
    }
    onSave(formData);
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      <div className="grid grid-cols-2 gap-4">
        <div>
          <Label htmlFor="name">Name *</Label>
          <Input
            id="name"
            value={formData.name}
            onChange={(e) => setFormData({ ...formData, name: e.target.value })}
            required
          />
        </div>
        <div>
          <Label htmlFor="widget_type">Widget Type *</Label>
          <Select
            value={formData.widget_type}
            onValueChange={(value) => setFormData({ ...formData, widget_type: value })}
          >
            <SelectTrigger>
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="card">Card</SelectItem>
              <SelectItem value="chart">Chart</SelectItem>
              <SelectItem value="table">Table</SelectItem>
              <SelectItem value="leaderboard">Leaderboard</SelectItem>
            </SelectContent>
          </Select>
        </div>
      </div>

      <div>
        <Label htmlFor="description">Description</Label>
        <Textarea
          id="description"
          value={formData.description || ''}
          onChange={(e) => setFormData({ ...formData, description: e.target.value })}
        />
      </div>

      <div>
        <Label htmlFor="metric_type">Metric *</Label>
        <Select
          value={formData.metric_type}
          onValueChange={(value) => setFormData({ ...formData, metric_type: value })}
        >
          <SelectTrigger>
            <SelectValue placeholder="Select metric" />
          </SelectTrigger>
          <SelectContent>
            {metrics.map((metric) => (
              <SelectItem key={metric.metric_key} value={metric.metric_key}>
                {metric.display_name}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>
      </div>

      <div className="grid grid-cols-2 gap-4">
        <div>
          <Label htmlFor="date_range_default">Default Date Range</Label>
          <Select
            value={formData.date_range_default}
            onValueChange={(value) => setFormData({ ...formData, date_range_default: value })}
          >
            <SelectTrigger>
              <SelectValue />
            </SelectTrigger>
            <SelectContent>
              <SelectItem value="today">Today</SelectItem>
              <SelectItem value="week">This Week</SelectItem>
              <SelectItem value="month">This Month</SelectItem>
              <SelectItem value="quarter">This Quarter</SelectItem>
              <SelectItem value="ytd">Year to Date</SelectItem>
            </SelectContent>
          </Select>
        </div>
        <div>
          <Label htmlFor="refresh_interval">Refresh Interval (seconds)</Label>
          <Input
            id="refresh_interval"
            type="number"
            value={formData.refresh_interval}
            onChange={(e) => setFormData({ ...formData, refresh_interval: parseInt(e.target.value) || 30 })}
          />
        </div>
      </div>

      <div className="flex items-center space-x-2">
        <Checkbox
          checked={formData.enabled !== false}
          onCheckedChange={(checked) => setFormData({ ...formData, enabled: checked as boolean })}
        />
        <Label>Enabled</Label>
      </div>

      <div className="flex gap-2">
        <Button type="submit">
          <Save className="mr-2 h-4 w-4" />
          Save
        </Button>
        <Button type="button" variant="outline" onClick={onCancel}>
          <X className="mr-2 h-4 w-4" />
          Cancel
        </Button>
      </div>
    </form>
  );
}

function MetricsList({ metrics }: { metrics: MetricDefinition[] }) {
  const grouped = metrics.reduce((acc, metric) => {
    if (!acc[metric.category]) {
      acc[metric.category] = [];
    }
    acc[metric.category].push(metric);
    return acc;
  }, {} as Record<string, MetricDefinition[]>);

  return (
    <div className="space-y-6">
      {Object.entries(grouped).map(([category, categoryMetrics]) => (
        <Card key={category}>
          <CardHeader>
            <CardTitle className="capitalize">{category} Metrics</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-2 gap-4">
              {categoryMetrics.map((metric) => (
                <div key={metric.metric_key} className="flex items-start justify-between p-3 border rounded">
                  <div>
                    <div className="font-medium">{metric.display_name}</div>
                    {metric.description && (
                      <div className="text-sm text-muted-foreground">{metric.description}</div>
                    )}
                    <div className="text-xs text-muted-foreground mt-1">
                      {metric.data_source} • {metric.format} • {metric.aggregation_type}
                    </div>
                  </div>
                  <div className="flex flex-col gap-1 text-xs">
                    {metric.leaderboard_supported && (
                      <span className="bg-blue-100 text-blue-800 px-2 py-1 rounded">Leaderboard</span>
                    )}
                    {metric.analytics_supported && (
                      <span className="bg-green-100 text-green-800 px-2 py-1 rounded">Analytics</span>
                    )}
                    {!metric.enabled && (
                      <span className="bg-gray-100 text-gray-800 px-2 py-1 rounded">Disabled</span>
                    )}
                  </div>
                </div>
              ))}
            </div>
          </CardContent>
        </Card>
      ))}
    </div>
  );
}

