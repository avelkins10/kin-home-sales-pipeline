'use client';

import { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { RefreshCw, MapPin, List } from 'lucide-react';
import type { FieldTrackingDashboardData } from '@/lib/types/operations';
import { FieldTrackingTaskCard } from './FieldTrackingTaskCard';
import { FieldTrackingActivityFeed } from './FieldTrackingActivityFeed';
import { FieldTrackingDetailModal } from './FieldTrackingDetailModal';
import { Skeleton } from '@/components/ui/skeleton';

interface FieldTrackingDashboardProps {
  data: FieldTrackingDashboardData;
  isLoading: boolean;
  onRefresh: () => void;
}

export function FieldTrackingDashboard({ data, isLoading, onRefresh }: FieldTrackingDashboardProps) {
  const [viewMode, setViewMode] = useState<'list' | 'map'>('list');
  const [selectedTaskId, setSelectedTaskId] = useState<string | null>(null);

  if (isLoading) {
    return <FieldTrackingDashboardSkeleton />;
  }

  const { tasks, entities, events, metrics } = data;

  return (
    <div className="space-y-6">
      {/* Metrics Cards */}
      <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-4">
        <MetricCard
          label="Total Tasks"
          value={metrics.total_tasks}
          color="text-blue-600"
          bgColor="bg-blue-50"
        />
        <MetricCard
          label="In Progress"
          value={metrics.in_progress}
          color="text-yellow-600"
          bgColor="bg-yellow-50"
        />
        <MetricCard
          label="Completed Today"
          value={metrics.completed_today}
          color="text-green-600"
          bgColor="bg-green-50"
        />
        <MetricCard
          label="Delayed"
          value={metrics.delayed}
          color="text-red-600"
          bgColor="bg-red-50"
        />
        <MetricCard
          label="Crews Active"
          value={metrics.crews_active}
          color="text-purple-600"
          bgColor="bg-purple-50"
        />
        <MetricCard
          label="Avg Time"
          value={metrics.avg_completion_time ? `${Math.round(metrics.avg_completion_time)} min` : 'N/A'}
          color="text-gray-600"
          bgColor="bg-gray-50"
        />
      </div>

      {/* Header with View Toggle */}
      <div className="flex items-center justify-between">
        <h2 className="text-2xl font-bold">Field Operations</h2>
        <div className="flex gap-2">
          <Button
            variant={viewMode === 'list' ? 'default' : 'outline'}
            size="sm"
            onClick={() => setViewMode('list')}
          >
            <List className="mr-2 h-4 w-4" />
            List
          </Button>
          <Button
            variant={viewMode === 'map' ? 'default' : 'outline'}
            size="sm"
            onClick={() => setViewMode('map')}
            disabled
          >
            <MapPin className="mr-2 h-4 w-4" />
            Map (Coming Soon)
          </Button>
          <Button variant="outline" size="sm" onClick={onRefresh}>
            <RefreshCw className="mr-2 h-4 w-4" />
            Refresh
          </Button>
        </div>
      </div>

      {/* Main Content */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        {/* Task List */}
        <div className="lg:col-span-2 space-y-4">
          {tasks.length === 0 ? (
            <Card>
              <CardContent className="p-6 text-center text-gray-500">
                No field tasks found. Create a task to get started.
              </CardContent>
            </Card>
          ) : (
            tasks.map((task) => (
              <FieldTrackingTaskCard
                key={task.id}
                task={task}
                onClick={() => setSelectedTaskId(task.quickbase_project_id)}
              />
            ))
          )}
        </div>

        {/* Activity Feed Sidebar */}
        <div className="lg:col-span-1">
          <Card>
            <CardHeader>
              <CardTitle>Activity Feed</CardTitle>
            </CardHeader>
            <CardContent>
              <FieldTrackingActivityFeed events={events} limit={20} />
            </CardContent>
          </Card>
        </div>
      </div>

      {/* Detail Modal */}
      <FieldTrackingDetailModal
        taskId={selectedTaskId}
        onClose={() => setSelectedTaskId(null)}
      />
    </div>
  );
}

function MetricCard({ label, value, color, bgColor }: {
  label: string;
  value: string | number;
  color: string;
  bgColor: string;
}) {
  return (
    <Card className="hover:shadow-md transition-shadow">
      <CardContent className="p-4">
        <div className="text-sm font-medium text-gray-600">{label}</div>
        <div className={`text-2xl font-bold ${color} mt-1`}>{value}</div>
      </CardContent>
    </Card>
  );
}

function FieldTrackingDashboardSkeleton() {
  return (
    <div className="space-y-6">
      <div className="grid grid-cols-2 md:grid-cols-3 lg:grid-cols-6 gap-4">
        {Array.from({ length: 6 }).map((_, i) => (
          <Card key={i}>
            <CardContent className="p-4">
              <Skeleton className="h-4 w-20 mb-2" />
              <Skeleton className="h-8 w-16" />
            </CardContent>
          </Card>
        ))}
      </div>
      <Skeleton className="h-10 w-full" />
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
        <div className="lg:col-span-2 space-y-4">
          {Array.from({ length: 3 }).map((_, i) => (
            <Skeleton key={i} className="h-32 w-full" />
          ))}
        </div>
        <Skeleton className="h-96 w-full" />
      </div>
    </div>
  );
}

