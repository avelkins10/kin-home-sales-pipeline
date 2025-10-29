'use client';

import { useState } from 'react';
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { RefreshCw, MapPin, List, Search, Wrench, ClipboardCheck, Settings } from 'lucide-react';
import type { FieldTrackingDashboardData } from '@/lib/types/operations';
import { FieldTrackingTaskCard } from './FieldTrackingTaskCard';
import { FieldTrackingActivityFeed } from './FieldTrackingActivityFeed';
import { FieldTrackingDetailModal } from './FieldTrackingDetailModal';
import { FieldTrackingFilterBar, type FieldTrackingFilterState } from './FieldTrackingFilterBar';
import { FieldTrackingAlertBanners } from './FieldTrackingAlertBanners';
import { Skeleton } from '@/components/ui/skeleton';

interface FieldTrackingDashboardProps {
  data: any; // Extended FieldTrackingDashboardData with taskCounts
  filters: FieldTrackingFilterState;
  onFilterChange: (filters: FieldTrackingFilterState) => void;
  isLoading: boolean;
  onRefresh: () => void;
}

export function FieldTrackingDashboard({ data, filters, onFilterChange, isLoading, onRefresh }: FieldTrackingDashboardProps) {
  const [viewMode, setViewMode] = useState<'list' | 'map'>('list');
  const [selectedTaskId, setSelectedTaskId] = useState<string | null>(null);

  if (isLoading) {
    return <FieldTrackingDashboardSkeleton />;
  }

  const { tasks, entities, metrics, taskCounts } = data;

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

      {/* Filter Bar */}
      <FieldTrackingFilterBar
        filters={filters}
        onFilterChange={onFilterChange}
        crewMembers={entities}
        taskCounts={taskCounts}
      />

      {/* Alert Banners for Critical Tasks */}
      <FieldTrackingAlertBanners
        tasks={tasks}
        onTaskClick={(taskId) => setSelectedTaskId(taskId)}
      />

      {/* Header with Quick Filters and View Toggle */}
      <div className="flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4">
        <div className="flex items-center gap-3">
          <h2 className="text-2xl font-bold">Field Operations</h2>
          {/* Today/Tomorrow Quick Filters */}
          <div className="flex gap-1 border-l pl-3">
            <Button
              variant={filters.dateFilter === 'today' ? 'default' : 'outline'}
              size="sm"
              onClick={() => onFilterChange({ ...filters, dateFilter: 'today' })}
            >
              Today
              <Badge variant={filters.dateFilter === 'today' ? 'secondary' : 'outline'} className="ml-2">
                {tasks.filter((t: any) => {
                  if (!t.scheduled_start) return false;
                  const taskDate = new Date(t.scheduled_start);
                  const today = new Date();
                  return taskDate.getDate() === today.getDate() &&
                         taskDate.getMonth() === today.getMonth() &&
                         taskDate.getFullYear() === today.getFullYear();
                }).length}
              </Badge>
            </Button>
            <Button
              variant={filters.dateFilter === 'tomorrow' ? 'default' : 'outline'}
              size="sm"
              onClick={() => onFilterChange({ ...filters, dateFilter: 'tomorrow' })}
            >
              Tomorrow
              <Badge variant={filters.dateFilter === 'tomorrow' ? 'secondary' : 'outline'} className="ml-2">
                {tasks.filter((t: any) => {
                  if (!t.scheduled_start) return false;
                  const taskDate = new Date(t.scheduled_start);
                  const tomorrow = new Date();
                  tomorrow.setDate(tomorrow.getDate() + 1);
                  return taskDate.getDate() === tomorrow.getDate() &&
                         taskDate.getMonth() === tomorrow.getMonth() &&
                         taskDate.getFullYear() === tomorrow.getFullYear();
                }).length}
              </Badge>
            </Button>
          </div>
        </div>
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

      {/* Task Type Tabs */}
      <div className="border-b">
        <div className="flex gap-1 overflow-x-auto pb-px">
          <Button
            variant={filters.taskType === 'all' ? 'default' : 'ghost'}
            size="sm"
            onClick={() => onFilterChange({ ...filters, taskType: 'all' })}
            className="gap-2 whitespace-nowrap"
          >
            <List className="h-4 w-4" />
            All Tasks
            <Badge variant={filters.taskType === 'all' ? 'secondary' : 'outline'} className="ml-1">
              {taskCounts?.byType['all'] || 0}
            </Badge>
          </Button>
          <Button
            variant={filters.taskType === 'survey' ? 'default' : 'ghost'}
            size="sm"
            onClick={() => onFilterChange({ ...filters, taskType: 'survey' })}
            className="gap-2 whitespace-nowrap"
          >
            <Search className="h-4 w-4" />
            Surveys
            <Badge variant={filters.taskType === 'survey' ? 'secondary' : 'outline'} className="ml-1">
              {taskCounts?.byType['survey'] || 0}
            </Badge>
          </Button>
          <Button
            variant={filters.taskType === 'install' ? 'default' : 'ghost'}
            size="sm"
            onClick={() => onFilterChange({ ...filters, taskType: 'install' })}
            className="gap-2 whitespace-nowrap"
          >
            <Wrench className="h-4 w-4" />
            Installs
            <Badge variant={filters.taskType === 'install' ? 'secondary' : 'outline'} className="ml-1">
              {taskCounts?.byType['install'] || 0}
            </Badge>
          </Button>
          <Button
            variant={filters.taskType === 'inspection' ? 'default' : 'ghost'}
            size="sm"
            onClick={() => onFilterChange({ ...filters, taskType: 'inspection' })}
            className="gap-2 whitespace-nowrap"
          >
            <ClipboardCheck className="h-4 w-4" />
            Inspections
            <Badge variant={filters.taskType === 'inspection' ? 'secondary' : 'outline'} className="ml-1">
              {taskCounts?.byType['inspection'] || 0}
            </Badge>
          </Button>
          <Button
            variant={filters.taskType === 'service' ? 'default' : 'ghost'}
            size="sm"
            onClick={() => onFilterChange({ ...filters, taskType: 'service' })}
            className="gap-2 whitespace-nowrap"
          >
            <Settings className="h-4 w-4" />
            Service
            <Badge variant={filters.taskType === 'service' ? 'secondary' : 'outline'} className="ml-1">
              {taskCounts?.byType['service'] || 0}
            </Badge>
          </Button>
          <Button
            variant={filters.taskType === 'other' ? 'default' : 'ghost'}
            size="sm"
            onClick={() => onFilterChange({ ...filters, taskType: 'other' })}
            className="gap-2 whitespace-nowrap"
          >
            Other
            <Badge variant={filters.taskType === 'other' ? 'secondary' : 'outline'} className="ml-1">
              {taskCounts?.byType['other'] || 0}
            </Badge>
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
            tasks.map((task: any) => (
              <FieldTrackingTaskCard
                key={task.id}
                task={task}
                crewMembers={entities}
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
              <FieldTrackingActivityFeed 
                onTaskClick={(taskId) => setSelectedTaskId(taskId)}
              />
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

