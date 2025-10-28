'use client';

import { useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import { FieldTrackingDashboard } from '@/components/operations';
import type { FieldTrackingDashboardData } from '@/lib/types/operations';
import { toast } from 'sonner';

export default function SchedulingPage() {
  // Fetch field tracking dashboard data
  const { data, isLoading, error, refetch } = useQuery<FieldTrackingDashboardData>({
    queryKey: ['field-tracking-dashboard'],
    queryFn: async () => {
      const response = await fetch('/api/operations/field-tracking/dashboard');
      if (!response.ok) {
        throw new Error('Failed to load field tracking data');
      }
      return response.json();
    },
    refetchInterval: 30000, // Refetch every 30 seconds for real-time updates
  });

  // Handle error state (wrapped in useEffect to prevent multiple toasts)
  useEffect(() => {
    if (error) {
      toast.error('Failed to load field operations dashboard');
    }
  }, [error]);

  return (
    <div className="space-y-4 mobile:space-y-6">
      <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
        <div>
          <h1 className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
            Field Operations
          </h1>
          <p className="text-sm mobile:text-base text-gray-600 mt-0.5">
            Track crews, tasks, and real-time field status
          </p>
        </div>
      </div>

      <FieldTrackingDashboard
        data={data || {
          tasks: [],
          entities: [],
          events: [],
          metrics: {
            total_tasks: 0,
            in_progress: 0,
            completed_today: 0,
            delayed: 0,
            crews_active: 0,
            avg_completion_time: 0,
          },
        }}
        isLoading={isLoading}
        onRefresh={() => refetch()}
      />
    </div>
  );
}
