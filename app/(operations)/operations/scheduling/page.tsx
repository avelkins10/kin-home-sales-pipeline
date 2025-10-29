'use client';

import { useEffect, useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useRouter, useSearchParams } from 'next/navigation';
import { FieldTrackingDashboard } from '@/components/operations';
import type { FieldTrackingDashboardData } from '@/lib/types/operations';
import type { FieldTrackingFilterState } from '@/components/operations/FieldTrackingFilterBar';
import { toast } from 'sonner';

export default function SchedulingPage() {
  const router = useRouter();
  const searchParams = useSearchParams();

  // Initialize filters from URL params
  const [filters, setFilters] = useState<FieldTrackingFilterState>({
    search: searchParams.get('search') || '',
    status: (searchParams.get('status') as any) || 'all',
    taskType: (searchParams.get('taskType') as any) || 'all',
    crewMember: searchParams.get('crewMember') || 'all',
    dateFilter: (searchParams.get('dateFilter') as any) || 'today', // Show today's tasks by default
    sortBy: (searchParams.get('sortBy') as any) || 'scheduled_start',
    sortOrder: (searchParams.get('sortOrder') as any) || 'desc', // Changed from 'asc' to 'desc' to show newest tasks first
  });

  // Update URL when filters change
  useEffect(() => {
    const params = new URLSearchParams();
    Object.entries(filters).forEach(([key, value]) => {
      if (value && value !== 'all' && value !== '') {
        params.set(key, value.toString());
      }
    });
    const newUrl = params.toString() ? `/operations/scheduling?${params.toString()}` : '/operations/scheduling';
    router.replace(newUrl, { scroll: false });
  }, [filters, router]);

  // Build API URL with filter params
  const buildApiUrl = () => {
    const params = new URLSearchParams();
    Object.entries(filters).forEach(([key, value]) => {
      if (value && value !== '') {
        params.set(key, value.toString());
      }
    });
    return `/api/operations/field-tracking/dashboard?${params.toString()}`;
  };

  // Fetch field tracking dashboard data with filters
  const { data, isLoading, error, refetch } = useQuery<any>({
    queryKey: ['field-tracking-dashboard', filters],
    queryFn: async () => {
      const response = await fetch(buildApiUrl());
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
          taskCounts: {
            total: 0,
            byStatus: {},
            byType: {},
          },
        }}
        filters={filters}
        onFilterChange={setFilters}
        isLoading={isLoading}
        onRefresh={() => refetch()}
      />
    </div>
  );
}
