'use client'

import { Suspense, useState, useEffect } from 'react';
import { useSession } from 'next-auth/react';
import { Skeleton } from '@/components/ui/skeleton';
import { syncUserTimezone } from '@/lib/utils/timezone';
import { isManagerRole } from '@/lib/utils/role-helpers';

// Skeleton components for loading states
function OperationsMetricsSkeleton() {
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
      {Array.from({ length: 4 }).map((_, i) => (
        <div key={i} className="p-6 border rounded-lg">
          <div className="flex items-center justify-between">
            <div className="space-y-2">
              <Skeleton className="h-4 w-20" />
              <Skeleton className="h-8 w-16" />
            </div>
            <Skeleton className="h-12 w-12 rounded-full" />
          </div>
        </div>
      ))}
    </div>
  );
}

function WorkOrdersSkeleton() {
  return (
    <div className="space-y-4">
      {Array.from({ length: 3 }).map((_, i) => (
        <div key={i} className="p-4 border rounded-lg">
          <div className="flex items-center justify-between">
            <div className="space-y-2">
              <Skeleton className="h-4 w-32" />
              <Skeleton className="h-3 w-48" />
            </div>
            <Skeleton className="h-6 w-16" />
          </div>
        </div>
      ))}
    </div>
  );
}

function InventoryStatusSkeleton() {
  return (
    <div className="p-6 border rounded-lg">
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {Array.from({ length: 4 }).map((_, i) => (
          <div key={i} className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
            <Skeleton className="h-9 w-9 rounded-lg" />
            <div className="flex-1">
              <Skeleton className="h-4 w-20 mb-2" />
              <Skeleton className="h-8 w-16" />
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

function CrewScheduleSkeleton() {
  return (
    <div className="p-6 border rounded-lg space-y-3">
      {Array.from({ length: 4 }).map((_, i) => (
        <div key={i} className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
          <Skeleton className="h-8 w-8 rounded-lg" />
          <div className="flex-1">
            <Skeleton className="h-4 w-16 mb-1" />
            <Skeleton className="h-3 w-32" />
          </div>
          <Skeleton className="h-6 w-20" />
        </div>
      ))}
    </div>
  );
}

function QualityControlSkeleton() {
  return (
    <div className="p-6 border rounded-lg">
      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
        {Array.from({ length: 6 }).map((_, i) => (
          <div key={i} className="p-4 bg-gray-50 rounded-lg">
            <div className="text-center">
              <Skeleton className="h-8 w-12 mx-auto mb-2" />
              <Skeleton className="h-12 w-12 mx-auto mb-2 rounded-lg" />
              <Skeleton className="h-4 w-20 mx-auto mb-1" />
              <Skeleton className="h-3 w-24 mx-auto" />
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}

export default function OperationsPage() {
  const { data: session, status } = useSession();

  // Sync user timezone on mount (must be before early returns)
  useEffect(() => {
    if (session?.user?.timezone) {
      syncUserTimezone(session.user.timezone);
    } else {
      // Fallback: log missing timezone data for debugging
      console.warn('User timezone not available in session, using browser default');
    }
  }, [session]);

  if (status === 'loading') {
    return (
      <div className="space-y-4 mobile:space-y-6">
        <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
          <div>
            <div className="h-8 w-48 bg-gray-200 rounded animate-pulse mb-2"></div>
            <div className="h-4 w-64 bg-gray-200 rounded animate-pulse"></div>
          </div>
        </div>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          {Array.from({ length: 4 }).map((_, i) => (
            <div key={i} className="p-6 border rounded-lg bg-white">
              <div className="flex items-center justify-between">
                <div className="space-y-2">
                  <div className="h-4 w-20 bg-gray-200 rounded animate-pulse"></div>
                  <div className="h-8 w-16 bg-gray-200 rounded animate-pulse"></div>
                </div>
                <div className="h-12 w-12 bg-gray-200 rounded-full animate-pulse"></div>
              </div>
            </div>
          ))}
        </div>
      </div>
    );
  }

  const isManager = isManagerRole(session.user.role);

  const getRoleDisplayName = (role: string) => {
    switch (role) {
      case 'operations_coordinator':
        return 'Operations Coordinator Dashboard';
      case 'operations_manager':
        return 'Operations Manager Dashboard';
      case 'office_leader':
        return 'Office Leader Dashboard';
      case 'regional':
        return 'Regional Manager Dashboard';
      case 'super_admin':
        return 'Super Admin Dashboard';
      default:
        return 'Operations Dashboard';
    }
  };

  return (
    <div className="space-y-4 mobile:space-y-6">
      {/* Page Header */}
      <div className="flex flex-col mobile:flex-row mobile:items-center mobile:justify-between gap-3 mobile:gap-0">
        <div>
          <h1 className="text-xl mobile:text-2xl ipad:text-3xl font-bold text-gray-900">
            Welcome back, {session.user.name}
          </h1>
          <p className="text-sm mobile:text-base text-gray-600 mt-0.5">
            {getRoleDisplayName(session.user.role)} -
            {isManager ? 'Team Operations Dashboard' : 'Your Operations Dashboard'}
          </p>
        </div>
      </div>

      {/* Operations Metrics */}
      <Suspense fallback={<OperationsMetricsSkeleton />}>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
          <div className="p-6 border rounded-lg bg-white">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Active Work Orders</p>
                <p className="text-2xl font-bold text-gray-900">24</p>
              </div>
              <div className="h-12 w-12 bg-blue-100 rounded-full flex items-center justify-center">
                <span className="text-blue-600 font-bold">24</span>
              </div>
            </div>
          </div>
          <div className="p-6 border rounded-lg bg-white">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Crews Scheduled</p>
                <p className="text-2xl font-bold text-gray-900">8</p>
              </div>
              <div className="h-12 w-12 bg-green-100 rounded-full flex items-center justify-center">
                <span className="text-green-600 font-bold">8</span>
              </div>
            </div>
          </div>
          <div className="p-6 border rounded-lg bg-white">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Quality Checks</p>
                <p className="text-2xl font-bold text-gray-900">12</p>
              </div>
              <div className="h-12 w-12 bg-yellow-100 rounded-full flex items-center justify-center">
                <span className="text-yellow-600 font-bold">12</span>
              </div>
            </div>
          </div>
          <div className="p-6 border rounded-lg bg-white">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm font-medium text-gray-600">Inventory Items</p>
                <p className="text-2xl font-bold text-gray-900">156</p>
              </div>
              <div className="h-12 w-12 bg-purple-100 rounded-full flex items-center justify-center">
                <span className="text-purple-600 font-bold">156</span>
              </div>
            </div>
          </div>
        </div>
      </Suspense>

      {/* Two Column Layout */}
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 mobile:gap-6">
        {/* Left Column - Work Orders */}
        <div className="space-y-4 mobile:space-y-6">
          <div className="p-6 border rounded-lg bg-white">
            <h3 className="text-lg font-semibold text-gray-900 mb-4">Recent Work Orders</h3>
            <Suspense fallback={<WorkOrdersSkeleton />}>
              <div className="space-y-3">
                <div className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                  <div>
                    <p className="font-medium text-gray-900">Installation - Project #1234</p>
                    <p className="text-sm text-gray-600">Scheduled for tomorrow</p>
                  </div>
                  <span className="px-2 py-1 text-xs font-medium bg-blue-100 text-blue-800 rounded-full">
                    Scheduled
                  </span>
                </div>
                <div className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                  <div>
                    <p className="font-medium text-gray-900">Maintenance - Project #1235</p>
                    <p className="text-sm text-gray-600">In progress</p>
                  </div>
                  <span className="px-2 py-1 text-xs font-medium bg-yellow-100 text-yellow-800 rounded-full">
                    In Progress
                  </span>
                </div>
                <div className="flex items-center justify-between p-3 bg-gray-50 rounded-lg">
                  <div>
                    <p className="font-medium text-gray-900">Quality Check - Project #1236</p>
                    <p className="text-sm text-gray-600">Completed today</p>
                  </div>
                  <span className="px-2 py-1 text-xs font-medium bg-green-100 text-green-800 rounded-full">
                    Completed
                  </span>
                </div>
              </div>
            </Suspense>
          </div>
        </div>

        {/* Right Column - Inventory & Scheduling */}
        <div className="space-y-4 mobile:space-y-6">
          <div className="p-6 border rounded-lg bg-white">
            <h3 className="text-lg font-semibold text-gray-900 mb-4">Inventory Status</h3>
            <Suspense fallback={<InventoryStatusSkeleton />}>
              <div className="space-y-3">
                <div className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
                  <div className="h-9 w-9 bg-green-100 rounded-lg flex items-center justify-center">
                    <span className="text-green-600 font-bold">✓</span>
                  </div>
                  <div className="flex-1">
                    <p className="font-medium text-gray-900">Solar Panels</p>
                    <p className="text-sm text-gray-600">45 in stock</p>
                  </div>
                </div>
                <div className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
                  <div className="h-9 w-9 bg-yellow-100 rounded-lg flex items-center justify-center">
                    <span className="text-yellow-600 font-bold">!</span>
                  </div>
                  <div className="flex-1">
                    <p className="font-medium text-gray-900">Inverters</p>
                    <p className="text-sm text-gray-600">Low stock - 3 remaining</p>
                  </div>
                </div>
                <div className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
                  <div className="h-9 w-9 bg-red-100 rounded-lg flex items-center justify-center">
                    <span className="text-red-600 font-bold">×</span>
                  </div>
                  <div className="flex-1">
                    <p className="font-medium text-gray-900">Mounting Hardware</p>
                    <p className="text-sm text-gray-600">Out of stock</p>
                  </div>
                </div>
              </div>
            </Suspense>
          </div>

          <div className="p-6 border rounded-lg bg-white">
            <h3 className="text-lg font-semibold text-gray-900 mb-4">Crew Schedule</h3>
            <Suspense fallback={<CrewScheduleSkeleton />}>
              <div className="space-y-3">
                <div className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
                  <div className="h-8 w-8 bg-blue-100 rounded-lg flex items-center justify-center">
                    <span className="text-blue-600 font-bold">A</span>
                  </div>
                  <div className="flex-1">
                    <p className="font-medium text-gray-900">Crew Alpha</p>
                    <p className="text-sm text-gray-600">Project #1234 - 8:00 AM</p>
                  </div>
                  <span className="px-2 py-1 text-xs font-medium bg-blue-100 text-blue-800 rounded-full">
                    On Schedule
                  </span>
                </div>
                <div className="flex items-center space-x-3 p-3 bg-gray-50 rounded-lg">
                  <div className="h-8 w-8 bg-green-100 rounded-lg flex items-center justify-center">
                    <span className="text-green-600 font-bold">B</span>
                  </div>
                  <div className="flex-1">
                    <p className="font-medium text-gray-900">Crew Beta</p>
                    <p className="text-sm text-gray-600">Project #1235 - 9:30 AM</p>
                  </div>
                  <span className="px-2 py-1 text-xs font-medium bg-green-100 text-green-800 rounded-full">
                    In Progress
                  </span>
                </div>
              </div>
            </Suspense>
          </div>
        </div>
      </div>

      {/* Quality Control Section */}
      <div className="p-6 border rounded-lg bg-white">
        <h3 className="text-lg font-semibold text-gray-900 mb-4">Quality Control Metrics</h3>
        <Suspense fallback={<QualityControlSkeleton />}>
          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
            <div className="p-4 bg-gray-50 rounded-lg text-center">
              <div className="text-2xl font-bold text-green-600 mb-2">98%</div>
              <div className="text-sm text-gray-600">Pass Rate</div>
            </div>
            <div className="p-4 bg-gray-50 rounded-lg text-center">
              <div className="text-2xl font-bold text-blue-600 mb-2">24</div>
              <div className="text-sm text-gray-600">Inspections Today</div>
            </div>
            <div className="p-4 bg-gray-50 rounded-lg text-center">
              <div className="text-2xl font-bold text-yellow-600 mb-2">2</div>
              <div className="text-sm text-gray-600">Issues Found</div>
            </div>
          </div>
        </Suspense>
      </div>
    </div>
  );
}
