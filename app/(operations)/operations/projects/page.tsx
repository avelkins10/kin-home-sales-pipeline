'use client';

import { useState } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery } from '@tanstack/react-query';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Input } from '@/components/ui/input';
import { AlertCircle, Calendar, XCircle, CheckCircle, Clock, Search } from 'lucide-react';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { InspectionsTable, InspectionTableSkeleton } from '@/components/operations';
import type { PCInspectionData, PCInspectionFilters } from '@/lib/types/operations';

export default function ProjectsPage() {
  const { data: session, status } = useSession();
  const [activeTab, setActiveTab] = useState<'inspections'>('inspections');
  const [filters, setFilters] = useState<PCInspectionFilters>({
    status: 'all',
    office: 'all',
    salesRep: 'all',
    search: ''
  });

  // Fetch inspection data
  const { data: inspectionData, isLoading, error, refetch } = useQuery<{ success: boolean; data: PCInspectionData }>({
    queryKey: ['inspections', session?.user?.email, filters],
    queryFn: async () => {
      const baseUrl = getBaseUrl();
      const params = new URLSearchParams({
        status: filters.status,
        office: filters.office,
        salesRep: filters.salesRep,
        search: filters.search
      });

      const response = await fetch(`${baseUrl}/api/operations/projects/inspections?${params}`);
      if (!response.ok) {
        throw new Error('Failed to fetch inspection data');
      }
      return response.json();
    },
    enabled: !!session?.user?.email && activeTab === 'inspections',
    refetchInterval: 30000, // 30 seconds
    staleTime: 15000 // 15 seconds
  });

  if (status === 'loading') {
    return (
      <div className="min-h-screen bg-slate-50 p-6">
        <div className="max-w-7xl mx-auto">
          <div className="mb-6">
            <div className="h-8 w-48 bg-gray-200 rounded animate-pulse mb-2" />
            <div className="h-4 w-96 bg-gray-200 rounded animate-pulse" />
          </div>
          <InspectionTableSkeleton />
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-slate-50">
      <div className="max-w-7xl mx-auto p-6">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-gray-900">Projects</h1>
          <p className="mt-1 text-sm text-gray-600">
            Track projects through various stages and milestones
          </p>
        </div>

        {/* Tabs */}
        <Tabs value={activeTab} onValueChange={(value) => setActiveTab(value as 'inspections')} className="space-y-6">
          <TabsList>
            <TabsTrigger value="inspections" className="flex items-center gap-2">
              <CheckCircle className="h-4 w-4" />
              Inspections
              {inspectionData?.data && (
                <span className="ml-2 px-2 py-0.5 text-xs font-semibold bg-blue-100 text-blue-700 rounded-full">
                  {Object.values(inspectionData.data.counts).reduce((a, b) => a + b, 0)}
                </span>
              )}
            </TabsTrigger>
            {/* Additional tabs can be added here in the future */}
            {/* <TabsTrigger value="pto">PTO</TabsTrigger> */}
            {/* <TabsTrigger value="nem">NEM</TabsTrigger> */}
          </TabsList>

          <TabsContent value="inspections" className="space-y-6">
            {/* Search and Filters */}
            <div className="bg-white rounded-lg border p-4">
              <div className="flex items-center gap-4">
                <div className="flex-1 relative">
                  <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
                  <Input
                    placeholder="Search by project ID or customer name..."
                    value={filters.search}
                    onChange={(e) => setFilters({ ...filters, search: e.target.value })}
                    className="pl-10"
                  />
                </div>
                <select
                  value={filters.office}
                  onChange={(e) => setFilters({ ...filters, office: e.target.value })}
                  className="text-sm border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                >
                  <option value="all">All Offices</option>
                  {/* Offices will be populated dynamically */}
                </select>
                <select
                  value={filters.salesRep}
                  onChange={(e) => setFilters({ ...filters, salesRep: e.target.value })}
                  className="text-sm border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                >
                  <option value="all">All Sales Reps</option>
                  {/* Sales reps will be populated dynamically */}
                </select>
              </div>
            </div>

            {/* Inspection Tables */}
            {isLoading ? (
              <div className="space-y-6">
                <InspectionTableSkeleton />
                <InspectionTableSkeleton />
                <InspectionTableSkeleton />
                <InspectionTableSkeleton />
              </div>
            ) : error ? (
              <div className="bg-white rounded-lg border p-6 text-center">
                <AlertCircle className="w-12 h-12 text-red-500 mx-auto mb-3" />
                <h3 className="text-lg font-semibold text-gray-900 mb-1">
                  Failed to Load Inspection Data
                </h3>
                <p className="text-sm text-gray-500">
                  {(error as Error).message || 'Please try again later.'}
                </p>
              </div>
            ) : inspectionData?.data ? (
              <div className="space-y-6">
                {/* Inspection Failed - Most urgent */}
                <InspectionsTable
                  projects={inspectionData.data.inspectionFailed}
                  status="inspection_failed"
                  title="Inspection Failed"
                  icon={<XCircle className="h-5 w-5 text-red-600" />}
                />

                {/* Waiting for Inspection - Second priority */}
                <InspectionsTable
                  projects={inspectionData.data.waitingForInspection}
                  status="waiting_for_inspection"
                  title="Waiting for Inspection"
                  icon={<Clock className="h-5 w-5 text-yellow-600" />}
                />

                {/* Inspection Scheduled */}
                <InspectionsTable
                  projects={inspectionData.data.inspectionScheduled}
                  status="inspection_scheduled"
                  title="Inspection Scheduled"
                  icon={<Calendar className="h-5 w-5 text-blue-600" />}
                />

                {/* Inspection Passed */}
                <InspectionsTable
                  projects={inspectionData.data.inspectionPassed}
                  status="inspection_passed"
                  title="Inspection Passed"
                  icon={<CheckCircle className="h-5 w-5 text-green-600" />}
                />
              </div>
            ) : (
              <div className="bg-white rounded-lg border p-6 text-center">
                <CheckCircle className="w-12 h-12 text-green-500 mx-auto mb-3" />
                <h3 className="text-lg font-semibold text-gray-900 mb-1">
                  No Inspection Data
                </h3>
                <p className="text-sm text-gray-500">
                  There are no projects in the inspection phase at this time.
                </p>
              </div>
            )}
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
}
