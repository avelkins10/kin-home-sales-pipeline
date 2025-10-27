'use client';

import { useState, useMemo } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery } from '@tanstack/react-query';
import { Tabs, TabsList, TabsTrigger, TabsContent } from '@/components/ui/tabs';
import { Input } from '@/components/ui/input';
import { Card, CardContent } from '@/components/ui/card';
import { AlertCircle, Calendar, XCircle, CheckCircle, Clock, Search } from 'lucide-react';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { InspectionsTable } from '@/components/operations/InspectionsTable';
import type { PCInspectionData, PCInspectionFilters, PCInspectionStatus, PCInspectionProject } from '@/lib/types/operations';

export default function ProjectsPage() {
  const { data: session, status } = useSession();
  const [activeTab, setActiveTab] = useState<PCInspectionStatus>('inspection_failed');
  const [filters, setFilters] = useState<PCInspectionFilters>({
    status: 'all',
    office: 'all',
    salesRep: 'all',
    search: '',
    dateRange: 'all',
    state: 'all'
  });

  // Fetch inspection data
  const { data: inspectionData, isLoading, error, refetch } = useQuery<{ success: boolean; data: PCInspectionData }>({
    queryKey: ['inspections', session?.user?.email, activeTab, filters],
    queryFn: async () => {
      const baseUrl = getBaseUrl();
      const params = new URLSearchParams({
        status: activeTab, // Fetch only the active tab's data
        office: filters.office,
        salesRep: filters.salesRep,
        search: filters.search,
        dateRange: filters.dateRange
      });

      if (filters.dateRange === 'custom' && filters.customStartDate && filters.customEndDate) {
        params.append('customStartDate', filters.customStartDate);
        params.append('customEndDate', filters.customEndDate);
      }

      const response = await fetch(`${baseUrl}/api/operations/projects/inspections?${params}`);
      if (!response.ok) {
        throw new Error('Failed to fetch inspection data');
      }
      return response.json();
    },
    enabled: !!session?.user?.email,
    refetchInterval: 30000, // 30 seconds
    staleTime: 15000 // 15 seconds
  });

  // Get projects for the active tab with client-side filtering
  const getActiveProjects = (): PCInspectionProject[] => {
    if (!inspectionData?.data) return [];

    let projects: PCInspectionProject[] = [];
    switch (activeTab) {
      case 'inspection_failed':
        projects = inspectionData.data.inspectionFailed;
        break;
      case 'waiting_for_inspection':
        projects = inspectionData.data.waitingForInspection;
        break;
      case 'inspection_scheduled':
        projects = inspectionData.data.inspectionScheduled;
        break;
      case 'inspection_passed':
        projects = inspectionData.data.inspectionPassed;
        break;
      default:
        return [];
    }

    // Apply state filter if selected
    if (filters.state && filters.state !== 'all') {
      projects = projects.filter(project => {
        if (!project.salesOffice) return false;
        const match = project.salesOffice.match(/[-\s]([A-Z]{2}|\w+)\s*(?:20\d{2})?$/);
        if (match && match[1]) {
          return match[1].toUpperCase() === filters.state?.toUpperCase();
        }
        return false;
      });
    }

    return projects;
  };

  const activeProjects = getActiveProjects();

  // Extract unique states from office names (e.g., "Molina - KC 2025" -> "KC")
  const uniqueStates = useMemo(() => {
    if (!inspectionData?.data) return [];
    const allProjects = [
      ...inspectionData.data.waitingForInspection,
      ...inspectionData.data.inspectionScheduled,
      ...inspectionData.data.inspectionFailed,
      ...inspectionData.data.inspectionPassed
    ];
    const states = new Set<string>();
    allProjects.forEach(project => {
      if (project.salesOffice) {
        // Extract state abbreviation from office name patterns like "Molina - KC 2025" or "Stevens - Iowa 2025"
        const match = project.salesOffice.match(/[-\s]([A-Z]{2}|\w+)\s*(?:20\d{2})?$/);
        if (match && match[1]) {
          states.add(match[1].toUpperCase());
        }
      }
    });
    return Array.from(states).sort();
  }, [inspectionData]);

  if (status === 'loading') {
    return (
      <div className="min-h-screen bg-slate-50 p-6">
        <div className="max-w-7xl mx-auto">
          <div className="mb-6">
            <div className="h-8 w-48 bg-gray-200 rounded animate-pulse mb-2" />
            <div className="h-4 w-96 bg-gray-200 rounded animate-pulse" />
          </div>
          <Card>
            <CardContent className="p-6">
              <div className="animate-pulse space-y-4">
                <div className="h-10 bg-gray-200 rounded" />
                <div className="h-10 bg-gray-200 rounded" />
                <div className="h-10 bg-gray-200 rounded" />
              </div>
            </CardContent>
          </Card>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-slate-50">
      <div className="max-w-7xl mx-auto p-6">
        {/* Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-gray-900">Projects - Inspections</h1>
          <p className="mt-1 text-sm text-gray-600">
            Track projects through inspection stages
          </p>
        </div>

        {/* Tabs */}
        <Tabs value={activeTab} onValueChange={(value) => setActiveTab(value as PCInspectionStatus)} className="space-y-6">
          <TabsList>
            <TabsTrigger value="inspection_failed" className="flex items-center gap-2">
              <XCircle className="h-4 w-4" />
              Failed
              {inspectionData?.data?.counts && (
                <span className="ml-1 px-2 py-0.5 text-xs font-semibold bg-red-100 text-red-700 rounded-full">
                  {inspectionData.data.counts.inspectionFailed}
                </span>
              )}
            </TabsTrigger>
            <TabsTrigger value="waiting_for_inspection" className="flex items-center gap-2">
              <Clock className="h-4 w-4" />
              Waiting
              {inspectionData?.data?.counts && (
                <span className="ml-1 px-2 py-0.5 text-xs font-semibold bg-yellow-100 text-yellow-700 rounded-full">
                  {inspectionData.data.counts.waitingForInspection}
                </span>
              )}
            </TabsTrigger>
            <TabsTrigger value="inspection_scheduled" className="flex items-center gap-2">
              <Calendar className="h-4 w-4" />
              Scheduled
              {inspectionData?.data?.counts && (
                <span className="ml-1 px-2 py-0.5 text-xs font-semibold bg-blue-100 text-blue-700 rounded-full">
                  {inspectionData.data.counts.inspectionScheduled}
                </span>
              )}
            </TabsTrigger>
            <TabsTrigger value="inspection_passed" className="flex items-center gap-2">
              <CheckCircle className="h-4 w-4" />
              Passed
              {inspectionData?.data?.counts && (
                <span className="ml-1 px-2 py-0.5 text-xs font-semibold bg-green-100 text-green-700 rounded-full">
                  {inspectionData.data.counts.inspectionPassed}
                </span>
              )}
            </TabsTrigger>
          </TabsList>

          {/* Search and Filters - Common for all tabs */}
          <Card>
            <CardContent className="p-4">
              <div className="flex flex-wrap items-center gap-4">
                <div className="flex-1 min-w-[200px] relative">
                  <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
                  <Input
                    placeholder="Search by project ID or customer name..."
                    value={filters.search}
                    onChange={(e) => setFilters({ ...filters, search: e.target.value })}
                    className="pl-10"
                  />
                </div>
                <select
                  value={filters.dateRange}
                  onChange={(e) => setFilters({ ...filters, dateRange: e.target.value as any })}
                  className="text-sm border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                >
                  <option value="all">All Dates</option>
                  <option value="7days">Last 7 Days</option>
                  <option value="30days">Last 30 Days</option>
                  <option value="90days">Last 90 Days</option>
                </select>
                <select
                  value={filters.state || 'all'}
                  onChange={(e) => setFilters({ ...filters, state: e.target.value })}
                  className="text-sm border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                >
                  <option value="all">All States</option>
                  {uniqueStates.map(state => (
                    <option key={state} value={state}>{state}</option>
                  ))}
                </select>
                <select
                  value={filters.office}
                  onChange={(e) => setFilters({ ...filters, office: e.target.value })}
                  className="text-sm border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                >
                  <option value="all">All Offices</option>
                </select>
                <select
                  value={filters.salesRep}
                  onChange={(e) => setFilters({ ...filters, salesRep: e.target.value })}
                  className="text-sm border border-gray-300 rounded-md px-3 py-2 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500"
                >
                  <option value="all">All Sales Reps</option>
                </select>
              </div>
            </CardContent>
          </Card>

          {/* Tab Content */}
          <TabsContent value="inspection_failed" className="space-y-4">
            <Card>
              <CardContent className="p-6">
                {isLoading ? (
                  <div className="animate-pulse space-y-4">
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                  </div>
                ) : error ? (
                  <div className="text-center py-8">
                    <AlertCircle className="w-12 h-12 text-red-500 mx-auto mb-3" />
                    <h3 className="text-lg font-semibold text-gray-900 mb-1">
                      Failed to Load Data
                    </h3>
                    <p className="text-sm text-gray-500">
                      {(error as Error).message || 'Please try again later.'}
                    </p>
                  </div>
                ) : (
                  <InspectionsTable
                    projects={activeProjects}
                    status="inspection_failed"
                  />
                )}
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="waiting_for_inspection" className="space-y-4">
            <Card>
              <CardContent className="p-6">
                {isLoading ? (
                  <div className="animate-pulse space-y-4">
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                  </div>
                ) : error ? (
                  <div className="text-center py-8">
                    <AlertCircle className="w-12 h-12 text-red-500 mx-auto mb-3" />
                    <h3 className="text-lg font-semibold text-gray-900 mb-1">
                      Failed to Load Data
                    </h3>
                    <p className="text-sm text-gray-500">
                      {(error as Error).message || 'Please try again later.'}
                    </p>
                  </div>
                ) : (
                  <InspectionsTable
                    projects={activeProjects}
                    status="waiting_for_inspection"
                  />
                )}
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="inspection_scheduled" className="space-y-4">
            <Card>
              <CardContent className="p-6">
                {isLoading ? (
                  <div className="animate-pulse space-y-4">
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                  </div>
                ) : error ? (
                  <div className="text-center py-8">
                    <AlertCircle className="w-12 h-12 text-red-500 mx-auto mb-3" />
                    <h3 className="text-lg font-semibold text-gray-900 mb-1">
                      Failed to Load Data
                    </h3>
                    <p className="text-sm text-gray-500">
                      {(error as Error).message || 'Please try again later.'}
                    </p>
                  </div>
                ) : (
                  <InspectionsTable
                    projects={activeProjects}
                    status="inspection_scheduled"
                  />
                )}
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="inspection_passed" className="space-y-4">
            <Card>
              <CardContent className="p-6">
                {isLoading ? (
                  <div className="animate-pulse space-y-4">
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                    <div className="h-10 bg-gray-200 rounded" />
                  </div>
                ) : error ? (
                  <div className="text-center py-8">
                    <AlertCircle className="w-12 h-12 text-red-500 mx-auto mb-3" />
                    <h3 className="text-lg font-semibold text-gray-900 mb-1">
                      Failed to Load Data
                    </h3>
                    <p className="text-sm text-gray-500">
                      {(error as Error).message || 'Please try again later.'}
                    </p>
                  </div>
                ) : (
                  <InspectionsTable
                    projects={activeProjects}
                    status="inspection_passed"
                  />
                )}
              </CardContent>
            </Card>
          </TabsContent>
        </Tabs>
      </div>
    </div>
  );
}
