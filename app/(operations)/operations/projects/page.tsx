'use client';

import { useState, useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useSearchParams, useRouter } from 'next/navigation';
import { useSession } from 'next-auth/react';
import { Input } from '@/components/ui/input';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue
} from '@/components/ui/select';
import { Button } from '@/components/ui/button';
import { Tabs, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { OperationsProjectCard } from '@/components/operations/OperationsProjectCard';
import { operationsProjectsListKey } from '@/lib/queryKeys';
import { formatMilestoneStatus } from '@/lib/utils/operations-milestones';
import {
  Package,
  MapPin,
  PenTool,
  FileText,
  Hammer,
  ClipboardCheck,
  Zap,
  Search,
  RefreshCw,
  X,
  Loader2
} from 'lucide-react';
import type { OperationsMilestone } from '@/lib/types/operations';

// 7-milestone configuration
const milestoneConfig: Record<OperationsMilestone, {
  icon: React.ComponentType<any>;
  label: string;
  color: string;
}> = {
  intake: { icon: Package, label: 'Intake', color: 'bg-orange-100 text-orange-700 border-orange-300' },
  survey: { icon: MapPin, label: 'Survey', color: 'bg-purple-100 text-purple-700 border-purple-300' },
  design: { icon: PenTool, label: 'Design', color: 'bg-blue-100 text-blue-700 border-blue-300' },
  permitting: { icon: FileText, label: 'Permitting', color: 'bg-teal-100 text-teal-700 border-teal-300' },
  install: { icon: Hammer, label: 'Install', color: 'bg-indigo-100 text-indigo-700 border-indigo-300' },
  inspections: { icon: ClipboardCheck, label: 'Inspections', color: 'bg-amber-100 text-amber-700 border-amber-300' },
  pto: { icon: Zap, label: 'PTO', color: 'bg-emerald-100 text-emerald-700 border-emerald-300' }
};

const allMilestones: OperationsMilestone[] = ['intake', 'survey', 'design', 'permitting', 'install', 'inspections', 'pto'];

export default function OperationsProjectsPage() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const { data: session } = useSession();

  // Get filters from URL
  const currentMilestone = searchParams.get('milestone') as OperationsMilestone | null;
  const currentStatus = searchParams.get('status') || 'all';
  const searchQuery = searchParams.get('search') || '';
  const officeFilter = searchParams.get('office') || '';
  const repFilter = searchParams.get('rep') || '';
  const sortFilter = searchParams.get('sort') || 'newest';

  // Local state for filters (before applying)
  const [search, setSearch] = useState(searchQuery);
  const [office, setOffice] = useState(officeFilter);
  const [rep, setRep] = useState(repFilter);
  const [sort, setSort] = useState(sortFilter);

  // Fetch projects using the new query key
  const userId = session?.user?.id || '';
  const userRole = session?.user?.role || '';

  const { data: projectsData, isLoading, error, refetch, isFetching } = useQuery({
    queryKey: operationsProjectsListKey(
      userId,
      userRole,
      currentMilestone || 'all',
      currentStatus,
      searchQuery,
      sortFilter,
      officeFilter,
      repFilter
    ),
    queryFn: async () => {
      const params = new URLSearchParams();
      if (currentMilestone) params.append('milestone', currentMilestone);
      if (currentStatus !== 'all') params.append('status', currentStatus);
      if (searchQuery) params.append('search', searchQuery);
      if (sortFilter) params.append('sort', sortFilter);
      if (officeFilter) params.append('office', officeFilter);
      if (repFilter) params.append('salesRep', repFilter);

      const response = await fetch(`/api/operations/projects?${params.toString()}`);
      if (!response.ok) throw new Error('Failed to fetch projects');
      return response.json();
    },
    staleTime: 300000, // 5 minutes
    enabled: !!userId && !!userRole
  });

  // Handle both grouped (milestone filtered) and flat array responses
  const responseData = projectsData?.data;
  const isGroupedResponse = currentMilestone && responseData && typeof responseData === 'object' && 'projects' in responseData;

  const projects = isGroupedResponse
    ? responseData.projects
    : (Array.isArray(responseData) ? responseData : []);

  const availableStatuses = isGroupedResponse ? responseData.availableStatuses || [] : [];
  const statusCounts = isGroupedResponse ? responseData.statusCounts || {} : {};
  const totalProjects = isGroupedResponse ? responseData.total || 0 : projects.length;

  // Get unique values for filters from current projects
  const uniqueOffices = useMemo(() => {
    const offices = projects.map((p: any) => p.salesOffice).filter(Boolean);
    return Array.from(new Set(offices)).sort();
  }, [projects]);

  const uniqueReps = useMemo(() => {
    const reps = projects.flatMap((p: any) => [p.closerName, p.setterName]).filter(Boolean);
    return Array.from(new Set(reps)).sort();
  }, [projects]);

  // Handle milestone filter
  const handleMilestoneFilter = (milestone: OperationsMilestone | null) => {
    const newParams = new URLSearchParams();
    if (milestone) newParams.set('milestone', milestone);
    if (searchQuery) newParams.set('search', searchQuery);
    if (sortFilter) newParams.set('sort', sortFilter);
    if (officeFilter) newParams.set('office', officeFilter);
    if (repFilter) newParams.set('rep', repFilter);
    router.push(`/operations/projects?${newParams.toString()}`);
  };

  // Handle status filter (tabs within milestone)
  const handleStatusChange = (newStatus: string) => {
    const newParams = new URLSearchParams();
    if (currentMilestone) newParams.set('milestone', currentMilestone);
    if (newStatus !== 'all') newParams.set('status', newStatus);
    if (searchQuery) newParams.set('search', searchQuery);
    if (sortFilter) newParams.set('sort', sortFilter);
    if (officeFilter) newParams.set('office', officeFilter);
    if (repFilter) newParams.set('rep', repFilter);
    router.push(`/operations/projects?${newParams.toString()}`);
  };

  // Apply filters
  const applyFilters = () => {
    const newParams = new URLSearchParams();
    if (currentMilestone) newParams.set('milestone', currentMilestone);
    if (currentStatus !== 'all') newParams.set('status', currentStatus);
    if (search) newParams.set('search', search);
    if (sort) newParams.set('sort', sort);
    if (office) newParams.set('office', office);
    if (rep) newParams.set('rep', rep);
    router.push(`/operations/projects?${newParams.toString()}`);
  };

  // Clear filters
  const clearFilters = () => {
    setSearch('');
    setOffice('');
    setRep('');
    setSort('newest');
    const newParams = new URLSearchParams();
    if (currentMilestone) newParams.set('milestone', currentMilestone);
    router.push(`/operations/projects?${newParams.toString()}`);
  };

  // Active filters count
  const activeFiltersCount = [
    searchQuery ? 1 : 0,
    officeFilter ? 1 : 0,
    repFilter ? 1 : 0
  ].reduce((a, b) => a + b, 0);

  return (
    <div className="min-h-screen bg-slate-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Page Header */}
        <div className="mb-6">
          <h1 className="text-3xl font-bold text-slate-900">Projects</h1>
          <p className="mt-1 text-sm text-slate-600">
            {currentMilestone
              ? `${milestoneConfig[currentMilestone].label} milestone`
              : 'All projects assigned to you'}
          </p>
        </div>

        {/* Milestone Filter Buttons */}
        <div className="mb-6 bg-white rounded-lg border border-slate-200 p-4 shadow-sm">
          <div className="flex items-center gap-2 flex-wrap">
            {/* All Projects Button */}
            <button
              onClick={() => handleMilestoneFilter(null)}
              className={`
                flex items-center gap-2 px-4 py-2.5 rounded-lg border-2 transition-all font-medium text-sm
                ${!currentMilestone
                  ? 'bg-slate-900 text-white border-slate-900 shadow-sm'
                  : 'bg-white text-slate-600 border-slate-200 hover:border-slate-300 hover:bg-slate-50'
                }
              `}
            >
              <Package className="h-4 w-4" />
              All Projects
              {!currentMilestone && totalProjects > 0 && (
                <span className="ml-1 px-2 py-0.5 text-xs font-semibold bg-white text-slate-900 rounded-full">
                  {totalProjects}
                </span>
              )}
            </button>

            {/* Milestone Buttons */}
            {allMilestones.map((milestone) => {
              const config = milestoneConfig[milestone];
              const Icon = config.icon;
              const isActive = currentMilestone === milestone;

              return (
                <button
                  key={milestone}
                  onClick={() => handleMilestoneFilter(milestone)}
                  className={`
                    flex items-center gap-2 px-4 py-2.5 rounded-lg border-2 transition-all font-medium text-sm
                    ${isActive
                      ? `${config.color} border-current shadow-sm`
                      : 'bg-white text-slate-600 border-slate-200 hover:border-slate-300 hover:bg-slate-50'
                    }
                  `}
                >
                  <Icon className="h-4 w-4" />
                  {config.label}
                </button>
              );
            })}
          </div>
        </div>

        {/* Filters Section */}
        <div className="mb-6 space-y-3 bg-white rounded-lg border border-slate-200 p-4 shadow-sm">
          {/* Search and Sort Row */}
          <div className="flex gap-3">
            <div className="flex-1 relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-slate-400" />
              <Input
                placeholder="Search by project ID, customer name, or address..."
                value={search}
                onChange={(e) => setSearch(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && applyFilters()}
                className="pl-10 border-slate-300"
              />
            </div>
            <Select value={sort} onValueChange={setSort}>
              <SelectTrigger className="w-[180px] border-slate-300">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="newest">Newest First</SelectItem>
                <SelectItem value="oldest">Oldest First</SelectItem>
                <SelectItem value="daysDesc">Days (High to Low)</SelectItem>
                <SelectItem value="projectId">Project ID</SelectItem>
                <SelectItem value="customer">Customer Name</SelectItem>
              </SelectContent>
            </Select>
          </div>

          {/* Office, Rep Filters + Action Buttons */}
          <div className="flex gap-3 items-center">
            <Select value={office} onValueChange={setOffice}>
              <SelectTrigger className="w-[200px] border-slate-300">
                <SelectValue placeholder="All Offices" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="">All Offices</SelectItem>
                {uniqueOffices.map(o => (
                  <SelectItem key={o} value={o}>{o}</SelectItem>
                ))}
              </SelectContent>
            </Select>

            <Select value={rep} onValueChange={setRep}>
              <SelectTrigger className="w-[200px] border-slate-300">
                <SelectValue placeholder="All Reps" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="">All Sales Reps</SelectItem>
                {uniqueReps.map(r => (
                  <SelectItem key={r} value={r}>{r}</SelectItem>
                ))}
              </SelectContent>
            </Select>

            <div className="flex gap-2 ml-auto">
              <Button onClick={applyFilters} disabled={isFetching} className="bg-blue-600 hover:bg-blue-700">
                {isFetching ? (
                  <>
                    <RefreshCw className="h-4 w-4 mr-2 animate-spin" />
                    Loading
                  </>
                ) : (
                  'Apply'
                )}
              </Button>

              {activeFiltersCount > 0 && (
                <Button variant="outline" onClick={clearFilters} className="border-slate-300">
                  <X className="h-4 w-4 mr-2" />
                  Clear ({activeFiltersCount})
                </Button>
              )}

              <Button variant="outline" onClick={() => refetch()} disabled={isFetching} className="border-slate-300">
                <RefreshCw className={`h-4 w-4 ${isFetching ? 'animate-spin' : ''}`} />
              </Button>
            </div>
          </div>
        </div>

        {/* Status Tabs (only when milestone is selected) */}
        {currentMilestone && availableStatuses.length > 0 && (
          <Tabs value={currentStatus} onValueChange={handleStatusChange} className="w-full mb-6">
            <TabsList className="w-full flex-wrap h-auto gap-2 bg-white border border-slate-200 p-2 rounded-lg shadow-sm">
              <TabsTrigger value="all" className="data-[state=active]:bg-slate-900 data-[state=active]:text-white">
                All
                <span className="ml-2 px-2 py-0.5 text-xs font-semibold bg-slate-100 text-slate-700 rounded-full">
                  {totalProjects}
                </span>
              </TabsTrigger>
              {availableStatuses.map((statusValue) => {
                const count = statusCounts[statusValue] || 0;
                if (count === 0) return null;

                return (
                  <TabsTrigger
                    key={statusValue}
                    value={statusValue}
                    className="data-[state=active]:bg-blue-600 data-[state=active]:text-white"
                  >
                    {formatMilestoneStatus(statusValue)}
                    <span className="ml-2 px-2 py-0.5 text-xs font-semibold bg-blue-100 text-blue-700 rounded-full">
                      {count}
                    </span>
                  </TabsTrigger>
                );
              })}
            </TabsList>
          </Tabs>
        )}

        {/* Project Grid */}
        {isLoading ? (
          <div className="grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4">
            {Array.from({ length: 6 }).map((_, i) => (
              <div key={i} className="bg-white rounded-lg border border-slate-200 p-6 animate-pulse">
                <div className="h-4 bg-slate-200 rounded w-1/2 mb-3"></div>
                <div className="h-3 bg-slate-200 rounded w-3/4 mb-2"></div>
                <div className="h-3 bg-slate-200 rounded w-1/2 mb-4"></div>
                <div className="h-20 bg-slate-100 rounded"></div>
              </div>
            ))}
          </div>
        ) : error ? (
          <div className="bg-white rounded-lg border border-slate-200 p-12 text-center shadow-sm">
            <p className="text-red-600 mb-4">Failed to load projects</p>
            <Button onClick={() => refetch()} variant="outline">
              <RefreshCw className="h-4 w-4 mr-2" />
              Try Again
            </Button>
          </div>
        ) : projects.length === 0 ? (
          <div className="bg-white rounded-lg border border-slate-200 p-12 text-center shadow-sm">
            <Package className="h-16 w-16 text-slate-300 mx-auto mb-4" />
            <h3 className="text-lg font-medium text-slate-900 mb-2">No projects found</h3>
            <p className="text-slate-600 mb-4">
              {currentMilestone
                ? `No projects in ${milestoneConfig[currentMilestone].label} milestone`
                : activeFiltersCount > 0
                ? 'Try adjusting your filters'
                : 'No projects assigned to you'}
            </p>
            {activeFiltersCount > 0 && (
              <Button onClick={clearFilters} variant="outline">
                Clear Filters
              </Button>
            )}
          </div>
        ) : (
          <>
            <div className="grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4">
              {projects.map((project: any) => (
                <OperationsProjectCard key={project.recordId} project={project} />
              ))}
            </div>

            {/* Results Summary */}
            <div className="mt-6 text-sm text-slate-600 text-center">
              Showing <span className="font-semibold">{projects.length}</span> project{projects.length !== 1 ? 's' : ''}
              {currentMilestone && ` in ${milestoneConfig[currentMilestone].label}`}
              {currentStatus !== 'all' && ` with status "${formatMilestoneStatus(currentStatus)}"`}
            </div>
          </>
        )}
      </div>
    </div>
  );
}
