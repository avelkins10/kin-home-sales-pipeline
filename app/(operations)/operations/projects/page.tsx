'use client';

import { useState, useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useSearchParams, useRouter } from 'next/navigation';
import { Input } from '@/components/ui/input';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue
} from '@/components/ui/select';
import { Button } from '@/components/ui/button';
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
  X
} from 'lucide-react';
import type { OperationsMilestone } from '@/lib/types/operations';
import Link from 'next/link';

// Milestone configuration
const milestoneConfig: Record<OperationsMilestone, {
  icon: React.ComponentType<any>;
  label: string;
  color: string;
}> = {
  intake: { icon: Package, label: 'Intake', color: 'bg-orange-100 text-orange-700 border-orange-300' },
  survey: { icon: MapPin, label: 'Survey', color: 'bg-yellow-100 text-yellow-700 border-yellow-300' },
  design: { icon: PenTool, label: 'Design', color: 'bg-blue-100 text-blue-700 border-blue-300' },
  permitting: { icon: FileText, label: 'Permitting', color: 'bg-purple-100 text-purple-700 border-purple-300' },
  install: { icon: Hammer, label: 'Install', color: 'bg-indigo-100 text-indigo-700 border-indigo-300' },
  inspection: { icon: ClipboardCheck, label: 'Inspection', color: 'bg-teal-100 text-teal-700 border-teal-300' },
  pto: { icon: Zap, label: 'PTO', color: 'bg-green-100 text-green-700 border-green-300' }
};

const allMilestones: OperationsMilestone[] = ['intake', 'survey', 'design', 'permitting', 'install', 'inspection', 'pto'];

export default function OperationsProjectsPage() {
  const router = useRouter();
  const searchParams = useSearchParams();

  // Get filters from URL (milestone is now optional)
  const currentMilestone = searchParams.get('milestone') as OperationsMilestone | null;
  const searchQuery = searchParams.get('search') || '';
  const officeFilter = searchParams.get('office') || 'all';
  const repFilter = searchParams.get('rep') || 'all';
  const sortFilter = searchParams.get('sort') || 'newest';

  // Local state for filters
  const [search, setSearch] = useState(searchQuery);
  const [office, setOffice] = useState(officeFilter);
  const [rep, setRep] = useState(repFilter);
  const [sort, setSort] = useState(sortFilter);

  // Fetch all projects with optional milestone filter
  const baseUrl = typeof window !== 'undefined' ? window.location.origin : '';
  const params = new URLSearchParams();
  if (search) params.append('search', search);
  if (office !== 'all') params.append('office', office);
  if (rep !== 'all') params.append('salesRep', rep);
  if (currentMilestone) params.append('milestone', currentMilestone);
  if (sort) params.append('sort', sort);

  const endpoint = `${baseUrl}/api/operations/projects?${params}`;

  const { data: projectsData, isLoading, error, refetch, isFetching } = useQuery<{ success: boolean; data: any[] }>({
    queryKey: ['operations-projects', currentMilestone, search, office, rep, sort],
    queryFn: async () => {
      const response = await fetch(endpoint);
      if (!response.ok) throw new Error('Failed to fetch projects');
      return response.json();
    },
    refetchInterval: 30000,
    staleTime: 10000
  });

  const projects = projectsData?.data || [];

  // Handle milestone filter
  const handleMilestoneFilter = (milestone: OperationsMilestone | null) => {
    const newParams = new URLSearchParams();
    if (milestone) newParams.set('milestone', milestone);
    if (search) newParams.set('search', search);
    if (office !== 'all') newParams.set('office', office);
    if (rep !== 'all') newParams.set('rep', rep);
    if (sort !== 'newest') newParams.set('sort', sort);
    router.push(`/operations/projects?${newParams.toString()}`);
  };

  // Apply filters
  const applyFilters = () => {
    const newParams = new URLSearchParams();
    if (currentMilestone) newParams.set('milestone', currentMilestone);
    if (search) newParams.set('search', search);
    if (office !== 'all') newParams.set('office', office);
    if (rep !== 'all') newParams.set('rep', rep);
    if (sort !== 'newest') newParams.set('sort', sort);
    router.push(`/operations/projects?${newParams.toString()}`);
  };

  // Clear filters
  const clearFilters = () => {
    setSearch('');
    setOffice('all');
    setRep('all');
    setSort('newest');
    router.push(`/operations/projects${currentMilestone ? `?milestone=${currentMilestone}` : ''}`);
  };

  // Get unique values for filters
  const uniqueOffices = useMemo(() => {
    return Array.from(new Set(projects.map(p => p.salesOffice).filter(Boolean))).sort();
  }, [projects]);

  const uniqueReps = useMemo(() => {
    return Array.from(new Set(projects.map(p => p.salesRepName).filter(Boolean))).sort();
  }, [projects]);

  // Active filters count
  const activeFiltersCount = [
    search ? 1 : 0,
    office !== 'all' ? 1 : 0,
    rep !== 'all' ? 1 : 0
  ].reduce((a, b) => a + b, 0);

  return (
    <div className="min-h-screen bg-slate-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {/* Page Header */}
        <div className="mb-6">
          <h1 className="text-2xl font-bold text-slate-900">Projects</h1>
          <p className="mt-1 text-sm text-slate-600">
            {currentMilestone
              ? `Viewing ${milestoneConfig[currentMilestone].label} milestone projects`
              : 'All projects assigned to you'}
          </p>
        </div>

        {/* Milestone Filter Buttons */}
        <div className="mb-6 bg-white rounded-lg border p-4">
          <div className="flex items-center gap-2 flex-wrap">
            {/* All Projects Button */}
            <button
              onClick={() => handleMilestoneFilter(null)}
              className={`
                flex items-center gap-2 px-4 py-2 rounded-lg border-2 transition-all font-medium text-sm
                ${!currentMilestone
                  ? 'bg-slate-100 text-slate-900 border-slate-400 shadow-sm'
                  : 'bg-white text-gray-600 border-gray-200 hover:border-gray-300 hover:bg-gray-50'
                }
              `}
            >
              <Package className="h-4 w-4" />
              All Projects
              {!currentMilestone && (
                <span className="ml-1 px-1.5 py-0.5 text-xs font-semibold bg-white rounded">
                  {projects.length}
                </span>
              )}
            </button>

            {/* Milestone Filter Buttons */}
            {allMilestones.map((milestone) => {
              const config = milestoneConfig[milestone];
              const Icon = config.icon;
              const isActive = currentMilestone === milestone;

              return (
                <button
                  key={milestone}
                  onClick={() => handleMilestoneFilter(milestone)}
                  className={`
                    flex items-center gap-2 px-4 py-2 rounded-lg border-2 transition-all font-medium text-sm
                    ${isActive
                      ? `${config.color} border-current shadow-sm`
                      : 'bg-white text-gray-600 border-gray-200 hover:border-gray-300 hover:bg-gray-50'
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

        {/* Filters */}
        <div className="mb-4 space-y-3">
          {/* Search and Sort Row */}
          <div className="flex gap-3">
            <div className="flex-1 relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
              <Input
                placeholder="Search by project ID or customer name..."
                value={search}
                onChange={(e) => setSearch(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && applyFilters()}
                className="pl-10"
              />
            </div>
            <Select value={sort} onValueChange={setSort}>
              <SelectTrigger className="w-[180px]">
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

          {/* Office, Rep Filters */}
          <div className="flex gap-3">
            <Select value={office} onValueChange={setOffice}>
              <SelectTrigger className="w-[200px]">
                <SelectValue placeholder="All Offices" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Offices</SelectItem>
                {uniqueOffices.map(o => (
                  <SelectItem key={o} value={o}>{o}</SelectItem>
                ))}
              </SelectContent>
            </Select>

            <Select value={rep} onValueChange={setRep}>
              <SelectTrigger className="w-[200px]">
                <SelectValue placeholder="All Reps" />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="all">All Sales Reps</SelectItem>
                {uniqueReps.map(r => (
                  <SelectItem key={r} value={r}>{r}</SelectItem>
                ))}
              </SelectContent>
            </Select>

            <Button onClick={applyFilters} disabled={isFetching}>
              {isFetching ? (
                <>
                  <RefreshCw className="h-4 w-4 mr-2 animate-spin" />
                  Loading...
                </>
              ) : (
                'Apply'
              )}
            </Button>

            {activeFiltersCount > 0 && (
              <Button variant="outline" onClick={clearFilters}>
                <X className="h-4 w-4 mr-2" />
                Clear ({activeFiltersCount})
              </Button>
            )}
          </div>
        </div>

        {/* Project Table */}
        {isLoading ? (
          <div className="space-y-4">
            {Array.from({ length: 5 }).map((_, i) => (
              <div key={i} className="bg-white rounded-lg border p-4 animate-pulse">
                <div className="h-4 bg-gray-200 rounded w-1/4 mb-3"></div>
                <div className="h-3 bg-gray-200 rounded w-1/2 mb-2"></div>
                <div className="h-3 bg-gray-200 rounded w-1/3"></div>
              </div>
            ))}
          </div>
        ) : error ? (
          <div className="bg-white rounded-lg border p-8 text-center">
            <p className="text-red-600 mb-4">Failed to load projects</p>
            <Button onClick={() => refetch()} variant="outline">
              Try Again
            </Button>
          </div>
        ) : projects.length === 0 ? (
          <div className="bg-white rounded-lg border p-8 text-center">
            <Package className="h-12 w-12 text-gray-300 mx-auto mb-3" />
            <p className="text-gray-600">
              {currentMilestone
                ? `No projects found in ${milestoneConfig[currentMilestone].label} milestone`
                : 'No projects found'}
            </p>
            {activeFiltersCount > 0 && (
              <Button onClick={clearFilters} variant="outline" className="mt-4">
                Clear Filters
              </Button>
            )}
          </div>
        ) : (
          <div className="space-y-3">
            {projects.map((project) => {
              const milestoneInfo = milestoneConfig[project.currentMilestone as OperationsMilestone];
              const MilestoneIcon = milestoneInfo?.icon || Package;

              return (
                <Link
                  key={project.recordId}
                  href={`/operations/projects/${project.recordId}`}
                  className="block bg-white rounded-lg border hover:shadow-md transition-shadow"
                >
                  <div className="p-4">
                    {/* Project Header */}
                    <div className="flex justify-between items-start mb-3">
                      <div className="flex-1">
                        <div className="flex items-center gap-2 mb-1">
                          <h3 className="font-semibold text-lg text-gray-900">{project.projectId}</h3>
                          <span className={`flex items-center gap-1 px-2 py-1 text-xs font-medium rounded ${milestoneInfo?.color || 'bg-gray-100 text-gray-700'}`}>
                            <MilestoneIcon className="h-3 w-3" />
                            {milestoneInfo?.label || 'Unknown'}
                          </span>
                        </div>
                        <p className="text-sm text-gray-600">{project.customerName}</p>
                        <p className="text-xs text-gray-500">{project.salesOffice}</p>
                      </div>
                      <div className="text-right">
                        <div className="text-2xl font-bold text-blue-600">{project.daysInMilestone}</div>
                        <div className="text-xs text-gray-500">days in stage</div>
                      </div>
                    </div>

                    {/* Project Details */}
                    <div className="grid grid-cols-2 md:grid-cols-4 gap-4 text-sm">
                      <div>
                        <div className="text-gray-500 text-xs">Status</div>
                        <div className="font-medium capitalize">
                          {project.currentStage || 'N/A'}
                        </div>
                      </div>
                      <div>
                        <div className="text-gray-500 text-xs">Sales Rep</div>
                        <div className="font-medium">{project.salesRepName || 'N/A'}</div>
                      </div>
                      <div>
                        <div className="text-gray-500 text-xs">Coordinator</div>
                        <div className="font-medium text-xs truncate">{project.coordinatorEmail || 'N/A'}</div>
                      </div>
                      <div>
                        <div className="text-gray-500 text-xs">Lender</div>
                        <div className="font-medium">{project.lenderName || 'N/A'}</div>
                      </div>
                    </div>

                    {/* Blockers/Holds */}
                    {(project.isBlocked || project.isOnHold) && (
                      <div className="mt-3 flex gap-2">
                        {project.isBlocked && (
                          <div className="px-2 py-1 bg-red-100 text-red-700 text-xs rounded">
                            ⚠ Blocked: {project.blockReason}
                          </div>
                        )}
                        {project.isOnHold && (
                          <div className="px-2 py-1 bg-yellow-100 text-yellow-700 text-xs rounded">
                            ⏸ On Hold: {project.holdReason}
                          </div>
                        )}
                      </div>
                    )}
                  </div>
                </Link>
              );
            })}
          </div>
        )}

        {/* Results Summary */}
        {!isLoading && !error && projects.length > 0 && (
          <div className="mt-4 text-sm text-gray-600 text-center">
            Showing {projects.length} project{projects.length !== 1 ? 's' : ''}
            {currentMilestone && ` in ${milestoneConfig[currentMilestone].label} milestone`}
          </div>
        )}
      </div>
    </div>
  );
}
