'use client';

import React, { useState, useMemo } from 'react';
import { useQuery } from '@tanstack/react-query';
import { useSearchParams, useRouter } from 'next/navigation';
import {
  Card,
  CardContent,
  CardDescription,
  CardHeader,
  CardTitle
} from '@/components/ui/card';
import { Input } from '@/components/ui/input';
import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue
} from '@/components/ui/select';
import { Button } from '@/components/ui/button';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import {
  Package,
  MapPin,
  PenTool,
  FileText,
  Hammer,
  ClipboardCheck,
  Zap,
  Search,
  Filter,
  TrendingUp,
  Clock,
  AlertTriangle,
  PauseCircle
} from 'lucide-react';
import type { OperationsMilestone, MilestoneDashboardData } from '@/lib/types/operations';

// Milestone configuration with icons and colors
const milestoneConfig: Record<OperationsMilestone, {
  icon: React.ComponentType<any>;
  label: string;
  color: string;
  description: string;
}> = {
  intake: {
    icon: Package,
    label: 'Intake',
    color: 'text-orange-600 bg-orange-100',
    description: 'Initial customer onboarding and document collection'
  },
  survey: {
    icon: MapPin,
    label: 'Survey',
    color: 'text-yellow-600 bg-yellow-100',
    description: 'Site survey scheduling and completion'
  },
  design: {
    icon: PenTool,
    label: 'Design',
    color: 'text-blue-600 bg-blue-100',
    description: 'System design and engineering'
  },
  permitting: {
    icon: FileText,
    label: 'Permitting',
    color: 'text-purple-600 bg-purple-100',
    description: 'AHJ, NEM, and HOA permit processing'
  },
  install: {
    icon: Hammer,
    label: 'Install',
    color: 'text-indigo-600 bg-indigo-100',
    description: 'Installation scheduling and completion'
  },
  inspection: {
    icon: ClipboardCheck,
    label: 'Inspection',
    color: 'text-teal-600 bg-teal-100',
    description: 'Inspection scheduling and approval'
  },
  pto: {
    icon: Zap,
    label: 'PTO',
    color: 'text-green-600 bg-green-100',
    description: 'Permission to Operate submission'
  }
};

// Milestone groupings
const milestoneGroups = {
  preInstall: ['intake', 'survey', 'design', 'permitting'] as OperationsMilestone[],
  postInstall: ['install', 'inspection', 'pto'] as OperationsMilestone[]
};

export default function ProjectsPage() {
  const router = useRouter();
  const searchParams = useSearchParams();

  // Get current milestone from URL or default to 'intake'
  const currentMilestone = (searchParams.get('milestone') || 'intake') as OperationsMilestone;
  const currentStatus = searchParams.get('status') || 'all';
  const searchQuery = searchParams.get('search') || '';
  const officeFilter = searchParams.get('office') || 'all';
  const repFilter = searchParams.get('rep') || 'all';
  const dateRangeFilter = searchParams.get('dateRange') || 'all';

  // Local state for filters
  const [search, setSearch] = useState(searchQuery);
  const [office, setOffice] = useState(officeFilter);
  const [rep, setRep] = useState(repFilter);
  const [dateRange, setDateRange] = useState(dateRangeFilter);
  const [status, setStatus] = useState(currentStatus);

  // Fetch milestone data
  const baseUrl = typeof window !== 'undefined' ? window.location.origin : '';
  const params = new URLSearchParams();
  if (status !== 'all') params.append('status', status);
  if (office !== 'all') params.append('office', office);
  if (rep !== 'all') params.append('rep', rep);
  if (search) params.append('search', search);
  if (dateRange !== 'all') params.append('dateRange', dateRange);
  params.append('showBlocked', 'true');
  params.append('showOnHold', 'true');

  const endpoint = `${baseUrl}/api/operations/milestones/${currentMilestone}?${params}`;

  const { data: milestoneData, isLoading, error, refetch } = useQuery<{ success: boolean; data: MilestoneDashboardData }>({
    queryKey: ['milestone', currentMilestone, status, office, rep, search, dateRange],
    queryFn: async () => {
      const response = await fetch(endpoint);
      if (!response.ok) throw new Error('Failed to fetch milestone data');
      return response.json();
    },
    refetchInterval: 30000, // Refetch every 30 seconds
    staleTime: 10000
  });

  // Update URL when milestone changes
  const handleMilestoneChange = (milestone: OperationsMilestone) => {
    const params = new URLSearchParams(searchParams);
    params.set('milestone', milestone);
    params.set('status', 'all'); // Reset status when changing milestone
    router.push(`/operations/projects?${params.toString()}`);
    setStatus('all');
  };

  // Update URL when filters change
  const updateFilters = () => {
    const params = new URLSearchParams();
    params.set('milestone', currentMilestone);
    if (status !== 'all') params.set('status', status);
    if (office !== 'all') params.set('office', office);
    if (rep !== 'all') params.set('rep', rep);
    if (search) params.set('search', search);
    if (dateRange !== 'all') params.set('dateRange', dateRange);
    router.push(`/operations/projects?${params.toString()}`);
  };

  // Get unique values for filters
  const uniqueOffices = useMemo(() => {
    if (!milestoneData?.data?.projects) return [];
    return Array.from(new Set(milestoneData.data.projects.map(p => p.salesOffice))).sort();
  }, [milestoneData]);

  const uniqueReps = useMemo(() => {
    if (!milestoneData?.data?.projects) return [];
    return Array.from(new Set(milestoneData.data.projects.map(p => p.salesRepName))).sort();
  }, [milestoneData]);

  const config = milestoneConfig[currentMilestone];
  const MilestoneIcon = config.icon;

  return (
    <div className="container mx-auto p-6 space-y-6">
      {/* Page Header */}
      <div className="space-y-2">
        <h1 className="text-3xl font-bold tracking-tight">Projects</h1>
        <p className="text-muted-foreground">
          Track projects through each stage of the operations lifecycle
        </p>
      </div>

      {/* Milestone Navigation */}
      <Card>
        <CardHeader>
          <CardTitle className="text-lg">Select Milestone</CardTitle>
        </CardHeader>
        <CardContent className="space-y-6">
          {/* Pre-Install Group */}
          <div>
            <h3 className="text-sm font-semibold text-gray-700 mb-3">Pre-Install</h3>
            <div className="grid grid-cols-2 md:grid-cols-4 gap-3">
              {milestoneGroups.preInstall.map((milestone) => {
                const config = milestoneConfig[milestone];
                const Icon = config.icon;
                const isActive = currentMilestone === milestone;

                return (
                  <button
                    key={milestone}
                    onClick={() => handleMilestoneChange(milestone)}
                    className={`
                      flex flex-col items-center gap-2 p-4 rounded-lg border-2 transition-all
                      ${isActive
                        ? 'border-blue-500 bg-blue-50 shadow-md'
                        : 'border-gray-200 hover:border-gray-300 hover:bg-gray-50'
                      }
                    `}
                  >
                    <div className={`p-2 rounded-full ${config.color}`}>
                      <Icon className="h-5 w-5" />
                    </div>
                    <span className="text-sm font-semibold">{config.label}</span>
                  </button>
                );
              })}
            </div>
          </div>

          {/* Post-Install Group */}
          <div>
            <h3 className="text-sm font-semibold text-gray-700 mb-3">Post-Install</h3>
            <div className="grid grid-cols-2 md:grid-cols-3 gap-3">
              {milestoneGroups.postInstall.map((milestone) => {
                const config = milestoneConfig[milestone];
                const Icon = config.icon;
                const isActive = currentMilestone === milestone;

                return (
                  <button
                    key={milestone}
                    onClick={() => handleMilestoneChange(milestone)}
                    className={`
                      flex flex-col items-center gap-2 p-4 rounded-lg border-2 transition-all
                      ${isActive
                        ? 'border-blue-500 bg-blue-50 shadow-md'
                        : 'border-gray-200 hover:border-gray-300 hover:bg-gray-50'
                      }
                    `}
                  >
                    <div className={`p-2 rounded-full ${config.color}`}>
                      <Icon className="h-5 w-5" />
                    </div>
                    <span className="text-sm font-semibold">{config.label}</span>
                  </button>
                );
              })}
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Current Milestone Dashboard */}
      <Card>
        <CardHeader>
          <div className="flex items-center gap-3">
            <div className={`p-3 rounded-lg ${config.color}`}>
              <MilestoneIcon className="h-6 w-6" />
            </div>
            <div>
              <CardTitle>{config.label} Milestone</CardTitle>
              <CardDescription>{config.description}</CardDescription>
            </div>
          </div>
        </CardHeader>
        <CardContent>
          {isLoading ? (
            <div className="flex items-center justify-center py-12">
              <div className="text-center space-y-3">
                <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600 mx-auto" />
                <p className="text-sm text-gray-500">Loading milestone data...</p>
              </div>
            </div>
          ) : error ? (
            <div className="flex items-center justify-center py-12">
              <div className="text-center space-y-3">
                <AlertTriangle className="h-12 w-12 text-red-500 mx-auto" />
                <p className="text-sm text-gray-700">Failed to load milestone data</p>
                <Button onClick={() => refetch()} variant="outline" size="sm">
                  Try Again
                </Button>
              </div>
            </div>
          ) : milestoneData?.data ? (
            <div className="space-y-6">
              {/* Summary Metrics */}
              <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm font-medium text-gray-600">
                      Total Projects
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center gap-2">
                      <TrendingUp className="h-4 w-4 text-blue-600" />
                      <span className="text-2xl font-bold">{milestoneData.data.metrics.total}</span>
                    </div>
                  </CardContent>
                </Card>

                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm font-medium text-gray-600">
                      Avg Days in Milestone
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center gap-2">
                      <Clock className="h-4 w-4 text-amber-600" />
                      <span className="text-2xl font-bold">{milestoneData.data.metrics.avgDaysInMilestone}</span>
                    </div>
                  </CardContent>
                </Card>

                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm font-medium text-gray-600">
                      Blocked Projects
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center gap-2">
                      <AlertTriangle className="h-4 w-4 text-red-600" />
                      <span className="text-2xl font-bold">{milestoneData.data.metrics.blockedCount}</span>
                    </div>
                  </CardContent>
                </Card>

                <Card>
                  <CardHeader className="pb-2">
                    <CardTitle className="text-sm font-medium text-gray-600">
                      On Hold
                    </CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center gap-2">
                      <PauseCircle className="h-4 w-4 text-gray-600" />
                      <span className="text-2xl font-bold">{milestoneData.data.metrics.onHoldCount}</span>
                    </div>
                  </CardContent>
                </Card>
              </div>

              {/* Filters */}
              <div className="flex flex-wrap gap-3 items-end">
                <div className="flex-1 min-w-[200px]">
                  <label className="text-sm font-medium text-gray-700 mb-1 block">
                    Search
                  </label>
                  <div className="relative">
                    <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-400" />
                    <Input
                      placeholder="Project ID or customer name..."
                      value={search}
                      onChange={(e) => setSearch(e.target.value)}
                      onKeyDown={(e) => e.key === 'Enter' && updateFilters()}
                      className="pl-10"
                    />
                  </div>
                </div>

                <div className="w-[180px]">
                  <label className="text-sm font-medium text-gray-700 mb-1 block">
                    Office
                  </label>
                  <Select value={office} onValueChange={setOffice}>
                    <SelectTrigger>
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All Offices</SelectItem>
                      {uniqueOffices.map(o => (
                        <SelectItem key={o} value={o}>{o}</SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                </div>

                <div className="w-[180px]">
                  <label className="text-sm font-medium text-gray-700 mb-1 block">
                    Sales Rep
                  </label>
                  <Select value={rep} onValueChange={setRep}>
                    <SelectTrigger>
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All Reps</SelectItem>
                      {uniqueReps.map(r => (
                        <SelectItem key={r} value={r}>{r}</SelectItem>
                      ))}
                    </SelectContent>
                  </Select>
                </div>

                <div className="w-[180px]">
                  <label className="text-sm font-medium text-gray-700 mb-1 block">
                    Date Range
                  </label>
                  <Select value={dateRange} onValueChange={setDateRange}>
                    <SelectTrigger>
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All Time</SelectItem>
                      <SelectItem value="7days">Last 7 Days</SelectItem>
                      <SelectItem value="30days">Last 30 Days</SelectItem>
                      <SelectItem value="90days">Last 90 Days</SelectItem>
                    </SelectContent>
                  </Select>
                </div>

                <Button onClick={updateFilters} className="gap-2">
                  <Filter className="h-4 w-4" />
                  Apply Filters
                </Button>
              </div>

              {/* Status Tabs */}
              <Tabs value={status} onValueChange={setStatus} className="w-full">
                <TabsList className="w-full flex-wrap h-auto gap-2">
                  <TabsTrigger value="all" className="flex items-center gap-2">
                    All
                    <span className="ml-1 px-2 py-0.5 text-xs font-semibold bg-gray-100 rounded-full">
                      {milestoneData.data.metrics.total}
                    </span>
                  </TabsTrigger>
                  {milestoneData.data.availableStatuses.map((statusValue) => {
                    const count = milestoneData.data.metrics.byStatus[statusValue] || 0;
                    if (count === 0) return null;

                    // Format status label
                    const label = statusValue
                      .split('_')
                      .map(word => word.charAt(0).toUpperCase() + word.slice(1))
                      .join(' ');

                    return (
                      <TabsTrigger key={statusValue} value={statusValue} className="flex items-center gap-2">
                        {label}
                        <span className="ml-1 px-2 py-0.5 text-xs font-semibold bg-blue-100 text-blue-700 rounded-full">
                          {count}
                        </span>
                      </TabsTrigger>
                    );
                  })}
                </TabsList>

                <TabsContent value={status} className="mt-6">
                  {/* Projects Table */}
                  <Card>
                    <CardHeader>
                      <CardTitle className="text-lg">
                        {status === 'all' ? 'All Projects' : status.split('_').map(w => w.charAt(0).toUpperCase() + w.slice(1)).join(' ')}
                      </CardTitle>
                    </CardHeader>
                    <CardContent>
                      {status === 'all' && milestoneData.data.projects.length === 0 ? (
                        <div className="text-center py-12">
                          <Package className="h-12 w-12 text-gray-300 mx-auto mb-3" />
                          <p className="text-gray-500">No projects in this milestone</p>
                        </div>
                      ) : (
                        <div className="space-y-3">
                          {/* Simple project list */}
                          {(status === 'all'
                            ? milestoneData.data.projects
                            : milestoneData.data.projectsByStatus[status] || []
                          ).map((project: any) => (
                            <div key={project.recordId} className="p-4 border rounded-lg hover:bg-gray-50">
                              <div className="flex justify-between items-start">
                                <div>
                                  <h4 className="font-semibold">{project.projectId}</h4>
                                  <p className="text-sm text-gray-600">{project.customerName}</p>
                                  <p className="text-xs text-gray-500">{project.salesOffice}</p>
                                </div>
                                <div className="text-right">
                                  <p className="text-sm font-medium">{project.daysInMilestone} days</p>
                                  <p className="text-xs text-gray-500">in milestone</p>
                                </div>
                              </div>
                            </div>
                          ))}
                        </div>
                      )}
                    </CardContent>
                  </Card>
                </TabsContent>
              </Tabs>
            </div>
          ) : null}
        </CardContent>
      </Card>
    </div>
  );
}
