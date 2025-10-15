'use client';

import { useParams, useSearchParams, useRouter } from 'next/navigation';
import { useSession } from 'next-auth/react';
import { redirect } from 'next/navigation';
import { useState, useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import Link from 'next/link';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import {
  ArrowLeft,
  User,
  TrendingUp,
  Package,
  Clock,
  CheckCircle,
  XCircle,
  PauseCircle,
  FileDown,
  BarChart3
} from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';
import type { RepDetailMetrics } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

// Loading skeleton component
function RepDetailSkeleton() {
  return (
    <div className="space-y-6">
      <div className="flex items-center gap-4">
        <Skeleton className="h-10 w-24" />
        <Skeleton className="h-8 w-64" />
      </div>
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        {Array.from({ length: 8 }).map((_, i) => (
          <Card key={i}>
            <CardHeader>
              <Skeleton className="h-4 w-24" />
            </CardHeader>
            <CardContent>
              <Skeleton className="h-8 w-16" />
            </CardContent>
          </Card>
        ))}
      </div>
      <Card>
        <CardHeader>
          <Skeleton className="h-6 w-32" />
        </CardHeader>
        <CardContent>
          <Skeleton className="h-64 w-full" />
        </CardContent>
      </Card>
    </div>
  );
}

export default function RepDetailPage() {
  const params = useParams();
  const searchParams = useSearchParams();
  const router = useRouter();
  const { data: session, status } = useSession();
  const repId = params.id as string;

  // Filter state from URL params
  const [timeRange, setTimeRange] = useState<TimeRange>('ytd');
  const [customDateRange, setCustomDateRange] = useState<CustomDateRange | undefined>();
  const [isExporting, setIsExporting] = useState(false);

  // Parse URL params on mount
  useEffect(() => {
    const timeRangeParam = searchParams.get('timeRange') as TimeRange;
    const startDateParam = searchParams.get('startDate');
    const endDateParam = searchParams.get('endDate');

    if (timeRangeParam && ['ytd', 'last_30', 'last_90', 'last_12_months', 'custom'].includes(timeRangeParam)) {
      setTimeRange(timeRangeParam);
    }

    if (timeRangeParam === 'custom' && startDateParam && endDateParam) {
      setCustomDateRange({
        startDate: startDateParam,
        endDate: endDateParam
      });
    }
  }, [searchParams]);

  // Fetch rep details
  const { data, isLoading, error } = useQuery<RepDetailMetrics>({
    queryKey: ['rep-details', repId, timeRange, customDateRange],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/rep-details?id=${encodeURIComponent(repId)}&timeRange=${timeRange}`;

      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }

      const response = await fetch(url);
      if (!response.ok) {
        const errorData = await response.json();
        throw new Error(errorData.error || 'Failed to fetch rep details');
      }
      return await response.json();
    },
  });

  if (status === 'loading') {
    return <div>Loading...</div>;
  }

  if (!session) {
    redirect('/login');
  }

  // Check if user has access to analytics
  const hasAnalyticsAccess = ['office_leader', 'regional', 'super_admin'].includes(session.user.role);
  if (!hasAnalyticsAccess) {
    return (
      <div className="flex items-center justify-center min-h-[400px]">
        <div className="text-center">
          <h1 className="text-2xl font-bold text-gray-900 mb-2">Access Denied</h1>
          <p className="text-gray-600">
            You don&apos;t have permission to access rep details.
          </p>
        </div>
      </div>
    );
  }

  const handleTimeRangeChange = (range: TimeRange, customRange?: CustomDateRange) => {
    const params = new URLSearchParams(searchParams.toString());
    params.set('timeRange', range);

    if (range === 'custom' && customRange) {
      params.set('startDate', customRange.startDate);
      params.set('endDate', customRange.endDate);
    } else {
      params.delete('startDate');
      params.delete('endDate');
    }

    router.push(`/analytics/rep/${repId}?${params.toString()}`);
  };

  const handleExportProjects = async () => {
    if (!data || !data.projects || data.projects.length === 0) return;

    setIsExporting(true);
    try {
      const exportData = data.projects.map(project => ({
        projectId: project.projectId,
        customerName: project.customerName,
        status: project.status,
        systemSize: project.systemSize,
        grossPpw: project.grossPpw,
        netPpw: project.netPpw,
        commissionablePpw: project.commissionablePpw,
        salesDate: project.salesDate || 'N/A',
        installDate: project.installDate || 'N/A',
        cycleTime: project.cycleTime ? `${project.cycleTime} days` : 'N/A',
        officeName: project.officeName || 'N/A'
      }));

      const headers = {
        projectId: 'Project ID',
        customerName: 'Customer',
        status: 'Status',
        systemSize: 'System Size (kW)',
        grossPpw: 'Gross PPW',
        netPpw: 'Net PPW',
        commissionablePpw: 'Commissionable PPW',
        salesDate: 'Sales Date',
        installDate: 'Install Date',
        cycleTime: 'Cycle Time',
        officeName: 'Office'
      };

      const filename = `${data.rep.repName.replace(/\s+/g, '-')}-projects-${new Date().toISOString().split('T')[0]}.csv`;

      exportAnalyticsToCSV(exportData, filename, headers);

      toast.success('Projects exported successfully');
    } catch (error) {
      toast.error('Failed to export projects');
      console.error('Export error:', error);
    } finally {
      setIsExporting(false);
    }
  };

  if (isLoading) {
    return <RepDetailSkeleton />;
  }

  if (error) {
    return (
      <div className="space-y-6">
        <Link href="/analytics">
          <Button variant="ghost" size="sm">
            <ArrowLeft className="h-4 w-4 mr-2" />
            Back to Analytics
          </Button>
        </Link>
        <Card>
          <CardContent className="p-6">
            <div className="bg-red-50 border border-red-200 rounded-lg p-4">
              <p className="text-red-600 font-medium">Unable to load rep details</p>
              <p className="text-red-500 text-sm mt-1">{(error as Error).message}</p>
            </div>
          </CardContent>
        </Card>
      </div>
    );
  }

  if (!data) {
    return (
      <div className="space-y-6">
        <Link href="/analytics">
          <Button variant="ghost" size="sm">
            <ArrowLeft className="h-4 w-4 mr-2" />
            Back to Analytics
          </Button>
        </Link>
        <Card>
          <CardContent className="p-6">
            <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
              <p className="text-slate-600">Rep not found</p>
            </div>
          </CardContent>
        </Card>
      </div>
    );
  }

  const { rep, projects, monthlyTrends } = data;

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-4">
          <Link href="/analytics">
            <Button variant="ghost" size="sm">
              <ArrowLeft className="h-4 w-4 mr-2" />
              Back
            </Button>
          </Link>
          <div>
            <div className="flex items-center gap-3">
              <div className="h-12 w-12 rounded-full bg-gradient-to-br from-indigo-500 to-indigo-600 flex items-center justify-center">
                <User className="h-6 w-6 text-white" />
              </div>
              <div>
                <h1 className="text-2xl font-bold text-gray-900">{rep.repName}</h1>
                <div className="flex items-center gap-2 mt-1">
                  <Badge variant={rep.role.toLowerCase() === 'closer' ? 'default' : 'secondary'}>
                    {rep.role}
                  </Badge>
                  {rep.officeName && (
                    <span className="text-sm text-gray-600">{rep.officeName}</span>
                  )}
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>

      {/* Time Range Filter */}
      <div className="flex justify-end">
        <div className="flex items-center space-x-1" role="group" aria-label="Filter by time range">
          {(['ytd', 'last_30', 'last_90', 'last_12_months'] as const).map((range, index) => {
            const isActive = timeRange === range;
            const labels = {
              ytd: 'Year to Date',
              last_30: 'Last 30 Days',
              last_90: 'Last 90 Days',
              last_12_months: 'Last 12 Months'
            };

            return (
              <Button
                key={range}
                variant={isActive ? 'default' : 'outline'}
                size="sm"
                onClick={() => handleTimeRangeChange(range)}
                className={`
                  px-4 py-2 text-sm font-medium transition-colors h-10
                  ${index === 0 ? 'rounded-l-lg' : ''}
                  ${index === 3 ? 'rounded-r-lg' : 'rounded-none'}
                  ${index !== 3 ? 'border-r-0' : ''}
                  ${isActive
                    ? 'bg-blue-600 text-white hover:bg-blue-700'
                    : 'bg-white text-gray-700 hover:bg-gray-50 border-gray-300'
                  }
                `}
              >
                {labels[range]}
              </Button>
            );
          })}
        </div>
      </div>

      {/* Key Metrics */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Total Projects</CardTitle>
            <Package className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{rep.totalProjects.toLocaleString()}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Avg System Size</CardTitle>
            <TrendingUp className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{formatSystemSize(rep.avgSystemSize)}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Avg Net PPW</CardTitle>
            <BarChart3 className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">{formatPPW(rep.avgNetPpw)}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Avg Cycle Time</CardTitle>
            <Clock className="h-4 w-4 text-muted-foreground" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold">
              {rep.avgCycleTime ? `${Math.round(rep.avgCycleTime)} days` : 'N/A'}
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Active Projects</CardTitle>
            <CheckCircle className="h-4 w-4 text-green-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-green-600">{rep.activeProjects.toLocaleString()}</div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Cancelled Projects</CardTitle>
            <XCircle className="h-4 w-4 text-red-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-red-600">{rep.cancelledProjects.toLocaleString()}</div>
            {rep.cancellationRate !== undefined && (
              <p className="text-xs text-muted-foreground mt-1">
                {formatPercentage(rep.cancellationRate)} rate
              </p>
            )}
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">On Hold</CardTitle>
            <PauseCircle className="h-4 w-4 text-orange-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-orange-600">{rep.onHoldProjects.toLocaleString()}</div>
            {rep.holdRate !== undefined && (
              <p className="text-xs text-muted-foreground mt-1">
                {formatPercentage(rep.holdRate)} rate
              </p>
            )}
          </CardContent>
        </Card>

        <Card>
          <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
            <CardTitle className="text-sm font-medium">Installs</CardTitle>
            <CheckCircle className="h-4 w-4 text-blue-600" />
          </CardHeader>
          <CardContent>
            <div className="text-2xl font-bold text-blue-600">{rep.installs.toLocaleString()}</div>
          </CardContent>
        </Card>
      </div>

      {/* Projects Table */}
      <Card>
        <CardHeader>
          <div className="flex items-center justify-between">
            <div>
              <CardTitle>Projects</CardTitle>
              <CardDescription>
                {projects.length} project{projects.length !== 1 ? 's' : ''} in selected time range
              </CardDescription>
            </div>
            <Button
              variant="outline"
              size="sm"
              onClick={handleExportProjects}
              disabled={isExporting || projects.length === 0}
            >
              {isExporting ? (
                <>
                  <FileDown className="h-4 w-4 mr-2 animate-pulse" />
                  Exporting...
                </>
              ) : (
                <>
                  <FileDown className="h-4 w-4 mr-2" />
                  Export CSV
                </>
              )}
            </Button>
          </div>
        </CardHeader>
        <CardContent>
          {projects.length === 0 ? (
            <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
              <p className="text-slate-600">No projects found for selected time range</p>
            </div>
          ) : (
            <div className="overflow-x-auto">
              <Table>
                <TableHeader>
                  <TableRow>
                    <TableHead>Project ID</TableHead>
                    <TableHead>Customer</TableHead>
                    <TableHead>Status</TableHead>
                    <TableHead className="text-right">Size</TableHead>
                    <TableHead className="text-right">Net PPW</TableHead>
                    <TableHead>Sales Date</TableHead>
                    <TableHead>Install Date</TableHead>
                    <TableHead className="text-right">Cycle Time</TableHead>
                  </TableRow>
                </TableHeader>
                <TableBody>
                  {projects.map((project) => (
                    <TableRow key={project.recordId}>
                      <TableCell className="font-medium">
                        <Link
                          href={`/projects/${project.recordId}`}
                          className="text-blue-600 hover:text-blue-800 hover:underline"
                        >
                          {project.projectId}
                        </Link>
                      </TableCell>
                      <TableCell>{project.customerName}</TableCell>
                      <TableCell>
                        <Badge variant="outline">{project.status}</Badge>
                      </TableCell>
                      <TableCell className="text-right">
                        {formatSystemSize(project.systemSize)}
                      </TableCell>
                      <TableCell className="text-right">
                        {formatPPW(project.netPpw)}
                      </TableCell>
                      <TableCell>
                        {project.salesDate ? new Date(project.salesDate).toLocaleDateString() : 'N/A'}
                      </TableCell>
                      <TableCell>
                        {project.installDate ? new Date(project.installDate).toLocaleDateString() : 'N/A'}
                      </TableCell>
                      <TableCell className="text-right">
                        {project.cycleTime ? `${project.cycleTime} days` : 'N/A'}
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}
