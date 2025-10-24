'use client';

import { useState } from 'react';
import { useSession } from 'next-auth/react';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { AlertCircle, TrendingDown, TrendingUp, Calendar, Building2, Users, FileX, Clock, CheckCircle, AlertTriangle, RefreshCw } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface OfficeRejectionStats {
  officeName: string;
  officeId: number | null;
  totalSubmitted: number;
  neverRejected: number;
  totalRejections: number;
  totalFixed: number;
  stillRejected: number;
  activeApproved: number;
  firstTimePassRate: number;
  rejectionRate: number;
  avgResolutionDays: number | null;
  topRejectionReasons: Array<{ reason: string; count: number; percentage: number }>;
  closerBreakdown: Array<{
    closerName: string;
    closerEmail: string;
    submitted: number;
    rejected: number;
    stillRejected: number;
    rejectionRate: number;
  }>;
}

interface IntakeRejectionsReport {
  offices: OfficeRejectionStats[];
  summary: {
    totalOffices: number;
    totalSubmitted: number;
    totalRejected: number;
    totalStillRejected: number;
    totalResolved: number;
    overallRejectionRate: number;
    avgResolvedDays: number | null;
    avgStillRejectedDays: number | null;
    topRejectionReasons: Array<{ reason: string; count: number; percentage: number }>;
  };
  metadata: {
    startDate: string;
    endDate: string;
    generatedAt: string;
  };
}

// Helper to get date 30 days ago
function getLast30Days() {
  const today = new Date();
  const thirtyDaysAgo = new Date(today);
  thirtyDaysAgo.setDate(today.getDate() - 30);

  return {
    start: thirtyDaysAgo.toISOString().split('T')[0],
    end: today.toISOString().split('T')[0],
  };
}

export default function IntakeRejectionsReportPage() {
  const { data: session } = useSession();
  const queryClient = useQueryClient();
  const last30 = getLast30Days();

  const [startDate, setStartDate] = useState(last30.start);
  const [endDate, setEndDate] = useState(last30.end);
  const [appliedStartDate, setAppliedStartDate] = useState(last30.start);
  const [appliedEndDate, setAppliedEndDate] = useState(last30.end);
  const [expandedOffices, setExpandedOffices] = useState<Set<string>>(new Set());

  const { data, isLoading, error, refetch } = useQuery<IntakeRejectionsReport>({
    queryKey: ['intake-rejections-report', appliedStartDate, appliedEndDate],
    queryFn: async () => {
      const url = `${getBaseUrl()}/api/reports/intake-rejections?startDate=${appliedStartDate}&endDate=${appliedEndDate}`;
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch report');
      return response.json();
    },
    enabled: !!session?.user, // Allow all authenticated users with proper role
    staleTime: 30000, // Data is considered stale after 30 seconds
    gcTime: 60000, // Cache is garbage collected after 1 minute
    refetchOnMount: true, // Always refetch when component mounts
    refetchOnWindowFocus: true, // Refetch when user returns to the tab
  });

  const handleApplyDates = () => {
    setAppliedStartDate(startDate);
    setAppliedEndDate(endDate);
  };

  const handleRefresh = () => {
    refetch();
  };

  const handleQuickRange = (days: number) => {
    const today = new Date();
    const startDay = new Date(today);
    startDay.setDate(today.getDate() - days);

    const newStart = startDay.toISOString().split('T')[0];
    const newEnd = today.toISOString().split('T')[0];

    setStartDate(newStart);
    setEndDate(newEnd);
    setAppliedStartDate(newStart);
    setAppliedEndDate(newEnd);
  };

  const handlePredefinedRange = (rangeType: string) => {
    const today = new Date();
    let newStart: string;
    let newEnd: string;

    switch (rangeType) {
      case 'this_week': {
        const day = today.getDay();
        const diff = today.getDate() - day + (day === 0 ? -6 : 1); // Adjust when day is Sunday
        const monday = new Date(today.setDate(diff));
        newStart = monday.toISOString().split('T')[0];
        newEnd = new Date().toISOString().split('T')[0];
        break;
      }
      case 'last_week': {
        const day = today.getDay();
        const diff = today.getDate() - day + (day === 0 ? -6 : 1);
        const lastMonday = new Date(today.setDate(diff - 7));
        const lastSunday = new Date(today.setDate(diff - 1));
        newStart = lastMonday.toISOString().split('T')[0];
        newEnd = lastSunday.toISOString().split('T')[0];
        break;
      }
      case 'this_month': {
        const firstDay = new Date(today.getFullYear(), today.getMonth(), 1);
        newStart = firstDay.toISOString().split('T')[0];
        newEnd = new Date().toISOString().split('T')[0];
        break;
      }
      case 'last_month': {
        const firstDay = new Date(today.getFullYear(), today.getMonth() - 1, 1);
        const lastDay = new Date(today.getFullYear(), today.getMonth(), 0);
        newStart = firstDay.toISOString().split('T')[0];
        newEnd = lastDay.toISOString().split('T')[0];
        break;
      }
      case 'ytd': {
        const firstDay = new Date(today.getFullYear(), 0, 1);
        newStart = firstDay.toISOString().split('T')[0];
        newEnd = new Date().toISOString().split('T')[0];
        break;
      }
      default:
        return;
    }

    setStartDate(newStart);
    setEndDate(newEnd);
    setAppliedStartDate(newStart);
    setAppliedEndDate(newEnd);
  };

  const toggleOfficeExpansion = (officeName: string) => {
    setExpandedOffices(prev => {
      const next = new Set(prev);
      if (next.has(officeName)) {
        next.delete(officeName);
      } else {
        next.add(officeName);
      }
      return next;
    });
  };

  if (isLoading) {
    return (
      <div className="space-y-6">
        <div className="flex items-center justify-between">
          <h1 className="text-3xl font-bold">Intake Rejections Report</h1>
        </div>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          {[...Array(8)].map((_, i) => (
            <Card key={i}>
              <CardContent className="p-6">
                <Skeleton className="h-20 w-full" />
              </CardContent>
            </Card>
          ))}
        </div>
        <Skeleton className="h-96 w-full" />
      </div>
    );
  }

  if (error) {
    return (
      <div className="space-y-6">
        <h1 className="text-3xl font-bold">Intake Rejections Report</h1>
        <Card>
          <CardContent className="p-6">
            <div className="bg-red-50 border border-red-200 rounded-lg p-4">
              <p className="text-red-600 font-medium">Failed to load report</p>
              <p className="text-red-500 text-sm mt-1">{error.message}</p>
            </div>
          </CardContent>
        </Card>
      </div>
    );
  }

  if (!data) return null;

  return (
    <div className="space-y-6 p-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h1 className="text-3xl font-bold">Intake Rejections Report</h1>
          <p className="text-muted-foreground mt-1">
            Office-by-office analysis of intake rejections and common issues
          </p>
        </div>
        <Button
          onClick={handleRefresh}
          variant="outline"
          size="sm"
          disabled={isLoading}
          className="flex items-center gap-2"
        >
          <RefreshCw className={`h-4 w-4 ${isLoading ? 'animate-spin' : ''}`} />
          Refresh
        </Button>
      </div>

      {/* Date Range Filters */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Calendar className="h-5 w-5" />
            Date Range
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="flex flex-wrap gap-2">
            <Button
              variant="outline"
              size="sm"
              onClick={() => handlePredefinedRange('this_week')}
            >
              This Week
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handlePredefinedRange('last_week')}
            >
              Last Week
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handlePredefinedRange('this_month')}
            >
              This Month
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handlePredefinedRange('last_month')}
            >
              Last Month
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handlePredefinedRange('ytd')}
            >
              Year to Date
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handleQuickRange(30)}
            >
              Last 30 Days
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => handleQuickRange(90)}
            >
              Last 90 Days
            </Button>
          </div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div>
              <Label htmlFor="startDate">Start Date</Label>
              <Input
                id="startDate"
                type="date"
                value={startDate}
                onChange={(e) => setStartDate(e.target.value)}
              />
            </div>
            <div>
              <Label htmlFor="endDate">End Date</Label>
              <Input
                id="endDate"
                type="date"
                value={endDate}
                onChange={(e) => setEndDate(e.target.value)}
              />
            </div>
            <div className="flex items-end">
              <Button onClick={handleApplyDates} className="w-full">
                Apply Date Range
              </Button>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Summary Cards */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Total Offices</p>
                <p className="text-2xl font-bold mt-1">{data.summary.totalOffices}</p>
              </div>
              <Building2 className="h-8 w-8 text-blue-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Total Submitted</p>
                <p className="text-2xl font-bold mt-1">{data.summary.totalSubmitted}</p>
              </div>
              <Users className="h-8 w-8 text-green-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Total Rejected</p>
                <p className="text-2xl font-bold mt-1">{data.summary.totalRejected}</p>
              </div>
              <FileX className="h-8 w-8 text-red-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Overall Rejection Rate</p>
                <p className="text-2xl font-bold mt-1">
                  {formatPercentage(data.summary.overallRejectionRate)}
                </p>
              </div>
              <TrendingDown className="h-8 w-8 text-orange-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Still Rejected</p>
                <p className="text-2xl font-bold mt-1 text-red-600">{data.summary.totalStillRejected}</p>
              </div>
              <AlertTriangle className="h-8 w-8 text-red-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Resolved</p>
                <p className="text-2xl font-bold mt-1 text-green-600">{data.summary.totalResolved}</p>
              </div>
              <CheckCircle className="h-8 w-8 text-green-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Avg Resolution Time</p>
                <p className="text-2xl font-bold mt-1">
                  {data.summary.avgResolvedDays !== null
                    ? `${data.summary.avgResolvedDays}d`
                    : 'N/A'}
                </p>
              </div>
              <Clock className="h-8 w-8 text-blue-500" />
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardContent className="p-6">
            <div className="flex items-center justify-between">
              <div>
                <p className="text-sm text-muted-foreground">Avg Age Still Rejected</p>
                <p className="text-2xl font-bold mt-1 text-red-600">
                  {data.summary.avgStillRejectedDays !== null
                    ? `${data.summary.avgStillRejectedDays}d`
                    : 'N/A'}
                </p>
              </div>
              <AlertCircle className="h-8 w-8 text-red-500" />
            </div>
          </CardContent>
        </Card>
      </div>

      {/* Top Global Rejection Reasons */}
      <Card>
        <CardHeader>
          <CardTitle>Top Rejection Reasons (All Offices)</CardTitle>
          <CardDescription>Most common issues across all submissions</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-3">
            {data.summary.topRejectionReasons.map((reason, idx) => (
              <div key={idx} className="flex items-center justify-between">
                <div className="flex-1">
                  <div className="flex items-center gap-2">
                    <Badge variant="outline">{idx + 1}</Badge>
                    <span className="font-medium">{reason.reason}</span>
                  </div>
                  <div className="w-full bg-gray-200 rounded-full h-2 mt-2">
                    <div
                      className="bg-red-500 h-2 rounded-full"
                      style={{ width: `${Math.min(reason.percentage, 100)}%` }}
                    />
                  </div>
                </div>
                <div className="text-right ml-4">
                  <p className="font-semibold">{reason.count}</p>
                  <p className="text-sm text-muted-foreground">{formatPercentage(reason.percentage)}</p>
                </div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>

      {/* Office Breakdown */}
      <div className="space-y-4">
        <h2 className="text-2xl font-bold">Office Breakdown</h2>
        {data.offices.map((office) => (
          <Card key={office.officeName}>
            <CardHeader>
              <div className="flex items-center justify-between">
                <div>
                  <CardTitle className="flex items-center gap-2">
                    <Building2 className="h-5 w-5" />
                    {office.officeName}
                  </CardTitle>
                  <CardDescription className="mt-2">
                    {office.totalSubmitted} submitted • {office.totalRejections} rejected • {formatPercentage(office.rejectionRate)} rejection rate
                  </CardDescription>
                </div>
                <Button
                  variant="outline"
                  size="sm"
                  onClick={() => toggleOfficeExpansion(office.officeName)}
                >
                  {expandedOffices.has(office.officeName) ? 'Collapse' : 'Expand'}
                </Button>
              </div>
            </CardHeader>

            {expandedOffices.has(office.officeName) && (
              <CardContent className="space-y-6">
                {/* Office Stats */}
                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                  <div>
                    <p className="text-sm text-muted-foreground">First-Time Pass Rate</p>
                    <p className="text-lg font-semibold text-green-600">
                      {formatPercentage(office.firstTimePassRate)}
                    </p>
                  </div>
                  <div>
                    <p className="text-sm text-muted-foreground">Still Rejected</p>
                    <p className="text-lg font-semibold text-red-600">{office.stillRejected}</p>
                  </div>
                  <div>
                    <p className="text-sm text-muted-foreground">Total Fixed</p>
                    <p className="text-lg font-semibold text-blue-600">{office.totalFixed}</p>
                  </div>
                  <div>
                    <p className="text-sm text-muted-foreground">Avg Resolution Time</p>
                    <p className="text-lg font-semibold">
                      {office.avgResolutionDays !== null ? `${office.avgResolutionDays} days` : 'N/A'}
                    </p>
                  </div>
                </div>

                {/* Top Rejection Reasons for This Office */}
                <div>
                  <h4 className="font-semibold mb-3">Top Rejection Reasons</h4>
                  <div className="space-y-2">
                    {office.topRejectionReasons.map((reason, idx) => (
                      <div key={idx} className="flex items-center justify-between text-sm">
                        <span className="flex-1">{reason.reason}</span>
                        <div className="flex items-center gap-2">
                          <span className="font-medium">{reason.count}</span>
                          <Badge variant="secondary">{formatPercentage(reason.percentage)}</Badge>
                        </div>
                      </div>
                    ))}
                  </div>
                </div>

                {/* Closer Breakdown */}
                <div>
                  <h4 className="font-semibold mb-3">Closer Performance</h4>
                  <div className="overflow-x-auto">
                    <table className="w-full text-sm">
                      <thead>
                        <tr className="border-b">
                          <th className="text-left p-2">Closer</th>
                          <th className="text-right p-2">Submitted</th>
                          <th className="text-right p-2">Rejected</th>
                          <th className="text-right p-2">Still Rejected</th>
                          <th className="text-right p-2">Rejection Rate</th>
                        </tr>
                      </thead>
                      <tbody>
                        {office.closerBreakdown.map((closer) => (
                          <tr key={closer.closerEmail} className="border-b">
                            <td className="p-2">{closer.closerName}</td>
                            <td className="text-right p-2">{closer.submitted}</td>
                            <td className="text-right p-2">{closer.rejected}</td>
                            <td className="text-right p-2">
                              <span className={closer.stillRejected > 0 ? 'text-red-600 font-semibold' : ''}>
                                {closer.stillRejected}
                              </span>
                            </td>
                            <td className="text-right p-2">
                              <Badge
                                variant={closer.rejectionRate > 30 ? 'destructive' : closer.rejectionRate > 15 ? 'secondary' : 'outline'}
                              >
                                {formatPercentage(closer.rejectionRate)}
                              </Badge>
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>
              </CardContent>
            )}
          </Card>
        ))}
      </div>
    </div>
  );
}
