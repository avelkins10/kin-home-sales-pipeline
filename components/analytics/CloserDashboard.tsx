'use client';

import { useState } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent, CardDescription } from '@/components/ui/card';
import { Table, TableHeader, TableBody, TableRow, TableHead, TableCell } from '@/components/ui/table';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { Progress } from '@/components/ui/progress';
import { 
  Calendar, 
  CheckCircle2, 
  XCircle, 
  DollarSign, 
  TrendingUp,
  Users,
  FileText,
  AlertCircle,
  ChevronDown,
  ChevronUp
} from 'lucide-react';
import { formatCurrency, formatPercentage, formatNumber } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

interface CloserDashboardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
}

interface CloserDashboardMetrics {
  closerId: string;
  closerName: string;
  closerEmail: string;
  office: string;
  appointmentsAssigned: number;
  appointmentsSat: number;
  appointmentsNoSit: number;
  sitRate: number;
  closed: number;
  followUps: number;
  otherSits: number;
  noShow: number;
  cancelled: number;
  rescheduled: number;
  noSitReasons: Record<string, number>;
  projectsLinked: number;
  avgPPW: number;
  avgSystemSize: number;
  totalRevenue: number;
}

export function CloserDashboard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: CloserDashboardProps) {
  const [expandedCloser, setExpandedCloser] = useState<string | null>(null);

  // Build query parameters
  const buildQueryParams = () => {
    const params = new URLSearchParams({
      timeRange,
    });
    
    if (customDateRange) {
      params.set('startDate', customDateRange.startDate);
      params.set('endDate', customDateRange.endDate);
    }
    
    if (officeIds && officeIds.length > 0) {
      params.set('officeIds', officeIds.join(','));
    }
    
    return params.toString();
  };

  const { data: metrics, isLoading, error } = useQuery<CloserDashboardMetrics[]>({
    queryKey: ['closer-dashboard', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = buildQueryParams();
      const response = await fetch(`${getBaseUrl()}/api/analytics/closer-dashboard?${params}`);
      if (!response.ok) {
        const errorData = await response.json().catch(() => ({}));
        throw new Error(errorData.error || 'Failed to fetch closer dashboard data');
      }
      const data = await response.json();
      // Ensure we return an array
      return Array.isArray(data) ? data : [];
    },
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Closer Performance Dashboard</CardTitle>
          <CardDescription>Comprehensive metrics combining RepCard appointments and QuickBase projects</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            {[1, 2, 3].map((i) => (
              <Skeleton key={i} className="h-32 w-full" />
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Closer Performance Dashboard</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-muted-foreground">
            <AlertCircle className="h-12 w-12 mx-auto mb-4 text-destructive" />
            <p>Failed to load closer dashboard data</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!metrics || metrics.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle>Closer Performance Dashboard</CardTitle>
          <CardDescription>Comprehensive metrics combining RepCard appointments and QuickBase projects</CardDescription>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-muted-foreground">
            <Users className="h-12 w-12 mx-auto mb-4" />
            <p>No closer data available for the selected time period</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Sort by appointments assigned (descending)
  const sortedMetrics = [...metrics].sort((a, b) => b.appointmentsAssigned - a.appointmentsAssigned);

  return (
    <Card>
      <CardHeader>
        <CardTitle className="flex items-center gap-2">
          <Users className="h-5 w-5" />
          Closer Performance Dashboard
        </CardTitle>
        <CardDescription>
          Comprehensive metrics combining RepCard appointments and QuickBase projects
        </CardDescription>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          {/* Summary Cards */}
          <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm font-medium text-muted-foreground">Total Closers</p>
                    <p className="text-2xl font-bold">{metrics.length}</p>
                  </div>
                  <Users className="h-8 w-8 text-muted-foreground" />
                </div>
              </CardContent>
            </Card>
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm font-medium text-muted-foreground">Total Appointments</p>
                    <p className="text-2xl font-bold">
                      {formatNumber(metrics.reduce((sum, m) => sum + m.appointmentsAssigned, 0))}
                    </p>
                  </div>
                  <Calendar className="h-8 w-8 text-muted-foreground" />
                </div>
              </CardContent>
            </Card>
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm font-medium text-muted-foreground">Avg. Sit Rate</p>
                    <p className="text-2xl font-bold">
                      {formatPercentage(
                        metrics.reduce((sum, m) => sum + m.sitRate, 0) / metrics.length
                      )}
                    </p>
                  </div>
                  <TrendingUp className="h-8 w-8 text-muted-foreground" />
                </div>
              </CardContent>
            </Card>
            <Card>
              <CardContent className="pt-6">
                <div className="flex items-center justify-between">
                  <div>
                    <p className="text-sm font-medium text-muted-foreground">Total Revenue</p>
                    <p className="text-2xl font-bold">
                      {formatCurrency(metrics.reduce((sum, m) => sum + m.totalRevenue, 0))}
                    </p>
                  </div>
                  <DollarSign className="h-8 w-8 text-muted-foreground" />
                </div>
              </CardContent>
            </Card>
          </div>

          {/* Closer Performance Table */}
          <div className="border rounded-lg">
            <Table>
              <TableHeader>
                <TableRow>
                  <TableHead>Closer</TableHead>
                  <TableHead className="text-right">Appointments</TableHead>
                  <TableHead className="text-right">Sat</TableHead>
                  <TableHead className="text-right">Sit Rate</TableHead>
                  <TableHead className="text-right">Closed</TableHead>
                  <TableHead className="text-right">Follow-ups</TableHead>
                  <TableHead className="text-right">No-Sits</TableHead>
                  <TableHead className="text-right">Avg PPW</TableHead>
                  <TableHead className="text-right">Avg Size</TableHead>
                  <TableHead className="text-right">Revenue</TableHead>
                  <TableHead className="w-12"></TableHead>
                </TableRow>
              </TableHeader>
              <TableBody>
                {sortedMetrics.map((closer) => {
                  const isExpanded = expandedCloser === closer.closerId;
                  const noSitReasonsArray = Object.entries(closer.noSitReasons)
                    .sort(([, a], [, b]) => b - a);

                  return (
                    <>
                      <TableRow 
                        key={closer.closerId}
                        className="cursor-pointer hover:bg-muted/50"
                        onClick={() => setExpandedCloser(isExpanded ? null : closer.closerId)}
                      >
                        <TableCell>
                          <div>
                            <div className="font-medium">{closer.closerName}</div>
                            <div className="text-sm text-muted-foreground">{closer.office}</div>
                          </div>
                        </TableCell>
                        <TableCell className="text-right font-medium">
                          {formatNumber(closer.appointmentsAssigned)}
                        </TableCell>
                        <TableCell className="text-right">
                          <div className="flex items-center justify-end gap-2">
                            <CheckCircle2 className="h-4 w-4 text-green-600" />
                            {formatNumber(closer.appointmentsSat)}
                          </div>
                        </TableCell>
                        <TableCell className="text-right">
                          <div className="flex items-center justify-end gap-2">
                            <Progress 
                              value={closer.sitRate} 
                              className="w-16 h-2"
                              indicatorClassName={closer.sitRate >= 60 ? 'bg-green-500' : closer.sitRate >= 40 ? 'bg-yellow-500' : 'bg-red-500'}
                            />
                            <span className="text-sm font-medium min-w-[3rem]">
                              {formatPercentage(closer.sitRate)}
                            </span>
                          </div>
                        </TableCell>
                        <TableCell className="text-right">
                          <Badge variant="default" className="bg-green-600">
                            {formatNumber(closer.closed)}
                          </Badge>
                        </TableCell>
                        <TableCell className="text-right">
                          <Badge variant="secondary">
                            {formatNumber(closer.followUps)}
                          </Badge>
                        </TableCell>
                        <TableCell className="text-right">
                          <div className="flex items-center justify-end gap-2">
                            <XCircle className="h-4 w-4 text-red-600" />
                            {formatNumber(closer.appointmentsNoSit)}
                          </div>
                        </TableCell>
                        <TableCell className="text-right">
                          {closer.avgPPW > 0 ? formatCurrency(closer.avgPPW) : '-'}
                        </TableCell>
                        <TableCell className="text-right">
                          {closer.avgSystemSize > 0 ? `${closer.avgSystemSize.toFixed(1)} kW` : '-'}
                        </TableCell>
                        <TableCell className="text-right font-medium">
                          {formatCurrency(closer.totalRevenue)}
                        </TableCell>
                        <TableCell>
                          <div className="flex items-center justify-center">
                            {isExpanded ? (
                              <ChevronUp className="h-4 w-4" />
                            ) : (
                              <ChevronDown className="h-4 w-4" />
                            )}
                          </div>
                        </TableCell>
                      </TableRow>
                      {isExpanded && (
                        <TableRow>
                          <TableCell colSpan={11} className="bg-muted/30">
                            <div className="p-4 space-y-4">
                              {/* Disposition Breakdown */}
                              <div>
                                <h4 className="font-semibold mb-2 flex items-center gap-2">
                                  <FileText className="h-4 w-4" />
                                  Disposition Breakdown
                                </h4>
                                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                                  <div className="flex items-center gap-2">
                                    <CheckCircle2 className="h-4 w-4 text-green-600" />
                                    <span className="text-sm">Closed: <strong>{closer.closed}</strong></span>
                                  </div>
                                  <div className="flex items-center gap-2">
                                    <TrendingUp className="h-4 w-4 text-blue-600" />
                                    <span className="text-sm">Follow-ups: <strong>{closer.followUps}</strong></span>
                                  </div>
                                  <div className="flex items-center gap-2">
                                    <Calendar className="h-4 w-4 text-yellow-600" />
                                    <span className="text-sm">Other Sits: <strong>{closer.otherSits}</strong></span>
                                  </div>
                                  <div className="flex items-center gap-2">
                                    <XCircle className="h-4 w-4 text-red-600" />
                                    <span className="text-sm">No Show: <strong>{closer.noShow}</strong></span>
                                  </div>
                                  <div className="flex items-center gap-2">
                                    <XCircle className="h-4 w-4 text-orange-600" />
                                    <span className="text-sm">Cancelled: <strong>{closer.cancelled}</strong></span>
                                  </div>
                                  <div className="flex items-center gap-2">
                                    <Calendar className="h-4 w-4 text-purple-600" />
                                    <span className="text-sm">Rescheduled: <strong>{closer.rescheduled}</strong></span>
                                  </div>
                                </div>
                              </div>

                              {/* No-Sit Reasons */}
                              {noSitReasonsArray.length > 0 && (
                                <div>
                                  <h4 className="font-semibold mb-2 flex items-center gap-2">
                                    <AlertCircle className="h-4 w-4 text-orange-600" />
                                    No-Sit Reasons
                                  </h4>
                                  <div className="flex flex-wrap gap-2">
                                    {noSitReasonsArray.map(([reason, count]) => (
                                      <Badge key={reason} variant="outline" className="text-xs">
                                        {reason}: {count}
                                      </Badge>
                                    ))}
                                  </div>
                                </div>
                              )}

                              {/* QuickBase Project Stats */}
                              <div>
                                <h4 className="font-semibold mb-2 flex items-center gap-2">
                                  <DollarSign className="h-4 w-4" />
                                  QuickBase Project Stats
                                </h4>
                                <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
                                  <div>
                                    <p className="text-sm text-muted-foreground">Projects Linked</p>
                                    <p className="text-lg font-semibold">{formatNumber(closer.projectsLinked)}</p>
                                  </div>
                                  <div>
                                    <p className="text-sm text-muted-foreground">Avg PPW</p>
                                    <p className="text-lg font-semibold">
                                      {closer.avgPPW > 0 ? formatCurrency(closer.avgPPW) : '-'}
                                    </p>
                                  </div>
                                  <div>
                                    <p className="text-sm text-muted-foreground">Avg System Size</p>
                                    <p className="text-lg font-semibold">
                                      {closer.avgSystemSize > 0 ? `${closer.avgSystemSize.toFixed(1)} kW` : '-'}
                                    </p>
                                  </div>
                                  <div>
                                    <p className="text-sm text-muted-foreground">Total Revenue</p>
                                    <p className="text-lg font-semibold">{formatCurrency(closer.totalRevenue)}</p>
                                  </div>
                                </div>
                              </div>
                            </div>
                          </TableCell>
                        </TableRow>
                      )}
                    </>
                  );
                })}
              </TableBody>
            </Table>
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

