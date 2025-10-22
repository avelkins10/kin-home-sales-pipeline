'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Target, TrendingUp, TrendingDown, Minus, Trophy } from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import type { RepPerformance } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface RepBenchmarkComparisonCardProps {
  userId: string;
  userEmail: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
}

interface RepBenchmarkMetrics {
  totalProjects: number;
  avgSystemSize: number;
  avgNetPpw: number;
  firstTimePassRate: number;
  cancellationRate: number;
  holdRate: number;
}

interface RepBenchmarkData {
  your: RepBenchmarkMetrics;
  average: RepBenchmarkMetrics;
  median: RepBenchmarkMetrics;
  percentile: number;
  topPerformers: Array<{
    rank: number;
    name: string;
    totalProjects: number;
  }>;
}

function RepBenchmarkComparisonSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-48" />
        </div>
        <Skeleton className="h-4 w-64" />
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          <Skeleton className="h-24 w-full" />
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
            {Array.from({ length: 6 }).map((_, i) => (
              <Skeleton key={i} className="h-40 w-full" />
            ))}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

// Calculate benchmark statistics for reps
function calculateRepBenchmarks(
  yourRep: RepPerformance | undefined,
  allReps: RepPerformance[]
): RepBenchmarkData {
  // Your metrics
  const your: RepBenchmarkMetrics = yourRep ? {
    totalProjects: yourRep.totalProjects,
    avgSystemSize: yourRep.avgSystemSize,
    avgNetPpw: yourRep.avgNetPpw,
    firstTimePassRate: yourRep.firstTimePassRate,
    cancellationRate: yourRep.cancellationRate || 0,
    holdRate: yourRep.holdRate || 0,
  } : {
    totalProjects: 0,
    avgSystemSize: 0,
    avgNetPpw: 0,
    firstTimePassRate: 0,
    cancellationRate: 0,
    holdRate: 0,
  };

  // Calculate weighted averages (weighted by total projects)
  const totalProjects = allReps.reduce((sum, r) => sum + r.totalProjects, 0);
  const average: RepBenchmarkMetrics = {
    totalProjects: totalProjects / allReps.length,
    avgSystemSize: totalProjects > 0
      ? allReps.reduce((sum, r) => sum + r.avgSystemSize * r.totalProjects, 0) / totalProjects
      : 0,
    avgNetPpw: totalProjects > 0
      ? allReps.reduce((sum, r) => sum + r.avgNetPpw * r.totalProjects, 0) / totalProjects
      : 0,
    firstTimePassRate: totalProjects > 0
      ? allReps.reduce((sum, r) => sum + r.firstTimePassRate * r.totalProjects, 0) / totalProjects
      : 0,
    cancellationRate: totalProjects > 0
      ? allReps.reduce((sum, r) => sum + (r.cancellationRate || 0) * r.totalProjects, 0) / totalProjects
      : 0,
    holdRate: totalProjects > 0
      ? allReps.reduce((sum, r) => sum + (r.holdRate || 0) * r.totalProjects, 0) / totalProjects
      : 0,
  };

  // Calculate medians
  const median: RepBenchmarkMetrics = {
    totalProjects: calculateMedian(allReps.map(r => r.totalProjects)),
    avgSystemSize: calculateMedian(allReps.map(r => r.avgSystemSize)),
    avgNetPpw: calculateMedian(allReps.map(r => r.avgNetPpw)),
    firstTimePassRate: calculateMedian(allReps.map(r => r.firstTimePassRate)),
    cancellationRate: calculateMedian(allReps.map(r => r.cancellationRate || 0)),
    holdRate: calculateMedian(allReps.map(r => r.holdRate || 0)),
  };

  // Calculate percentile based on total projects
  const sorted = allReps.map(r => r.totalProjects).sort((a, b) => a - b);
  const rank = sorted.filter(v => v <= your.totalProjects).length;
  const percentile = allReps.length > 0 ? (rank / allReps.length) * 100 : 0;

  // Get top 3 performers
  const topPerformers = [...allReps]
    .sort((a, b) => b.totalProjects - a.totalProjects)
    .slice(0, 3)
    .map((rep, index) => ({
      rank: index + 1,
      name: rep.repName,
      totalProjects: rep.totalProjects,
    }));

  return { your, average, median, percentile, topPerformers };
}

function calculateMedian(values: number[]): number {
  if (values.length === 0) return 0;
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 0
    ? (sorted[mid - 1] + sorted[mid]) / 2
    : sorted[mid];
}

// Comparison bar component
function ComparisonBar({
  label,
  yourValue,
  avgValue,
  medianValue,
  formatter,
  lowerIsBetter = false
}: {
  label: string;
  yourValue: number;
  avgValue: number;
  medianValue: number;
  formatter: (val: number) => string;
  lowerIsBetter?: boolean;
}) {
  const vsAvg = yourValue - avgValue;
  const vsAvgPercent = avgValue > 0 ? (vsAvg / avgValue) * 100 : 0;
  const vsMedian = yourValue - medianValue;
  const vsMedianPercent = medianValue > 0 ? (vsMedian / medianValue) * 100 : 0;

  const isAboveAvg = yourValue > avgValue;
  const isGood = lowerIsBetter ? !isAboveAvg : isAboveAvg;

  return (
    <div className={`p-4 rounded-lg border-2 ${
      isGood ? 'bg-green-50 border-green-200' : 'bg-orange-50 border-orange-200'
    }`}>
      <div className="text-sm font-medium text-gray-700 mb-3">{label}</div>

      {/* Your value */}
      <div className="mb-3">
        <div className="text-xs text-gray-500 mb-1">Your Performance</div>
        <div className="text-2xl font-bold text-gray-900">{formatter(yourValue)}</div>
      </div>

      {/* vs Average */}
      <div className="flex items-center justify-between mb-2 pb-2 border-b border-gray-200">
        <div>
          <div className="text-xs text-gray-500">Role Average</div>
          <div className="text-sm font-medium text-gray-700">{formatter(avgValue)}</div>
        </div>
        <div className={`flex items-center space-x-1 text-xs font-semibold ${
          (lowerIsBetter ? vsAvg < 0 : vsAvg > 0) ? 'text-green-700' : 'text-orange-700'
        }`}>
          {Math.abs(vsAvg) < 0.01 ? (
            <>
              <Minus className="h-3 w-3" />
              <span>On par</span>
            </>
          ) : vsAvg > 0 ? (
            <>
              <TrendingUp className="h-3 w-3" />
              <span>+{Math.abs(vsAvgPercent).toFixed(1)}%</span>
            </>
          ) : (
            <>
              <TrendingDown className="h-3 w-3" />
              <span>-{Math.abs(vsAvgPercent).toFixed(1)}%</span>
            </>
          )}
        </div>
      </div>

      {/* vs Median */}
      <div className="flex items-center justify-between">
        <div>
          <div className="text-xs text-gray-500">Role Median</div>
          <div className="text-sm font-medium text-gray-700">{formatter(medianValue)}</div>
        </div>
        <div className={`flex items-center space-x-1 text-xs font-semibold ${
          (lowerIsBetter ? vsMedian < 0 : vsMedian > 0) ? 'text-green-700' : 'text-orange-700'
        }`}>
          {Math.abs(vsMedian) < 0.01 ? (
            <>
              <Minus className="h-3 w-3" />
              <span>On par</span>
            </>
          ) : vsMedian > 0 ? (
            <>
              <TrendingUp className="h-3 w-3" />
              <span>+{Math.abs(vsMedianPercent).toFixed(1)}%</span>
            </>
          ) : (
            <>
              <TrendingDown className="h-3 w-3" />
              <span>-{Math.abs(vsMedianPercent).toFixed(1)}%</span>
            </>
          )}
        </div>
      </div>
    </div>
  );
}

export function RepBenchmarkComparisonCard({
  userId,
  userEmail,
  role,
  timeRange,
  customDateRange,
  officeIds
}: RepBenchmarkComparisonCardProps) {
  // Fetch all reps in the same role
  const { data: allReps, isLoading } = useQuery<RepPerformance[]>({
    queryKey: ['rep-performance-v2', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/rep-performance?timeRange=${timeRange}`;
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch rep performance data');
      const result = await response.json();
      return result.metrics || [];
    },
  });

  if (isLoading) {
    return <RepBenchmarkComparisonSkeleton />;
  }

  if (!allReps || allReps.length === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No rep performance data available</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Find your rep data
  const yourRep = allReps.find(r => r.repEmail === userEmail);

  if (!yourRep) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No performance data found for your account</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Filter to same role
  const sameRoleReps = allReps.filter(r => r.role.toLowerCase() === yourRep.role.toLowerCase());
  const benchmarks = calculateRepBenchmarks(yourRep, sameRoleReps);
  const roleTitle = yourRep.role.charAt(0).toUpperCase() + yourRep.role.slice(1);

  return (
    <Card className="w-full" aria-label="Rep benchmark comparison">
      <CardHeader>
        <div className="flex items-center justify-between flex-wrap gap-4">
          <div className="flex items-center space-x-2">
            <Target className="h-6 w-6 text-blue-600" />
            <CardTitle>Your Performance vs {roleTitle}s</CardTitle>
          </div>
          {/* Percentile badge */}
          <div className="flex items-center space-x-2">
            <span className="text-sm text-gray-600">You&apos;re in the</span>
            <span className={`px-3 py-1 rounded-full text-sm font-bold ${
              benchmarks.percentile >= 75 ? 'bg-green-100 text-green-700' :
              benchmarks.percentile >= 50 ? 'bg-blue-100 text-blue-700' :
              benchmarks.percentile >= 25 ? 'bg-orange-100 text-orange-700' :
              'bg-red-100 text-red-700'
            }`}>
              Top {(100 - benchmarks.percentile).toFixed(0)}%
            </span>
          </div>
        </div>
        <p className="text-sm text-gray-600">
          Compare your metrics against {sameRoleReps.length} {roleTitle.toLowerCase()}{sameRoleReps.length !== 1 ? 's' : ''}
        </p>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          {/* Top Performers Section */}
          <div className="bg-gradient-to-r from-yellow-50 to-orange-50 border border-yellow-200 rounded-lg p-4">
            <div className="flex items-center space-x-2 mb-3">
              <Trophy className="h-5 w-5 text-yellow-600" />
              <h3 className="font-semibold text-gray-900">Top {roleTitle}s</h3>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
              {benchmarks.topPerformers.map((performer) => (
                <div
                  key={performer.rank}
                  className={`p-3 rounded-lg border-2 ${
                    performer.name === yourRep.repName
                      ? 'bg-blue-50 border-blue-400'
                      : 'bg-white border-gray-200'
                  }`}
                >
                  <div className="flex items-center justify-between mb-2">
                    <span className={`text-xs font-bold px-2 py-1 rounded ${
                      performer.rank === 1 ? 'bg-yellow-400 text-yellow-900' :
                      performer.rank === 2 ? 'bg-gray-300 text-gray-900' :
                      'bg-orange-300 text-orange-900'
                    }`}>
                      #{performer.rank}
                    </span>
                    {performer.name === yourRep.repName && (
                      <span className="text-xs font-semibold text-blue-700">You</span>
                    )}
                  </div>
                  <div className="text-sm font-medium text-gray-900 truncate">{performer.name}</div>
                  <div className="text-xs text-gray-600 mt-1">{performer.totalProjects} projects</div>
                </div>
              ))}
            </div>
          </div>

          {/* Benchmark Comparisons */}
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-4">
            <ComparisonBar
              label="Total Projects"
              yourValue={benchmarks.your.totalProjects}
              avgValue={benchmarks.average.totalProjects}
              medianValue={benchmarks.median.totalProjects}
              formatter={(val) => val.toLocaleString()}
            />

            <ComparisonBar
              label="Avg System Size"
              yourValue={benchmarks.your.avgSystemSize}
              avgValue={benchmarks.average.avgSystemSize}
              medianValue={benchmarks.median.avgSystemSize}
              formatter={formatSystemSize}
            />

            <ComparisonBar
              label="Avg PPW"
              yourValue={benchmarks.your.avgNetPpw}
              avgValue={benchmarks.average.avgNetPpw}
              medianValue={benchmarks.median.avgNetPpw}
              formatter={formatPPW}
            />

            <ComparisonBar
              label="First-Time Pass Rate"
              yourValue={benchmarks.your.firstTimePassRate}
              avgValue={benchmarks.average.firstTimePassRate}
              medianValue={benchmarks.median.firstTimePassRate}
              formatter={formatPercentage}
            />

            <ComparisonBar
              label="Cancellation Rate"
              yourValue={benchmarks.your.cancellationRate}
              avgValue={benchmarks.average.cancellationRate}
              medianValue={benchmarks.median.cancellationRate}
              formatter={formatPercentage}
              lowerIsBetter={true}
            />

            <ComparisonBar
              label="Hold Rate"
              yourValue={benchmarks.your.holdRate}
              avgValue={benchmarks.average.holdRate}
              medianValue={benchmarks.median.holdRate}
              formatter={formatPercentage}
              lowerIsBetter={true}
            />
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
