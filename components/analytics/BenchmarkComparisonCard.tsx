'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Target, TrendingUp, TrendingDown, Minus } from 'lucide-react';
import { formatSystemSize, formatPPW, formatPercentage } from '@/lib/utils/formatters';
import type { OfficeMetrics } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';

interface BenchmarkComparisonCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
}

interface BenchmarkMetrics {
  totalProjects: number;
  avgSystemSize: number;
  avgNetPpw: number;
  firstTimePassRate: number;
  avgCycleTime: number | null;
  cancellationRate: number;
}

interface BenchmarkData {
  your: BenchmarkMetrics;
  average: BenchmarkMetrics;
  median: BenchmarkMetrics;
  percentile: number; // What percentile you're in (0-100)
}

function BenchmarkComparisonSkeleton() {
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
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
          {Array.from({ length: 6 }).map((_, i) => (
            <Skeleton key={i} className="h-40 w-full" />
          ))}
        </div>
      </CardContent>
    </Card>
  );
}

// Calculate benchmark statistics
function calculateBenchmarks(
  yourOffices: OfficeMetrics[],
  allOffices: OfficeMetrics[]
): BenchmarkData {
  // Your metrics (aggregate selected offices)
  const yourTotalProjects = yourOffices.reduce((sum, o) => sum + o.totalProjects, 0);
  const your: BenchmarkMetrics = {
    totalProjects: yourTotalProjects,
    avgSystemSize: yourTotalProjects > 0
      ? yourOffices.reduce((sum, o) => sum + o.avgSystemSize * o.totalProjects, 0) / yourTotalProjects
      : 0,
    avgNetPpw: yourTotalProjects > 0
      ? yourOffices.reduce((sum, o) => sum + o.avgNetPpw * o.totalProjects, 0) / yourTotalProjects
      : 0,
    firstTimePassRate: yourTotalProjects > 0
      ? yourOffices.reduce((sum, o) => sum + o.firstTimePassRate * o.totalProjects, 0) / yourTotalProjects
      : 0,
    avgCycleTime: calculateWeightedAvgCycleTime(yourOffices),
    cancellationRate: yourTotalProjects > 0
      ? (yourOffices.reduce((sum, o) => sum + (o.cancelledProjects || 0), 0) / yourTotalProjects) * 100
      : 0
  };

  // Company average
  const companyTotalProjects = allOffices.reduce((sum, o) => sum + o.totalProjects, 0);
  const average: BenchmarkMetrics = {
    totalProjects: companyTotalProjects / allOffices.length,
    avgSystemSize: companyTotalProjects > 0
      ? allOffices.reduce((sum, o) => sum + o.avgSystemSize * o.totalProjects, 0) / companyTotalProjects
      : 0,
    avgNetPpw: companyTotalProjects > 0
      ? allOffices.reduce((sum, o) => sum + o.avgNetPpw * o.totalProjects, 0) / companyTotalProjects
      : 0,
    firstTimePassRate: companyTotalProjects > 0
      ? allOffices.reduce((sum, o) => sum + o.firstTimePassRate * o.totalProjects, 0) / companyTotalProjects
      : 0,
    avgCycleTime: calculateWeightedAvgCycleTime(allOffices),
    cancellationRate: companyTotalProjects > 0
      ? (allOffices.reduce((sum, o) => sum + (o.cancelledProjects || 0), 0) / companyTotalProjects) * 100
      : 0
  };

  // Company median
  const median: BenchmarkMetrics = {
    totalProjects: calculateMedian(allOffices.map(o => o.totalProjects)),
    avgSystemSize: calculateMedian(allOffices.map(o => o.avgSystemSize)),
    avgNetPpw: calculateMedian(allOffices.map(o => o.avgNetPpw)),
    firstTimePassRate: calculateMedian(allOffices.map(o => o.firstTimePassRate)),
    avgCycleTime: calculateMedian(allOffices.map(o => o.avgCycleTime || 0).filter(v => v > 0)),
    cancellationRate: calculateMedian(
      allOffices.map(o => o.totalProjects > 0 ? ((o.cancelledProjects || 0) / o.totalProjects) * 100 : 0)
    )
  };

  // Calculate percentile based on total projects
  const sorted = allOffices.map(o => o.totalProjects).sort((a, b) => a - b);
  const rank = sorted.filter(v => v <= yourTotalProjects).length;
  const percentile = (rank / sorted.length) * 100;

  return { your, average, median, percentile };
}

function calculateMedian(values: number[]): number {
  if (values.length === 0) return 0;
  const sorted = [...values].sort((a, b) => a - b);
  const mid = Math.floor(sorted.length / 2);
  return sorted.length % 2 === 0
    ? (sorted[mid - 1] + sorted[mid]) / 2
    : sorted[mid];
}

function calculateWeightedAvgCycleTime(offices: OfficeMetrics[]): number | null {
  const officesWithCycleTime = offices.filter(o => o.avgCycleTime !== null && o.avgCycleTime > 0);
  if (officesWithCycleTime.length === 0) return null;

  const totalProjects = officesWithCycleTime.reduce((sum, o) => sum + o.totalProjects, 0);
  if (totalProjects === 0) return null;

  return officesWithCycleTime.reduce((sum, o) => sum + (o.avgCycleTime || 0) * o.totalProjects, 0) / totalProjects;
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
  // Determine if you're above/below average
  const vsAvg = yourValue - avgValue;
  const vsAvgPercent = avgValue > 0 ? (vsAvg / avgValue) * 100 : 0;

  const vsMedian = yourValue - medianValue;
  const vsMedianPercent = medianValue > 0 ? (vsMedian / medianValue) * 100 : 0;

  // Determine status
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
          <div className="text-xs text-gray-500">Company Average</div>
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
          <div className="text-xs text-gray-500">Company Median</div>
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

export function BenchmarkComparisonCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: BenchmarkComparisonCardProps) {
  // Fetch your selected offices
  const { data: yourOffices, isLoading: isLoadingYours } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-metrics-yours', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/office-metrics?timeRange=${timeRange}`;
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch your office metrics');
      const result = await response.json();
      return result.metrics || [];
    },
  });

  // Fetch all company offices for benchmarking
  const { data: allOffices, isLoading: isLoadingAll } = useQuery<OfficeMetrics[]>({
    queryKey: ['office-metrics-all', userId, role, timeRange, customDateRange],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/office-metrics?timeRange=${timeRange}`;
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      // Don't filter by officeIds to get company-wide data
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch company office metrics');
      const result = await response.json();
      return result.metrics || [];
    },
  });

  if (isLoadingYours || isLoadingAll) {
    return <BenchmarkComparisonSkeleton />;
  }

  if (!yourOffices || yourOffices.length === 0 || !allOffices || allOffices.length === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">Unable to load benchmark comparison data</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  const benchmarks = calculateBenchmarks(yourOffices, allOffices);

  return (
    <Card className="w-full" aria-label="Benchmark comparison">
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <Target className="h-6 w-6 text-blue-600" />
            <CardTitle>Benchmark Comparison</CardTitle>
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
          Your performance vs company average and median • {yourOffices.length} office{yourOffices.length !== 1 ? 's' : ''} selected
        </p>
      </CardHeader>
      <CardContent>
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
            label="Avg Cycle Time"
            yourValue={benchmarks.your.avgCycleTime || 0}
            avgValue={benchmarks.average.avgCycleTime || 0}
            medianValue={benchmarks.median.avgCycleTime || 0}
            formatter={(val) => val > 0 ? `${Math.round(val)} days` : 'N/A'}
            lowerIsBetter={true}
          />

          <ComparisonBar
            label="Cancellation Rate"
            yourValue={benchmarks.your.cancellationRate}
            avgValue={benchmarks.average.cancellationRate}
            medianValue={benchmarks.median.cancellationRate}
            formatter={formatPercentage}
            lowerIsBetter={true}
          />
        </div>
      </CardContent>
    </Card>
  );
}
