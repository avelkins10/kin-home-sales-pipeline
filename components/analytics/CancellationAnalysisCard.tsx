'use client';

import { useQuery } from '@tanstack/react-query';
import { useState } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, Cell, LabelList } from 'recharts';
import { XCircle, FileDown, ArrowUp, ArrowDown, Minus } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import type { CancellationAnalysis } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';

interface CancellationAnalysisCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds?: number[];
  showComparison?: boolean;
}

interface ChartData {
  category: string;
  count: number;
  percentage: number;
  dominantSource: string;
  color: string;
}

// Loading skeleton component
function CancellationAnalysisSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-40" />
        </div>
        <Skeleton className="h-4 w-56" />
      </CardHeader>
      <CardContent>
        <div className="space-y-4">
          <Skeleton className="h-4 w-full" />
          <Skeleton className="h-96 w-full" />
          <Skeleton className="h-16 w-full" />
        </div>
      </CardContent>
    </Card>
  );
}

// Custom tooltip for bar chart
const CustomTooltip = ({ active, payload, label }: any) => {
  if (active && payload && payload.length) {
    const data = payload[0].payload;
    // Add defensive checks for undefined/null data
    if (!data || typeof data.count === 'undefined' || data.count === null) {
      return null;
    }
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">{label}</p>
        <div className="space-y-1 text-sm text-gray-600">
          <p>Count: {data.count.toLocaleString()}</p>
          <p>Percentage: {formatPercentage(data.percentage ?? 0)}</p>
          <p>Parsed from: {data.dominantSource ?? 'unknown'}</p>
        </div>
      </div>
    );
  }
  return null;
};

export function CancellationAnalysisCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds,
  showComparison = false
}: CancellationAnalysisCardProps) {
  const [isExporting, setIsExporting] = useState(false);

  // Fetch current period data
  const { data: currentData, isLoading, error } = useQuery({
    queryKey: ['cancellation-analysis', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/cancellation-analysis?timeRange=${timeRange}`;
      
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch cancellation analysis');
      return response.json();
    },
  });

  // Fetch previous period data for comparison
  const { data: previousData } = useQuery({
    queryKey: ['cancellation-analysis-previous', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      if (!showComparison) return null;
      
      // Calculate previous period dates
      let previousTimeRange = timeRange;
      let previousStartDate: string | undefined;
      let previousEndDate: string | undefined;
      
      const now = new Date();
      
      switch (timeRange) {
        case 'ytd':
          previousTimeRange = 'custom';
          const lastYear = now.getFullYear() - 1;
          previousStartDate = `${lastYear}-01-01`;
          // Use the same month/day as today but last year for prior YTD
          const lastYearToday = new Date(lastYear, now.getMonth(), now.getDate());
          previousEndDate = lastYearToday.toISOString().split('T')[0];
          break;
        case 'month':
          previousTimeRange = 'custom';
          const lastMonth = new Date(now.getFullYear(), now.getMonth() - 1, 1);
          const lastMonthEnd = new Date(now.getFullYear(), now.getMonth(), 0);
          previousStartDate = lastMonth.toISOString().split('T')[0];
          previousEndDate = lastMonthEnd.toISOString().split('T')[0];
          break;
        case 'custom':
          if (customDateRange) {
            const start = new Date(customDateRange.startDate);
            const end = new Date(customDateRange.endDate);
            const duration = end.getTime() - start.getTime();
            previousEndDate = new Date(start.getTime() - 1).toISOString().split('T')[0];
            previousStartDate = new Date(start.getTime() - duration).toISOString().split('T')[0];
          }
          break;
        default:
          return null;
      }
      
      let url = `${getBaseUrl()}/api/analytics/cancellation-analysis?timeRange=${previousTimeRange}`;
      
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      
      if (previousStartDate && previousEndDate) {
        url += `&startDate=${previousStartDate}&endDate=${previousEndDate}`;
      }
      
      const response = await fetch(url);
      if (!response.ok) return null;
      return response.json();
    },
    enabled: showComparison,
  });

  const handleExport = async () => {
    if (!currentData?.analysis) return;
    
    setIsExporting(true);
    try {
      // Prepare data for export
      const exportData = currentData.analysis.map((item: CancellationAnalysis) => ({
        category: item.category,
        count: item.count,
        percentage: item.percentage
      }));
      
      // Define headers
      const headers = {
        category: 'Reason',
        count: 'Count',
        percentage: 'Percentage'
      };
      
      // Generate filename with timestamp
      const filename = `cancellation-analysis-${new Date().toISOString().split('T')[0]}.csv`;
      
      // Export to CSV
      exportAnalyticsToCSV(exportData, filename, headers);
      
      toast.success('Cancellation analysis exported successfully');
    } catch (error) {
      toast.error('Failed to export cancellation analysis');
      console.error('Export error:', error);
    } finally {
      setIsExporting(false);
    }
  };

  if (isLoading) {
    return <CancellationAnalysisSkeleton />;
  }

  if (error) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load cancellation analysis</p>
            <p className="text-red-500 text-sm mt-1">{error.message}</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!currentData?.analysis || currentData.analysis.length === 0) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-slate-50 border border-slate-200 rounded-lg p-4">
            <p className="text-slate-600">No cancellations found for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Calculate period-over-period comparison
  let comparisonAlert = null;
  if (showComparison && previousData?.metadata && currentData?.metadata) {
    const currentTotal = currentData.metadata.totalCancellations || 0;
    const previousTotal = previousData.metadata.totalCancellations || 0;
    const delta = currentTotal - previousTotal;
    const percentageChange = previousTotal > 0 ? (delta / previousTotal) * 100 : 0;
    
    let trend: 'up' | 'down' | 'stable' = 'stable';
    if (Math.abs(percentageChange) > 5) {
      trend = percentageChange > 0 ? 'up' : 'down';
    }
    
    const TrendIcon = trend === 'up' ? ArrowUp : trend === 'down' ? ArrowDown : Minus;
    const trendColor = trend === 'up' ? 'text-red-600' : trend === 'down' ? 'text-green-600' : 'text-gray-600';
    const alertBg = trend === 'up' ? 'bg-red-50 border-red-200' : trend === 'down' ? 'bg-green-50 border-green-200' : 'bg-gray-50 border-gray-200';
    
    comparisonAlert = (
      <Alert className={alertBg}>
        <TrendIcon className={`h-4 w-4 ${trendColor}`} />
        <AlertDescription className={trend === 'up' ? 'text-red-800' : trend === 'down' ? 'text-green-800' : 'text-gray-800'}>
          <strong>Total Cancellations:</strong> {currentTotal.toLocaleString()} vs {previousTotal.toLocaleString()} 
          ({delta > 0 ? '+' : ''}{delta.toLocaleString()}, {percentageChange > 0 ? '+' : ''}{percentageChange.toFixed(1)}%)
        </AlertDescription>
      </Alert>
    );
  }

  // Prepare chart data with colors based on severity - filter out invalid entries
  const chartData: ChartData[] = currentData.analysis
    .slice(0, 10)
    .filter((item: CancellationAnalysis) =>
      typeof item.count !== 'undefined' &&
      typeof item.percentage !== 'undefined' &&
      item.count !== null &&
      item.percentage !== null
    )
    .map((item: CancellationAnalysis, index: number) => {
      let color = '#6b7280'; // Default gray

      // Assign colors based on category severity
      if (item.category.includes('Finance') || item.category.includes('Failed Loan')) {
        color = '#ef4444'; // Red for financial issues
      } else if (item.category.includes('Customer') || item.category.includes('Unresponsive')) {
        color = '#f59e0b'; // Orange for customer issues
      } else if (item.category.includes('Documentation') || item.category.includes('Rejected')) {
        color = '#eab308'; // Yellow for documentation issues
      } else if (item.category.includes('Property') || item.category.includes('Roof')) {
        color = '#8b5cf6'; // Purple for property issues
      } else if (item.category.includes('Permit')) {
        color = '#06b6d4'; // Cyan for permit issues
      }

      return {
        category: item.category || 'Unknown',
        count: item.count,
        percentage: item.percentage,
        dominantSource: item.dominantSource || 'unknown',
        color
      };
    });

  const totalCancellations = currentData?.metadata?.totalCancellations || 0;
  const mostCommon = chartData[0];

  return (
    <Card className="w-full" aria-label="Cancellation analysis">
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <XCircle className="h-6 w-6 text-red-600" />
            <CardTitle>Cancellation Analysis</CardTitle>
          </div>
          <Button
            variant="outline"
            size="sm"
            onClick={handleExport}
            disabled={isExporting}
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
        <p className="text-sm text-gray-600">
          Top reasons for project cancellations
        </p>
      </CardHeader>
      <CardContent>
        <div className="space-y-6">
          {/* Period-over-Period Comparison */}
          {comparisonAlert}
          
          {/* Bar Chart */}
          <div className="h-96 w-full">
            <ResponsiveContainer width="100%" height="100%">
              <BarChart
                data={chartData}
                layout="vertical"
                margin={{
                  top: 20,
                  right: 30,
                  left: 200,
                  bottom: 20,
                }}
              >
                <XAxis type="number" label={{ value: 'Count', position: 'insideBottom', offset: -5 }} />
                <YAxis 
                  type="category" 
                  dataKey="category" 
                  width={200}
                  fontSize={12}
                  tick={{ fontSize: 11 }}
                />
                <Tooltip content={<CustomTooltip />} />
                <Bar dataKey="count" radius={[0, 4, 4, 0]}>
                  {chartData.map((entry, index) => (
                    <Cell key={`cell-${index}`} fill={entry.color} />
                  ))}
                  <LabelList 
                    dataKey="percentage" 
                    position="right" 
                    formatter={(value: number) => `${value.toFixed(1)}%`}
                    fontSize={11}
                  />
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          </div>

          {/* Summary Stats */}
          <div className="space-y-4">
            <div className="bg-red-50 border border-red-200 rounded-lg p-4">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm font-medium text-red-900">Total Cancellations</p>
                  <p className="text-2xl font-bold text-red-900">
                    {totalCancellations.toLocaleString()} projects
                  </p>
                </div>
                <div className="text-right">
                  <p className="text-sm text-red-700">
                    {currentData?.metadata?.officeCount || 0} office{currentData?.metadata?.officeCount !== 1 ? 's' : ''}
                  </p>
                </div>
              </div>
            </div>
            
            {mostCommon && typeof mostCommon.count !== 'undefined' && typeof mostCommon.percentage !== 'undefined' && (
              <div className="bg-orange-50 border border-orange-200 rounded-lg p-4">
                <p className="text-sm font-medium text-orange-900">Most Common Reason</p>
                <p className="text-lg font-bold text-orange-900">{mostCommon.category}</p>
                <p className="text-sm text-orange-700">
                  {mostCommon.count.toLocaleString()} projects ({mostCommon.percentage.toFixed(1)}%)
                </p>
              </div>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
