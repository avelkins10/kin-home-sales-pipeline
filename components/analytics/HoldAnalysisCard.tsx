'use client';

import { useQuery } from '@tanstack/react-query';
import { useState } from 'react';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Skeleton } from '@/components/ui/skeleton';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { BarChart, Bar, XAxis, YAxis, Tooltip, ResponsiveContainer, Cell, LabelList } from 'recharts';
import { Pause, FileDown, ArrowUp, ArrowDown, Minus, CheckCircle, Clock, AlertTriangle } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import { exportAnalyticsToCSV } from '@/lib/utils/csv-export';
import type { HoldAnalysis } from '@/lib/types/analytics';
import type { TimeRange, CustomDateRange } from '@/lib/types/dashboard';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { toast } from 'sonner';

interface HoldAnalysisCardProps {
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
  avgResolutionDays: number | null;
  dominantSource: string;
  color: string;
}

// Loading skeleton component
function HoldAnalysisSkeleton() {
  return (
    <Card className="w-full">
      <CardHeader>
        <div className="flex items-center space-x-2">
          <Skeleton className="h-6 w-6" />
          <Skeleton className="h-6 w-32" />
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
    // Add defensive checks for undefined data
    if (!data || typeof data.count === 'undefined') {
      return null;
    }
    return (
      <div className="bg-white p-3 border border-gray-200 rounded-lg shadow-lg">
        <p className="font-medium text-gray-900">{label}</p>
        <div className="space-y-1 text-sm text-gray-600">
          <p>Count: {data.count.toLocaleString()}</p>
          <p>Percentage: {formatPercentage(data.percentage)}</p>
          <p>Avg Resolution: {data.avgResolutionDays ? `${data.avgResolutionDays} days` : 'Not resolved'}</p>
          <p>Parsed from: {data.dominantSource}</p>
        </div>
      </div>
    );
  }
  return null;
};

export function HoldAnalysisCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds,
  showComparison = false
}: HoldAnalysisCardProps) {
  const [isExporting, setIsExporting] = useState(false);

  // Fetch current period data
  const { data: currentData, isLoading, error } = useQuery({
    queryKey: ['hold-analysis', userId, role, timeRange, customDateRange, officeIds],
    queryFn: async () => {
      let url = `${getBaseUrl()}/api/analytics/hold-analysis?timeRange=${timeRange}`;
      
      if (officeIds && officeIds.length > 0) {
        url += `&officeIds=${officeIds.join(',')}`;
      }
      
      if (timeRange === 'custom' && customDateRange) {
        url += `&startDate=${customDateRange.startDate}&endDate=${customDateRange.endDate}`;
      }
      
      const response = await fetch(url);
      if (!response.ok) throw new Error('Failed to fetch hold analysis');
      return response.json();
    },
  });

  // Fetch previous period data for comparison
  const { data: previousData } = useQuery({
    queryKey: ['hold-analysis-previous', userId, role, timeRange, customDateRange, officeIds],
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
      
      let url = `${getBaseUrl()}/api/analytics/hold-analysis?timeRange=${previousTimeRange}`;
      
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
      const exportData = currentData.analysis.map((item: HoldAnalysis) => ({
        category: item.category,
        count: item.count,
        percentage: item.percentage,
        avgResolutionDays: item.avgResolutionDays || 'Not resolved'
      }));
      
      // Define headers
      const headers = {
        category: 'Reason',
        count: 'Count',
        percentage: 'Percentage',
        avgResolutionDays: 'Avg Resolution (days)'
      };
      
      // Generate filename with timestamp
      const filename = `hold-analysis-${new Date().toISOString().split('T')[0]}.csv`;
      
      // Export to CSV
      exportAnalyticsToCSV(exportData, filename, headers);
      
      toast.success('Hold analysis exported successfully');
    } catch (error) {
      toast.error('Failed to export hold analysis');
      console.error('Export error:', error);
    } finally {
      setIsExporting(false);
    }
  };

  if (isLoading) {
    return <HoldAnalysisSkeleton />;
  }

  if (error) {
    return (
      <Card className="w-full">
        <CardContent className="p-6">
          <div className="bg-red-50 border border-red-200 rounded-lg p-4">
            <p className="text-red-600 font-medium">Unable to load hold analysis</p>
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
            <p className="text-slate-600">No holds found for selected filters</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Calculate period-over-period comparison
  let comparisonAlert = null;
  if (showComparison && previousData?.metadata && currentData?.metadata) {
    const currentTotal = currentData.metadata.totalHolds || 0;
    const previousTotal = previousData.metadata.totalHolds || 0;
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
          <strong>Total Holds:</strong> {currentTotal.toLocaleString()} vs {previousTotal.toLocaleString()} 
          ({delta > 0 ? '+' : ''}{delta.toLocaleString()}, {percentageChange > 0 ? '+' : ''}{percentageChange.toFixed(1)}%)
        </AlertDescription>
      </Alert>
    );
  }

  // Prepare chart data with colors based on resolution time
  const chartData: ChartData[] = currentData.analysis.slice(0, 10).map((item: HoldAnalysis) => {
    let color = '#6b7280'; // Default gray for no resolution
    
    if (item.avgResolutionDays !== null) {
      if (item.avgResolutionDays < 7) {
        color = '#10b981'; // Green for fast resolution
      } else if (item.avgResolutionDays <= 14) {
        color = '#f59e0b'; // Yellow for medium resolution
      } else {
        color = '#ef4444'; // Red for slow resolution
      }
    }
    
    return {
      category: item.category,
      count: item.count,
      percentage: item.percentage,
      avgResolutionDays: item.avgResolutionDays,
      dominantSource: item.dominantSource,
      color
    };
  });

  const totalHolds = currentData?.metadata?.totalHolds || 0;
  const resolvedHolds = currentData?.metadata?.resolvedHolds || 0;
  const activeHolds = currentData?.metadata?.activeHolds || 0;
  const mostCommon = chartData[0];

  // Use weighted average resolution time from API
  const overallAvgResolution = currentData?.metadata?.overallAvgResolution;

  return (
    <Card className="w-full" aria-label="Hold analysis">
      <CardHeader>
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-2">
            <Pause className="h-6 w-6 text-orange-600" />
            <CardTitle>Hold Analysis</CardTitle>
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
          Top reasons for project holds and resolution times
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
            <div className="bg-orange-50 border border-orange-200 rounded-lg p-4">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm font-medium text-orange-900">Total Holds</p>
                  <p className="text-2xl font-bold text-orange-900">
                    {totalHolds.toLocaleString()} projects
                  </p>
                </div>
                <div className="text-right">
                  <p className="text-sm text-orange-700">
                    {currentData?.metadata?.officeCount || 0} office{currentData?.metadata?.officeCount !== 1 ? 's' : ''}
                  </p>
                </div>
              </div>
            </div>
            
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div className="bg-green-50 border border-green-200 rounded-lg p-4">
                <p className="text-sm font-medium text-green-900">Resolved</p>
                <p className="text-lg font-bold text-green-900">
                  {resolvedHolds.toLocaleString()} projects
                </p>
                <p className="text-sm text-green-700">
                  {totalHolds > 0 ? formatPercentage((resolvedHolds / totalHolds) * 100) : '0%'} of total
                </p>
              </div>
              
              <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-4">
                <p className="text-sm font-medium text-yellow-900">Active</p>
                <p className="text-lg font-bold text-yellow-900">
                  {activeHolds.toLocaleString()} projects
                </p>
                <p className="text-sm text-yellow-700">
                  {totalHolds > 0 ? formatPercentage((activeHolds / totalHolds) * 100) : '0%'} of total
                </p>
              </div>
            </div>
            
            {overallAvgResolution && (
              <div className="bg-blue-50 border border-blue-200 rounded-lg p-4">
                <p className="text-sm font-medium text-blue-900">Average Resolution Time</p>
                <p className="text-lg font-bold text-blue-900">
                  {overallAvgResolution} days
                </p>
                <p className="text-sm text-blue-700">Across all resolved holds</p>
              </div>
            )}
            
            {mostCommon && (
              <div className="bg-purple-50 border border-purple-200 rounded-lg p-4">
                <p className="text-sm font-medium text-purple-900">Most Common Reason</p>
                <p className="text-lg font-bold text-purple-900">{mostCommon.category}</p>
                <div className="flex items-center justify-between mt-2">
                  <p className="text-sm text-purple-700">
                    {mostCommon.count.toLocaleString()} projects ({mostCommon.percentage.toFixed(1)}%)
                  </p>
                  {mostCommon.avgResolutionDays !== null ? (
                    <div className="flex items-center space-x-1">
                      {mostCommon.avgResolutionDays < 7 ? (
                        <CheckCircle className="h-4 w-4 text-green-600" />
                      ) : mostCommon.avgResolutionDays <= 14 ? (
                        <Clock className="h-4 w-4 text-yellow-600" />
                      ) : (
                        <AlertTriangle className="h-4 w-4 text-red-600" />
                      )}
                      <span className="text-sm text-purple-700">
                        avg {mostCommon.avgResolutionDays} days
                      </span>
                    </div>
                  ) : (
                    <div className="flex items-center space-x-1">
                      <Minus className="h-4 w-4 text-gray-600" />
                      <span className="text-sm text-purple-700">Not resolved</span>
                    </div>
                  )}
                </div>
              </div>
            )}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}
