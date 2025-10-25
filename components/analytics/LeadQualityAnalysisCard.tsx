'use client';

import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Badge } from '@/components/ui/badge';
import { Target, CheckCircle, TrendingUp, Users } from 'lucide-react';
import { formatPercentage } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { cn } from '@/lib/utils';
import { TimeRange, CustomDateRange } from '@/lib/types/dashboard';

interface LeadQualityAnalysisCardProps {
  userId: string;
  role: string;
  timeRange: TimeRange;
  customDateRange?: CustomDateRange;
  officeIds: number[];
}

// Generate mock quality metrics based on average quality score
const generateMockQualityMetrics = (avgQualityScore: number) => {
  // Use quality score as base and derive other metrics with realistic correlations
  const baseScore = Math.max(0, Math.min(100, avgQualityScore));
  
  // Show rate: correlates with quality score (higher quality = better show rates)
  const showRate = Math.max(60, Math.min(95, baseScore + (Math.random() * 20 - 10)));
  
  // Sit rate: slightly lower than show rate, also correlates with quality
  const sitRate = Math.max(50, Math.min(90, showRate - 5 + (Math.random() * 10 - 5)));
  
  // Close rate: lower than sit rate, but still correlates with quality
  const closeRate = Math.max(15, Math.min(45, (sitRate * 0.4) + (Math.random() * 10 - 5)));
  
  // Follow-up rate: high correlation with quality score
  const followUpRate = Math.max(70, Math.min(98, baseScore + (Math.random() * 15 - 7.5)));
  
  return {
    showRate: Math.round(showRate * 10) / 10,
    sitRate: Math.round(sitRate * 10) / 10,
    closeRate: Math.round(closeRate * 10) / 10,
    followUpRate: Math.round(followUpRate * 10) / 10
  };
};

const getQualityColor = (value: number, goodThreshold: number, warningThreshold: number): string => {
  if (value >= goodThreshold) return 'text-green-600';
  if (value >= warningThreshold) return 'text-yellow-600';
  return 'text-red-600';
};

export function LeadQualityAnalysisCard({
  userId,
  role,
  timeRange,
  customDateRange,
  officeIds
}: LeadQualityAnalysisCardProps) {
  // Fetch quality score data to use as base for mock metrics
  const { data: qualityData, isLoading, error } = useQuery({
    queryKey: ['lead-quality-analysis', timeRange, customDateRange, officeIds],
    queryFn: async () => {
      const params = new URLSearchParams({
        metric: 'quality_score',
        timeRange
      });
      
      if (officeIds.length > 0) {
        params.append('officeIds', officeIds.join(','));
      }
      
      if (customDateRange) {
        params.set('startDate', customDateRange.startDate);
        params.set('endDate', customDateRange.endDate);
      }

      const response = await fetch(`${getBaseUrl()}/api/repcard/leaderboard?${params}`);
      if (!response.ok) throw new Error('Failed to fetch quality data');
      return response.json();
    }
  });

  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Target className="h-5 w-5 text-purple-600" />
            Lead Quality Analysis
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8">
            <p className="text-red-600 mb-4">Failed to load quality data</p>
            <p className="text-sm text-gray-500">{error.message}</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (isLoading) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Target className="h-5 w-5 text-purple-600" />
            Lead Quality Analysis
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            {Array.from({ length: 4 }).map((_, i) => (
              <div key={i} className="space-y-2">
                <Skeleton className="h-4 w-20" />
                <Skeleton className="h-8 w-16" />
                <Skeleton className="h-3 w-12" />
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    );
  }

  // Calculate average quality score from leaderboard data
  const avgQualityScore = qualityData?.leaderboard?.reduce((sum: number, entry: any) => 
    sum + (entry.metricValue || 0), 0) / (qualityData?.leaderboard?.length || 1) || 75;

  // Generate mock quality metrics
  const qualityMetrics = generateMockQualityMetrics(avgQualityScore);

  const metrics = [
    {
      label: 'Show Rate',
      value: qualityMetrics.showRate,
      icon: CheckCircle,
      color: 'text-green-600',
      bgColor: 'bg-green-50',
      borderColor: 'border-green-200',
      goodThreshold: 85,
      warningThreshold: 75,
      target: '≥85%'
    },
    {
      label: 'Sit Rate',
      value: qualityMetrics.sitRate,
      icon: Users,
      color: 'text-blue-600',
      bgColor: 'bg-blue-50',
      borderColor: 'border-blue-200',
      goodThreshold: 80,
      warningThreshold: 70,
      target: '≥80%'
    },
    {
      label: 'Close Rate',
      value: qualityMetrics.closeRate,
      icon: Target,
      color: 'text-purple-600',
      bgColor: 'bg-purple-50',
      borderColor: 'border-purple-200',
      goodThreshold: 30,
      warningThreshold: 20,
      target: '≥30%'
    },
    {
      label: 'Follow-Up Rate',
      value: qualityMetrics.followUpRate,
      icon: TrendingUp,
      color: 'text-orange-600',
      bgColor: 'bg-orange-50',
      borderColor: 'border-orange-200',
      goodThreshold: 90,
      warningThreshold: 80,
      target: '≥90%'
    }
  ];

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Target className="h-5 w-5 text-purple-600" />
            Lead Quality Analysis
          </CardTitle>
          <Badge variant="secondary" className="bg-yellow-100 text-yellow-800">
            Coming Soon
          </Badge>
        </div>
        <p className="text-sm text-gray-600">
          Aggregate quality metrics across all reps in selected offices
        </p>
      </CardHeader>
      <CardContent>
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
          {metrics.map((metric, index) => {
            const IconComponent = metric.icon;
            const valueColor = getQualityColor(
              metric.value, 
              metric.goodThreshold, 
              metric.warningThreshold
            );
            
            return (
              <div
                key={index}
                className={`p-4 rounded-lg border ${metric.bgColor} ${metric.borderColor}`}
              >
                <div className="flex items-center gap-2 mb-2">
                  <IconComponent className={`h-4 w-4 ${metric.color}`} />
                  <span className="text-sm font-medium text-gray-700">
                    {metric.label}
                  </span>
                </div>
                <div className={`text-2xl font-bold ${valueColor}`}>
                  {formatPercentage(metric.value)}
                </div>
                <div className="text-xs text-gray-500 mt-1">
                  Target: {metric.target}
                </div>
              </div>
            );
          })}
        </div>
        <div className="mt-4 p-3 bg-yellow-50 border border-yellow-200 rounded-lg">
          <p className="text-sm text-yellow-800">
            <strong>Note:</strong> This is placeholder data. Full implementation requires aggregated quality metrics from RepCard API.
          </p>
        </div>
      </CardContent>
    </Card>
  );
}
