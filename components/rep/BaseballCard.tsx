'use client';

import { useState, useEffect } from 'react';
import { useQuery } from '@tanstack/react-query';
import { Card, CardHeader, CardTitle, CardContent } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Skeleton } from '@/components/ui/skeleton';
import { 
  User, 
  TrendingUp, 
  TrendingDown, 
  Award, 
  Target, 
  Zap, 
  Clock, 
  CheckCircle, 
  FileDown, 
  AlertCircle, 
  DoorOpen, 
  Calendar, 
  Users, 
  DollarSign, 
  BarChart3,
  Minus
} from 'lucide-react';
import { formatCurrency, formatPercentage, formatLargeNumber } from '@/lib/utils/formatters';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import { exportElementToPDF } from '@/lib/utils/pdf-export';
import { UserStatsResponse, LeaderboardResponse, LeaderboardEntry } from '@/lib/repcard/types';
import { toast } from 'sonner';
import { cn } from '@/lib/utils/cn';

export interface BaseballCardProps {
  userId: string;
  startDate?: string;
  endDate?: string;
  timeRange?: string;
  showExport?: boolean;
  className?: string;
}

export function BaseballCardSkeleton() {
  return (
    <Card className="w-full shadow-lg">
      <CardHeader className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white">
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-4">
            <Skeleton className="h-16 w-16 rounded-full bg-white/20" />
            <div>
              <Skeleton className="h-8 w-48 bg-white/20" />
              <Skeleton className="h-4 w-32 bg-white/20 mt-2" />
            </div>
          </div>
          <Skeleton className="h-8 w-24 bg-white/20" />
        </div>
        <Skeleton className="h-4 w-40 bg-white/20 mt-2" />
      </CardHeader>
      <CardContent className="p-6 space-y-6">
        {/* Volume Stats Skeleton */}
        <div>
          <Skeleton className="h-6 w-32 mb-3" />
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {Array.from({ length: 4 }).map((_, i) => (
              <div key={i} className="bg-gray-50 rounded-lg p-4">
                <Skeleton className="h-5 w-5 mb-2" />
                <Skeleton className="h-4 w-20 mb-1" />
                <Skeleton className="h-8 w-16" />
              </div>
            ))}
          </div>
        </div>

        {/* Quality Stats Skeleton */}
        <div>
          <Skeleton className="h-6 w-32 mb-3" />
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {Array.from({ length: 4 }).map((_, i) => (
              <div key={i} className="bg-gray-50 rounded-lg p-4">
                <Skeleton className="h-4 w-24 mb-1" />
                <Skeleton className="h-8 w-16 mb-1" />
                <Skeleton className="h-3 w-20" />
              </div>
            ))}
          </div>
        </div>

        {/* Efficiency Stats Skeleton */}
        <div>
          <Skeleton className="h-6 w-32 mb-3" />
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {Array.from({ length: 4 }).map((_, i) => (
              <div key={i} className="bg-gray-50 rounded-lg p-4">
                <Skeleton className="h-4 w-24 mb-1" />
                <Skeleton className="h-8 w-16" />
              </div>
            ))}
          </div>
        </div>

        {/* Rankings Skeleton */}
        <div>
          <Skeleton className="h-6 w-32 mb-3" />
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            {Array.from({ length: 2 }).map((_, i) => (
              <div key={i} className="bg-gray-50 rounded-lg p-4">
                <Skeleton className="h-4 w-20 mb-1" />
                <Skeleton className="h-12 w-16 mb-1" />
                <Skeleton className="h-3 w-24" />
              </div>
            ))}
          </div>
        </div>
      </CardContent>
    </Card>
  );
}

export default function BaseballCard({
  userId,
  startDate,
  endDate,
  timeRange,
  showExport = true,
  className
}: BaseballCardProps) {
  const [isExporting, setIsExporting] = useState(false);
  const [cardId, setCardId] = useState('');

  // Generate unique card ID for PDF export
  useEffect(() => {
    setCardId(`baseball-card-${userId}-${Date.now()}`);
  }, [userId]);

  // Default date range to current month if not provided
  const defaultStartDate = startDate || new Date(new Date().getFullYear(), new Date().getMonth(), 1).toISOString().split('T')[0];
  const defaultEndDate = endDate || new Date().toISOString().split('T')[0];

  // Query 1: User Stats
  const { data: userStats, isLoading: statsLoading, error: statsError, refetch: refetchStats } = useQuery({
    queryKey: ['baseball-card-stats', userId, defaultStartDate, defaultEndDate],
    queryFn: async (): Promise<UserStatsResponse | { hasRepcardData: false; message: string; user: any }> => {
      const response = await fetch(
        `${getBaseUrl()}/api/repcard/users/${userId}/stats?startDate=${defaultStartDate}&endDate=${defaultEndDate}`
      );
      if (!response.ok) {
        throw new Error(`Failed to fetch user stats: ${response.statusText}`);
      }
      return response.json();
    },
    enabled: !!userId,
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  // Query 2: Overall Leaderboard Rank
  const { data: overallLeaderboard } = useQuery({
    queryKey: ['baseball-card-leaderboard-overall', userId, defaultStartDate, defaultEndDate],
    queryFn: async (): Promise<LeaderboardResponse> => {
      const response = await fetch(
        `${getBaseUrl()}/api/repcard/leaderboard?metric=quality_score&timeRange=custom&startDate=${defaultStartDate}&endDate=${defaultEndDate}&limit=100`
      );
      if (!response.ok) {
        throw new Error(`Failed to fetch overall leaderboard: ${response.statusText}`);
      }
      return response.json();
    },
    enabled: !!userId && !!userStats && !('hasRepcardData' in userStats),
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  // Query 3: Office Leaderboard Rank
  const { data: officeLeaderboard } = useQuery({
    queryKey: ['baseball-card-leaderboard-office', userId, userStats?.user?.office, defaultStartDate, defaultEndDate],
    queryFn: async (): Promise<LeaderboardResponse> => {
      const response = await fetch(
        `${getBaseUrl()}/api/repcard/leaderboard?metric=quality_score&timeRange=custom&startDate=${defaultStartDate}&endDate=${defaultEndDate}&officeIds=${userStats.user.office}&limit=100`
      );
      if (!response.ok) {
        throw new Error(`Failed to fetch office leaderboard: ${response.statusText}`);
      }
      return response.json();
    },
    enabled: !!userId && !!userStats && !('hasRepcardData' in userStats) && !!userStats.user?.office,
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  // Query 4: Region Leaderboard Rank
  const { data: regionLeaderboard } = useQuery({
    queryKey: ['baseball-card-leaderboard-region', userId, userStats?.user?.office, defaultStartDate, defaultEndDate],
    queryFn: async (): Promise<LeaderboardResponse> => {
      // TODO: Implement region filtering when data model supports it
      // For now, return placeholder data
      return {
        leaderboard: [],
        metadata: {
          role: 'all',
          metric: 'quality_score',
          timeRange: 'custom',
          startDate: defaultStartDate,
          endDate: defaultEndDate,
          totalEntries: 0,
          page: 1,
          limit: 100,
          totalPages: 0,
          cached: false,
          calculatedAt: new Date().toISOString()
        }
      };
    },
    enabled: false, // Disabled until region support is implemented
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  // Query 5: QuickBase Projects Data
  const { data: quickbaseProjects } = useQuery({
    queryKey: ['baseball-card-quickbase-projects', userId, defaultStartDate, defaultEndDate],
    queryFn: async () => {
      const response = await fetch(
        `${getBaseUrl()}/api/quickbase/projects?userId=${userId}&startDate=${defaultStartDate}&endDate=${defaultEndDate}`
      );
      if (!response.ok) {
        throw new Error(`Failed to fetch QuickBase projects: ${response.statusText}`);
      }
      return response.json();
    },
    enabled: !!userId && !!userStats && !('hasRepcardData' in userStats),
    staleTime: 15 * 60 * 1000, // 15 minutes
  });

  // Helper Functions
  const findUserRank = (leaderboard: LeaderboardEntry[], userId: string): number | null => {
    const entry = leaderboard.find(entry => entry.userId === userId);
    return entry ? entry.rank : null;
  };

  const getTrendIcon = (trend?: 'up' | 'down' | 'same' | 'new') => {
    switch (trend) {
      case 'up':
        return <TrendingUp className="h-5 w-5 text-green-600" />;
      case 'down':
        return <TrendingDown className="h-5 w-5 text-red-600" />;
      case 'same':
        return <Minus className="h-5 w-5 text-gray-600" />;
      case 'new':
        return <Award className="h-5 w-5 text-blue-600" />;
      default:
        return null;
    }
  };

  const getQualityColor = (percentage: number): string => {
    if (percentage >= 90) return 'text-green-600';
    if (percentage >= 75) return 'text-yellow-600';
    return 'text-red-600';
  };

  const handleExportPDF = async () => {
    if (!cardId) return;
    
    setIsExporting(true);
    try {
      const userName = userStats && 'user' in userStats ? userStats.user.name : 'rep';
      const filename = `${userName.replace(/\s+/g, '-').toLowerCase()}-baseball-card.pdf`;
      
      await exportElementToPDF(cardId, filename, {
        scale: 2,
        orientation: 'portrait'
      });
      
      toast.success('Baseball card exported successfully!');
    } catch (error) {
      console.error('PDF export failed:', error);
      toast.error('Failed to export PDF. Please try again.');
    } finally {
      setIsExporting(false);
    }
  };

  // Loading state
  if (statsLoading) {
    return <BaseballCardSkeleton />;
  }

  // Error state
  if (statsError) {
    return (
      <Card className="w-full shadow-lg">
        <CardContent className="p-6">
          <div className="flex items-center space-x-2 text-red-600">
            <AlertCircle className="h-5 w-5" />
            <div>
              <h3 className="font-semibold">Failed to load rep data</h3>
              <p className="text-sm text-gray-600 mt-1">
                {statsError instanceof Error ? statsError.message : 'An unexpected error occurred'}
              </p>
              <Button 
                variant="outline" 
                size="sm" 
                onClick={() => refetchStats()}
                className="mt-2"
              >
                Try Again
              </Button>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  // No RepCard data
  if (userStats && 'hasRepcardData' in userStats && !userStats.hasRepcardData) {
    return (
      <Card className="w-full shadow-lg">
        <CardHeader className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white">
          <div className="flex items-center space-x-4">
            <div className="h-16 w-16 rounded-full bg-white/20 flex items-center justify-center">
              <User className="h-8 w-8 text-white" />
            </div>
            <div>
              <CardTitle className="text-2xl font-bold">
                {userStats.userName || 'Rep Profile'}
              </CardTitle>
              <div className="flex items-center gap-2 mt-1">
                <Badge variant="secondary">Rep</Badge>
                <span className="text-sm text-white/80">{userStats.officeName || 'Office'}</span>
              </div>
            </div>
          </div>
        </CardHeader>
        <CardContent className="p-6">
          <div className="flex items-center space-x-2 text-yellow-600">
            <AlertCircle className="h-5 w-5" />
            <div>
              <h3 className="font-semibold">RepCard Account Not Linked</h3>
              <p className="text-sm text-gray-600 mt-1">
                This user is not linked to RepCard. RepCard data is required to display canvassing and quality metrics.
              </p>
              <p className="text-xs text-gray-500 mt-2">
                Contact your administrator to link the RepCard account.
              </p>
            </div>
          </div>
        </CardContent>
      </Card>
    );
  }

  // Data loaded successfully
  if (!userStats || 'hasRepcardData' in userStats) {
    return null;
  }

  const { user, volumeStats, qualityStats, efficiencyStats, metadata } = userStats;
  const overallRank = overallLeaderboard ? findUserRank(overallLeaderboard.leaderboard, userId) : null;
  const officeRank = officeLeaderboard ? findUserRank(officeLeaderboard.leaderboard, userId) : null;
  const regionRank = regionLeaderboard ? findUserRank(regionLeaderboard.leaderboard, userId) : null;
  const overallTrend = overallLeaderboard?.leaderboard.find(entry => entry.userId === userId)?.trend;
  const officeTrend = officeLeaderboard?.leaderboard.find(entry => entry.userId === userId)?.trend;
  const regionTrend = regionLeaderboard?.leaderboard.find(entry => entry.userId === userId)?.trend;

  return (
    <Card id={cardId} className={cn('w-full shadow-lg', className)}>
      {/* Header Section */}
      <CardHeader className="bg-gradient-to-r from-blue-600 to-indigo-600 text-white">
        <div className="flex items-center justify-between">
          <div className="flex items-center space-x-4">
            {/* User Avatar Circle */}
            <div className="h-16 w-16 rounded-full bg-white/20 flex items-center justify-center">
              <User className="h-8 w-8 text-white" />
            </div>
            {/* User Info */}
            <div>
              <CardTitle className="text-2xl font-bold">{user.name}</CardTitle>
              <div className="flex items-center gap-2 mt-1">
                <Badge variant="secondary">{user.role}</Badge>
                <span className="text-sm text-white/80">{user.office}</span>
              </div>
            </div>
          </div>
          {/* Export Button */}
          {showExport && (
            <Button
              variant="outline"
              size="sm"
              onClick={handleExportPDF}
              disabled={isExporting}
              className="bg-white/10 hover:bg-white/20 text-white border-white/30"
            >
              {isExporting ? (
                <>
                  <FileDown className="h-4 w-4 mr-2 animate-pulse" />
                  Exporting...
                </>
              ) : (
                <>
                  <FileDown className="h-4 w-4 mr-2" />
                  Export PDF
                </>
              )}
            </Button>
          )}
        </div>
        {/* Time Range Label */}
        <p className="text-sm text-white/80 mt-2">
          {timeRange || `${defaultStartDate} to ${defaultEndDate}`}
        </p>
      </CardHeader>

      <CardContent className="p-6 space-y-6">
        {/* Volume Stats Section */}
        <div>
          <h3 className="text-lg font-semibold mb-3 flex items-center">
            <BarChart3 className="h-5 w-5 mr-2 text-blue-600" />
            Volume Stats
          </h3>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {/* Doors Knocked */}
            <div className="bg-purple-50 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <DoorOpen className="h-5 w-5 text-purple-600" />
              </div>
              <p className="text-sm text-gray-600">Doors Knocked</p>
              <p className="text-2xl font-bold text-gray-900">
                {formatLargeNumber(volumeStats.doorsKnocked)}
              </p>
            </div>
            
            {/* Appointments Set */}
            <div className="bg-blue-50 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <Calendar className="h-5 w-5 text-blue-600" />
              </div>
              <p className="text-sm text-gray-600">Appointments Set</p>
              <p className="text-2xl font-bold text-gray-900">
                {formatLargeNumber(volumeStats.appointmentsSet)}
              </p>
            </div>
            
            {/* Sales Closed */}
            <div className="bg-green-50 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <CheckCircle className="h-5 w-5 text-green-600" />
              </div>
              <p className="text-sm text-gray-600">Sales Closed</p>
              <p className="text-2xl font-bold text-gray-900">
                {formatLargeNumber(volumeStats.salesClosed)}
              </p>
            </div>
            
            {/* Revenue Generated */}
            <div className="bg-emerald-50 rounded-lg p-4">
              <div className="flex items-center justify-between mb-2">
                <DollarSign className="h-5 w-5 text-emerald-600" />
              </div>
              <p className="text-sm text-gray-600">Revenue</p>
              <p className="text-2xl font-bold text-gray-900">
                {formatCurrency(volumeStats.revenueGenerated)}
              </p>
            </div>
          </div>
        </div>

        {/* Quality Stats Section */}
        <div>
          <h3 className="text-lg font-semibold mb-3 flex items-center">
            <Target className="h-5 w-5 mr-2 text-green-600" />
            Quality Metrics
          </h3>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {/* Appointment Speed */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Appointment Speed</p>
              <p className={cn('text-2xl font-bold', getQualityColor(qualityStats.appointmentSpeed.percentage))}>
                {formatPercentage(qualityStats.appointmentSpeed.percentage)}
              </p>
              <p className="text-xs text-gray-500 mt-1">
                Avg: {qualityStats.appointmentSpeed.averageHours.toFixed(1)}h
              </p>
            </div>
            
            {/* Attachment Rate */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Power Bill Rate</p>
              <p className={cn('text-2xl font-bold', getQualityColor(qualityStats.attachmentRate.percentage))}>
                {formatPercentage(qualityStats.attachmentRate.percentage)}
              </p>
              <p className="text-xs text-gray-500 mt-1">
                {qualityStats.attachmentRate.totalAttachments} attachments
              </p>
            </div>
            
            {/* Reschedule Rate */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Reschedule Rate</p>
              <p className={cn('text-2xl font-bold', qualityStats.rescheduleRate.average < 1 ? 'text-green-600' : qualityStats.rescheduleRate.average < 2 ? 'text-yellow-600' : 'text-red-600')}>
                {qualityStats.rescheduleRate.average.toFixed(2)}
              </p>
              <p className="text-xs text-gray-500 mt-1">
                {qualityStats.rescheduleRate.totalReschedules} total
              </p>
            </div>
            
            {/* Follow-Up Consistency */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Follow-Up Rate</p>
              <p className={cn('text-2xl font-bold', getQualityColor(qualityStats.followUpConsistency.percentage))}>
                {formatPercentage(qualityStats.followUpConsistency.percentage)}
              </p>
              <p className="text-xs text-gray-500 mt-1">
                {qualityStats.followUpConsistency.totalFollowUps} follow-ups
              </p>
            </div>
          </div>
        </div>

        {/* Efficiency Stats Section */}
        <div>
          <h3 className="text-lg font-semibold mb-3 flex items-center">
            <Zap className="h-5 w-5 mr-2 text-yellow-600" />
            Efficiency Metrics
          </h3>
          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {/* Doors per Appointment */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Doors per Appt</p>
              <p className="text-2xl font-bold text-gray-900">
                {efficiencyStats.doorsPerAppointment.toFixed(1)}
              </p>
            </div>
            
            {/* Appointments per Sale */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Appts per Sale</p>
              <p className="text-2xl font-bold text-gray-900">
                {efficiencyStats.appointmentsPerSale.toFixed(1)}
              </p>
            </div>
            
            {/* Average Deal Size */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Avg Deal Size</p>
              <p className="text-2xl font-bold text-gray-900">
                {formatCurrency(efficiencyStats.averageDealSize)}
              </p>
            </div>
            
            {/* Time to Close */}
            <div className="bg-slate-50 rounded-lg p-4">
              <p className="text-sm text-gray-600 mb-1">Time to Close</p>
              <p className="text-2xl font-bold text-gray-900">
                {efficiencyStats.averageTimeToClose > 0 ? `${efficiencyStats.averageTimeToClose} days` : 'N/A'}
              </p>
            </div>
          </div>
        </div>

        {/* Rankings Section */}
        <div>
          <h3 className="text-lg font-semibold mb-3 flex items-center">
            <Award className="h-5 w-5 mr-2 text-yellow-600" />
            Leaderboard Rankings
          </h3>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {/* Overall Rank */}
            <div className="bg-gradient-to-br from-yellow-50 to-orange-50 rounded-lg p-4 border-2 border-yellow-200">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600 mb-1">Overall Rank</p>
                  <div className="flex items-center space-x-2">
                    <p className="text-3xl font-bold text-yellow-700">
                      #{overallRank || 'N/A'}
                    </p>
                    {overallTrend && getTrendIcon(overallTrend)}
                  </div>
                  <p className="text-xs text-gray-500 mt-1">
                    Out of {overallLeaderboard?.metadata.totalEntries || 0} reps
                  </p>
                </div>
                <Users className="h-10 w-10 text-yellow-600 opacity-50" />
              </div>
            </div>
            
            {/* Office Rank */}
            <div className="bg-gradient-to-br from-blue-50 to-indigo-50 rounded-lg p-4 border-2 border-blue-200">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600 mb-1">Office Rank</p>
                  <div className="flex items-center space-x-2">
                    <p className="text-3xl font-bold text-blue-700">
                      #{officeRank || 'N/A'}
                    </p>
                    {officeTrend && getTrendIcon(officeTrend)}
                  </div>
                  <p className="text-xs text-gray-500 mt-1">
                    {user.office} • {officeLeaderboard?.metadata.totalEntries || 0} reps
                  </p>
                </div>
                <Users className="h-10 w-10 text-blue-600 opacity-50" />
              </div>
            </div>

            {/* Region Rank */}
            <div className="bg-gradient-to-br from-green-50 to-emerald-50 rounded-lg p-4 border-2 border-green-200">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600 mb-1">Region Rank</p>
                  <div className="flex items-center space-x-2">
                    <p className="text-3xl font-bold text-green-700">
                      #{regionRank || 'N/A'}
                    </p>
                    {regionTrend && getTrendIcon(regionTrend)}
                  </div>
                  <p className="text-xs text-gray-500 mt-1">
                    Region • {regionLeaderboard?.metadata.totalEntries || 0} reps
                  </p>
                </div>
                <Users className="h-10 w-10 text-green-600 opacity-50" />
              </div>
            </div>
          </div>
        </div>

        {/* Metadata Footer */}
        <div className="text-xs text-gray-500 text-center pt-4 border-t">
          Last updated: {new Date(metadata.calculatedAt).toLocaleString()}
          {metadata.cached && ' • Cached data'}
        </div>
      </CardContent>
    </Card>
  );
}
