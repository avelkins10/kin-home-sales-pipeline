'use client'

// components/dashboard/TeamActivityFeed.tsx
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Button } from '@/components/ui/button';
import { useQuery } from '@tanstack/react-query';
import { formatDaysAgo } from '@/lib/utils/formatters';
import { Activity, ArrowRight, CheckCircle, Pause, XCircle, AlertCircle, ChevronLeft, ChevronRight } from 'lucide-react';
import Link from 'next/link';
import { useState } from 'react';
import type { TeamActivityFeed, TeamActivityItem, TeamActivityType } from '@/lib/types/dashboard';

interface TeamActivityFeedProps {
  userId: string;
}

export function TeamActivityFeed({ userId }: TeamActivityFeedProps) {
  const [currentPage, setCurrentPage] = useState(0);
  const itemsPerPage = 10;
  const offset = currentPage * itemsPerPage;

  const { data, isLoading, error } = useQuery<TeamActivityFeed>({
    queryKey: ['team-activity', userId, currentPage],
    queryFn: async () => {
      const response = await fetch(`/api/dashboard/team-activity?limit=${itemsPerPage}&offset=${offset}`);
      if (!response.ok) {
        throw new Error('Failed to fetch team activity');
      }
      return response.json();
    },
    staleTime: 60000, // 1 minute
  });

  if (isLoading) {
    return <TeamActivityFeedSkeleton />;
  }

  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="text-lg font-semibold">Team Activity</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-4 text-gray-500">
            <Activity className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p className="text-sm">Unable to load team activity</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!data || data.activities.length === 0) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="text-lg font-semibold">Team Activity</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <Activity className="h-12 w-12 mx-auto mb-4 text-gray-300" />
            <p className="text-sm">No recent team activity in the last 7 days.</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader>
        <CardTitle className="text-lg font-semibold">Team Activity</CardTitle>
        <p className="text-sm text-gray-500">Recent updates from your team</p>
      </CardHeader>
      <CardContent className="space-y-3">
        {data.activities.map((activity) => (
          <Link
            key={activity.recordId}
            href={`/projects/${activity.recordId}`}
            className="flex items-start space-x-3 p-3 rounded-lg hover:bg-gray-50 transition-colors"
          >
            <div className={`p-1 rounded-full ${getActivityColor(activity.activityType)}`} data-testid={`activity-icon-${activity.activityType}`}>
              {getActivityIcon(activity.activityType)}
            </div>
            <div className="flex-1 min-w-0">
              <div className="flex items-start justify-between">
                <div className="flex-1">
                  <p className="text-sm font-medium text-gray-900">
                    {activity.activityDescription}
                  </p>
                  <p className="text-xs text-gray-500">
                    {activity.teamMemberName} • {activity.customerName}
                  </p>
                </div>
                <ArrowRight className="h-4 w-4 text-gray-400 flex-shrink-0 ml-2" />
              </div>
              <p className="text-xs text-gray-500 mt-1">
                {formatDaysAgo(activity.timestamp)}
              </p>
            </div>
          </Link>
        ))}
        
        {/* Pagination Controls */}
        {data && (data.hasMore || currentPage > 0) && (
          <div className="flex items-center justify-between pt-4 border-t">
            <div className="text-sm text-gray-500">
              Showing {offset + 1}-{Math.min(offset + itemsPerPage, data.totalCount)} of {data.totalCount} activities
            </div>
            <div className="flex items-center space-x-2">
              <Button
                variant="outline"
                size="sm"
                onClick={() => setCurrentPage(prev => Math.max(0, prev - 1))}
                disabled={currentPage === 0}
                data-testid="prev-page-button"
              >
                <ChevronLeft className="h-4 w-4" />
                Previous
              </Button>
              <Button
                variant="outline"
                size="sm"
                onClick={() => setCurrentPage(prev => prev + 1)}
                disabled={!data.hasMore}
                data-testid="next-page-button"
              >
                Next
                <ChevronRight className="h-4 w-4" />
              </Button>
            </div>
          </div>
        )}
      </CardContent>
    </Card>
  );
}

function getActivityIcon(type: TeamActivityType) {
  const iconClass = "h-5 w-5";
  
  switch (type) {
    case 'install_completed':
      return <CheckCircle className={`${iconClass} text-green-600`} />;
    case 'pto_approved':
      return <CheckCircle className={`${iconClass} text-blue-600`} />;
    case 'placed_on_hold':
      return <Pause className={`${iconClass} text-orange-600`} />;
    case 'cancelled':
      return <XCircle className={`${iconClass} text-red-600`} />;
    default:
      return <Activity className={`${iconClass} text-gray-600`} />;
  }
}

function getActivityColor(type: TeamActivityType): string {
  switch (type) {
    case 'install_completed':
      return 'text-green-600 bg-green-50';
    case 'pto_approved':
      return 'text-blue-600 bg-blue-50';
    case 'placed_on_hold':
      return 'text-orange-600 bg-orange-50';
    case 'cancelled':
      return 'text-red-600 bg-red-50';
    default:
      return 'text-gray-600 bg-gray-50';
  }
}

export function TeamActivityFeedSkeleton() {
  return (
    <Card data-testid="team-activity-skeleton">
      <CardHeader>
        <Skeleton className="h-6 w-32 mb-2" />
        <Skeleton className="h-4 w-48" />
      </CardHeader>
      <CardContent className="space-y-3">
        {Array.from({ length: 5 }).map((_, i) => (
          <div key={i} className="flex items-start space-x-3 p-3">
            <Skeleton className="w-7 h-7 rounded-full" />
            <div className="flex-1">
              <Skeleton className="h-4 w-full mb-1" />
              <Skeleton className="h-3 w-24" />
            </div>
          </div>
        ))}
      </CardContent>
    </Card>
  );
}
