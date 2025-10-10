'use client'

// components/dashboard/NotificationsFeed.tsx
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card';
import { Skeleton } from '@/components/ui/skeleton';
import { Badge } from '@/components/ui/badge';
import { useQuery, useQueryClient } from '@tanstack/react-query';
import { formatDaysAgo } from '@/lib/utils/formatters';
import { Bell, ExternalLink } from 'lucide-react';
import { useMarkAsRead } from '@/lib/hooks/useNotifications';
import type { NotificationsSummary } from '@/lib/types/dashboard';

interface NotificationsFeedProps {
  userId: string;
  role: string;
}

export function NotificationsFeed({ userId, role }: NotificationsFeedProps) {
  const queryClient = useQueryClient();
  const markAsReadMutation = useMarkAsRead();
  
  const { data, isLoading, error } = useQuery<NotificationsSummary>({
    queryKey: ['dashboard-notifications', userId],
    queryFn: async () => {
      const response = await fetch('/api/dashboard/notifications-summary');
      if (!response.ok) {
        throw new Error('Failed to fetch notifications');
      }
      return response.json();
    },
    refetchInterval: 30000, // Poll every 30 seconds
    staleTime: 15000, // Consider stale after 15 seconds
  });

  if (isLoading) {
    return <NotificationsFeedSkeleton />;
  }

  if (error) {
    return (
      <Card>
        <CardHeader>
          <CardTitle className="text-lg font-semibold">Recent Notifications</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="text-center py-4 text-gray-500">
            <Bell className="h-8 w-8 mx-auto mb-2 text-gray-300" />
            <p className="text-sm">Unable to load notifications</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  if (!data || data.notifications.length === 0) {
    return (
      <Card>
        <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
          <CardTitle className="text-lg font-semibold">Recent Notifications</CardTitle>
          {data && data.totalUnread > 0 && (
            <Badge variant="destructive" className="text-xs">
              {data.totalUnread}
            </Badge>
          )}
        </CardHeader>
        <CardContent>
          <div className="text-center py-8 text-gray-500">
            <Bell className="h-12 w-12 mx-auto mb-4 text-gray-300" />
            <p className="text-sm">All caught up! No new notifications.</p>
          </div>
        </CardContent>
      </Card>
    );
  }

  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
        <CardTitle className="text-lg font-semibold">Recent Notifications</CardTitle>
        <div className="flex items-center space-x-2">
          {data.criticalCount > 0 && (
            <Badge variant="destructive" className="text-xs">
              {data.criticalCount} Critical
            </Badge>
          )}
          {data.totalUnread > 0 && (
            <Badge variant="secondary" className="text-xs">
              {data.totalUnread} Unread
            </Badge>
          )}
        </div>
      </CardHeader>
      <CardContent className="space-y-3">
        {data.notifications.map((notification) => {
          const isCritical = notification.priority === 'critical';
          const dotColor = isCritical ? 'bg-red-500' : notification.priority === 'normal' ? 'bg-blue-500' : 'bg-gray-400';

          return (
            <div
              key={notification.id}
              className="flex items-start space-x-3 p-3 rounded-lg hover:bg-gray-50 transition-colors cursor-pointer"
              onClick={() => {
                // Mark as read first
                markAsReadMutation.mutate(notification.id, {
                  onSuccess: () => {
                    // Invalidate dashboard notifications query to refresh counts
                    queryClient.invalidateQueries({ queryKey: ['dashboard-notifications', userId] });
                    // Then navigate if action_url exists
                    if (notification.action_url) {
                      window.location.href = notification.action_url;
                    }
                  },
                });
              }}
            >
              <div className={`w-2 h-2 rounded-full ${dotColor} mt-2 flex-shrink-0`} />
              <div className="flex-1 min-w-0">
                <div className="flex items-start justify-between">
                  <p className="text-sm font-medium text-gray-900 truncate">
                    {notification.title}
                  </p>
                  {notification.action_url && (
                    <ExternalLink className="h-3 w-3 text-gray-400 flex-shrink-0 ml-2" />
                  )}
                </div>
                <p className="text-xs text-gray-500 mt-1">
                  {formatDaysAgo(notification.created_at)}
                </p>
              </div>
            </div>
          );
        })}

        {data.hasMore && (
          <div className="pt-3 border-t border-gray-200">
            <button className="text-sm text-blue-600 hover:text-blue-800 font-medium">
              View All Notifications
            </button>
          </div>
        )}
      </CardContent>
    </Card>
  );
}

function NotificationsFeedSkeleton() {
  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
        <Skeleton className="h-6 w-40" />
        <Skeleton className="h-6 w-16" />
      </CardHeader>
      <CardContent className="space-y-3">
        {Array.from({ length: 3 }).map((_, i) => (
          <div key={i} className="flex items-start space-x-3 p-3">
            <Skeleton className="w-2 h-2 rounded-full mt-2" />
            <div className="flex-1">
              <Skeleton className="h-4 w-full mb-1" />
              <Skeleton className="h-3 w-16" />
            </div>
          </div>
        ))}
      </CardContent>
    </Card>
  );
}
