'use client';

import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import type {
  Notification,
  UnreadCounts,
  NotificationsResponse,
} from '@/lib/types/notification';

const POLLING_INTERVAL = 30000; // 30 seconds

/**
 * Fetch notifications for current user
 */
async function fetchNotifications(options: {
  limit?: number;
  offset?: number;
  unreadOnly?: boolean;
  projectId?: number;
}): Promise<NotificationsResponse> {
  const params = new URLSearchParams();
  if (options.limit) params.set('limit', options.limit.toString());
  if (options.offset) params.set('offset', options.offset.toString());
  if (options.unreadOnly) params.set('unreadOnly', 'true');
  if (options.projectId) params.set('projectId', options.projectId.toString());

  const response = await fetch(
    `${getBaseUrl()}/api/notifications?${params.toString()}`
  );

  if (!response.ok) {
    throw new Error('Failed to fetch notifications');
  }

  return response.json();
}

/**
 * Fetch unread counts
 */
async function fetchUnreadCounts(): Promise<UnreadCounts> {
  const response = await fetch(`${getBaseUrl()}/api/notifications/unread-counts`);

  if (!response.ok) {
    throw new Error('Failed to fetch unread counts');
  }

  return response.json();
}

/**
 * Mark notification as read
 */
async function markAsRead(notificationId: number): Promise<void> {
  const response = await fetch(
    `${getBaseUrl()}/api/notifications/${notificationId}/read`,
    { method: 'POST' }
  );

  if (!response.ok) {
    throw new Error('Failed to mark notification as read');
  }
}

/**
 * Hook to fetch and manage notifications
 */
export function useNotifications(options: {
  limit?: number;
  offset?: number;
  unreadOnly?: boolean;
  projectId?: number;
  enablePolling?: boolean;
} = {}) {
  const { enablePolling = true, ...fetchOptions } = options;

  return useQuery({
    queryKey: ['notifications', fetchOptions],
    queryFn: () => fetchNotifications(fetchOptions),
    refetchInterval: enablePolling ? POLLING_INTERVAL : false,
    refetchIntervalInBackground: true,
    staleTime: 20000, // Consider data stale after 20 seconds
  });
}

/**
 * Hook to fetch unread counts with polling
 */
export function useUnreadCounts(enablePolling = true) {
  return useQuery({
    queryKey: ['notifications', 'unread-counts'],
    queryFn: fetchUnreadCounts,
    refetchInterval: enablePolling ? POLLING_INTERVAL : false,
    refetchIntervalInBackground: true,
    staleTime: 20000,
  });
}

/**
 * Hook to mark a notification as read
 */
export function useMarkAsRead() {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: markAsRead,
    onSuccess: () => {
      // Invalidate both notifications and unread counts
      queryClient.invalidateQueries({ queryKey: ['notifications'] });
    },
    onError: (error) => {
      console.error('Failed to mark notification as read:', error);
    },
  });
}

/**
 * Hook to get unread count for a specific project
 */
export function useProjectUnreadCount(projectId: number) {
  const { data } = useUnreadCounts();
  return data?.by_project[projectId] || 0;
}

/**
 * Hook to get total critical unread count
 */
export function useCriticalUnreadCount() {
  const { data } = useUnreadCounts();
  return data?.by_priority.critical || 0;
}
