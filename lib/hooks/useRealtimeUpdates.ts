'use client';

import { useQueryClient } from '@tanstack/react-query';
import { useEffect, useState, useMemo } from 'react';

interface UseRealtimeUpdatesOptions {
  queryKeys: string[][];
  interval?: number;
  enabled?: boolean;
}

interface UseRealtimeUpdatesReturn {
  isRefreshing: boolean;
  manualRefresh: () => void;
}

/**
 * Hook for managing real-time updates with automatic polling and manual refresh
 * 
 * @param queryKeys - Array of query keys to invalidate (e.g., [['pc-dashboard'], ['pc-outreach']])
 * @param interval - Polling interval in milliseconds (default: 30000 = 30 seconds)
 * @param enabled - Whether to enable automatic polling (default: true)
 * @returns Object with isRefreshing state and manualRefresh function
 * 
 * @example
 * ```typescript
 * const { isRefreshing, manualRefresh } = useRealtimeUpdates([
 *   ['pc-dashboard', session?.user?.email],
 *   ['pc-outreach', session?.user?.email],
 *   ['pc-escalations', session?.user?.email]
 * ], 30000, !!session?.user);
 * ```
 */
export function useRealtimeUpdates({
  queryKeys,
  interval = 30000,
  enabled = true
}: UseRealtimeUpdatesOptions): UseRealtimeUpdatesReturn {
  const queryClient = useQueryClient();
  const [isRefreshing, setIsRefreshing] = useState(false);

  // Memoize queryKeys to prevent unnecessary interval recreation
  const memoizedQueryKeys = useMemo(() => queryKeys, [JSON.stringify(queryKeys)]);

  // Manual refresh function
  const manualRefresh = async () => {
    if (isRefreshing) return;
    
    setIsRefreshing(true);
    try {
      // Invalidate all specified query keys
      await Promise.all(
        memoizedQueryKeys.map(queryKey => 
          queryClient.invalidateQueries({ queryKey })
        )
      );
    } catch (error) {
      console.error('Error during manual refresh:', error);
    } finally {
      setIsRefreshing(false);
    }
  };

  // Set up automatic polling
  useEffect(() => {
    if (!enabled || memoizedQueryKeys.length === 0) return;

    const intervalId = setInterval(async () => {
      try {
        // Invalidate all specified query keys
        await Promise.all(
          memoizedQueryKeys.map(queryKey => 
            queryClient.invalidateQueries({ queryKey })
          )
        );
      } catch (error) {
        console.error('Error during automatic refresh:', error);
      }
    }, interval);

    return () => clearInterval(intervalId);
  }, [queryClient, memoizedQueryKeys, interval, enabled]);

  return {
    isRefreshing,
    manualRefresh
  };
}
