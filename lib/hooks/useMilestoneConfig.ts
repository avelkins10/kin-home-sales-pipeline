/**
 * React Hook to fetch milestone configuration
 *
 * Fetches merged configuration from API (database overrides + JSON defaults)
 * Provides configuration for milestone-engine functions
 */

import { useQuery } from '@tanstack/react-query';
import { getBaseUrl } from '@/lib/utils/baseUrl';
import fallbackConfig from '@/lib/config/milestones.json';

export interface MilestoneConfig {
  version: string;
  lastUpdated: string;
  milestones: any[];
}

export function useMilestoneConfig() {
  const query = useQuery({
    queryKey: ['milestone-config'],
    queryFn: async (): Promise<MilestoneConfig> => {
      const response = await fetch(`${getBaseUrl()}/api/admin/milestones/config`, {
        headers: { 'Content-Type': 'application/json' }
      });

      if (!response.ok) {
        console.warn('Failed to fetch milestone config from API, using fallback');
        throw new Error('Failed to fetch milestone config');
      }

      return response.json();
    },
    staleTime: 5 * 60 * 1000, // 5 minutes
    gcTime: 10 * 60 * 1000, // 10 minutes (formerly cacheTime)
    retry: 2,
    // Use fallback config on error
    placeholderData: fallbackConfig as MilestoneConfig,
  });

  return {
    config: query.data || (fallbackConfig as MilestoneConfig),
    isLoading: query.isLoading,
    isError: query.isError,
    error: query.error,
    refetch: query.refetch,
  };
}
