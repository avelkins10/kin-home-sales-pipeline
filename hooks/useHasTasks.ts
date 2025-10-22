'use client'

import { useQuery } from '@tanstack/react-query'
import { tasksKey } from '@/lib/queryKeys'

interface UseHasTasksOptions {
  projectId: string | number
  enabled?: boolean
}

/**
 * Lightweight hook to check if a project has task groups
 * Uses a minimal query to avoid loading full task data
 */
export function useHasTasks({ projectId, enabled = true }: UseHasTasksOptions) {
  return useQuery({
    queryKey: [...tasksKey(projectId), 'count'],
    queryFn: async () => {
      const response = await fetch(`/api/projects/${projectId}/tasks`)
      if (!response.ok) {
        throw new Error('Failed to fetch task count')
      }
      const taskGroups = await response.json()
      return taskGroups.length > 0
    },
    enabled: enabled && !!projectId,
    staleTime: 5 * 60 * 1000, // 5 minutes
    retry: 2,
    select: (data) => data, // Return boolean directly
  })
}
