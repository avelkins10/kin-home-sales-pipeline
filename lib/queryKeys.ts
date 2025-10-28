/**
 * Centralized query key helpers for React Query
 * Ensures consistent query key structure across the application
 */

export const projectKey = (id: string | number) => ['project', String(id)] as const;

export const tasksKey = (projectId: string | number) => ['tasks', String(projectId)] as const;

export const projectsListKey = (
  userId: string,
  role: string,
  view?: string,
  search?: string,
  sort?: string,
  memberEmail?: string,
  ownership?: string,
  office?: string,
  setter?: string,
  closer?: string,
  withTasks?: string,
  dateFilter?: string,
  startDate?: string,
  endDate?: string
) => [
  'projects',
  userId,
  role,
  view || 'all',
  search || '',
  sort || 'default',
  memberEmail || '',
  ownership || 'all',
  office || '',
  setter || '',
  closer || '',
  withTasks || '',
  dateFilter || '',
  startDate || '',
  endDate || ''
] as const;

export const calendarEventsKey = (params?: Record<string, string | undefined>) => {
  const baseKey = ['calendar', 'events'] as const;
  if (!params) return baseKey;

  // Create a stable key by sorting params
  const sortedParams = Object.entries(params)
    .filter(([_, value]) => value !== undefined)
    .sort(([a], [b]) => a.localeCompare(b))
    .map(([key, value]) => `${key}:${value}`);

  return [...baseKey, ...sortedParams] as const;
};

/**
 * Query key for operations projects list
 * Used by operations coordinators to view their assigned projects
 */
export const operationsProjectsListKey = (
  userId: string,
  role: string,
  milestone?: string,
  status?: string,
  search?: string,
  sort?: string,
  office?: string,
  salesRep?: string
) => [
  'operations-projects',
  userId,
  role,
  milestone || 'all',
  status || 'all',
  search || '',
  sort || 'default',
  office || '',
  salesRep || ''
] as const;
