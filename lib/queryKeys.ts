/**
 * Centralized query key helpers for React Query
 * Ensures consistent query key structure across the application
 */

export const projectKey = (id: string | number) => ['project', String(id)] as const;

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
